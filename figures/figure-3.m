(* following log file
/wati/drop/ecology/projects/niches/math/logs/24-2-13.txt
and limit.m
 *)

<< ../basins.m

<< limit.m

<< compile-c-4.m

answers := answers = allbp [results4];

graphall[r_] :=
  Show[Sequence @@ Table[graphl[r, i],{i, 11, 20}], 
    Sequence @@ Table[graphll[r, i],{i, 11, 20}], 
  PlotRange ->{{-10, 0},{6, 10}}, AspectRatio-> 2, AxesOrigin-> {0,6},
  AxesLabel->{"ln p", "biomass"}, 
        LabelStyle ->{FontSize -> 14}]


graphd[r_] :=
  ListPlot[Table[{4.5 + 0.5*i, D[fit[r, i], x]},{i, 1,Length[r]}],
     PlotRange -> {{ 4, 15},{-0.2, 0.4}}, AxesLabel->{"L", "slope"}, 
        LabelStyle ->{FontSize -> 14}]


figure[3, 3] := Export["continuum-slopes.pdf", graphall[answers]]

figure[3, 2] := Export["slopes.pdf", graphd[answers]]

(* from spikes.m *)

(* accumulated from 30, 60, 90, etc. *)

sdata = {{30, {0.341275, 0.658689, 0.0000355685}},
   {60, {0.36179, 0.6382, 0.00001}},
   {90, {0.362084, 0.637896, 0.0000200002}}}

curve[data_, i_] :=
  Map[{#[[1]], #[[2, i]]} &, data]

graph[3, 1] :=
 Show[ListPlot[curve[ sdata, 1], PlotStyle -> Red],
   ListPlot[curve[ sdata, 2], PlotStyle -> Blue],
   ListPlot[curve[ sdata, 3], PlotStyle -> Purple],
   AxesOrigin->{0, 0},
   PlotRange ->{{0, 90},{0, 1}}, AxesLabel->{"S", "p(s)"}, 
   Ticks ->{{30, 60, 90}, Automatic},
        LabelStyle ->{FontSize -> 14}]
