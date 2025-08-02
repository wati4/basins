(* analyzing continuum limit *)

 (* assumes e.g. compile-c-4 data *)
 (* returns number, biomass, spikes, sequence *)
relevant[data_, l_] :=
 With[{m = matrix[Apply[Plus, data[[1, 2]]], l]},
  Map[{#[[3]], energy[m, configuration[#[[2]]]][[1]], #[[1]], #[[2]]} &, data]]

(* given results of relevant, compute weighted mean biomass, fraction
for each number *)

(* make x a dummy variable or problems. *)

bp[data_, t_, i_] :=
 With[{s= Select[data, #[[3]] == i &]},
  With [{x =Apply[Plus, Map[First, s]]},
  {x/t, Apply[Plus, Map[#[[1]]*#[[2]] &, s]]/x}]]


bp[data_] :=
 With[{v= Union[Map[#[[3]] &, data]], t = Apply[Plus, Map[First, data]]},
  Map[bp[data,  t, #]&, v]]

allbp[r_] :=
 Table[{4.5 + i*0.5, bp[relevant[r[[i]], 4.5 + i*0.5]]},{i, 1,Length[r]}]

graphbp[r_] :=
  Apply[Show, Append[Table[ListPlot[r[[i, 2]]],{i, 1,Length[r]}],
    PlotRange ->{{0, 1},{0, -10}}]]


graphbp[r_] :=
  Show[Graphics[ListPlot[r[[1, 2]]],
    ListPlot[r[[1, 2]]],
    ListPlot[r[[8, 2]]],
    ListPlot[r[[9, 2]]],
    ListPlot[r[[12, 2]]],
    ListPlot[r[[15, 2]]],
    ListPlot[r[[19, 2]]],
    ListPlot[r[[20, 2]]]],
    PlotRange ->{{0, 1},{0, -10}}]

graphl[r_, i_] :=
  ListPlot[Map[{Log[#[[1]]], -#[[2]]} &, r[[i, 2]]],
        PlotStyle -> ColorData[1, "ColorList"][[Mod [i-1, 15] + 1 ]]]

graphall[r_] :=
  Show[Sequence @@ Table[graphl[r, i],{i, 11, 20}], PlotRange ->{{-10,
  0},{6, 10}}, AspectRatio-> 2, AxesOrigin-> {0, 6}]

fit[r_, i_] :=
  Fit[Map[{Log[#[[1]]], -#[[2]]} &, r[[i, 2]]],{1, x}, x]

graphll[r_, i_] :=
 With[{xs = Map[First, Map[{Log[#[[1]]], -#[[2]]} &, r[[i, 2]]]]},
  Plot[fit[r, i] /. x -> y,{y, Min[xs], Max[xs]},
        PlotStyle -> ColorData[1, "ColorList"][[Mod [i-1, 15] + 1 ]]]]

graphall[r_] :=
  Show[Sequence @@ Table[graphl[r, i],{i, 11, 20}], 
    Sequence @@ Table[graphll[r, i],{i, 11, 20}], 
  PlotRange ->{{-10, 0},{6, 10}}, AspectRatio-> 2, AxesOrigin-> {0,6},
  AxesLabel->{"ln p", "biomass"}]

graphd[r_] :=
  ListPlot[Table[{4.5 + 0.5*i, D[fit[r, i], x]},{i, 1,Length[r]}],
     PlotRange -> {{ 4, 15},{-0.2, 0.4}}, AxesLabel->{"L", "slope"}]
