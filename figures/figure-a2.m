(* figures for appendix 2 *)

(* depends on all-solutions  (updated version) *)
(* convention add 10 for appendix *)
<< all-solutions.m

graph[12] :=
 ListPlot[logtable, AxesLabel ->
        {ToExpression["S", TeXForm],
         "ln # solutions"}, 
        LabelStyle ->{FontSize -> 14}]

graph[12] :=
 Show[ListPlot[logtable, AxesLabel ->
        {ToExpression["S", TeXForm],
         "ln # solutions"}, 
        LabelStyle ->{FontSize -> 14}, PlotRange ->{{20, 90}, {0, 20}}],
      Plot[x Log[2.503]/4.85,{x, 20, 90}, PlotStyle -> Black],
     ListPlot[{{30,Log[281]}}, PlotStyle -> Red]]
      
figure[12] := Export["figure-a2.pdf",graph[12]]

(* convergence as s increases *)

(* uses stuff from model/model.m *)

(* showing convergence *)

tablex[f_] :={f@1, f@3, f@4, f@5, f@6}

(* needs wm2.m  *)
<< wm2.m
<< ../model.m

graph[12, 1] :=
  Show[ListPlot[
        tablex[{#-0.14, -Log[predict[{#}, 1, wm2[[1]]]]} &], 
          PlotStyle -> Red, PlotRange ->{{0, 6.1},{0, 5.5}},
        AxesLabel -> {ToExpression["g", TeXForm],
                ToExpression["\\alpha(g)", TeXForm]}, 
        LabelStyle ->{FontSize -> 14}],
       ListPlot[
        tablex[{# -0.07, -Log[predict[{#}, 1, wm2[[2]]]]} &],
  PlotStyle -> Purple], 
       ListPlot[
        tablex[{#,-Log[predict[{#}, 1, wm2[[2]]]]}&], PlotStyle ->
  Blue, PlotRange ->{{0, 6.1},{0, 5.5}}],
    PlotRange ->{{0, 6.1},{0, 5.5}}]

figure[12, 1] := Export["figure-a2a.pdf",graph[12, 1]]

(* figure b *)
(* needs w90 *)
<< w90.m

sequences ={{6, 6, 6, 6, 6, 6},{5, 5, 5, 6, 6, 6},{5, 5, 5, 5, 5, 5},
        {4, 5, 5, 6, 5, 5},{3, 5, 5, 6, 6, 5},{5, 5, 5, 5, 5, 3}}

graph[12, 2] :=
  Show[ListPlot[Table[{i- 0.07*5, -Log[predict[sequences[[i]],
          1,wm[90][[1]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[1, 0, 0], PlotRange ->{{0, 6.1},{0, 9}}],
       ListPlot[Table[{i- 0.07*4, -Log[predict[sequences[[i]],
          2,wm[90][[2]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[0.8, 0, 0.2]],
       ListPlot[Table[{i- 0.07*3, -Log[predict[sequences[[i]],
          3,wm[90][[3]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[0.6, 0, 0.4]],
       ListPlot[Table[{i- 0.07*2, -Log[predict[sequences[[i]],
          4,wm[90][[4]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[0.4, 0, 0.6]],
       ListPlot[Table[{i- 0.07*1, -Log[predict[sequences[[i]],
          5,wm[90][[ 5]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[0.2, 0, 0.8]],
       ListPlot[Table[{i- 0.07*0, -Log[predict[sequences[[i]],
          6,wm[90][[6]]]]},{i, 1,Length[sequences]}],
          PlotStyle -> RGBColor[0.0, 0, 1.0]],
        AxesLabel->{"", "-ln p"},
        Ticks ->{{{1, "666666"},{2, "555666"},
        {3, "555555"},{4, "455655"},{5, "355665"},{6, "555553"}}}, 
        LabelStyle ->{FontSize -> 14}
]

figure[12, 2] := Export["figure-a2b.pdf",graph[12, 2]]
