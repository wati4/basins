(* figure a4 *)

<< ../model.m

<< 60-all.m

<< 60-sorted.m

<< ../basins.m



(* now looks at probabilities predicted and actual, uses 60-all *)

normalized[data_] :=
 With[{total = N[Apply[Plus, Map[First, data]]]},
  Table[{data[[i,1]]/total,           data[[i, 2]]},
    {i, 1,Length[data]}]]

ncomplete[n_] :=
 Table[{E^probs[n][[i]], allsolutions[n][[ i]]},{i, 1,Length[probs[n]]}]

nd :=nd = normalized[distribution[60]] ;
nc :=nc = Reverse[Sort [ncomplete[60]]];

identify[data_, sample_] :=
 With[{x =Select[data, #[[2]] == sample &]},
  If[x =={}, 0, x[[1, 1]]]]

merge[all_, data_] :=
 Table[{all[[i, 1]], identify[data, all[[i, 2]]], all[[i, 2]]},{i, Length[all]}]


nc = Sort [ncomplete[60]];

nd =normalized[distribution[60]] ;

mm = merge[nc, nd];

graph[14] := ListPlot[Map[Log[Drop[#, -1]] &, mm],
                PlotRange -> {{-15, 0},{-15, 0}},
                AxesLabel ->{"ln p (observed)",   "ln p (predicted)"}, 
        LabelStyle ->{FontSize -> 12}, AspectRatio -> 1]
