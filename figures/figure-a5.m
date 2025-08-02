(* using stuff from model: cs-60, analysis *)

<< cs-60.m

list = cs[60]

graph[15] :=
 Show[ListPlot[Map[{#[[4]]/60, Log[#[[1]]]} &, list]],
      ListPlot[Map[{#[[4]]/60, Log[#[[2]]]} &, Select[list, #[[2]] > 0 &]],
                 PlotStyle -> Red],
      AxesLabel -> {ToExpression["b", TeXForm], "ln p"}, 
        LabelStyle ->{FontSize -> 14}
      ]
