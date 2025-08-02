(* from ../model/graphs.m *)

(* uses data from model *)

<< all-solutions-p.m


statistics[samples_] :=
 With[{s= Map[#[[1, 1]]*E^#[[1, 2]] &, samples]},
    total = Apply[Plus, s];
(* broken *)
    t = Map[{#[[1, 1]], E^#[[1, 2]]/total}&, samples];
    m0 = Apply[Plus, Map[#[[1]]*#[[2]] Log[#[[2]]]&, t]];
    v0 = Apply[Plus, Map[#[[1]]*#[[2]] Log[#[[2]]]^2&, t]] -m0^2;
    s1 = Apply[Plus, Map[First, t]];
    m = Apply[Plus, Map[#[[1]] Log[#[[2]]]&, t]]/s1;
    v = Apply[Plus, Map[#[[1]] Log[#[[2]]]^2&, t]]/s1 - (m/s1)^2;
      (*     l = Apply[Plus, Map[#[[1, 1]] Length[#[[2]]]&, samples]]/s1; *)
    {m0, v0, m, v, m0-v0}]

graph[13] :=
  With[{ss= Map[statistics[#[[2]]] &, sp]},
    Show[
	 Plot[-0.1415 x,{x, 10, 90}, PlotStyle -> Black],
        ListPlot[Table[{x + 19, ss[[x, 1]]},{x, 1, 71}]],
(*       Plot[-0.353 x,{x, 10, 90}, PlotStyle -> Black],  *)
	(* 337 is without going to lattice *)
	(* -0.2408: apparent answer from theory *)
	(* -0.262: apparent answer from sequences -- test5*)
	(*  -0.2583 using proper gap average test6*)
	Plot[-0.2583 x,{x, 10, 90}, PlotStyle -> Black],
        ListPlot[Table[{x + 19, ss[[x, 3]]},{x, 1, 71}], PlotStyle ->
        Red],
	Plot[0.048 x,{x, 10, 90}, PlotStyle -> Black],
        ListPlot[Table[{x + 19, ss[[x, 2]]},{x, 1, 71}], PlotStyle ->
        Purple],
	PlotRange ->{{0, 90},{-25, 8}}, AxesLabel->{ToExpression["S", TeXForm], "ln p, variance"},
	AxesOrigin->{0, 0}, 
        LabelStyle ->{FontSize -> 13}
]]

figure[13] := Export["figure-a3.pdf",graph[13]]
