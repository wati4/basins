(* from analysis.m *)


(*  For the continuum: counts peaks, neglects 1's *)

peaks[list_] :=Length[ Select[list, #>1 &]]

countpeaks[mx_, t_, h_, i_] :=
  Apply[Plus, Table[
        If[And[peaks[t[[j]]]== i,
	       stable[mx, configuration[t[[j]]]]], h[[j]], 0],
                    {j, 1,Length[ h]}]]

(* finds acceptable types *)

acceptable[mx_, t_] :=
  Select[t,
        And[positive[configuration[#]],
            stable[mx, configuration[#]]]&]

unacceptable[mx_, t_] :=
  Select[t,
        Not[And[positive[configuration[#]],
            stable[mx, configuration[#]]]]&]

(* from original spikes.m *)

(* given data of types, hits, find number of ok (non-negative)
  with each number of spikes *)



spikes :=
 With[{d =Table[{hits[[i]], types[[i]]},{i, 1,Length[hits]}]},
    ds =Select[d, positive[#[[2]]] &];
    Table[Apply[Plus, Map[First, Select[ds, peaks[#[[2]]]== i &]]],{i, 1, 12}]]

normalize[list_] := list/Apply[Plus, list]

(* accumulated from 30, 60, 90, etc. *)

sdata = {{30, {0.341275, 0.658689, 0.0000355685}},
   {60, {0.36179, 0.6382, 0.00001}},
   {90, {0.362084, 0.637896, 0.0000200002}}}

curve[data_, i_] :=
  Map[{#[[1]], #[[2, i]]} &, data]

graph[5] :=
 Show[ListPlot[curve[ sdata, 1], PlotStyle -> Red],
   ListPlot[curve[ sdata, 2], PlotStyle -> Blue],
   ListPlot[curve[ sdata, 3], PlotStyle -> Purple],
   AxesOrigin->{0, 0},
   PlotRange ->{{0, 90},{0, 1}}, AxesLabel->{"N", "p(s)"}]
