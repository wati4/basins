<< ../basins.m

(* mathematica version of dynamics in general N species model *)

dynamics[m_, initial_, time_] :=
 Block[{size =Length[m], r, v, v0, d},
      r =Table[1,{size}];
      v=Table[x[i][t],{i, 1, size}];
      v0 =Table[x[i][0],{i, 1, size}];
(*      Print[r, v, v0]; *) 
      NDSolve[{
        Table[D[v[[i]],t] == v[[i]](1-  m[[i]].v),{i, 1, size}], 
        v0 == initial},Table[x[i],{ i, 1, size}],
                {t, time}]]

random[mean_, scale_, m_] :=
 Block[{size =Length[m], v},
    v=Table[x[i][t],{i, 1, size}];
    ((v/. t -> 500) /. 
      dynamics[m,RandomReal[NormalDistribution[0,scale],{size}]+ mean, 500])[[1]]]

random[mean_, scale_, m_, 1000] :=
 Block[{size =Length[m], v},
    v=Table[x[i][t],{i, 1, size}];
    ((v/. t -> 1000) /. 
      dynamics[m,RandomReal[NormalDistribution[0,scale],{size}]+ mean,
 1000])[[1]]]


cutoff[list_, cutoff_:0.00001] :=
 Table[If[Abs[list[[i]]] > cutoff, list[[i]], 0],{i, 1,Length[list]}]

count[list_] := Length[Select[list, #> 0 &]]

(* gets locations where list is nonzero *)
locations[list_]:=
  Position[Map[#> 0 &, list], True]

(* finds bad elements of a subset pushed negative *)

negative[m_, list_] :=
 Block[{ss= subset[m, list]},
    Inverse[ss].Table[1,{Length[list]}]]

(* try converging *)
(* only run for 100 steps at a time to hopefully avoid crashing *)

random[initial_, m_] :=
 Block[{size =Length[m], v},
    v=Table[x[i][t],{i, 1, size}];
    ((v/. t -> 500) /. 
      dynamics[m, initial, 500])[[1]]]

(* note: sometimes problematic and Length[p] = 0, terminate if so *)

converge[initial_, m_, previous_:Table[1,{Length[m]}]] :=
 Block[{rr = random[initial, m],p},
  p = Apply[Join, locations[cutoff[rr]]];
(*  Print[Length[p], ":", p]; *) 
  If[p =={}, previous, If[stable[m, p], p, converge[rr, m, p]]]]

init[mean_, scale_, size_] :=
  Map[Abs, RandomReal[NormalDistribution[0,scale],{size}]+ mean]
