(* sets up a regular diagonal matrix *)
(* from niches/math/diagonal.m *)

<< ../basins.m
<< dynamics.m

diagonal[size_, a1_, ak_] :=
 Table[Table[If[i == j, a1 + (i -1)*(ak-a1)/(size -1), 2],
  {i, 1, size}],{j, 1, size}]

(* uses dynamics code from dynamics.m (formerly multiple.m) *)

experiment[1, n_: 100] := 
 Block[{r, answers ={}},
  For[j = 1, j <= n, j += 1,
    r = converge[init[0, 1, 20], diagonal[20, 0.5, 2]];
(*    Print[j, ":", r[[1]], "(", Length[r], ")"]; *) 
    If[Length[r] == 1,answers =Append[answers, r[[1]]]]];
(*    {Length[r], If[Length[r] == 1, r[[1]],{}]} >>> "experiment-diagonal-20.m"; *) 
    Sort[Tally[answers]] >>> "tally-diagonal-20.m"]

(* extends a tally to include all numbers to limit *)

extend[t_, n_] :=
 Table[With[{p = Position[Map[First, t], i]},
        If[p =={},{i, 0}, t[[p[[1, 1]]]]]],{i, 1, n}]

(* make a figure of probabilities for domains of diagonal 20 matrix *)

figure[20, tally_] :=
  With[{ds=Table[diagonal[20, 0.5, 2][[i, i]],{i, 1, 20}],
        x = extend[tally, 20]},
    t = Map[#[[2]] &, x]/Apply[Plus, x][[2]];
    ListPlot[Table[{1/ds[[i]], t[[i]]},{i, 1, 20}]]]

