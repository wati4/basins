(* produce random matrices and test for solutions *)

(* stable[m]: stable equilibria *)

(* creates a random matrix with diagonal -1, connectivity c, variance sigma^2 *)


random[n_, c_, sigma_, mean_] :=
 Table[If[i == j, -1, 
        If[Random[]< c, mean +RandomVariate[NormalDistribution[0, sigma]], 0]],
        {i, 1, n},{j, 1, n}]

symmetric[matrix_] :=
 Table[If[i < j, matrix[[j, i]], matrix[[i, j]]],
        {i, 1, Length[matrix]},{j, 1, Length[matrix]}]
 
(* from multiple.m *)


(* first let's identify which subsets have good equilibria *)

subset[m_, set_] :=
 Table[m[[set[[i]],  set[[j]]]],{ i, 1,Length[set]},{j, 1,Length[set]}]

(* is there a good equilibrium? *)

positive[list_] :=Apply[And, Map[#> 0 &, list]]

(* finds stable equilibria *)

(* need to check against invasion *)

replace[size_, set_, numbers_, position_:1]:=
 If[size < position,{},
  If[And[Length[set] > 0, set[[1]] == position], 
        Join[{numbers[[1]]}, replace[size,Drop[set, 1], Drop[numbers, 1],
          position + 1]],
        Join[{0}, replace[size, set, numbers, position + 1]]]]

(* checks if stable to invasion *)
(* returns True if stable, also vector with info on directions *)

invasion[m_, set_] :=
 Block[{ss= subset[m, set], l =Length[set], lm =Length[m], pp, pa, r0, u},
  pp = Inverse[ss].Table[1,{l}];
  pa = replace[lm, set, pp];
(*   Print["test", m, pa]; *)
  r0 = Table[1,{lm}] -m.pa;
(*   Print[r0]; *)
  u =Table[Or[MemberQ[set,  i], r0[[i]] < 0], {i, 1, lm}];
(*   Print[u]; *)
  {Apply[And, u], u}]


feasible[system_] := positive[Inverse[system].Table[1,{Length[system]}]]

stable[m_, set_] :=
 With[{ss= subset[m, set], l =Length[set]},
  And[positive[Inverse[ss].Table[1,{l}]],
      positive[Eigensystem[ss][[1]]],
      invasion[m, set][[1]]]]

(* unstable equilibria, but stable to invasion *)

unstable[m_, set_] :=
 With[{ss= subset[m, set], l =Length[set]},
  And[positive[Inverse[ss].Table[1,{l}]],
      Not[positive[Eigensystem[ss][[1]]]],
      invasion[m, set][[1]]]]

stable[m_] :=
 Select[Drop[Subsets[Table[i,{i, 1, Length[m]}]], 1], stable[m, #]&]

(* searching for systems with multiple equilibria *)

multiple[k_, n_, c_, sigma_] :=
  Select[Table[ Abs[-symmetric[random[n, c, sigma]]],{k}],Length[stable[#]]> 1 &]

(* dynamics *)


cutoff[list_, cutoff_:0.00001] :=
 Table[If[Abs[list[[i]]] > cutoff, list[[i]], 0],{i, 1,Length[list]}]


count[list_] := Length[Select[list, #> 0 &]]

(* gets locations where list is nonzero *)
locations[list_]:=
  Position[Map[#> 0 &, list], True]


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


 (* was "random" in multiple.m *)
run[initial_, m_] :=
 Block[{size =Length[m], v},
    v=Table[x[i][t],{i, 1, size}];
    ((v/. t -> 500) /. 
      dynamics[m, initial, 500])[[1]]]


converge[initial_, m_, previous_:Table[1,{Length[m]}]] :=
 Block[{rr = run[initial, m],p},
  p = Apply[Join, locations[cutoff[rr]]];
(*  Print[Length[p], ":", p]; *) 
  If[p =={}, previous, If[stable[m, p], p, converge[rr, m, p]]]]

init[mean_, scale_, size_] :=
  Map[Abs, RandomReal[NormalDistribution[0,scale],{size}]+ mean]

(* compute solution with subset *)

solution[m_, s_] :=
  Inverse[subset[m, s]].Table[1,{Length[s]}]

(* computes "energies" of solutions *)

energies[m_, ss_] :=
 Map[Apply[Plus, solution[m, #]]&, ss]

(* using equilibrium with 8 solutions *)

experiment[1, m_, n_: 100] := 
 Block[{r, i},
  For[j = 1, j <= n, j += 1,
    i =init[0, 1.0, Length[m]];
    Print[i]; (* looking for bugs *)
    r = converge[i, m];
    Print[j, ":", Length[r]];
    {Length[r], r} >>> "experiment-1.m"]]

(* from ../analysis.m *)
(* apply to Tally[result] *)
collate[r_] := Reverse[Sort[Map[Reverse, r]]]

probabilities[s_] :=
 (Map[#[[1]]*1.0 &, s])/Apply[Plus, Map[First, s]]

(* given matrix, tallied results, get v, ln p *)

pairs[r_, m_] :=
 With[{rr = collate[r]},
  With[{ps= probabilities[rr]}, 
   Table[{-Log[ps[[i]]], energies[m, {rr[[i, 2, 2]]}][[1]]},{ i, 1,Length[r]}]]]

(* investigating average number of equilibria, over k tries *)

stats[list_] :=
 {If[Length[list] > 0, 1, 0],Length[list]}

degeneracy[n_, c_, sigma_, mean_, k_] :=
 With[{s=1.0*Sum[stats[stable[-symmetric[random[n, c, sigma, mean]]]],
                {i, 1,k}]},
    {If[s[[1]] > 0, s[[2]]/s[[1]], 0], s}]

(* collect statistics *)

collect[1, size_, n_: 100, jj_:30] := 
 Block[{r},
  For[j = 1, j <= jj, j += 1,
    r = Table[degeneracy[size, 1.0, x*0.1, -j*0.1, n],{x, 1,10 +j}];
    Print[j, ":", Map[First, r]];
    {{size, 1.0, -j*0.1, n}, r} >>> "collection-1.m"]]


(* truncate a matrix at fixed range *)

truncate[m_, l_] :=
 With[{s=Length[m]},
   Table[If[Min[Abs[i-j], s-Abs[i-j]] <= l, m[[i, j]], 0],
    {i, 1, s},{j, 1, s}]]

(* given matrix, collated results,  find mean and standard deviation
for all appearing multiplicities*)
(* rr collated *)

es[rr_, m_, k_] :=
 energies[m, Map[#[[2, 2]] &, Select[rr, #[[1]] == k &]]]

means[rr_, m_] :=
  With[{total =Apply[Plus, Map[First, rr]], ns=Union[Map[First, rr]]}, 
   Table[{-Log[1.0*ns[[i]]/total], Mean[es[rr, m, ns[[i]]]]},{i, 1,Length[ns]}]]
