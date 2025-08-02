(* some data on single diagonal variation in matrix *)


(* select allowed indices *)

indices[size_:200] :=Select[Table[i,{i, 0,  size}], Not[IntegerQ[matrix[#][[1]]]] &]

(* recognize translated format for vectors {{}} *)

ratios[i_, j_] :=
 {1.0/matrix[i][[j, j]], results[i][[1, j]]}

(* collect from a subset with same variance, i through j *)

collect[is_, i_, j_, size_:20] :=
 Apply[Join, Map[Table[ratios[#, k],{k, 1, size}] &,
            Select[is, And[#>= i, #<= j] &]]]

(* for this particular case *)

collection[is_, a_] :=
 collect[is, a*20, a*20 + 19]

(* --------------------------  old stuff *)

(* theory *)

single[size_, a_] :=
 Table[If[i == j, If[i == 1, a, 1], 2],{i, 1, size},{j, 1, size}]

equilibrium[size_, a_] :=
 Inverse[single[size, a]].Table[1,{size}]

(* looking for coefficient c *)

coefficients[{n_, p_}] :=
 With[{x = p/250.0, y =  x/(19 +  x)},{n,Log[19y/(1 -y)]}]

(* looking at where equilibrium becomes unstable  *)

unstable[k_] :=
 Table[ a /. Solve[0 == N[Det[single[n, a]]], a][[1]],{ n, 1, k}]

(* now, use sphere.m for spherical angles. *)

basis[d_, i_, a_] :=Table[If[j == 1, 1, If[j <= i, 2-a, 0]],{j, 1, d}]
basis[d_, a_] :=Table[basis[d, i, a],{i, 1, d}]

estimation[d_, a_, m_] :=
 estimate[d, normalized[basis[d, a]], m]

(* outside radius of convergence  let's look at 3D *)
 

solid3[a_, b_, c_] :=
  With[{aa = Norm[a], bb = Norm[b], cc = Norm[c]},
        2 ArcTan[Det[{a, b, c}]/(aa bb cc + aa (b.c) + bb (a.c) + cc (a.b))]]

answer3[a_] := Apply[solid3, basis[3, a]]
