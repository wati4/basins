(* analyze data on single diagonal variation in matrix *)


(* select allowed indices *)

indices[size_:200] :=Select[Table[i,{i, 0,  size}], Not[IntegerQ[matrix[#][[1]]]] &]

ratiosold[i_] :=
 Map[{1.0/matrix[#][[1]], results[#][[1]]} &, i]

ratiosoldnew[i_] :=
 Map[{1.0/matrix[#][[1]], 
    results[#][[1]]/Sum[results[#][[j]],
                {j,1,Length[results[#]] -1}]} &, i]

(* for translated from Julia version *)

ratios[i_] :=
 Map[{1.0/matrix[#][[1, 1]], 
    results[#][[1, 1]]/Sum[results[#][[1, j]],
                {j,1,Length[results[#][[1]]] -1}]} &, i]

(* averaging over all common initial values *)

averaging[list_, value_] :=
 Mean[Select[list, #[[1]] == value &]]

combining[list_] :=
 With[{first = Union[Map[First, list]]},
   Map[averaging[list, #]&, first]]

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
