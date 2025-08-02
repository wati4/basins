(* puts pairs into bins *)

binning[p_, i_, f_, d_] :=
 Table[{i +  (j -1/2) d, Mean[Map[#[[2]] &, 
        Select[p, And[#[[1]] > i + (j-1) d, #[[1]] < i + j d] &]]]},
       {j, 1, (f-i)/d}]
