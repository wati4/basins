(* computes probabilities for everything, needs
 model.m, all-solutions.m, a model like m4.m *)

predictions[list_, k_, w_] :=
  Block[{x= 0},
     For[j = 1, j <= Length[list], j += 1,
        x =  Map[{logpredictpair[#, k, w], #} &, all[[j, 2, 2]]];
        {all[[j, 1]],  x }>>> scratch.m
	Print[j + 19,": ", Length[x]]
        ];
     ]

(* predictions[all, 4, w4] *)
