(* figure 4 *)

<< ../basins.m

m = matrix[200, 50];


vector[m_, n_] :=Table[E^(2 Pi I k m/n),{k, 0, n -1}]

eigenvalues[n_, size_] :=  eigenvalues[n, size] =
 With[{mm = matrix[n, size]},
  Table[Re [(mm.vector[m,n])[[1]]],{ m, 0, n-1}]]

graph[4] :=
  ListPlot[eigenvalues[200, 50], AxesLabel ->{k, Text[ToExpression["\\lambda", TeXForm]]}, 
        LabelStyle ->{FontSize -> 14}]

figure[4] := Export["figure-4.pdf",graph[4]]
