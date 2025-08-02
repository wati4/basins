(* from niches/math/log.txt *)

(* uses nearest neighbor approximation for interaction, good in this range. *)

graph[11, 1] := 
    Plot[1/ (x(1+ 2E^(-x^4))),{x, 0.91, 1.82},
      AxesLabel ->{Text[ToExpression["D/\\Delta", TeXForm]], "biomass density"}, 
        LabelStyle ->{FontSize -> 14}]

function[d_] := 
 2*Gamma[5/4]*HypergeometricPFQ[{}, {1/2, 3/4}, Pi^4/(16*d^4)] - 
  (Pi^2*Gamma[3/4]*HypergeometricPFQ[{}, {5/4, 3/2}, Pi^4/(16*d^4)])/d^2
                  
graph[11, 2] := Plot[function[d],{d, 0.9, 1.8},
        AxesLabel ->{Text[ToExpression["D/\\Delta", TeXForm]], 
                Text[ToExpression["\\lambda", TeXForm]]}, 
        LabelStyle ->{FontSize -> 14}]
