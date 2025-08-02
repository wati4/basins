<< ../basins.m
(* Some examples: *)

ex ={
{36, {5, 10, 15, 21, 27, 33, 39, 45, 51, 57, 63, 69, 74, 79, 85, 91, 96, 101, 
 106, 112, 118, 123, 128, 134, 139, 144, 149, 154, 160, 166, 172, 177, 182, 
 188, 194, 200}},
{39, {5, 11, 16, 21, 26, 31, 36, 41, 44, 49, 55, 60, 65, 70, 75, 80, 85, 90, 
 95, 100, 105, 110, 115, 119, 124, 129, 135, 141, 147, 153, 158, 163, 169, 
 175, 180, 185, 190, 195, 200}},
 {50, Table[ 4i,{i, 1, 50}]}
}

mx := mx = matrix[200, 50]

equilibrium[m_] :=
 Inverse[m].Table[1,{Length[m]}]


equilibrium[m_, set_] :=
 With[{e = equilibrium[subset[m, set]]},
  Table[If[MemberQ[set, i], e[[Position[set, i][[1, 1]]]], 0],{i,Length[m]}]]

eqs[m_, set_] :=
 With[{e = equilibrium[subset[m, set]]},
  {set, e}]



figure[2] :=
 Show[ListPlot[equilibrium[mx, ex[[1, 2]]], 
               PlotJoined -> True, PlotStyle -> Blue],
      ListPlot[equilibrium[mx, ex[[4, 2]]], 
               PlotJoined -> True, PlotStyle -> Red]]

(* makes bars in appropriate places for 200 solution *)

g200[{s_, sizes_}, color_:RGBColor[0, 0, 1]]:=
  Join[{(* Dashing[0.02],  *) Line[{{0, 0},{200, 0}}]},
    Table[Line[{{x, 0},{x, 5}}],{x, 0, 200}],
    Table[Line[{{x, 0},{x, 10}}],{x, 0, 200, 10}],
(*     Table[Text[ Style[i, 30],{-0.5 + i, -0.4}],{i, 50, 200,}], *)
    {Dashing[None], color},
    Table[Rectangle[{s[[i]] -1, 0},{s[[i]], sizes[[i]]*15}],
                {i, 1,Length[s]}]]

colors={RGBColor[0, 0, 1], RGBColor[1, 0, 0], RGBColor[1, 0, 1]}

(* figure[12, i]i = 1--3 produces graphics for figure 1b *)

figure[12, i_] := 
  Show[Graphics[g200[eqs[mx, ex[[i, 2]]], colors[[i]]]]]

