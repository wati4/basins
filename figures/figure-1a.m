
(* figures of s= 6 equilibria *)

(* takes species and magnitude *)
g3[{s_, size_}]:=
  Join[{(* Dashing[0.02],  *) Line[{{-3, 0},{3, 0}}]},
    Table[Line[{{-x, 0},{-x, 5}}],{x, -3, 3}],
    Table[Text[ Style[i, 36],{-3.5 + i, -0.4}],{i, 1, 6}],
    {Dashing[None], RGBColor[0, 0, 1]},
    Table[Rectangle[{-4 + s[[i]], 0},{- 3 + s[[i]], size*5}],
                {i, 1,Length[s]}],
    {Thickness[0.01]},
    Table[Circle[{-3.5 + s[[i]], -0.4}, 0.35],{i, 1,Length[s]}]]

data[3] :={{{1, 3, 5}, 0.576}, {{2, 4, 6}, 0.576},
    {{1, 4}, 0.9875}, {{2, 5}, 0.9875}, {{3, 6}, 0.9875}}

(* figure[1, i], i = 1--5 produces 5 graphics for figure 1a *)

figure[1, i_] := 
  Show[Graphics[g3[data[3][[i]]]]]
