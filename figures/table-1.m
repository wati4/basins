(* figures for table 1 *)

(* from ~/drop/talks/ecology/posters/Gordon/math/figures.m *)

(* various equilibria *)

gs={{6, 6, 6, 6, 6}, { 5, 5, 5, 5, 5, 5}, { 4, 5, 5, 5, 6, 5},
    { 4, 5, 5, 6, 5, 5}, 
    { 1, 6, 6, 5, 6, 6}, {4, 4, 5, 6, 6, 5}, {3, 5, 5, 6, 6, 5},
    {4, 4, 4, 4, 5, 4, 5},
    {4, 4, 4, 4, 4, 5, 5}}

info[g_] := 
 With[{m = matrix[30, 7.5], c = configuration[g]},
   {c, energy[m, c]}]

c2 = {{{1, 7, 13, 19, 25}, {0.987499, 0.987499, 0.987499, 0.987499,
0.987499}},
   {{1, 5, 10, 15, 21, 26}, {0.676265, 0.676265, 0.861131, 0.91923,
   0.91923, 0.861131}},
    {{1, 5, 9, 13, 17, 22, 26},
        {0.763411, 0.481644, 0.645626, 0.481644, 0.763411, 0.682483, 0.682483}}}

infoall := Map[info, gs]

i1 := Map[{#[[1]], #[[2, 1]]} &, infoall]
i2 := Map[{#[[1]], #[[2, 2]]} &, infoall]

g2[{s_, sizes_}] :=
  Join[Table[Circle[{x, 0}, 0.4],{x, 1, 30}], {RGBColor[0, 0,1]},
    Table[Disk[{s[[i]], 0}, 0.5*sizes[[i]]],{i, 1,Length[sizes]}]]

graphic[1, i_] := Show[Graphics[g2[i2[[i]]]]]

table[1, i_] := Export[StringJoin["table-1", ToString[i], ".pdf"], graphic[1, i]]

(* stuff for probabilities *)
(* note different ordering! *)

numbers={{2938671, {6, 6, 6, 6, 6}}, {2529317, {4, 5, 5, 5, 6, 5}}, 
 {2145647, {5, 5, 5, 5, 5, 5}}, {843490, {4, 5, 5, 6, 5, 5}}, 
 {512182, {3, 5, 5, 6, 6, 5}}, {371561, {1, 6, 6, 5, 6, 6}}, 
 {358376, {4, 4, 5, 6, 6, 5}}, {239, {4, 4, 4, 4, 4, 5, 5}}, 
 {106, {4, 4, 4, 4, 5, 4, 5}}}

configs ={6, 60, 5, 30, 60, 30, 30, 30, 30}

(* gets probabilities and standard error *)

ps:= With[{total = Apply[Plus, Map[First, numbers]]},
  Table[With[{x = N[numbers[[i, 1]]/(total)]},
        {numbers[[i, 2]], 
        x/configs[[i]],
        Sqrt[x(1-x)/total]/configs[[i]]}],{ i, 1, 9}]]

(* doing this again given types and hits in any order *)
(* if something is negative, drop *)

cs[t_] := With[{p = Position[numbers, t]},
  If[p =={}, 0, configs[[p[[1,1]]]]]]

stats[t_, h_] := With[{total = Apply[Plus, h]},
  Table[With[{x = N[h[[i]]/(total)], c = cs[t[[i]]]},
        {t[[i]], 
        x/c,
        Sqrt[x(1-x)/total]/c}],{ i, 1, 9}]]
