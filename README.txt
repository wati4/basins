Files to accompany paper "On the structure of
multiple stable equilibria in competitive ecological systems", to
appear in Theoretical Ecology 

This paper, and the accompanying code, analyze a set of competitive
ecological models, focusing on identifying systems with multiple
stable equilibrium and computing the sizes of the domains of
attraction of the equilibria.  Primary focus is on competitive niche
models, other models considered include monodominant and random matrix models.

-------------------------------------

This contains  4 sets of files:

(1) Mathematica code to analyze periodic niche interaction matrices
and associated equilibria

(2) Julia code to run simulations and collect resulting stable
equilibrium states

(3) Mathematica code to produce and analyze a statistical model given
data from many simulations.

More details regarding these code files are in files examples-i.txt, i
 = 1, 2, 3, which give examples of use with commentary.

(4) Mathematica code and explanations for figures and calculations in text.
These files are in the subdirectory figures/.

------------------------------------

Mathematica code is fairly version-independent, mostly implemented and
run in version: Mathematica 11.1.1 Kernel for Linux x86 (64-bit)
(used in command line/shell mode, without notebook interface)
with no extra packages.

Julia code is fairly version-independent, mostly implemented and run
in version v1.6 but should be fully compatible with Version 1.10.4
(2024-06-04).  
Uses packages: DifferentialEquations, Random, LinearAlgebra

All data files were produced by software included here; no data is
empirical or taken from other sources.

A full list of files in this directory appears at the end of this
file, along with some standard data formats; files in the figures
subdirectories are listed in figures/README-figures-calculations.txt

----------------------------------------

(1) Mathematica code to analyze matrices and equilibria

basins.m: mathematica code to compute periodic niche interaction
  matrices and analyze associated stable equilibria

examples-1.txt: examples of usage of basins.m

----------------------------------------

(2) Julia code to run simulations

niches.jl: code to run simulations
combine.jl: code to combine output from multiple simulations

examples-2.txt: example of usage (2 * 100 runs at S= 30)

output: results of simulations (output of niches.jl)
header-10.jl: auxiliary header file to combine simulation results
results-200.jl: header + output
combined: combined results of 2*100 simulations (output of combine.jl)

----------------------------------------

(3) Mathematica code to build statistical model from data

model.m: code to take simulation results and build statistical model
combined.m: data from combined simulations, in mathematica format

examples-3.txt: examples of usage of model.m code

model-30.m: statistical mechanical model computed from code and simple
 data set above in file combined.m
 (w4 contains e^(-alpha), e^(-beta)'s, m4 is probabilities p(g_1), etc.)

sorted-200.m: tabulation of equilibria and frequencies for sample 200 runs

tree.m: code to generate all possible stable subset equilibria
 compatible with a given local model

90-sorted.m: results of 10^6 simulations at size S= 90 (99.5% gave stable
 solutions).
m4-90.m: statistical mechanical model built from data from 90-all at k = 4.

--------------------------------------

(4) Mathematica code to generate figures, and explanations for figures
and computations in text.

figures/figure-*.m: mathematica files to generate figures

figures/README.txt Further explanations for figures and associated data,
and for computations in text.


====================================

List of files in this directory and some data formats.

Standard data formats:

(*) "gap" format:
For results of multiple simulations, e.g. 90-sorted.m:
List of stable equilibria reached with multiplicity, format
{multiplicity, {gap sequence}}
where gap sequence is in canonical form (lexicographically first over
all equivalent sequences under rotation and reflection)

(*) "types/hits" format:
Another format for results of multiple simulations, when done in blocks on
cluster in julia:
For each simulation block i, file contains entry (matrix and parameters
dropped when uniform across simulations)

matrix[i] = [matrix A used in simulation block i]
parameters[i] = [S, L = S/\Delta]
types[i] = [list of stable equilibria reached in canonical gap sequence format]
hits[i] = [number of times each equilibrium in types is reached]

(*) "model" format:
A format for statistical models describing distributions of gap
sequences.  Can describe raw probabilities of subsequences 
p(g_1, ..., g_k) as in equation (8) or connected probabilities
p_c(g_1, ..., g_k) = e^(-\beta_k (g_1,..., g_k)) as in (A25).
Format is list {ps(1), ps(2), ...} where each ps(i) is a list of
probabilities p or p_c of the form
{{g_1, ..., g_i}, p(g_1, ..., g_i)}.

------------------------------------
List of files (relevance and usage of each described in more detail above):

90-sorted.m: "gap" format results of 10^7 simulations of S= 90 model

basins.m: mathematica code to construct and analyze periodic niche systems

combined: "types/hits" format results of 200 example runs of julia code

combined.m: combined converted into mathematica format

combine.jl: julia code to combine output from multiple simulations

examples-1.txt: description and example runs of basic basins.m mathematica code

examples-2.txt: description and example runs of julia simulation code niches.jl

examples-3.txt: description and example runs of model.m mathematica code

figures: subdirectory containing code to produce figures

header-10.jl: header file for combining julia simulation outputs

m4-90.m: "model" format data file containing SM model built from 90-sorted.m

model-30.m: example k = 3 "model" format data file from example simulations

model.m: mathematica code to produce SM model from data

niches.jl: julia code to run periodic niche model simulations

output: output of sample run of julia code in "types/hits" format

README.txt: this file

results-200.jl: combined header-10.jl with combined data file

sorted-200.m: results-200.jl converted into "gap" format (see examples-3.txt)

tree.m: mathematica code to use "model" data to produce all possible
  compatible equilibrium solutions for a given niche model
