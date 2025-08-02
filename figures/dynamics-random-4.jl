# solves dynamics of LV with multiple stable equilibria
# version 3: only a single a != 1 in off-diagonal.
# version 4: fixes types

# arguments: size S, v: a = 1-v, #runs, index

#import Pkg
#Pkg.add("DifferentialEquations")

using DifferentialEquations
using Random
using LinearAlgebra

# pick a random matrix, with given size, mean, variance  for diagonal, off
# for example, usage: randommatrix(5, 1, 2, 0.1)

function randommatrix(size, dm, om, sd)
 a = Matrix{Float64}(undef, size, size)
 y = fill!(a, om) + sd*randn((size, size)) + (dm-om)*I
 return y
end

# a matrix of size size, with only one deviation in 1, 1 element

function matrixsingle(size, v) 
 a = Matrix{Float64}(undef, size, size)
 y = fill!(a, 2) + (-1)*I
 y[1, 1] = 1-v
 return y
end
 

# start with initial values, run for time t, give output

function run(m::Symmetric{Float64, Matrix{Float64}}, initial, tt)
 f(u, p, t) = u - u.*(m*u)
 problem = ODEProblem(f, initial, (0.0, tt))
 solution = solve(problem)
 solution(tt)
end
 
# find what species are present

function remaining(list, tolerance)
 [z for z in 1:length(list) if list[z] > tolerance]
end

# run system with random initial values many times and get species
#  remaining

function randomrunone(m, tt, scale = 1.0, tolerance = 0.000001)
 i = map(x -> abs(scale*x), randn(size(m)[1]))
 r = run(m, i, tt)
# (i, r, remaining(r, tolerance))
 remaining(r, tolerance)
end

# run system with random initial values many times and get species
#  remaining
# each time around do multiple passes and get rid of negative populations

function randomrun(m::Symmetric{Float64, Matrix{Float64}}, tt, blocks::Int, scale = 1.0, tolerance = 0.000001)
 i = map(x -> abs(scale*x), randn(size(m)[1]))
 for j in 1:blocks
  r = run(m, i, tt)
  i = map(x -> (x < 0.0) ? 0.0 : x, r)
#  print(j, r, i)
 end
# (i, r, remaining(r, tolerance))
 remaining(i, tolerance)
end

# runs with a fixed matrix m for count times

function dynamics(m, count, null)
    o = open(string("output"), "a")
    write(o, string(m))
    for k in 1:parse(Int, count)
    	r = randomrun(m, 100, 100)
        write(o, string(r, "\n"))
    end
    close(o)
end

# runs some number of times, assuming only a single species at minimum
#computes statistics

#dynamics(ARGS[1],ARGS[2])

# runs some number of times, assuming only a single species at minimum
#computes statistics

function dynamicsstats(size, v, count, index) #size, count as strings
    m = Symmetric(matrixsingle(size, v))
    o = open(string("output"), "a")
    write(o, "matrix[" * string(index) * "] =" * string(m, "\n"))
    hits = fill(0, size+ 1)
    # print(string(hits))
    for k in 1:count
    	r = randomrun(m, 100, 100)
	if length(r) == 1
	   i = r[1] # creates an index
	else 
	   i = size + 1
        end
	# print(k, r)
        hits[i] += 1
    end
    write(o, "results[" * string(index) * "] =" * string(hits, "\n"))
    close(o)
end

#dynamics(ARGS[1],ARGS[2])

# runs a single time with a random matrix of size size, matching
#  mathematica initialization

dynamicsstats(parse(Int, ARGS[1]),parse(Float64, ARGS[2]),parse(Int, ARGS[3]),parse(Int, ARGS[4]))
