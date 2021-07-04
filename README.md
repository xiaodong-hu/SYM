# Sym
Julia Package for Symbolic Computation.


## Variables Claim
Single variables can be claimed in the way of `x = Sym{Real}(:x)` (if ). Arguments of `Type{String}` and `Type{Expr}` are also supported:
```
y = Sym{Int}("y")
eq = Sym(:(sin(x+y)^z))
```

Macros `@def` claim variables with its name (`Symbol`) the variable itself (it also accept `Type{String}`)
```
v = @def x y "z"
```
Similarly we can claim variables from `Vector{Symbol}` like
```
def_from_vec([:x :y :z])
```


## Substitution (both Numerical and Symbolic) by `sub!()`
As one example, numerical substitution can be done as following: 
```
test_ex = sin(x)^z+y
sub!(test_ex, (x=>1,y=>2))
```
**Symbolic** substitution to `Type{Symbol}` or `Type{Sym{T}}` can also be achieved in the same way
```
@def a
test_ex = sin(x)^z+y
sub!(test_ex, (x=>a,y=>:b))
```


## Lambdification (Numerical) by `lambdify()` and `lambdify_fast()`
I have two kinds of functions: `lambdify()` and `lambdify_fast()` (with slight differences on their inputs of parameters). Both support usual expression like
```
@def x y z
test = x*exp(y+z^2)

lambdify(test, [:x, :y, :z])
lambdify_fast(test, [x, y, z])
```

The ordinary `lambdify()` also supports vector of expression and matrix of expression, which is enough for common usage. But I notice that it lacks efficiency for extreme large arrays (due to the slow linear search on the *abstract syntax tree* (AST)). For example (with pkg `BenchmarkTools.jl` used)
```
@btime begin # 17.204 s (15956305 allocations: 751.22 MiB)
	# test for 100*100 matrix
	test = Array{Sym{Real}}(undef, 100, 100)
	for i in 1:100
		for j in 1:100
			test[i,j] = sin(i*x+y^j)
		end
	end

	func = lambdify(test, [:x, :y])
	Base.invokelatest(func,1,2); # to avoid the world age problem, see https://discourse.julialang.org/t/how-to-bypass-the-world-age-problem/7012/25
end
```

Instead of generating an expresssion of function `Expr(:function,...)`, `lambdify_fast` lambdifies the function by *direct substitution*, which turns out to be much faster (also with much less memory cost) for large arrays, and supports arbitrary dimension of arrays. For example,
```
@btime begin # 5.302 s (3361039 allocations: 196.32 MiB)
	# test for 100*100 matrix
	test = Array{Sym{Real}}(undef, 100, 100)
	for i in 1:100
		for j in 1:100
			test[i,j] = sin(i*x+y^j)
		end
	end

	func = lambdify_fast(test, [x,y])
	Base.invokelatest(func,1,2); # to avoid the world age problem, see https://discourse.julialang.org/t/how-to-bypass-the-world-age-problem/7012/25
end
```

## Possible issues
1. To avoid the issues on the compatibility with the default Type{Complex} (recall that in Julia, imaginary unit is defined as `const im = Complex(false, true)`), it is highly recommend to use symbolic `I` instead
    ```
    I = Sym{Int64}(:im)
    ```
    and evaluate the complex expression at the last stage by substituting `I` to `im`.
