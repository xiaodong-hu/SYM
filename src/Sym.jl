module Sym

using BenchmarkTools

import Base:+,-,*,/,^,sin,cos,tan,exp # methods for ordinary numbers are needed, so we have to import explicitly (since we have changed their methods)
import Base:convert, show

#####################################################################
########## Types, Promotion Rules, and Convert Start Here ###########
#####################################################################
"""
An instance of Symbolic variable can be realized by
	`Sym{T}(expr)`, where T is the SubType of the Real domain.
Here I confine the variable in real just for convenience of the promotion rule with complex numbers since Complex{T}(x) is defined only for x of the
	subtype of Real, while Complex{Sym} is illegal.
"""
abstract type Symbolic{T} end # <: Number

struct Sym{T<:Number} <: Symbolic{T} # Symbolic{T}
	expr # of Type{Symbol} or Type{Expr}
end

"Claim realization of the instance for other types"
Sym{T}(x::String) where {T} = Sym{T}(Meta.parse(x))
Sym{T}(x::Sym) where {T} = Sym{T}(x.expr) # change the symtype, or keep unchanged for repeated invokation
Sym(x) = Sym{Real}(x) # add a method for default type of Real
Sym(list::Vector) = [Sym{Real}(i) for i in list] # add a method for generating a list of symbols
#= for example,
	x = Sym{Real}(:x) # creat one instance of Real Symbolic variable
	y = Sym{Real}("y")
	z = Sym(x)
	x = Sym{Real}(gensym())	# creat one instance for an anonymous variable
=#

"Define a new type-check function by adding methods"
function symtype end
symtype(::Sym{T}) where {T} = T
symtype(x) = typeof(x) # for other known Types of x like Number, Symbol, and Expr etc.

"""
Define promote rules for Type{Sym} and Type{Number}.
This is necessary since basic binary operations always act in the way of, for example, +(x::Number, y::Number) = +(promote(x,y)...)
See https://docs.julialang.org/en/v1/manual/conversion-and-promotion/#Promotion
"""
Base.promote_rule(x::Type{Sym{T}}, y::Type{S}) where {T, S<:Real} = Sym{Union{T, S}}
# for example, promote_rule(Sym{Float64}, Int64) = Sym{Float64}

"""
Add methods for basic binary operations +,-,*,/,^
Here we distinguish real and complex numbers
"""
+(x::Sym{T}, y::Number) where {T} = Sym{T}(eval(:(:($($x.expr)+$$y))))
+(y::Number, x::Sym{T}) where {T} = Sym{T}(eval(:(:($($x.expr)+$$y))))
+(x::Sym{T}, y::Sym{S}) where {T,S} = Sym{Union{T,S}}(eval(:(:($($x.expr)+$($y.expr)))))
-(x::Sym{T}, y::Number) where {T} = Sym{T}(eval(:(:($($x.expr)-$$y))))
-(y::Number, x::Sym{T}) where {T} = Sym{T}(eval(:(:($($x.expr)-$$y))))
-(x::Sym{T}, y::Sym{S}) where {T,S} = Sym{Union{T,S}}(eval(:(:($($x.expr)-$($y.expr)))))
*(x::Sym{T}, y::Number) where {T} = Sym{T}(eval(:(:($($x.expr)*$$y))))
*(y::Number, x::Sym{T}) where {T} = Sym{T}(eval(:(:($($x.expr)*$$y))))
*(x::Sym{T}, y::Sym{S}) where {T,S} = Sym{Union{T,S}}(eval(:(:($($x.expr)*$($y.expr)))))
/(x::Sym{T}, y::Number) where {T} = Sym{T}(eval(:(:($($x.expr)/$$y))))
/(y::Number, x::Sym{T}) where {T} = Sym{T}(eval(:(:($$y/$($x.expr)))))
/(x::Sym{T}, y::Sym{S}) where {T,S} = Sym{Union{T,S}}(eval(:(:($($x.expr)/$($y.expr)))))
^(x::Sym{T}, y::Number) where {T} = Sym{T}(eval(:(:($($x.expr)^$$y))))
^(y::Number, x::Sym{T}) where {T} = Sym{T}(eval(:(:($$y^$($x.expr)))))
^(x::Sym{T}, y::Sym{S}) where {T,S} = Sym{Union{T,S}}(eval(:(:($($x.expr)^$($y.expr)))))

# define the zero unit for abelian groups
Base.one(s::Sym) = one(symtype(s))
Base.zero(s::Sym) = zero(symtype(s))

"""
Add methods for basic single-variable mathematical functions exp, sin, cos, tan (to be continue...)
"""
exp(x::Sym{T}) where {T} = Sym{T}(eval(:(:(exp($($x.expr))))))
exp(x::Array{Sym{T}}) where {T} = Sym{T}(:(exp($x)))
sin(x::Sym{T}) where {T} = Sym{T}(eval(:(:(sin($($x.expr))))))
sin(x::Array{Sym{T}}) where {T} = Sym{T}(:(sin($x)))
cos(x::Sym{T}) where {T} = Sym{T}(eval(:(:(cos($($x.expr))))))
cos(x::Array{Sym{T}}) where {T} = Sym{T}(:(cos($x)))
tan(x::Sym{T}) where {T} = Sym{T}(eval(:(:(tan($($x.expr))))))
tan(x::Array{Sym{T}}) where {T} = Sym{T}(:(tan($x)))


"""
Convert methods
	motivated by https://github.com/symengine/SymEngine.jl/blob/master/src/subs.jl
"""
function convert(::Type{Expr}, x::Sym{T}) where {T}
	return x.expr
end

function convert(::Type{Expr}, v::Vector{Sym{T}}) where {T}# slow for large array
    Expr(:vcat, map(x->x.expr, v)...) # map .expr to the vector v. Such Vectorization treatment is much faster.
end

function convert(::Type{Expr}, m::Matrix{Sym{T}}) where {T} # slow for large array
    col_args = []
    for i = 1:size(m, 1)
        row_args = []
        for j = 1:size(m, 2)
            push!(row_args, m[i,j].expr)
        end
        row = Expr(:hcat, row_args...)
        push!(col_args, row)
    end
    Expr(:vcat, col_args...)
end


"Change the show methods for Type{Sym} (default looks like Sym{T}(:x); here we now display its Expr representation" 
function Base.show(io::IO, x::Sym)
	print(io, "$(x.expr)")
end


#####################################################################
######################### Macros Start Here #########################
#####################################################################
"""
Claim (real) symbolic variables (with the Type{Sym{Real}}) by its Symbol representations
"""
macro def(x...) # accept multiple arguments
	q = [] # variable list
	for s in x
		if s isa String
			s = Meta.parse(s) # convert to Symbol or Expr
			eval(Expr(:(=), s, Expr(:call, :Sym, Expr(:quote, s)))) # claim symbolic variables
			push!(q, eval(s))
		elseif s isa Symbol
			eval(Expr(:(=), s, Expr(:call, :Sym, Expr(:quote, s)))) # claim symbolic variables
			push!(q, eval(s))
		else
			error("Symbolic variables cannot be constructed: Not implemented!")
		end
	end
	return q
end

"Claim (real) symbolic variables (with the Type{Sym{Real}}) by its Symbol representations"
function def_from_vec(x::Vector) # accept multiple arguments
	q = []
	for s in x
		if isa(s, Symbol)
			eval(Expr(:(=), s, Expr(:call, :Sym, Expr(:quote, s)))) # claim symbolic variables
			push!(q,eval(s))
		elseif isa(s, String)
			s = Meta.parse(s)
			eval(Expr(:(=), s, Expr(:call, :Sym, Expr(:quote, s)))) # claim symbolic variables
			push!(q,eval(s))
		end
	end
	return q
end

#####################################################################
########### Substitution and Lambdification Start Here ##############
#####################################################################
const binary_op_list = [:+, :-, :*, :/, :^]

function AST_iterate_search_replacement(ex, rules::Tuple) # Slow serial tree search, wait to be optimized
	if ex isa Expr
		if ex.head == :call
			for (i,s) in enumerate(ex.args)
				if s isa Symbol && !(s in binary_op_list)
					for r in rules
						if s == r.first.expr
							if r.second isa Sym
								ex.args[i] = r.second.expr
							elseif r.second isa Union{Number, Symbol}
								ex.args[i] = r.second
							else
								error("Illegal replacement for the second arguments!")
							end
						end
					end
				elseif s isa Expr
					AST_iterate_search_replacement(s, rules)
				end
			end
		else
			error("Illegal expression: not implemented!")
		end
	elseif ex isa Symbol
		for r in rules
			if ex == r.first.expr
				if r.second isa Sym
					ex = r.second.expr
				elseif r.second isa Union{Number, Symbol}
					ex = r.second
				else
					error("Illegal replacement for the second arguments!")
				end
			end
		end
	end
	return ex
end

function sub!(sym::Sym, rules::Tuple) # motivated by the discussion of https://discourse.julialang.org/t/substitute-symbols-in-expression/23099
	ex = sym.expr
	ex = AST_iterate_search_replacement(ex, rules)
	return ex # fortunately, `eval(x::Sym)=x`.
end

function sub!(ex, rules::Tuple) # ex isa Symbol or Expr is OK
	return AST_iterate_search_replacement(ex, rules)
end

function sub!(m::Array, rules::Tuple) # add the method for substituting Array{Sym} of any dimension
	return sub!.(m, Ref(rules))
end


"""
Lambdify a muti-variable function with arbitrary vector of Symbols 
	Symbolic lambdification (with variables not given enough, or extra variables given) is also supported!
"""
function lambdify(x::Sym, vars::Vector{Symbol})
	return eval(Expr(:function, Expr(:call, gensym(), vars...), x.expr))
	# only Symbol can be legal function arguments, so we have to `convert(Expr, x)`, which is simply done by `x.expr`.
end

function lambdify(m::Array, vars::Vector{Symbol}) # add a method for lambdifying Vector{Sym} or Matrix{Sym}
	return eval(Expr(:function, Expr(:call, gensym(), vars...), convert(Expr, m))) # such convert is slow for large arrays. Wait to be optimized.
end

"""
Fast lambdification (with less memory cost) by direct substitution
	x accept both Type{Sym} and Type{Array{Sym}} of any dimensions.
"""
function lambdify_fast(x, vars::Vector{Sym{T}}) where {T}
	function func(args::Number...)
		eval.(sub!(x, Tuple(Pair.(vars, args))))
		# Vectorized evaluation
	end
	return func
end


#####################################################################
########################## Tests Start Here #########################
#####################################################################
#=
@def x y z
v = [x y z]
#I = Sym{Int64}(:im) # to avoid the issues of compatibility for the default Type{Complex} (recall that in Julia, `const im = Complex(false, true)`)
test = [sin(x)+y x^y; x+y x]
#test = sub!(test,(x=>:a,y=>z))

@btime begin # 5.302 s (3361039 allocations: 196.32 MiB)
	# test for 100*100 matrix
	test = Array{Sym{Real}}(undef, 100, 100)
	for i in 1:100
		for j in 1:100
			test[i,j] = sin(i*x+y^j)
		end
	end

	func = lambdify_fast(test, [x,y])
	Base.invokelatest(func,1,2); # to avoid world age problem, see https://discourse.julialang.org/t/how-to-bypass-the-world-age-problem/7012/25
end

@btime begin # 17.204 s (15956305 allocations: 751.22 MiB)
	# test for 100*100 matrix
	test = Array{Sym{Real}}(undef, 100, 100)
	for i in 1:100
		for j in 1:100
			test[i,j] = sin(i*x+y^j)
		end
	end

	func = lambdify(test, [:x, :y])
	Base.invokelatest(func,1,2); # to avoid world age problem, see https://discourse.julialang.org/t/how-to-bypass-the-world-age-problem/7012/25
end=#

end