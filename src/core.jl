############ managing functions symbols ################

#### First, we make a basic tree system 

abstract type AbstractCodeSpace end
abstract type NSyntaxNode end

const NodeType = Union{Number, Symbol, Expr}

struct NSyntaxTree{T <: NSyntaxNode}
    root::T
end
NSyntaxTree(n::NSyntaxNode) = NSyntaxTree{typeof(n)}(n)

mutable struct SymbolicSpace{T <: Any} <: AbstractCodeSpace 
    code::NSyntaxTree
    const var::Dict{Symbol, T}

    ## Constructor 

    SymbolicSpace{T}() where T<: Any = new{T}(NSyntaxTree(ConstNode(0)), Dict{Symbol, T}())
    SymbolicSpace{T}(ex::Expr) where T<: Any = new{T}(totree(ex), Dict{Symbol, T}())
    
end

## Better use immutable struct for this. type parameters reduce the need for the compiler to infer fields type 


struct ConstNode{T <: Number} <: NSyntaxNode
    n::T
end
ConstNode(n::Number) = ConstNode{typeof(n)}(n)

struct SymbNode <: NSyntaxNode
    n::Symbol
end

struct AddNode <: NSyntaxNode
    n1::NSyntaxNode
    n2::NSyntaxNode
    
    function AddNode(n1::NSyntaxNode, n2::NSyntaxNode)
        if n1 isa ConstNode && n2 isa ConstNode
            return ConstNode(n1.n + n2.n)
        elseif iszero(n1)
            return n2
        elseif iszero(n2)
            return n1
        else
            return new(n1, n2)
        end
    end
end

AddNode(n1::NodeType, n2::NodeType) = AddNode(_make_node(n1), _make_node(n2))

struct SubNode <: NSyntaxNode
    n1::NSyntaxNode
    n2::NSyntaxNode

    function SubNode(n1::NSyntaxNode, n2::NSyntaxNode)
        if n1 isa ConstNode && n2 isa ConstNode
            return ConstNode(n1.n - n2.n)
        elseif iszero(n2)
            return n1
        else
            return new(n1, n2)
        end
    end
end


SubNode(n1::NodeType, n2::NodeType) = SubNode(_make_node(n1), _make_node(n2))

struct ProdNode <: NSyntaxNode
    n1::NSyntaxNode
    n2::NSyntaxNode

    function ProdNode(n1::NSyntaxNode, n2::NSyntaxNode)
        if n1 isa ConstNode && n2 isa ConstNode
            return ConstNode(n1.n * n2.n)
        elseif iszero(n1) || iszero(n2)
            return ConstNode(0)
        elseif isone(n1)
            return n2
        elseif isone(n2)
            return n1
        else
            return new(n1, n2)
        end
    end
end
ProdNode(n1::NodeType, n2::NodeType) = ProdNode(_make_node(n1), _make_node(n2))

struct DivNode <: NSyntaxNode
    n1::NSyntaxNode
    n2::NSyntaxNode
    
    function DivNode(n1::NSyntaxNode, n2::NSyntaxNode)
        if n1 isa ConstNode && n2 isa ConstNode
            return ConstNode(n1.n / n2.n)
        elseif iszero(n1) && !iszero(n2)
            return ConstNode(0)
        elseif iszero(n2)
            throw(ZeroDivisionError("can create a node that divide by 0"))
        elseif isone(n2)
            return n1
        else
            return DivNode(n1, n2)
        end
    end
end

DivNode(n1::NodeType, n2::NodeType) = DivNode(_make_node(n1), _make_node(n2))

struct PowNode <: NSyntaxNode
    n1::NSyntaxNode
    n2::NSyntaxNode
    
    function PowNode(n1::NSyntaxNode, n2::NSyntaxNode)
        if n1 isa ConstNode && n2 isa ConstNode
            return ConstNode(n1.n ^ n2.n)
        elseif iszero(n1)
            return ConstNode(0)
        elseif iszero(n2)
            return ConstNode(1)
        else
            return new(n1, n2)
        end
    end
end


PowNode(n1::NodeType, n2::NodeType) = PowNode(_make_node(n1), _make_node(n2))

## Todo : Add checks for non n positive values
struct LnNode <: NSyntaxNode
    n::NSyntaxNode

    LnNode(n::NSyntaxNode) = new(n)
end

LnNode(n::NodeType) = LnNode(_make_node(n))

struct LogNode <: NSyntaxNode
    n::NSyntaxNode

    LogNode(n::NSyntaxNode) = new(n)
end

LogNode(n::NodeType) = LogNode(_make_node(n))
### Spaces function 

setvar(space::SymbolicSpace{T}, s::Symbol, val) where T = (space.var[s] = convert(T, val))

####### Operations

## since I will likely modify the field's names, it's safer for this case to rely on their order
Base.getindex(n::NSyntaxNode, I::Integer) = begin
    fields = fieldnames(typeof(n))
    getfield(n, fields[I])
end

## TODO : Add more operator when the code base will be ready
getop(::AddNode) = :+
getop(::SubNode) = :-
getop(::ProdNode) = :*
getop(::DivNode) = :/
getop(::PowNode) = :^
getop(::LnNode) = :log
getop(::LogNode) = :log10

toexpr(tree::NSyntaxTree) = toexpr(tree.root)
toexpr(n::ConstNode) = n.n
toexpr(n::SymbNode) = n.n
toexpr(n::NSyntaxNode) = Expr(:call, getop(n), toexpr(n.n1), toexpr(n.n2))
toexpr(n::LnNode) = Expr(:call, getop(n), toexpr(n[1]))
toexpr(n::LogNode) = Expr(:call, getop(n), toexpr(n[1]))

totree(ex::Expr) = NSyntaxTree(_make_node(ex))

_make_node(n::Number) = ConstNode(n)
_make_node(s::Symbol) = SymbNode(s)
_make_node(::Val{:+}, n1::NodeType, n2::NodeType) = AddNode(n1, n2)
_make_node(::Val{:-}, n1::NodeType, n2::NodeType) = SubNode(n1, n2)
_make_node(::Val{:*}, n1::NodeType, n2::NodeType) = ProdNode(n1, n2)
_make_node(::Val{:/}, n1::NodeType, n2::NodeType) = DivNode(n1, n2)
_make_node(::Val{:^}, n1::NodeType, n2::NodeType) = PowNode(n1, n2)
_make_node(::Val{:ln}, n::NodeType) = LnNode(n)
_make_node(::Val{:log}, n::NodeType) = LogNode(n)
_make_node(ex::Expr) = begin
    ch = ex.args
    if length(ch) == 2
        _make_node(Val(ch[1]), ch[2])
    elseif length(ch) == 3
        _make_node(Val(ch[1]), ch[2], ch[3])
    end
end

### Evaluate code

Base.eval(sp::SymbolicSpace) = eval(sp.code, sp.var)
Base.eval(tr::NSyntaxTree, var::Dict) = eval(tr.root, var)
Base.eval(n::AddNode, var::Dict) = eval(n[1], var) + eval(n[2], var)
Base.eval(n::SubNode, var::Dict) = eval(n[1], var) - eval(n[2], var)
Base.eval(n::ProdNode, var::Dict) = eval(n[1], var) * eval(n[2], var)
Base.eval(n::DivNode, var::Dict) = eval(n[1], var) / eval(n[2], var)
Base.eval(n::PowNode, var::Dict) = eval(n[1], var) ^ eval(n[2], var)
Base.eval(n::ConstNode, var::Dict) = n[1]
Base.eval(s::SymbNode, var::Dict) = var[s[1]]
