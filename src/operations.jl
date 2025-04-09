################# Operations on a symbolic system ####################

# ToDo : Fix this for node evaluating to zero
Base.iszero(n::NSyntaxNode) = false
Base.iszero(n::ConstNode) = iszero(n.n)

# TODO : Fix this for node evaluating to one
Base.isone(n::NSyntaxNode) = false
Base.isone(n::ConstNode) = isone(n.n)
get_children(A::AbstractArray) = A
get_children(e::Expr) = e.args
get_children(n) = ()
get_children(n::NSyntaxNode) = [getfield(n,f) for f in fieldnames(n)]
get_children(n::SymbNode) = ()
get_children(n::ConstNode) = ()

get_child_count(n) = length(get_childre(n))

isleave(n) = isempty(get_children(n))

negate(n::NSyntaxNode) = n

derivate(n::Number) = 0
derivate(n::ConstNode) = ConstNode(0)
derivate(n::SymbNode) = ConstNode(1)

derivate(n::AddNode) = derivate(n.n1) + derivate(n.n2)
derivate(n::SubNode) = derivate(n.n1) - derivate(n.n2)
derivate(n::ProdNode) = derivate(n.n1)*n.n2 + n.n1*derivate(n.n2)
derivate(n::DivNode) = (derivate(n.n1)*n.n2 - n.n1*derivate(n.n2))/ n[2]^2
derivate(n::PowNode) = begin 
    if n.n2 isa ConstNode
        return n.n2*derivate(n.n1)*n.n1^(n.n2.n-1)
    else
        return n.n1^n.n2 * (derivate(n.n2)*LnNode(n.n1) + n.n2* derivate(n.n1))/n.n1
    end
end
derivate(n::LnNode) = derivate(n.n)/n.n
derivate(n::LogNode) = derivate(n.n)/ (n.n*log(10))

derivate(tree::NSyntaxTree) = NSyntaxTree(derivate(tree.root))


### Traversal utilities

function DFS_traversal(tree::NSyntaxTree)

    stack = Pair[tree.root => false]
    result = NSyntaxNode[]

    while !isempty(stack)
        node, visited = pop!(stack)
        
        if visited
            push!(result, node)
        else
            push!(stack, node => true)
            for ch in get_children(node)
                push!(stack, ch => false)
            end
        end
    end

    return result
end

function BFS_traversal(tree::NSyntaxTree)

    stack = NSyntaxNode[tree.root]
    result = NSyntaxNode[tree.root]

    while !isempty(stack)
        
        node = pop!(stack)

        for ch in get_children(node)
            push!(result, ch)
            push!(stack, ch)
        end
    end

    return result
end

function _iterative_deriv(tree::NSyntaxTree)
    node_arr = DFS_traversal(tree)
    der_arr = NSyntaxNode[]
    
    while !isempty(node_arr)
        n = pop!(node_arr)

        if isleave(n)
            ## if we encounter a leaf, it means the derivative of the node can be obtained just from itself
            der = derivate(n)
            push!(der_arr, der)
        else
            children = get_children(n)
            l = length(children)
            
            # Compute the derivatives of the child nodes
            child_ders = [pop!(der_arr) for _ in 1:l]
            push!(der_arr, derivate(n, reverse(child_ders)...))
        end
    end

    return NSyntaxTree(pop!(der_arr))
end
