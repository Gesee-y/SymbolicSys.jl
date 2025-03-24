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

is_leave(n) = isempty(get_children(n))

negate(n::NSyntaxNode) = n

derivate(n::Number) = 0
derivate(n::ConstNode) = ConstNode(0)
derivate(n::SymbNode) = ConstNode(1)

derivate(n::AddNode) = AddNode(derivate(n.n1), derivate(n.n2))
derivate(n::SubNode) = SubNode(derivate(n.n1), derivate(n.n2))
derivate(n::ProdNode) = AddNode(ProdNode(derivate(n.n1), n.n2), ProdNode(n.n1, derivate(n.n2)))
derivate(n::DivNode) = DivNode(SubNode(ProdNode(derivate(n.n1), n.n2), ProdNode(n.n1, derivate(n.n2))), PowNode(n[2], ConstNode(2)))
derivate(n::PowNode) = ProdNode(PowNode(n.n1,n.n2), AddNode(ProdNode(derivate(n.n2), LnNode(n.n1)), DivNode(ProdNode(n.n2, derivate(n.n1)), n.n1)))
derivate(n::LnNode) = DivNode(derivate(n.n), n.n)
derivate(n::LogNode) = DivNode(derivate(n.n), ProdNode(n.n, ConstNode(log(10))))

derivate(tree::NSyntaxTree) = NSyntaxTree(derivate(tree.root))


### Traversal utilities

function DFS_traversal(tree::NSyntaxTree)

    stack = Pair[tree.root => false]
    result = NSyntaxNode[]

    while!isempty(stack)
        node, visited = pop!(stack)
        
        if visited
            push!(result, node)
        else
            if isleave(node)
                push!(result, node)
            else
                push!(stack, node => true)

                for ch in get_children(node)
                    push!(stack, ch => false)
                end
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
end
