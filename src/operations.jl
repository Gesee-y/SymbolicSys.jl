################# Operations on a symbolic system ####################

# ToDo : Fix this for node evaluating to zero
iszero(n::NSyntaxNode) = false
iszero(n::ConstNode) = iszero(n.n)

# TODO : Fix this for node evaluating to one
isone(n::NSyntaxNode) = false
isone(n::ConstNode) = isone(n.n)
get_children(A::AbstractArray) = A
get_children(e::Expr) = e.args
get_children(n) = ()

is_leave(n) = isempty(get_children(n))

negate(n::NSyntaxNode) = n

derivate(n::Number) = 0
derivate(n::ConstNode) = 0
derivate(n::SymbNode) = 1

derivate(n::AddNode) = AddNode(derivate(n.n1), derivate(n.n2))
derivate(n::SubNode) = SubNode(derivate(n.n1), derivate(n.n2))
derivate(n::ProdNode) = AddNode(ProdNode(derivate(n.n1), n.n2), ProdNode(n.n1, derivate(n.n2)))
derivate(n::PowNode) = ProdNode(ProdNode(n.n2, derivate(n.n1)), PowNode(n.n1, ConstNode(n.n2[1] - 1)))

derivate(tree::NSyntaxTree) = NSyntaxTree(deriva
