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
derivate(n::DivNode) = DivNode(SubNode(ProdNode(derivate(n.n1), n.n2), ProdNode(n.n1, derivate(n.n2))), PowNode(n[2], ConstNode(2)))
derivate(n::PowNode) = ProdNode(PowNode(n.n1,n.n2), AddNode(ProdNode(derivate(n.n2), LnNode(n.n1)), DivNode(ProdNode(n.n2, derivate(n.n1)), n.n1)))
derivate(n::LnNode) = DivNode(derivate(n.n), n.n)
derivate(n::LogNode) = DivNode(derivate(n.n), ProdNode(n.n, ConstNode(log(10))))

derivate(tree::NSyntaxTree) = NSyntaxTree(derivate(tree.root))
