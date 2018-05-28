datatype Tree<T> = Leaf | Node(Tree<T>, Tree<T>, T)
datatype List<T> = Nil | Cons(T, List<T>)

function flatten<T>(tree:Tree<T>):List<T>
    decreases tree;
{
    match tree 
        case Leaf => Nil
        case Node(left, right, root) => Cons(root, append(flatten(left), flatten(right)))
}

function append<T>(xs:List<T>, ys:List<T>):List<T>
    decreases xs;
{
	match xs 
        case Nil => ys
        case Cons(z,zs) => Cons(z,append(zs,ys))
}

function treeContains<T>(tree:Tree<T>, element:T):bool
    decreases tree;
{
	match tree
        case Leaf => false 
        case Node(left, right, root) => if root == element
                                        then true 
                                        else treeContains(left, element) || treeContains(right, element)
} 

function listContains<T>(xs:List<T>, element:T):bool
    decreases xs;
{
    match xs  
        case Nil => false
        case Cons(z,zs) => if z == element 
                           then true 
                           else listContains(zs, element)
}
lemma listContainsAppend<T>(xs:List<T>, ys:List<T>, element:T)
    ensures listContains(append(xs, ys), element) == (listContains(xs, element) || listContains(ys, element));
    decreases xs;
{ 
    match xs {
        case Nil => 
            calc {
                listContains(append(Nil, ys), element);
             == listContains(ys, element);
             == { assert listContains(Nil, ys) == false; }
                listContains(Nil, ys) || listContains(ys, element);
            }
        case Cons(z,zs) =>
            calc {
                listContains(append(Cons(z,zs), ys), element);
             == // definition of append
                listContains(Cons(z, append(zs, ys)), element);
             == // definition of listContains
                if z == element then true else listContains(append(zs, ys), element);
             == { listContainsAppend(zs, ys, element); }
                if z == element then true else listContains(zs, element) || listContains(ys, element);
             == listContains(Cons(z,zs), element) || listContains(ys, element);
            }
    }
}

lemma sameElements<T>(tree:Tree<T>, element:T)
    ensures treeContains(tree, element) <==> listContains(flatten(tree), element);
    decreases tree;
{
    match tree {
        case Leaf => 
            calc {
                treeContains(Leaf, element);
            ==> listContains(Nil, element);
            ==> listContains(flatten(Leaf), element);
            }

            calc {
                listContains(flatten(Leaf), element);
            ==> listContains(Nil, element);
            ==> treeContains(Leaf, element);
            }
        case Node(left, right, root) =>
            calc {
                treeContains(Node(left, right, root), element);
            ==> // definition of treeContains
                if root == element then true else treeContains(left, element) || treeContains(right, element);
            ==> { sameElements(left, element); sameElements(right, element); }
                if root == element then true else listContains(flatten(left), element) || listContains(flatten(right), element);
            ==> { listContainsAppend(flatten(left), flatten(right), element); }
                if root == element then true else listContains(append(flatten(left), flatten(right)), element);
            ==> listContains(Cons(root, append(flatten(left), flatten(right))), element);
            ==> listContains(flatten(Node(left, right, root)), element);
            }

            calc {
                listContains(flatten(Node(left, right, root)), element);
            ==> // definition of listContains
                listContains(Cons(root, append(flatten(left), flatten(right))), element);
            ==> if root == element then true else listContains(append(flatten(left), flatten(right)), element);
            ==> { listContainsAppend(flatten(left), flatten(right), element); }
                if root == element then true else listContains(flatten(left), element) || listContains(flatten(right), element);
            ==> { sameElements(left, element); sameElements(right, element); }
                if root == element then true else treeContains(left, element) || treeContains(right, element);
            ==> // definition of treeContains
                treeContains(Node(left, right, root), element);
            }
    }
}