(* Modern Compiler Implementation in ML
 * Exercises written in OCaml
 *
 * Yuhui Li
 * 1.1 bst
 *)

type key = string
type tree =
  | LEAF
  | TREE of tree * key * tree

let empty = LEAF

let rec insert k = function
  | LEAF -> TREE(LEAF,k,LEAF)
  | TREE(l,k',r) ->
    if k < k' then TREE((insert k l),k',r)
    else if k > k' then TREE(l,k',(insert k r))
    else TREE(l,k,r)
