(* Modern Compiler Implementation in ML
 * Exercises written in OCaml
 *
 * Yuhui Li
 * 1.1b Key-Value Pair BST
 *)

type key = string
type 'a tree =
  | LEAF
  | TREE of 'a tree * (key * 'a) * 'a tree

let empty = LEAF

let rec insert k v = function
  | LEAF -> TREE(LEAF,(k,v),LEAF)
  | TREE(l,(k',v'),r) ->
    if k < k' then TREE((insert k v l),(k',v'),r)
    else if k > k' then TREE(l,(k',v'),(insert k v r))
    else TREE(l,(k',v'),r)
