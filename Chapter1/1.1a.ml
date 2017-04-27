(* Modern Compiler Implementation in ML
* Exercises written in OCaml
*
* Yuhui Li
* 1.1a BST search
*)

#use "1.1.ml";;
let rec member k = function
  | LEAF -> false
  | TREE(l,k',r) ->
    if k = k' then true
    else if k < k' then (member k l)
    else (member k r)
