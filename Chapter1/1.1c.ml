(* Modern Compiler Implementation in ML
* Exercises written in OCaml
*
* Yuhui Li
* 1.1c BST insert
*)

#use "1.1.ml";;

let (|>) x f = f x

let rec list_char ch = match ch with
  | "" -> []
  | ch -> (String.get ch 0 ) :: (list_char (String.sub ch 1 ( (String.length ch)-1) ) )
let list_char_to_list_string lc =
  List.fold_right (fun x acc -> (String.make 1 x)::acc) lc []
let tree_of_list_string ls =
  List.fold_left (fun acc x -> (insert x acc)) empty ls
let str_to_tree str =
  list_char str
  |> list_char_to_list_string
  |> tree_of_list_string

let seq1str = "tspipfbst"
let seq2str = "abcdefghi"

let t1 = str_to_tree seq1str
let t2 = str_to_tree seq2str
