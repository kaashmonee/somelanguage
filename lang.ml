(* Declaring some datatypes... *)

(* type 'a value = INT of 'a *)

type operation = 
    Add
  | Subtract
  | Multiply
  | Divide

type value = INT of int

type token = Val of value | Op of operation 
type expression = token list

(* 
    3+4 => [Val 3, Op Add, Val 4]
    => (7, Empty)
 *)

type ast = Leaf of value | Node of ast * operation * ast | Empty

let rec buildast (exp : expression) : ast = 
    match exp with 
        [] -> Empty
      | e::[] -> match e with Val x -> Leaf x | _ -> failwith "Leaf not value...?"
      | v::op::es -> Node (Leaf v, op, buildast es)


let rec parseast (tree : ast) : value = 
    match tree with
        Leaf v -> v
      | Node (left, op, right) -> 
            match op with
                Add -> (parseast left) + (parseast right)
              | Subtract -> (parseast left) - (parseast right)
              | Multiply -> (parseast left) * (parseast right)
              | Divide -> (parseast left) / (parseast right)
      | Empty -> INT 0

(* 
 * TODO: write a main function to test output of some expression
 *)

