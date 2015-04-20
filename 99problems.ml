
let last = print_int 1

let my_favorite_language languages =
    match languages with
    | first :: the_rest -> first
    | [] -> "OCaml" (* A good default! *)
;;


let my_favorite_language2 = function
    | first :: the_rest -> first
    | [] -> "OCaml" (* A good default! *)
;;

my_favorite_language ["English";"Spanish";"French"];;
  my_favorite_language2 ["English";"Spanish";"French"];;

  let length list =
    let rec length_h n list =
      match list with
      | [] -> n
      | (x::xs) -> length_h (n+1) xs
    in length_h 0 list
  ;;

length [3;4;2;4;3];;
    length [];;


let reverse list = 
    let rec reverse_h list rlist =
        match list with
        | [] -> rlist
        | x::xs -> reverse_h xs (x::rlist)
    in reverse_h list []
;;
  
reverse [4;3;2];;

let is_palindrome list =
  if list = reverse list then true else false
;;

is_palindrome [1;2;3;2;1];;
is_palindrome [1;2;3;2;3];;

(* There is no nested list type in OCaml, so we need to define one
    first. A node of a nested list is either an element, or a list of
    nodes. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let flatten list = 
    let rec flatten' acc = function
    | [] -> acc
    | One x :: xs -> flatten' (x::acc) xs
    | Many x :: xs -> flatten' acc (x@xs)
    in reverse (flatten' [] list)
;;

  flatten [One 2];;
  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
