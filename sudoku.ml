open Core.Std

let compliment a b =
  Array.filter ~f:(fun x -> not (Array.mem b x)) a
;;

let union a b =
  Array.filter ~f:(fun x -> (Array.mem b x)) a
;;

let fullA = [|1;2;3;4;5;6;7;8;9|];;
let fullAOpt = [|Some 1;Some 2;Some 3;Some 4;Some 5;Some 6;Some 7;Some 8;Some 9|];;

let getRow n table =
  Array.get table n
;;

let getCol n table =
  Array.map ~f:(fun x -> Array.get x n) table
;;

let getZone x y table =
  let getTris i = Array.filteri ~f:(fun j _ -> j / 3 = i / 3) in
  let row3 = getTris x table in
  Array.concat_map ~f:(getTris y) row3
;;

let getCell x y table =
  let row = getRow x table in
  Array.get row y
;;


let candidates x y table =
  let rowCandidates = compliment fullA (Array.filter_opt(getRow x table)) in
  let colCandidates = compliment fullA (Array.filter_opt(getCol y table)) in
  let zoneCandidates = compliment fullA (Array.filter_opt(getZone x y table)) in
  match (getCell x y table) with
  | Some x -> [|x|]
  | None -> union zoneCandidates (union rowCandidates colCandidates)
;;

let missingInSet set = compliment fullAOpt set

let fillPosibleInSet set =
  let someOrMissing e =
    match e with
    | Some x -> [|Some x|]
    | None -> missingInSet set
  in
  Array.map ~f:(someOrMissing) set

let range9 = [0; 1; 2; 3; 4; 5; 6; 7; 8];;
let range9A = [|0;1;2;3;4;5;6;7;8|];;

let allCandidates table =
  let perRow x = List.map ~f:(fun y -> candidates x y table) range9 in
  List.map ~f:perRow range9
;;

let fillSingleCandidate table =
  let singleOrNone candidates = if (Array.length candidates = 1) then Some (Array.get candidates 0) else None in
  let perRow x = Array.map ~f:(fun y -> candidates x y table |> singleOrNone) range9A in
  Array.map ~f:perRow range9A

let rec printRow row = 
  let printOpt = function
    | None -> print_string "."
    | Some i -> print_int i
    in
  match row with
  | [] -> print_endline "|"
  | x::xs -> print_string "|"; printOpt x; printRow xs

let printTable table =
  List.map ~f:(fun r -> printRow (Array.to_list r)) (Array.to_list table)

(* let testPrintTable = printTable easyS;; *)
(* let testPrintTablePass1 = fillSingleCandidate easyS |> printTable ;; *)

let isDone table =
  let rec fullRowList = function
    | [] -> true
    | x::xs -> match x with
               | Some _ -> fullRowList xs
               | None -> false
  in
  let fullRow a = Array.to_list a |> fullRowList in
  Array.for_all table fullRow
;;

let doneTest =
  [|
    [|Some 1; Some 2; Some 3|];
    [|Some 4; Some 5; Some 6|];
    [|Some 7; Some 8; Some 9|]
   |];;

let solve passes table =
  let rec solve_run cnt table =
    if cnt = 0
    then table
    else fillSingleCandidate table |> solve_run (cnt-1) in
  solve_run passes table
(* |6|2|9|1|7|8|4|3|5| *)
(* |.|4|.|3|6|2|.|9|.| *)
(* |1|.|.|5|9|4|.|.|6| *)
(* |.|.|8|6|.|3|5|.|.| *)
(* |3|.|.|2|.|5|.|.|4| *)
(* |4|5|6|7|.|9|2|.|.| *)
(* |9|.|.|8|5|7|.|.|2| *)
(* |.|8|.|9|2|1|.|6|.| *)
(* |.|1|.|4|3|6|.|5|.| *)
