open Core.Std
open Printf

type sType =
  | None
  | Fixed of int
  | CList of int list


let testS = [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;]

let testS2 = [11;12;13;14;15;16;17;18;19;21;22;23;24;25;26;27;28;29;31;32;33;34;35;36;37;38;39;41;42;43;44;45;46;47;48;49;51;52;53;54;55;56;57;58;59;61;62;63;64;65;66;67;68;69;71;72;73;74;75;76;77;78;79;81;82;83;84;85;86;87;88;89;91;92;93;94;95;96;97;98;99]

exception Cant_parse_string
let easySText = "97..21..3..1.4....64.3...898.6....2.4.52.68.7.2....9.458...2.16....8.2..2..76..48"
let easySText2 = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
let firstSText = "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........"
let sStringToTable s =
  let chat_to_sType c =
    if Char.is_digit c then Fixed (Char.get_digit_exn c)
    else if (c = '.') then None
    else raise Cant_parse_string
  in
  String.to_list s |> List.map ~f:chat_to_sType

let firstS = sStringToTable firstSText
let easyS = sStringToTable easySText
let easyS2 = sStringToTable easySText2

let getRowSet table index =
  let offsetedTable = List.drop table (index * 9) in
  List.take offsetedTable 9


let getColSet table index =
  let rec getColSet' table =
    match table with
    | [] -> []
    | x::xs -> x :: getColSet' (List.drop xs (9-1))
  in
  List.drop table index |> getColSet'

let getZoneSet table index =
  let transIndex i = (i/3)*27+i%3*3 in
  let rec getZoneSet' times table =
    if List.length table >= 3 && times > 0 then
      List.append (List.take table 3) (List.drop table 9 |> getZoneSet' (times-1))
    else
      []
  in
  List.drop table (transIndex index) |> getZoneSet' 3


let compliment a b =
  List.filter ~f:(fun x -> not (List.mem b x)) a


let union a b =
  List.filter ~f:(fun x -> (List.mem b x)) a


(* let fullSet = [Fixed 1; Fixed 2; Fixed 3; Fixed 4; Fixed 5; Fixed 6; Fixed 7; Fixed 8; Fixed 9] *)
let fullSet = [1;2;3;4;5;6;7;8;9]

let testttt = [Fixed 3; None; CList [1;2;4]; CList [1;4;4]]

let rec getFixed set =
  match set with
  | [] -> []
  | x::xs -> match x with
             | Fixed v -> v :: getFixed xs
             | _ -> getFixed xs

let rec getCListed set =
  match set with
  | [] -> []
  | x::xs -> match x with
             | CList v -> v :: getCListed xs
             | _ -> getCListed xs

let getMissing set =
  getFixed set |> compliment fullSet


let fillMissing set =
  let missing = CList (getMissing set) in
  let rec fillMissing' set =
  match set with
  | [] -> []
  | x::xs -> match x with
             | Fixed v -> Fixed v :: fillMissing' xs
             | _ -> missing :: fillMissing' xs
  in
  fillMissing' set

let testSet = [Fixed 1; None; Fixed 3; Fixed 4; None; Fixed 5; Fixed 6; None; Fixed 7;]
let testFill = fillMissing testSet

let fillAllMissing listOfSets =
  List.map ~f:fillMissing listOfSets


let fillAllMissingBySetType table setGetter =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  let listOfSets = List.map ~f:(setGetter table) iRange in
  fillAllMissing listOfSets

let getAllRows table =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  List.map ~f:(getRowSet table) iRange

let getAllCols table =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  List.map ~f:(getColSet table) iRange

let getAllZones table =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  List.map ~f:(getZoneSet table) iRange

let tva = testS2
let tre = getAllCols testS2 |> List.concat |> getAllCols |> List.concat
let fyra = getAllZones testS2 |> List.concat |> getAllZones |> List.concat
let _ = getRowSet firstS 1
let _ = getAllCols firstS
   
let r = fillAllMissingBySetType firstS getRowSet
let c = fillAllMissingBySetType firstS getColSet
let z = fillAllMissingBySetType firstS getZoneSet
let transformRowsToTable rows = List.concat rows
let transformColsToTable cols = List.concat cols |> getAllCols |> List.concat
let transformZonesToTable zones = List.concat zones |> getAllZones |> List.concat

let mergeCell c1 c2 =
  let uList = union c1 c2 in
  match uList with
  | [] -> None
  | x::[] -> Fixed x
  | _ -> CList uList

let rec mergeTables t1 t2 =
  match t1, t2 with
  | [], ys -> ys
  | xs, [] -> xs
  | x::xs, y::ys -> match x, y with
                    | CList x', CList y' -> mergeCell x' y' :: mergeTables xs ys
                    | Fixed x' , _ -> Fixed x' :: mergeTables xs ys
                    | _ , Fixed y' -> Fixed y' :: mergeTables xs ys
                    | None , _ -> None :: mergeTables xs ys
                    | _ , None  -> None :: mergeTables xs ys

let _ = mergeTables [Fixed 1; CList [1; 2; 3]; None] [CList [1; 2; 4]; CList [1; 2; 4]; None]
let _ = mergeTables [None; CList [1; 2; 3]; None] [Fixed 2; CList [1; 2; 4]; None]
                                                


(* List.iter ~f:(printf " %d") v *)

let rec rPrint row =
  let cell3 = List.take row 3 in
  let cellRest = List.drop row 3 in
  let rec rPrintInner cell3 =
  match cell3 with
  | [] -> ()
  | x::xs -> match x with
            | CList _ -> print_string " * |";rPrintInner xs
            | Fixed i-> printf " %d |" i; rPrintInner xs
            | None -> print_string " . |"; rPrintInner xs
  in
  match row with
  | [] -> print_string "\n"; ()
  | _ -> rPrintInner cell3; print_string "|"; rPrint cellRest

let rec rfPrint row =
  match row with
  | [] -> print_string "\n"; ()
  | x::xs -> match x with
            | CList v -> List.iter ~f:(printf " %d") v; print_string " |";rfPrint xs
            | Fixed i-> printf " %d |" i; rfPrint xs
            | None -> print_string " . |"; rfPrint xs

let printTable table =
  let rowsT = getAllRows table in
  let rec printTable' rows =
    let row3 = List.take rows 3 in
    let rowRest = List.drop rows 3 in
    match row3 with
    | [] -> ()
    | l -> List.map ~f:(rPrint) l; print_string "===========00===========00===========00\n"; printTable' rowRest
  in
  printTable' rowsT

let allCandidates table =
  let r' = fillAllMissingBySetType table getRowSet in
  let c' = fillAllMissingBySetType table getColSet in
  let z' = fillAllMissingBySetType table getZoneSet in
  let r = transformRowsToTable r' in
  let c = transformColsToTable c' in
  let z = transformZonesToTable z' in
  (* List.iter ~f:(printf "%d ") r *)
(*   List.iter ~f:(rPrint) (getAllRows r);
  print_string "\n";
  List.iter ~f:(rPrint) (getAllRows c);
  print_string "\n";
  List.iter ~f:(rPrint) (getAllRows z);
   print_string "\n"; *)
  mergeTables r c |> mergeTables z

let printTableAndReturn t =
  printTable t; print_string "\n"; t

let solveTest table =
  printTableAndReturn table |> 
  allCandidates |> printTableAndReturn |>
  allCandidates |> printTableAndReturn |>
  allCandidates |> printTableAndReturn |>
  allCandidates |> printTableAndReturn |>
  allCandidates |> printTableAndReturn |>
  allCandidates |> printTableAndReturn
;;

let grade table = List.length (getFixed table)

let solve table =
  let rec solve' t l =
    let newTable = allCandidates t in
    let newGrade = grade newTable in
    if l = 81 then 
      printTable newTable
    else if l = newGrade then printTable newTable
    else solve' newTable newGrade;
    in
  solve' table (grade table)


let listOfUniqueInSet set =
  let allCListed = getCListed set |> List.concat in
  unique allCListed


let unique l =
  let inOrOut count acc elem =
    if (count = 1) then elem::acc
    else acc 
  in
  let rec unique' count acc = function
  | [] -> []
  | [hd] -> inOrOut (count + 1) acc hd
  | hd :: (snd :: _ as tl) -> 
    if hd = snd then unique' (count+1) acc tl
    else unique' 0 (inOrOut (count+1) acc hd) tl
  in
  List.sort compare l |> unique' 0 []

let t table =
  let cols = allCandidates table |> getAllCols in
  List.map cols ~f:(listOfUniqueInSet)

let rec k uList set = 
  let hasUniq uList v =
    let u = union uList v in
    if List.length u = 1 then Fixed (List.hd_exn u)
    else CList v
  in
  match set with
  | [] -> []
  | x::xs -> match x with
            | CList v -> (hasUniq uList v) :: k uList xs
            | Fixed i -> Fixed i :: k uList xs
            | None -> None :: k uList xs