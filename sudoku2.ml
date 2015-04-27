open Core.Std

let testS = [1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;1;2;3;4;5;6;7;8;9;];;

let testS2 = [1;2;3;4;5;6;7;8;9;11;12;13;14;15;16;17;18;19;21;22;23;24;25;26;27;28;29;31;32;33;34;35;36;37;38;39;41;42;43;44;45;46;47;48;49;51;52;53;54;55;56;57;58;59;61;62;63;64;65;66;67;68;69;71;72;73;74;75;76;77;78;79;81;82;83;84;85;86;87;88;89];;

exception Cant_parse_string;;

let firstSText = "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........";;
let sStringToTable s =
  let chat_to_sType c =
    if Char.is_digit c then Fixed (Char.get_digit_exn c)
    else if (c = '.') then None
    else raise Cant_parse_string
  in
  String.to_list s |> List.map ~f:chat_to_sType
;;
let firstS = sStringToTable firstSText;;

let getRowSet table index =
  let offsetedTable = List.drop table (index * 9) in
  List.take offsetedTable 9
;;

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
;;

let compliment a b =
  List.filter ~f:(fun x -> not (List.mem b x)) a
;;

let union a b =
  List.filter ~f:(fun x -> (List.mem b x)) a
;;

type sType =
  | None
  | Fixed of int
  | CList of int list
;;

(* let fullSet = [Fixed 1; Fixed 2; Fixed 3; Fixed 4; Fixed 5; Fixed 6; Fixed 7; Fixed 8; Fixed 9];; *)
let fullSet = [1;2;3;4;5;6;7;8;9];;

let testttt = [Fixed 3; None; CList [1;2;4]];;

let rec getFixed set =
  match set with
  | [] -> []
  | x::xs -> match x with
             | Fixed v -> v :: getFixed xs
             | _ -> getFixed xs
;;

let getMissing set =
  getFixed set |> compliment fullSet
;;

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
;;
let testSet = [Fixed 1; None; Fixed 3; Fixed 4; None; Fixed 5; Fixed 6; None; Fixed 7;];;
let testFill = fillMissing testSet;;

let fillAllMissing listOfSets =
  List.map ~f:fillMissing listOfSets
;;

let fillAllMissingBySetType table setGetter =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  let listOfSets = List.map ~f:(setGetter table) iRange in
  fillAllMissing listOfSets
;;

let getAllCols table =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  List.map ~f:(getColSet table) iRange
;;
let getAllZones table =
  let iRange = [0;1;2;3;4;5;6;7;8] in
  List.map ~f:(getZoneSet table) iRange
;;
let tva = testS2;;
let tre = getAllCols testS2 |> List.concat |> getAllCols |> List.concat;;
let fyra = getAllZones testS2 |> List.concat |> getAllZones |> List.concat;;
let r = fillAllMissingBySetType firstS getRowSet;;
let c = fillAllMissingBySetType firstS getColSet;;
let z = fillAllMissingBySetType firstS getZoneSet;;
let transformRowsToTable rows = List.concat rows;;
let transformColsToTable cols = List.concat cols |> getAllCols |> List.concat;;
let transformZonesToTable zones = List.concat zones |> getAllCols |> List.concat;;

let allCandidates table =
  
  (* 0 *)
  (* 3 *)
  (* 6 *)
  (* 27 (1*27+0) *)
  (* 30 (1*27+3) *)
  (* 33 (1*27+6) *)
  (* (2*27+0) *)
  (* (2*27+3) *)
  (* (2*27+6) *)

let test_getRowSet = getRowSet testS 7;;
let test_getColSet = getColSet testS 7;;
let test_getColSet = getZoneSet testS2 0;;
let test_getColSet = getZoneSet testS2 1;;
let test_getColSet = getZoneSet testS2 2;;
let test_getColSet = getZoneSet testS2 3;;
let test_getColSet = getZoneSet testS2 4;;
let test_getColSet = getZoneSet testS2 5;;
let test_getColSet = getZoneSet testS2 6;;
let test_getColSet = getZoneSet testS2 7;;
let test_getColSet = getZoneSet testS2 8;;

