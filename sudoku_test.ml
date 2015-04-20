open Core.Std
open OUnit2


let test_compliment test_ctxt = assert_equal (compliment fullA [|2;3;4|]) [|1;5;6;7;8;9|];;
let test_union test_ctxt =
  let a = union fullA ([|2;3;4|]) in
  let b = ([|2;3;4|]) in
  assert_equal a b
;;

let tes =
  union fullA ([|2;3;4|])
;;

let testTable = [|[|Some 1;Some 2;None;None;None;Some 3;None;None;Some 6|];
                  [|Some 8;Some 9;None;Some 4;None;Some 6;None;None;None|];
                  [|Some 4;Some 2;Some 7;None;None;Some 6;None;None;None|];
                 |];;

let easyS =
[|[|None; Some 2; None; Some 1; Some 7; Some 8; None; Some 3; None|];
  [|None; Some 4; None; Some 3; None; Some 2; None; Some 9; None|];
  [|Some 1; None; None; None; None; None; None; None; Some 6|];
  [|None; None; Some 8; Some 6; None; Some 3; Some 5; None; None|];
  [|Some 3; None; None; None; None; None; None; None; Some 4|];
  [|None; None; Some 6; Some 7; None; Some 9; Some 2; None; None|];
  [|Some 9; None; None; None; None; None; None; None; Some 2|];
  [|None; Some 8; None; Some 9; None; Some 1; None; Some 6;None|];
  [|None; Some 1; None; Some 4; Some 3; Some 6;None; Some 5; None|];
|];;


let tRow = getRow 1 testTable;;
let tCol = getCol 1 testTable;;
let test_getRow_filtered test_ctxt =
  assert_equal (Array.filter_opt tRow) ([|8;9;4;6|]);;
let test_getCol_filtered test_ctxt =
  assert_equal (Array.filter_opt tCol) ([|2;9;2|]);;
let test_getCol_filtered2 test_ctxt =
  assert_equal (Array.filter_opt (getCol 8 easyS)) ([|6;4;2|]);;
let test_getRow_filtered2 test_ctxt =
  assert_equal (Array.filter_opt (getRow 8 easyS)) ([|1;4;3;6;5|]);;

let test_getCell test_ctxt =
  assert_equal (getCell 0 3 easyS) (Some 1);;
let test_getCell2 test_ctxt =
  assert_equal (getCell 0 2 easyS) (None);;

let test_candidates test_ctxt =
  assert_equal (candidates 8 8 easyS) ([|7;8;9|]);;
let test_candidates2 test_ctxt =
  assert_equal (candidates 0 8 easyS) ([|5|]);;

let suite =
  "aTest">:::
    ["test_compliment">:: test_compliment;
     "test_union">:: test_union;
     "test_getRow_filtered">:: test_getRow_filtered;
     "test_getRow_filtered2">:: test_getRow_filtered2;
     "test_getCol_filtered">:: test_getCol_filtered;
     "test_getCol_filtered2">:: test_getCol_filtered2;
     "test_candidates">:: test_candidates;
     "test_candidates2">:: test_candidates2;
     "test_getCell">:: test_getCell;
     "test_getCell2">:: test_getCell2;
   ]
;;

let () =
  run_test_tt_main suite
;;
