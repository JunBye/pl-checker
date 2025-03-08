(* Exercise 1. merge *)
open Ex1
open Testlib

module TestEx1: TestEx =
  struct
    type testcase =
      | MERGE of int list * int list * int list

    let testcases: testcase list =
      [ MERGE ([8; 7; 6; 5], [4; 3; 2; 1], [8; 7; 6; 5; 4; 3; 2; 1]);
        MERGE ([4; 3; 2; 1], [8; 7; 6; 5], [8; 7; 6; 5; 4; 3; 2; 1]);
        MERGE ([8; 6; 4; 2], [7; 5; 3; 1], [8; 7; 6; 5; 4; 3; 2; 1]);
        MERGE ([8; 7; 6; 5; 4; 3; 2; 1], [], [8; 7; 6; 5; 4; 3; 2; 1]);
        MERGE ([8; 8; 8; 7], [8; 8; 7; 7], [8; 8; 8; 8; 8; 7; 7; 7]);
      ]

    let runner (tc: testcase): bool =
      match tc with
      | MERGE (a, b, ans) -> merge(a,b) = ans
    
    let string_of_list (lst : int list): string =
      "[" ^ (String.concat ", " (List.map string_of_int lst)) ^ "]"

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | MERGE (a, b, ans) ->
          ( string_of_list a ^ string_of_list b 
          , string_of_list ans
          , string_of_list (merge(a,b))
          )
  end

open TestEx1
let _ = wrapper testcases runner string_of_tc
