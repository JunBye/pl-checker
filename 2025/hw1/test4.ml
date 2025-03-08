(* Exercise 4. parenize *)
open Ex4
open Testlib

module TestEx4: TestEx =
  struct
    type testcase =
      | PARENIZE of tourna * string * string

    let testcases: testcase list =
      [ PARENIZE (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil),
                  "NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)", 
                  "((Korea Portugal) Brazil)");
      ]

    let runner (tc: testcase): bool =
      match tc with
      | PARENIZE (a, st, ans) -> parenize(a) = ans
    

    let string_of_tc (tc: testcase): string * string * string =
      match tc with
      | PARENIZE (a, st, ans) ->
          ( st
          , ans
          , parenize(a)
          )
  end

open TestEx4
let _ = wrapper testcases runner string_of_tc
