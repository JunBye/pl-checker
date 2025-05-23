(* Exercise 5. eval formula *)
open Ex5
open Testlib
open Printf

module TestEx5: TestEx =
  struct
    type testcase =
      | EVAL of formula * bool

    let testcases =
      [ EVAL (TRUE, true)
      ; EVAL (FALSE, false)
      ; EVAL (NOT TRUE, false)
      ; EVAL (NOT FALSE, true)
      ; EVAL (ANDALSO (TRUE, TRUE), true)
      ; EVAL (ANDALSO (TRUE, FALSE), false)
      ; EVAL (ANDALSO (FALSE, TRUE), false)
      ; EVAL (ANDALSO (FALSE, FALSE), false)
      ; EVAL (ORELSE (TRUE, TRUE), true)
      ; EVAL (ORELSE (TRUE, FALSE), true)
      ; EVAL (ORELSE (FALSE, TRUE), true)
      ; EVAL (ORELSE (FALSE, FALSE), false)
      ; EVAL (IMPLY (TRUE, TRUE), true)
      ; EVAL (IMPLY (TRUE, FALSE), false)
      ; EVAL (IMPLY (FALSE, TRUE), true)
      ; EVAL (IMPLY (FALSE, FALSE), true)
      ; EVAL (LESS (NUM 1, NUM 2), true)
      ; EVAL (LESS (NUM 2, NUM 1), false)
      ; EVAL (LESS (NUM (-100), NUM (-200)), false)
      ; EVAL (LESS (NUM (-200), NUM (-100)), true)
      ; EVAL (LESS (NUM (-10000), NUM 10000), true)
      ; EVAL (LESS (NUM 10000, NUM (-10000)), false)
      ; EVAL (LESS (PLUS (NUM 1, NUM 2), NUM 3), false)
      ; EVAL (LESS (PLUS (NUM 1, NUM 2), NUM 4), true)
      ; EVAL (LESS (MINUS (NUM 10, NUM 5), NUM 5), false)
      ; EVAL (LESS (MINUS (NUM 10, NUM 5), NUM 6), true)
      ; EVAL (LESS (PLUS (NUM (-12345), NUM 23456), NUM 11111), false)
      ; EVAL (LESS (PLUS (NUM (-12345), NUM 23456), NUM 11112), true)
      ; EVAL (LESS (MINUS (NUM 12345, NUM 23456), NUM (-11111)), false)
      ; EVAL (LESS (MINUS (NUM 12345, NUM 23456), NUM (-11110)), true)
      ; EVAL (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)), false)
      ; EVAL (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))), true)
      ; EVAL (NOT (ORELSE (IMPLY(TRUE, ANDALSO (TRUE, TRUE)), ANDALSO (TRUE, ANDALSO (TRUE, TRUE)))), false)
      ; EVAL (ORELSE (IMPLY(LESS (NUM (-10), NUM (-100)), ANDALSO (NOT TRUE, TRUE)), ANDALSO (TRUE, ANDALSO (LESS (NUM 10, PLUS (MINUS (NUM 10, NUM (-10)), NUM 30)), TRUE))), true)
      ; EVAL (ANDALSO (ORELSE (TRUE, FALSE), NOT (IMPLY (TRUE, FALSE))), true)
      ; EVAL (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2)), false)
      ; EVAL (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2)), true)
      ; EVAL (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE), false)
      ; EVAL (IMPLY (LESS (NUM 1, NUM 0), ANDALSO (TRUE, ORELSE (NOT TRUE, LESS (NUM 2, NUM 1)))), true)
      ; EVAL (NOT (NOT (NOT (NOT TRUE))), true)
      ; EVAL (LESS (MINUS (NUM 0, MINUS (NUM 0, NUM 0)), NUM 0), false)
      ; EVAL (LESS (PLUS (NUM (-5), NUM (-5)), MINUS (NUM 0, NUM 11)), false)
      ; EVAL (LESS (MINUS (PLUS (NUM 100, NUM (-50)), NUM 25), PLUS (NUM 0, NUM 26)), true)
      ; EVAL (IMPLY (IMPLY (TRUE, FALSE), FALSE), true)
      ; EVAL (IMPLY (IMPLY (FALSE, FALSE), FALSE), false)
      ; EVAL (ANDALSO (ORELSE (NOT FALSE, FALSE), IMPLY (TRUE, ORELSE (TRUE, FALSE))), true)
      ; EVAL (ORELSE (ANDALSO (NOT TRUE, TRUE), ANDALSO (FALSE, NOT FALSE)), false)
      ; EVAL (LESS (PLUS (NUM 3, MINUS (NUM 10, PLUS (NUM 2, NUM 3))), MINUS (NUM 10, PLUS (NUM 1, NUM 2))), false)
      ; EVAL (LESS (MINUS (PLUS (NUM 20, NUM (-5)), NUM 10), PLUS (MINUS (NUM 10, NUM 5), NUM 0)), false)
      ; EVAL (IMPLY (NOT (IMPLY (FALSE, TRUE)), TRUE), true)
      ]

    let rec string_of_expr e =
      match e with
      | NUM n ->
          if n < 0 then "(" ^ (string_of_int n) ^ ")"
          else string_of_int n
      | PLUS (e1, e2) -> sprintf "(%s + %s)" (string_of_expr e1) (string_of_expr e2)
      | MINUS (e1, e2) -> sprintf "(%s - %s)" (string_of_expr e1) (string_of_expr e2)

    let rec string_of_fomula f =
      match f with
      | TRUE -> "T"
      | FALSE -> "F"
      | NOT f' -> sprintf "(not %s)" (string_of_fomula f')
      | ANDALSO (f1, f2) -> sprintf "(%s && %s)" (string_of_fomula f1) (string_of_fomula f2)
      | ORELSE (f1, f2) -> sprintf "(%s || %s)" (string_of_fomula f1) (string_of_fomula f2)
      | IMPLY (f1, f2) -> sprintf "(%s -> %s)" (string_of_fomula f1) (string_of_fomula f2)
      | LESS (e1, e2) -> sprintf "(%s < %s)" (string_of_expr e1) (string_of_expr e2)

    let runner tc =
      match tc with
      | EVAL (f, ans) -> eval f = ans

    let string_of_tc tc =
      match tc with
      | EVAL (f, ans) -> (string_of_fomula f, string_of_bool ans, string_of_bool (eval f))
  end

open TestEx5
let _ = wrapper testcases runner string_of_tc
