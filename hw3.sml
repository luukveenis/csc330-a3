(* Assign 03 Provided Code *)

(*  Version 1.0 *)

exception NoAnswer

datatype pattern = Wildcard
                 | Variable of string
                 | UnitP
                 | ConstP of int
                 | TupleP of pattern list
                 | ConstructorP of string * pattern

datatype valu = Const of int
              | Unit
              | Tuple of valu list
              | Constructor of string * valu

fun g f1 f2 p =
  let
    val r = g f1 f2
  in
    case p of
      Wildcard          => f1 ()
    | Variable x        => f2 x
    | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
    | ConstructorP(_,p) => r p
    | _                 => 0
  end


  (**** put all your code after this line ****)

fun only_capitals(lst: string list) =
  List.filter (fn s => Char.isUpper(String.sub(s,0))) lst

fun longest_string1(lst: string list) =
  foldl (fn (x,y) => if String.size(x) > String.size(y) then x else y)  "" lst

fun longest_string2(lst: string list) =
  foldl (fn (x,y) => if String.size(x) >= String.size(y) then x else y)  "" lst

fun longest_string_helper f lst =
  foldl (fn (x,y) => if f(String.size(x),String.size(y)) then x else y) "" lst

fun longest_string3(lst: string list) =
  let
    val f = fn (x,y) => x > y
  in
    longest_string_helper f lst
  end

fun longest_string4(lst: string list) =
  let
    val f = fn (x,y) => x >= y
  in
    longest_string_helper f lst
  end

fun rev_string(s: string) =
  (String.implode o rev o String.explode) s

fun first_answer f lst =
  case lst of
    [] => raise NoAnswer
  | x::lst' => case f(x) of
                 NONE   => first_answer f lst'
               | SOME y => y

fun all_answers f lst =
  let
    fun aux(f, lst, acc) =
      case lst of
        []      => SOME acc
      | x::lst' => case f(x) of
                     NONE   => NONE
                   | SOME l => aux(f, lst', acc @ l)
  in
    aux(f, lst, [])
  end
