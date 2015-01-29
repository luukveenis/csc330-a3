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

(* g takes as parameters: two functions f1 and f2, and a variable p *)
(* g creates a val r which is a function that takes one parameter p (fixes f1/f2) *)
(* if p is a Wildcard it evaluates f1 *)
(* if p is a Variable it evaluates f2 passing the string contained in it as a paremeter *)
(* if p is a TupleP it sums the results of calling r on each element in the list ps *)
(* if p is a ConstructorP it evaluates r with p as a parameter *)
(* if p is a UnitP or ConstP it returns 0 *)
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

val longest_capitalized = longest_string1 o only_capitals

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

fun count_wildcards p =
  let
    val f1 = fn() => 1
    val f2 = fn(x) => 0
  in
    g f1 f2 p
  end

fun count_wild_and_variable_lengths p =
  let
    val f1 = fn()  => 1
    val f2 = fn(s) => String.size(s)
  in
    g f1 f2 p
  end

fun count_some_var(s, p) =
  let
    val f1 = fn() => 0
    val f2 = fn(x) => if x=s then 1 else 0
  in
    g f1 f2 p
  end

fun check_pat p =
  let
    fun all_strings p =
      case p of
        Variable x        => [x]
      | TupleP ps         => List.foldl (fn (p,acc) => (acc @ all_strings p)) [] ps
      | ConstructorP(_,p) => all_strings p
      | _                 => []
    fun has_repeats lst =
      case lst of
        [] => false
      | s::lst' => List.exists (fn(x) => x=s) lst' orelse has_repeats lst'
  in
    (not o has_repeats o all_strings) p
  end

fun match(v, p) =
  case p of
    Wildcard   => SOME []
  | Variable s => SOME [(s,v)]
  | UnitP      => (case v of
                    Unit => SOME []
                  | _    => NONE)
  | ConstP x   => (case v of
                    Const y => if x=y then SOME [] else NONE
                  | _       => NONE)
  | TupleP ps  => (case v of
                     Tuple vs => if List.length(ps)=List.length(vs) then
                                   (all_answers match o ListPair.zip) (vs, ps)
                                 else NONE
                   | _ => NONE)
  | ConstructorP (s1,p) => (case v of
                             Constructor(s2,v) => if s1=s2 then match(v,p)
                                                  else NONE
                           | _ => NONE)

fun first_match v ps =
    SOME (first_answer (fn(x) => match(v, x)) ps) handle NoAnswer => NONE
