(* Functional Programming *)
(*          with          *)
(*   Abstract Data Types  *)
(*           in           *)
(*       Standard ML      *)

(* ML = "Meta Language" in this class *)
(* ML is NOT "Machine Learning" here  *)

val x = 1 + 2 * 3;

(* Pattern Matching *)

fun and_bool(true, true) = true
  | and_bool(_   , _   ) = false;

and_bool(true, true);
and_bool(true, false);

(*
fun or_bool(false, false) = false
  | or_bool(_,  _) = true;
*)

fun or_bool(false, false) = false
  | or_bool(true,  false) = true
  | or_bool(false, true)  = true
  | or_bool(true,  true)  = true;

or_bool(false, true);
or_bool(false, false);

(* Algebraic Data Types *)

datatype ABC = A | B | C;

fun spell A = "a"
  | spell B = "b"
  | spell C = "c";

spell A;
spell C;

datatype num_or_string
  = a_num of int
  | a_str of string;

fun convert(a_num(n)) = 2 * n
  | convert(a_str(s)) = String.size s;

convert;
(* convert(6)  <-- type error! *)
convert(a_num(6));
convert(a_str("hello"));

(* Null references were a "billion-dollar mistake" -- Tony Hoare *)

fun head(x :: xs) = x;

fun tail(x :: xs) = xs;

head nums;
tail nums;
tail(tail(nums));
(* head [];   <-- Runtime error! *)

datatype 'a option = NONE | SOME of 'a;

fun safe_head (x :: xs) = SOME x
  | safe_head []        = NONE;

fun safe_tail (x :: xs) = SOME xs
  | safe_tail []        = NONE;

safe_head null;
safe_head nums;
safe_tail nums;
(* safe_tail(safe_tail(nums)); <-- type error! *)
(* an (int list) option (which may be a "null" or a "real" pointer) is different
   from an assured int list! *)
case safe_tail nums of
    NONE => NONE
  | SOME ns => safe_tail ns;

fun index 0 (x :: xs) = SOME x
  | index n (x :: xs) = index (n-1) xs
  | index n []        = NONE;

index 1 nums;
index 2 nums;
index 5 nums;

(* index 1 nums + 1; <-- type error! *)
case index 1 nums of
		NONE => 0
	| SOME n => n + 1;

case index 5 nums of
		NONE => 0
	| SOME n => n + 1;

(* Polymorphism *)

val nums = [1, 2, 3];

val nums2 = 1 :: 2 :: 3 :: [];

val nums3 = 1 :: (2 :: (3 :: []));

val tuple = (1, "abc", true);

val abc = #2(tuple);

val null = [];

fun is_null []         = true
  | is_null (x :: xs)  = false;

is_null(null);
is_null(nums);

fun map f []        = []
  | map f (x :: xs) = f x :: map f xs;

fun reduce f z []        = z
  | reduce f z (x :: xs) = f x (reduce f z xs);

map (fn x => x * x) nums;
map (fn x => x >= 2) nums;
map String.size ["ab", "cde", "f"];

map String.size;
map (fn x => x >= 2);
map (fn x => x * x);
