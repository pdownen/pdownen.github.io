(* Syntactic Interpreters *)
(*          with          *)
(*  Algebraic Data Types  *)
(*           in           *)
(*       Standard ML      *)

(* Remember our language of conditional arithmetic *)

(*
    n ::= 0 | 1 | 2 | 3 | ...
    A ::= n | plus(A, A) | minus(A, A) | if B then A else A

    b ::= true | false
    B ::= b | or(B, B) | geq?(A, A)
*)

(* This syntax, written as a BNF grammar can be represented as an
   algebraic data type *)

datatype ArithExpr
	= NUM of int
	| PLUS of ArithExpr * ArithExpr
	| MINUS of ArithExpr * ArithExpr
	| IF of CondExpr * ArithExpr * ArithExpr

and  CondExpr
	= BOOL of bool
	| OR of CondExpr * CondExpr
	| GEQ of ArithExpr * ArithExpr;

val ex1 = MINUS(PLUS(NUM 5, NUM 3), NUM 3);
val ex2 = MINUS(PLUS(NUM 2, NUM 3), NUM 4);
val ex3 = IF(BOOL false, MINUS(NUM 0, NUM 1), PLUS(NUM 0, NUM 1));
val ex4 = GEQ(PLUS(NUM 2, NUM 2), NUM 4);
val ex5 = GEQ(MINUS(NUM 0, NUM 1), NUM 0);

fun find_numbers (NUM(n)) = [n]
	| find_numbers (PLUS(left, right)) = find_numbers left @ find_numbers right
	| find_numbers (MINUS(left, right)) = find_numbers left @ find_numbers right
	| find_numbers (IF(chk, thn, els)) = find_numbers' chk @ find_numbers thn @ find_numbers els
and find_numbers' (BOOL(b)) = []
	| find_numbers' (OR(left, right)) = find_numbers' left @ find_numbers' right
	| find_numbers' (GEQ(left, right)) = find_numbers left @ find_numbers right;


exception NegativeNumber of int;

fun eval_arith (NUM(n)) = n
	(* —————
		 n ⇓ n *)

	| eval_arith (PLUS(left, right)) =
		eval_arith left + eval_arith right
	(* A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ + n₂ = n
		 ——————————————————————————————
		 plus(A₁, A₂) ⇓ n *)

	| eval_arith (MINUS(left, right)) =
		let val n1 = eval_arith left
				val n2 = eval_arith right
		in if n1 >= n2
			 then n1 - n2
			 else raise NegativeNumber (n1 - n2)
		end
	(* A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ - n₂ = n  n₁ ≥ n₂
		 ——————————————————————————————————————
		 minus(A₁, A₂) ⇓ n *)

	| eval_arith (IF(chk, thn, els)) =
		if eval_cond chk
		then eval_arith thn
		else eval_arith els
  (* B ⇓ true  A₁ ⇓ n₁
     —————————————————————————
     if B then A₁ else A₂ ⇓ n₁

     B ⇓ false  A₂ ⇓ n₂
     —————————————————————————
     if B then A₁ else A₂ ⇓ n₂ *)

and eval_cond (BOOL(b)) = b
	(* —————
     b ⇓ b *)

	| eval_cond (OR(left, right)) =
		if eval_cond left
		then true
		else eval_cond right
	(* B₁ ⇓ false  B₂ ⇓ b₂
		 ———————————————————
		 or(B₁, B₂) ⇓ b₂
	
		 B₁ ⇓ true
		 —————————————————
		 or(B₁, B₂) ⇓ true *)

	| eval_cond (GEQ(left, right)) =
		eval_arith left >= eval_arith right;
  (* A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ ≥ n₂
		 —————————————————————————
		 geq?(A₁, A₂) ⇓ true

		 A₁ ⇓ n₁  A₂ ⇓ n₂  n₁ < n₂
		 —————————————————————————
		 geq?(A₁, A₂) ⇓ false *)


fun print_arith (NUM(n)) = Int.toString n
	| print_arith (PLUS(left, right)) = "(" ^ print_arith left ^ ") + (" ^ print_arith right ^ ")"
	| print_arith (MINUS(left, right)) = "(" ^ print_arith left ^ ") - (" ^ print_arith right ^ ")"
	| print_arith (IF(chk,thn,els)) = "if " ^ print_cond chk ^ " then " ^ print_arith thn ^ " else " ^ print_arith els
and print_cond (BOOL(true)) = "true"
	| print_cond (BOOL(false)) = "false"
	| print_cond (OR(left,right)) = "(" ^ print_cond left ^ ") or (" ^ print_cond right ^ ")"
	| print_cond (GEQ(left,right)) = "(" ^ print_arith left ^ ") >= (" ^ print_arith right ^ ")";

