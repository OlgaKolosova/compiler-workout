open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string with show

(* The type for the stack machine program *)                                                               
type prg = insn list

(* The type for the stack machine configuration: a stack and a configuration from statement
   interpreter
 *)
type config = int list * Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
let rec eval cfg prg = 
let (st, pr) =cfg in
let (s,i,o) = pr in
match prg with
		| BINOP op -> (Language.Expr.operator op (List.hd (List.tl st)) (List.hd st) :: (List.tl (List.tl st)), pr)
		| CONST n -> (n :: st, pr)
		| READ -> (List.hd i :: st, (s, List.tl i, o))
		| WRITE -> (List.tl st, (s, i, o @ [List.hd st]))
		| LD x -> (s x :: st, pr)
		| ST x -> (List.tl st, (Language.Expr.update x (List.hd st) s, i, o))
		
let eval cfg prg = List.fold_left eval cfg prg

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i = let (_, (_, _, o)) = eval ([], (Language.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)
(* let compile _ = failwith "Not yet implemented" *)
let rec compile_expr exp = match exp with
		| Language.Expr.Const n -> [CONST n]
		| Language.Expr.Var x -> [LD x]
		| Language.Expr.Binop (op, left, right) -> (compile_expr left)@(compile_expr right)@[BINOP op]

let rec compile sta = match sta with
		| Language.Stmt.Read a -> [READ; ST a]
		| Language.Stmt.Write expr -> (compile_expr expr) @ [WRITE]
		| Language.Stmt.Assign (x, e)   -> (compile_expr e) @ [ST x]
                | Language.Stmt.Seq (l, r) -> (compile l) @ (compile r);;
