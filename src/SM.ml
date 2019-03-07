open GT       
       
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
type config = int list * Syntax.Stmt.config

(* Stack machine interpreter

     val eval : config -> prg -> config

   Takes a configuration and a program, and returns a configuration as a result
 *)                         
(* let eval _ = failwith "Not yet implemented" *)
let rec eval cfg prg = 
let (st, pr) =config in
let (s,i,o) = pr in
match prg with
		| BINOP op -> (Syntax.Expr.operator op (List.hd (List.tl st)) (List.hd st) :: (List.tl (List.tl st)), pr)
		| CONST n -> (n :: st, pr)
		| READ -> (List.hd i :: st, (s, List.tl i, o))
		| WRITE -> (List.tl st, (s, i, o @ [List.hd st]))
		| LD x -> (s x :: st, pr)
		| ST x -> (List.tl st, (Syntax.Expr.update x (List.hd st) s, i, o))
		
		let eval cfg prg = List.fold_left eval cfg prg

(* Top-level evaluation

     val run : int list -> prg -> int list

   Takes an input stream, a program, and returns an output stream this program calculates
*)
let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Syntax.Stmt.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
 *)

(* let compile _ = failwith "Not yet implemented" *)

let rec compile_expr exp = match exp with
		| Syntax.Expr.Const n -> [CONST n]
		| Syntax.Expr.Var x -> [LD x]
		| Syntax.Expr.Binop (op, left, right) -> (compile_expr left)@(compile_expr right)@[BINOP op]

let rec compile sta = match sta with
		| Syntax.Stmt.Read a -> [READ; ST a]
		| Syntax.Stmt.Write expr -> (compile_expr expr) @ [WRITE]
		| Syntax.Stmt.Assign (x, e)   -> (compile_expr e) @ [ST x]
		| Syntax.Stmt.Seq (l, r) -> (compile l) @ (compile r);;
