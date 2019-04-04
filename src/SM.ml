open GT       
open Language
       
(* The type for the stack machine instructions *)
@type insn =
(* binary operator                 *) | BINOP of string
(* put a constant on the stack     *) | CONST of int                 
(* read to stack                   *) | READ
(* write from stack                *) | WRITE
(* load a variable to the stack    *) | LD    of string
(* store a variable from the stack *) | ST    of string
(* a label                         *) | LABEL of string
(* unconditional jump              *) | JMP   of string
(* conditional jump                *) | CJMP  of string * string
(* begins procedure definition     *) | BEGIN of string * string list * string list
(* end procedure definition        *) | END
(* calls a function/procedure      *) | CALL  of string * int * bool
(* returns from a function         *) | RET   of bool with show
                                                   
(* The type for the stack machine program *)                                                               
type prg = insn list
                            
(* The type for the stack machine configuration: control stack, stack and configuration from statement
   interpreter
 *)
type config = (prg * State.t) list * int list * Expr.config

(* Stack machine interpreter

     val eval : env -> config -> prg -> config

   Takes an environment, a configuration and a program, and returns a configuration as a result. The
   environment is used to locate a label to jump to (via method env#labeled <label_name>)
*)                                                  
let rec eval env ((cst, sta, ((s, inp, out) as c)) as conf) instruction = match instruction with
| [] -> (cst, sta, (s, inp, out))
| BINOP op :: p -> let y :: x :: sta = sta in 
	eval env (cst, Expr.to_func op x y :: sta, (s, inp, out)) p
| CONST c :: p -> eval env (cst, c :: sta, (s, inp, out)) p
| READ   :: p -> eval env (cst, (List.hd inp) :: sta, (s, List.tl inp, out)) p
| WRITE :: p -> eval env (cst, List.tl sta, (s, inp, out @ [List.hd sta])) p
| LD x :: p -> eval env (cst, Language.State.eval s x :: sta, (s, inp, out)) p
| ST x :: p -> eval env (cst, List.tl sta, (Language.State.update x (List.hd sta) s, inp, out)) p
| LABEL l :: p -> eval env (cst, sta, (s, inp, out)) p
| JMP l :: _   -> eval env (cst, sta, (s, inp, out)) (env#labeled l)
| CJMP (z, l) :: p  -> let b = if z = "z" then (List.hd sta) == 0 else (List.hd sta) != 0 in
if b then eval env (cst, List.tl sta, (s, inp, out)) (env#labeled l) else eval env (cst, List.tl sta, (s, inp, out)) p
| BEGIN (_, a, l) :: p -> let state = Language.State.enter s (a @ l) in
	let s, sta = List.fold_left (fun (s, x::stack) name -> (State.update name x s, stack)) (state, sta) a in eval env (cst, sta, (s, inp, out)) p
| CALL (f, _, _) :: p -> eval env ((p, s)::cst, sta, (s, inp, out)) (env#labeled f)
| (RET _ | END) :: _ -> (match cst with
    | (p, old_s)::cst -> eval env (cst, sta, (Language.State.leave s old_s, inp, out)) p
    | _ -> (cst, sta, c))

(* Top-level evaluation

     val run : prg -> int list -> int list

   Takes a program, an input stream, and returns an output stream this program calculates
*)
let run p i =
  let module M = Map.Make (String) in
  let rec make_map m = function
  | []              -> m
  | (LABEL l) :: tl -> make_map (M.add l tl m) tl
  | _ :: tl         -> make_map m tl
  in
  let m = make_map M.empty p in
  let (_, _, (_, _, o)) = eval (object method labeled l = M.find l m end) ([], [], (State.empty, i, [])) p in o

(* Stack machine compiler

     val compile : Language.t -> prg

   Takes a program in the source language and returns an equivalent program for the
   stack machine
*)
let label = object
val mutable n=0
method get s = n <- n+1; s ^ string_of_int n
end

 let rec compileWithLabels p lastL =
  let rec expr = function
  | Expr.Var   x          -> [LD x]
  | Expr.Const n          -> [CONST n]
  | Expr.Binop (op, x, y) -> expr x @ expr y @ [BINOP op]
  | Expr.Call (f, args) -> let compile_args = List.concat (List.map (expr) (List.rev args)) in
compile_args @ [CALL (f, List.length args, true)] 
  in match p with
  | Stmt.Seq (s1, s2)  -> (let newLabel = label#get "l_seq" in
                           let (compiled1, used1) = compileWithLabels s1 newLabel in
                           let (compiled2, used2) = compileWithLabels s2 lastL in
                           (compiled1 @ (if used1 then [LABEL newLabel] else []) @ compiled2), used2)
  | Stmt.Read x        -> [READ; ST x], false
  | Stmt.Write e       -> (expr e @ [WRITE]), false
  | Stmt.Assign (x, e) -> (expr e @ [ST x]), false
  | Stmt.If (e, s1, s2) ->
    let l_else = label#get "l_else" in
    let (compiledS1, used1) = compileWithLabels s1 lastL in
    let (compiledS2, used2) = compileWithLabels s2 lastL in
    (expr e @ [CJMP ("z", l_else)]
    @ compiledS1 @ (if used1 then [] else [JMP lastL]) @ [LABEL l_else]
    @ compiledS2 @ (if used2 then [] else [JMP lastL])), true
  | Stmt.While (e, body) ->
    let lCheck = label#get "l_check" in
    let lLoop = label#get "l_loop" in
    let (doBody, _) = compileWithLabels body lCheck in
    ([JMP lCheck; LABEL lLoop] @ doBody @ [LABEL lCheck] @ expr e @ [CJMP ("nz", lLoop)]), false
  | Stmt.Repeat (body, e) ->
    let lLoop = label#get "l_repeat" in
    let (repeatBody, _) = compileWithLabels body lastL in
    ([LABEL lLoop] @ repeatBody @ expr e @ [CJMP ("z", lLoop)]), false
  | Stmt.Skip -> [], false
  | Stmt.Call (f, args) ->
List.concat (List.map (expr) (List.rev args)) @ [CALL (f, List.length args, false)], false
  | Stmt.Return e -> (match e with 
			| Some x -> (expr x) @ [RET true]
			| _ -> [RET false]), false

 let rec compile_m p =
  let label = label#get "l_main" in
  let compiled, used = compileWithLabels p label in
  compiled @ (if used then [LABEL label] else []) 

let rec compile_d defs=
List.fold_left (fun (p) (name, (args,locals,body)) ->
	let body = compile_m body in
	p @ [LABEL name] @ [BEGIN (name, args, locals)] @ body @ [END]) ([]) defs

let rec compile (defs, main) =
let main = compile_m main in
let defs = compile_d defs in
main @ [END] @ defs
