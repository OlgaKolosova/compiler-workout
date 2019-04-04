(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap
open Combinators
                         
(* States *)
module State =
  struct
                                                                
    (* State: global state, local state, scope variables *)
    type t = {g : string -> int; l : string -> int; scope : string list}

    (* Empty state *)
    let empty =
      let e x = failwith (Printf.sprintf "Undefined variable: %s" x) in
      {g = e; l = e; scope = []}

    (* Update: non-destructively "modifies" the state s by binding the variable x 
       to value v and returns the new state w.r.t. a scope
    *)
    let update x v s =
      let u x v s = fun y -> if x = y then v else s y in
      if List.mem x s.scope then {s with l = u x v s.l} else {s with g = u x v s.g}

    (* Evals a variable in a state w.r.t. a scope *)
    let eval s x = (if List.mem x s.scope then s.l else s.g) x

    (* Creates a new scope, based on a given state *)
    let enter st xs = {empty with g = st.g; scope = xs}

    (* Drops a scope *)
    let leave st st' = {st' with g = st.g}

  end
    
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t
    (* function call    *) | Call  of string * t list with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)

    (* The type of configuration: a state, an input stream, an output stream, an optional value *)
    type config = State.t * int list * int list * int option
                                                            
    (* Expression evaluator

          val eval : env -> config -> t -> config


       Takes an environment, a configuration and an expresion, and returns another configuration. The 
       environment supplies the following method

           method definition : env -> string -> int list -> config -> config

       which takes an environment (of the same type), a name of the function, a list of actual parameters and a configuration, 
       an returns resulting configuration
    *)          
                                             
let to_func op =
      let bti   = function true -> 1 | _ -> 0 in
      let itb b = b <> 0 in
      let (|>) f g   = fun x y -> f (g x y) in
      match op with
      | "+"  -> (+)
      | "-"  -> (-)
      | "*"  -> ( * )
      | "/"  -> (/)
      | "%"  -> (mod)
      | "<"  -> bti |> (< )
      | "<=" -> bti |> (<=)
      | ">"  -> bti |> (> )
      | ">=" -> bti |> (>=)
      | "==" -> bti |> (= )
      | "!=" -> bti |> (<>)
      | "&&" -> fun x y -> bti (itb x && itb y)
      | "!!" -> fun x y -> bti (itb x || itb y)
      | _    -> failwith (Printf.sprintf "Unknown binary operator %s" op)    
    
    let rec eval env ((s, inp, out, _) as conf) expr =      
      match expr with
      | Const n -> (s, inp, out, Some n)
      | Var   x -> (s, inp, out, Some State.eval s x)
      | Binop (op, x, y) -> 
			let (s, inp, out, Some r1) = eval env conf x in
			let (s, inp, out, Some r2) = eval env (s, inp, out, None) y in (s, inp, out, Some to_func op r1 r2)
      | Call (f, args) ->
			let (s, inp, out, args) = List. fold_left (fun (s, inp, out, args) arg ->
			let (s, inp, out, Some res) = eval env (s, inp, out, None) arg in (s, inp, out, args @ [res])) (s, inp, out, []) args in env#definition env f args (s, inp, out, None)
  
         
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string                                                                                                                  
    *)
ostap (                                      
      parse:
	  !(Ostap.Util.expr 
             (fun x -> x)
	     (Array.map (fun (a, s) -> a, 
                           List.map  (fun s -> ostap($(s)), (fun x y -> Binop (s, x, y))) s
                        ) 
              [|                
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      primary:
        n:DECIMAL {Const n}
      | -"(" parse -")"
      | x:IDENT   p:("(" args:!(Ostap.Util.list0 parse) ")" {Call (x,args)} | empty {Var x}) {p}

    )
    
  end
                    
(* Simple statements: syntax and sematics *)
module Stmt =
  struct

    (* The type for statements *)
    @type t =
    (* read into the variable           *) | Read   of string
    (* write the value of an expression *) | Write  of Expr.t
    (* assignment                       *) | Assign of string * Expr.t
    (* composition                      *) | Seq    of t * t 
    (* empty statement                  *) | Skip
    (* conditional                      *) | If     of Expr.t * t * t
    (* loop with a pre-condition        *) | While  of Expr.t * t
    (* loop with a post-condition       *) | Repeat of t * Expr.t
    (* return statement                 *) | Return of Expr.t option
    (* call a procedure                 *) | Call   of string * Expr.t list with show
                                                                    
    (* Statement evaluator

         val eval : env -> config -> t -> config

       Takes an environment, a configuration and a statement, and returns another configuration. The 
       environment is the same as for expressions
    *)
let _seq x y = match x with 
| Skip -> y
| _ -> match y with 
	| Skip -> x
	| _ -> Seq(x, y)
let r_cond cond = Expr.Binop ("==", cond, Expr.Const 0)

let rec eval env ((st, input, output, r) as conf) k op =
		match op with
| Read var	-> eval env (State.update var (List.hd input) st, List.tl input, output, None) Skip k
| Write expr    -> 
			let (st, input, output, Some r) = Expr.eval env conf expr in eval env (st, input, output @ [r], None) Skip k
| Assign (var, expr) -> 
			let (st, input, output, Some r) = Expr.eval env conf expr in eval env (State.update var r st, input, output, Some r) Skip k
| Seq (t1, t2)       -> eval env conf (_seq t2 k) t1
| Skip          -> (match k with 
	| Skip -> conf
	| _ -> eval env conf Skip k)
| If (expr, thenStmt, elseStmt) -> 
			let (st, input, output, Some r) = Expr.eval env conf expr in eval env (st, input, output, None) k ( if r <> 0 then thenStmt else elseStmt)
| While (expr, wStmt) -> 
			let ( st, input, output, Some r) = Expr.eval env conf expr in if r != 0 then eval env (st, input, output, None) (_seq op k) wStmt else eval env (st, input, output, None) Skip k
| Repeat (ruStmt, expr)   -> eval env conf (_seq (While (r_cond expr, ruStmt)) k) ruStmt
| Call (name, args) -> eval env (Expr.eval env conf (Expr.Call (name, args))) Skip k
| Return x   -> (match x with
	| Some x -> Expr.eval env conf x
	| _ -> (st, input, output, None))

                                
    (* Statement parser *)
ostap (
      simple:
	"read" "(" x:IDENT ")"         {Read x}
        | "write" "(" e:!(Expr.parse) ")" {Write e}
        | x:IDENT ":=" e:!(Expr.parse)    {Assign (x, e)}
        | "if" expr:!(Expr.parse) "then" thenStmt:!(parse)
		elif:(%"elif" !(Expr.parse) %"then" !(parse))*
		els:(%"else" !(parse))?
		"fi" {
			let e_body = match els with
			 | Some x -> x
			 | _ -> Skip
			in let t = List.fold_right (fun (cond, body) curr -> If (cond, body, curr)) elif e_body in If (expr, thenStmt, t)
		      }
	| "skip" {Skip}
	| "while" expr:!(Expr.parse) "do" wStmt:parse "od" {While (expr, wStmt)}
	| "repeat" ruStmt:parse "until" expr:!(Expr.parse) {Repeat (ruStmt, expr)}
	| "for"  initStmt:parse "," whileCond:!(Expr.parse) "," forStmt:parse
        "do" body:parse "od" {Seq (initStmt, While (whileCond, Seq (body, forStmt)))}
        | name:IDENT "(" args:!(Ostap.Util.list0 Expr.parse) ")"    {Call (name, args)}
	| "return" e:!(Expr.parse)? {Return e};

      parse:
        l:simple ";" rest:parse {Seq (l, rest)}
        | simple
    )
      
  end

(* Function and procedure definitions *)
module Definition =
  struct

    (* The type for a definition: name, argument list, local variables, body *)
    type t = string * (string list * string list * Stmt.t)

ostap (       
      arg: IDENT;                               
      parse: "fun" name:IDENT "(" args:!(Util.list0 arg) ")" local:(%"local" !(Util.list arg))? "{" body:!(Stmt.parse) "}"
{
let local = match local with
| Some x -> x
| _ -> [] in
name, (args, local, body)
} 
    )

  end
    
(* The top-level definitions *)

(* The top-level syntax category is a pair of definition list and statement (program body) *)
type t = Definition.t list * Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval (defs, body) i =
  let module M = Map.Make (String) in
  let m          = List.fold_left (fun m ((name, _) as def) -> M.add name def m) M.empty defs in  
  let _, _, o, _ =
    Stmt.eval
      (object
         method definition env f args (st, i, o, r) =                                                                      
           let xs, locs, s      =  snd @@ M.find f m in
           let st'              = List.fold_left (fun st (x, a) -> State.update x a st) (State.enter st (xs @ locs)) (List.combine xs args) in
           let st'', i', o', r' = Stmt.eval env (st', i, o, r) Stmt.Skip s in
           (State.leave st'' st, i', o', r')
       end)
      (State.empty, i, [], None)
      Stmt.Skip
      body
  in
  o

(* Top-level parser *)
let parse = ostap (!(Definition.parse)* !(Stmt.parse))
