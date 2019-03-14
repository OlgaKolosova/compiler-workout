(* Opening a library for generic programming (https://github.com/dboulytchev/GT).
   The library provides "@type ..." syntax extension and plugins like show, etc.
*)
open GT

(* Opening a library for combinator-based syntax analysis *)
open Ostap.Combinators
       
(* Simple expressions: syntax and semantics *)
module Expr =
  struct
    
    (* The type for expressions. Note, in regular OCaml there is no "@type..." 
       notation, it came from GT. 
    *)
    @type t =
    (* integer constant *) | Const of int
    (* variable         *) | Var   of string
    (* binary operator  *) | Binop of string * t * t with show

    (* Available binary operators:
        !!                   --- disjunction
        &&                   --- conjunction
        ==, !=, <=, <, >=, > --- comparisons
        +, -                 --- addition, subtraction
        *, /, %              --- multiplication, division, reminder
    *)
                                                            
    (* State: a partial map from variables to integer values. *)
    type state = string -> int 

    (* Empty state: maps every variable into nothing. *)
    let empty = fun x -> failwith (Printf.sprintf "Undefined variable %s" x)

    (* Update: non-destructively "modifies" the state s by binding the variable x 
      to value v and returns the new state.
    *)
    let update x v s = fun y -> if x = y then v else s y

    (* Expression evaluator

          val eval : state -> t -> int
 
       Takes a state and an expression, and returns the value of the expression in 
       the given state.
     *)                                                       
   (* let eval _ _ = failwith "Not yet implemented" *)
let int2bool i = i != 0
let bool2int b = if b then 1 else 0

let operator op_operator left right = match op_operator with
        | "+" -> left + right
        | "-" -> left - right
        | "*" -> left * right
        | "/" -> left / right
        | "%" -> left mod right
        | "<" -> bool2int(left < right)
        | ">" -> bool2int(left > right)
        | "<=" -> bool2int(left <= right)
        | ">=" -> bool2int(left >= right)
        | "==" -> bool2int(left = right)
        | "!=" -> bool2int(left != right)
        | "&&" -> bool2int((int2bool left) && (int2bool right))
        | "!!" -> bool2int((int2bool left) || (int2bool right))

let rec eval state expr = match expr with
|Const c -> c
|Var vr -> state vr
|Binop(op_operator,left,right) -> (operator op_operator) (eval state left) (eval state right);;

let b_parse op_operator =ostap(- $(op_operator)),(fun x y ->Binop (op_operator,x,y))
    (* Expression parser. You can use the following terminals:

         IDENT   --- a non-empty identifier a-zA-Z[a-zA-Z0-9_]* as a string
         DECIMAL --- a decimal constant [0-9]+ as a string
                                                                                                                  
    *)
    ostap (                                      
     (* parse: empty {failwith "Not yet implemented"} *)
expr:
!(Ostap.Util.expr
(fun x->x)
(Array.map(fun (aso,ops) ->aso, List.map b_parse ops)
[|
`Lefta, ["!!"];
`Lefta, ["&&"];
`Nona, ["<=";"<";">=";">";"==";"!="];
`Lefta, ["+";"-"];
`Lefta, ["*";"/";"%"];
|])
b_t);
b_t:
x:IDENT {Var x} |
d:DECIMAL {Const d}|
-"("expr-")" 
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
    (* composition                      *) | Seq    of t * t with show

    (* The type of configuration: a state, an input stream, an output stream *)
    type config = Expr.state * int list * int list 

    (* Statement evaluator

         val eval : config -> t -> config

       Takes a configuration and a statement, and returns another configuration
    *)
   (* let eval _ _ = failwith "Not yet implemented" *)
let rec eval (s,i,o) sta=match sta with
|Read a-> (Expr.update a (List.hd i) s, List.tl i,o)
|Write b-> (s,i,o @ [Expr.eval s b])
|Assign (a,b) -> (Expr.update a (Expr.eval s b) s, i, o)
|Seq (l,r) -> eval (eval (s,i,o) l)r;;

    (* Statement parser *)
    ostap (
    (*  parse: empty {failwith "Not yet implemented"} *)
b_l:
"read" "(" x:IDENT ")" {Read x}
| "write" "(" w:!(Expr.expr) ")" {Write w}
|x:IDENT ":=" w:!(Expr.expr) {Assign (x,w)};

parse: x:b_l ";" xs:parse {Seq (x,xs)} | b_l
    )
      
  end

(* The top-level definitions *)

(* The top-level syntax category is statement *)
type t = Stmt.t    

(* Top-level evaluator

     eval : t -> int list -> int list

   Takes a program and its input stream, and returns the output stream
*)
let eval p i =
  let _, _, o = Stmt.eval (Expr.empty, i, []) p in o

(* Top-level parser *)
let parse = Stmt.parse                                                     
