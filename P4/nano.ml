exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = match listAssoc (x,evn) with
  | Some i -> i
  | _ -> failwith ("Variable not bound: "^x)

let rec eval (evn,e) = match e with
  | NilExpr -> Nil
  | Const i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var x -> lookup (x,evn)
  | Bin (a,b,c) -> (match b with
    | Cons -> (match eval (evn,a) with
      | Int one -> (match eval (evn,c) with
        | Nil -> Pair (Int one,Nil)
        | Pair (x,y) -> Pair (Int one,Pair (x,y)))
      | Pair (x1,y1) -> (match eval (evn,c) with
        | Pair (x2,y2) -> Pair ((Pair (x1,y1),Pair (x2,y2)))))
    | Plus -> (match eval (evn,a) with
      | Int m -> match eval (evn,c) with
        | Int n -> Int (m + n))
    | Minus -> (match eval (evn,a) with
      | Int m -> match eval (evn,c) with
        | Int n -> Int (m - n))
    | Mul -> (match eval (evn,a) with
      | Int m -> match eval (evn,c) with
        | Int n -> Int (m * n))
    | Div -> (match eval (evn,a) with
      | Int m -> match eval (evn,c) with
        | Int n -> Int (m - n))
    | Eq -> Bool (if ((eval (evn,a) = eval (evn,c)) = true) then true else false)
    | Ne -> Bool (if ((eval (evn,a) != eval (evn,c)) = true) then true else false)
    | Lt -> Bool (if ((eval (evn,a) < eval (evn,c)) = true) then true else false)
    | Le -> Bool (if ((eval (evn,a) <= eval (evn,c)) = true) then true else false)
    | And -> Bool (if ((eval (evn,a) == Bool true) && (eval (evn,c) == Bool true)) then true else false)
    | Or -> Bool (if ((eval (evn,a) == Bool false)) || ((eval (evn,c) == Bool false)) then false else true)
    | _ -> (failwith ("Invalid operator"))
  )
  | If (a,b,c) -> if (eval (evn,a) = (Bool true)) then (eval (evn,b)) else (eval (evn,c))
  | Let (a,b,c) -> (match [(a,eval(evn,b))]@evn with
    | environment -> let a = eval(environment,b) in eval(environment,c))
  | Letrec (a,b,c) -> (match b with
    | Fun (input,output) -> match eval (evn,b) with
      | Closure (place,choice,domain,range) -> eval ((a,Closure (place,Some a,domain,range))::evn,c)
      | _ -> let Closure(place,Some a,domain,range) = eval(evn,b) in eval ((a,Closure(place,Some a,domain,range))::evn,c))
  | Fun (a,b) -> Closure (evn,None,a,b)
  | App (a,b) -> (match a with
    | Var ("hd") -> (match eval (evn,b) with Pair (x,y) -> x)
    | Var ("tl") -> (match eval (evn,b) with Pair (x,y) -> y)
    | _ -> match eval (evn,a) with
      | Closure (place,choice,domain,range) -> (match choice with
        | Some i -> eval (((i, Closure(place,choice,domain,range))::((domain,eval (evn,b))::place)),range)
        | None -> eval (((domain, eval (evn,b))::place),range)))


(**********************     Testing Code  ******************************)
