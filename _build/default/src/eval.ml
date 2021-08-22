open SmallCTypes
open EvalUtils
open TokenTypes


exception TypeError of string
exception DeclareError of string
exception DivByZeroError

(*helper function *)
let rec pow x y = 
    if y = 0 then 1 else x * pow x (y-1);;

(*helper function*)
let inrange x y num =
  if x >= y then 
    if num <= x && num > y then true else false
  else
    if num >= x && num < y then true else false

(*helper function*)
let increment x y = 
  if x >= y then -1 else 1

let rec eval_expr env t =
  match t with
  | Int i -> Int_Val i
  | Bool b -> Bool_Val b
  | ID id -> (match List.assoc_opt id env with 
              | None -> raise (DeclareError "no binding")
              | _ -> List.assoc id env)
  | Add (x, y) -> (match (eval_expr env x) with
                  | Int_Val xi -> (match (eval_expr env y) with
                                  | Int_Val yi -> Int_Val (xi + yi)
                                  | _ -> raise (TypeError "argument not integer"))
                  |_ -> raise(TypeError "argument not integer"))
  | Sub (x, y) -> (match (eval_expr env x) with
                  | Int_Val xi -> (match (eval_expr env y) with
                                  | Int_Val yi -> Int_Val (xi - yi)
                                  | _ -> raise (TypeError "argument not integer"))
                  |_ -> raise(TypeError "argument not integer"))
  | Mult (x, y) -> (match (eval_expr env x) with
                  | Int_Val xi -> (match (eval_expr env y) with
                                  | Int_Val yi -> Int_Val (xi * yi)
                                  | _ -> raise (TypeError "argument not integer"))
                  |_ -> raise(TypeError "argument not integer"))
  | Div (x, y) -> (match (eval_expr env x) with
                  | Int_Val xi -> (match (eval_expr env y) with
                                  | Int_Val yi -> if yi = 0 then raise(DivByZeroError) 
                                                  else Int_Val (xi / yi)
                                  | _ -> raise (TypeError "argument not integer"))
                  |_ -> raise(TypeError "argument not integer"))
  | Pow (x, y) -> (match (eval_expr env x) with
                  | Int_Val xi -> (match (eval_expr env y) with
                                  | Int_Val yi -> Int_Val (pow xi yi)
                                  | _ -> raise (TypeError "argument not integer"))
                  | _ -> raise(TypeError "argument not integer"))
  | Or (x, y) -> (match (eval_expr env x) with
                  | Bool_Val xb -> (match (eval_expr env y) with
                                   | Bool_Val yb -> Bool_Val (xb || yb)
                                   | _ -> raise (TypeError "argument not boolean"))
                  | _ -> raise(TypeError "argument not boolean"))
  | And (x, y) -> (match (eval_expr env x) with
                  | Bool_Val xb -> (match (eval_expr env y) with
                                   | Bool_Val yb -> Bool_Val (xb && yb)
                                   | _ -> raise (TypeError "argument not boolean"))
                  | _ -> raise(TypeError "argument not boolean"))
  | Not (x) -> (match (eval_expr env x) with
               | Bool_Val xb -> Bool_Val (not xb)
               | _ -> raise (TypeError "argument not boolean"))
  | Greater (x,y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi > yi)
                               | _ -> raise (TypeError "argument not integer"))
               | _ -> raise (TypeError "argument not integer"))
  | Less (x,y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi < yi)
                               | _ -> raise (TypeError "argument not integer"))
               | _ -> raise (TypeError "argument not integer"))
  | GreaterEqual (x,y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi >= yi)
                               | _ -> raise (TypeError "argument not integer"))
               | _ -> raise (TypeError "argument not integer"))
  | LessEqual (x,y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi <= yi)
                               | _ -> raise (TypeError "argument not integer"))
               | _ -> raise (TypeError "argument not integer"))
  | Equal (x, y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi = yi)
                               | _ -> raise (TypeError "arguments not same type"))
               | Bool_Val xb -> (match (eval_expr env y) with
                               | Bool_Val yb -> Bool_Val (xb = yb)
                               | _ -> raise (TypeError "arguments not same type")))
  | NotEqual (x, y) -> (match (eval_expr env x) with
               | Int_Val xi -> (match (eval_expr env y) with
                               | Int_Val yi -> Bool_Val (xi != yi)
                               | _ -> raise (TypeError "arguments not same type"))
               | Bool_Val xb -> (match (eval_expr env y) with
                               | Bool_Val yb -> Bool_Val (xb != yb)
                               | _ -> raise (TypeError "arguments not same type")))
                               

let rec eval_stmt env s =
  match s with
  | NoOp -> env
  | Seq (stmt1, stmt2) -> let env' = eval_stmt env stmt1 in 
                          eval_stmt env' stmt2
  | Declare (t, n) -> if List.mem_assoc n env then 
                        raise (DeclareError "binding already exists")
                      else
                        (match t with
                          | Int_Type -> env@[(n, Int_Val (0))]
                          | Bool_Type -> env@[(n, Bool_Val (false))])
  | Assign (n, expr) -> if List.mem_assoc n env then (*if the binding exists*)
                          (match (List.assoc n env) with (*check binding type*)
                          | Int_Val i -> (match (eval_expr env expr) with  (*check new val type*)
                                          | Int_Val i1 ->  (n, Int_Val i1)::env
                                          | _ -> raise (TypeError "new value type does not match"))
                          | Bool_Val b -> (match (eval_expr env expr) with
                                          | Bool_Val b1 -> (n, Bool_Val b1)::env
                                          | _ -> raise (TypeError "new value type does not match")))
                        else
                          raise (DeclareError "binding does not already exist")
  | If (expr, stmt1, stmt2) -> (match (eval_expr env expr) with
                                | Bool_Val b -> if b then eval_stmt env stmt1 else eval_stmt env stmt2
                                | _ -> raise (TypeError "argument not boolean"))
  | While (expr, stmt) -> (match (eval_expr env expr) with
                           | Bool_Val b -> if b then 
                                              let env' = eval_stmt env stmt in eval_stmt env' s 
                                           else env
                           | _ -> raise (TypeError "argument not boolean"))
  | For (id, expr1, expr2, stmt) -> (match (eval_expr env expr1) with
                                     | Int_Val i1 -> (match (eval_expr env expr2) with
                                                     | Int_Val i2 -> 
                                                     if List.mem_assoc id env then
                                                        (match (List.assoc id env) with
                                                        | Int_Val i3 ->
                                                        (*match List.assoc id env with int*)
                                                        if (inrange i1 i2 i3) then
                                                          let env' = eval_stmt env stmt in
                                                            print_output_string "hello"; eval_stmt ((id, Int_Val (i3 + (increment i1 i2)))::env') s
                                                        else env
                                                        | _ -> print_output_string "oh no"; raise (TypeError "argument not integer"))
                                                      else
                                                        let env' = eval_stmt env stmt in
                                                        print_output_string "first hello"; eval_stmt ((id, Int_Val (i1 + (increment i1 i2)))::env') s
                                                      | _ -> print_output_string "oh no"; raise (TypeError "argument not integer"))
                                     | _ -> print_output_string "oh no"; raise (TypeError "argument not integer"))
 
  | Print (expr) -> (match (eval_expr env expr) with
                    | Int_Val i -> let print_final num = print_output_int num ; print_output_newline in
                                   (print_final i ()); env
                    | Bool_Val b -> let print_final num = print_output_bool num ; print_output_newline in
                                   (print_final b ()) ; env)


                        
  
