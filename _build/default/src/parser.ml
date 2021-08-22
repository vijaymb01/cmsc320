open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers (you don't need to modify these) *)

(* Return types for parse_stmt and parse_expr *)
type stmt_result = token list * stmt
type expr_result = token list * expr

(* Return the next token in the token list, throwing an error if the list is empty *)
let lookahead (toks : token list) : token =
  match toks with
  | [] -> raise (InvalidInputException "No more tokens")
  | h::_ -> h

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

(* Parsing (TODO: implement your code below) *)

(*
Expr -> OrExpr
OrExpr -> AndExpr || OrExpr | AndExpr
AndExpr -> EqualityExpr && AndExpr | EqualityExpr
EqualityExpr -> RelationalExpr EqualityOperator EqualityExpr | RelationalExpr
EqualityOperator -> == | !=
RelationalExpr -> AdditiveExpr RelationalOperator RelationalExpr | AdditiveExpr
RelationalOperator -> < | > | <= | >=
AdditiveExpr -> MultiplicativeExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
AdditiveOperator -> + | -
MultiplicativeExpr -> PowerExpr MultiplicativeOperator MultiplicativeExpr | PowerExpr
MultiplicativeOperator -> * | /
PowerExpr -> UnaryExpr ^ PowerExpr | UnaryExpr
UnaryExpr -> ! UnaryExpr | PrimaryExpr
PrimaryExpr -> Tok_Int | Tok_Bool | Tok_ID | ( Expr )
 *)

 (*return expression resulting from tok list*)
 (*expr_result is token list * expr*)

let rec parse_expr toks : expr_result =
  let (toks_after_parse_or, expr_after_parse_or) = parse_or toks in
  (toks_after_parse_or, expr_after_parse_or)
  
and parse_or toks =
  let (toks_after_parse_and, expr_after_parse_and) = parse_and toks in
  match (lookahead toks_after_parse_and) with
  | Tok_Or -> let toks2 = match_token toks_after_parse_and Tok_Or in
              let (toks2_after_parse_or, expr_after_parse_or) = parse_or toks2 in
              (toks2_after_parse_or, Or (expr_after_parse_and, expr_after_parse_or))
  | _ -> (toks_after_parse_and, expr_after_parse_and)

and parse_and toks =
  let (toks_after_parse_eq, expr_after_parse_eq) = parse_eq toks in
  match (lookahead toks_after_parse_eq) with
  | Tok_And -> let toks2 = match_token toks_after_parse_eq Tok_And in
               let (toks2_after_parse_and, expr_after_parse_and) = parse_and toks2 in
               (toks2_after_parse_and, And (expr_after_parse_eq, expr_after_parse_and))
  |_ -> (toks_after_parse_eq, expr_after_parse_eq)

and parse_eq toks =
    let (toks_after_parse_rel, expr_after_parse_rel) = parse_rel toks in
    match (lookahead toks_after_parse_rel) with
    | Tok_Equal -> let toks2 = match_token toks_after_parse_rel Tok_Equal in
                   let (toks2_after_parse_eq, expr_after_parse_eq) = parse_eq toks2 in
                   (toks2_after_parse_eq, Equal (expr_after_parse_rel, expr_after_parse_eq))
    | Tok_NotEqual -> let toks2 = match_token toks_after_parse_rel Tok_NotEqual in
                      let (toks2_after_parse_eq, expr_after_parse_eq) = parse_eq toks2 in
                      (toks2_after_parse_eq, NotEqual (expr_after_parse_rel, expr_after_parse_eq))
    | _ -> (toks_after_parse_rel, expr_after_parse_rel)

and parse_rel toks = 
    let (toks_after_parse_add, expr_after_parse_add) = parse_add toks in
    match (lookahead toks_after_parse_add) with
    | Tok_Less -> let toks2 = match_token toks_after_parse_add Tok_Less in
                  let (toks2_after_parse_rel, expr_after_parse_rel) = parse_rel toks2 in
                  (toks2_after_parse_rel, Less (expr_after_parse_add, expr_after_parse_rel))
    | Tok_Greater -> let toks2 = match_token toks_after_parse_add Tok_Greater in
                     let (toks2_after_parse_rel, expr_after_parse_rel) = parse_rel toks2 in
                     (toks2_after_parse_rel, Greater (expr_after_parse_add, expr_after_parse_rel))
    | Tok_LessEqual -> let toks2 = match_token toks_after_parse_add Tok_LessEqual in
                       let (toks2_after_parse_rel, expr_after_parse_rel) = parse_rel toks2 in
                      (toks2_after_parse_rel, LessEqual (expr_after_parse_add, expr_after_parse_rel))
    | Tok_GreaterEqual -> let toks2 = match_token toks_after_parse_add Tok_GreaterEqual in
                          let (toks2_after_parse_rel, expr_after_parse_rel) = parse_rel toks2 in
                          (toks2_after_parse_rel, GreaterEqual (expr_after_parse_add, expr_after_parse_rel))
    | _ -> (toks_after_parse_add, expr_after_parse_add)

and parse_add toks =
  let (toks_after_parse_mult, expr_after_parse_mult) = parse_mult toks in
  match (lookahead toks_after_parse_mult) with
  | Tok_Add -> let toks2 = match_token toks_after_parse_mult Tok_Add in
               let (toks2_after_parse_add, expr_after_parse_add) = parse_add toks2 in
               (toks2_after_parse_add, Add (expr_after_parse_mult, expr_after_parse_add))
  | Tok_Sub -> let toks2 = match_token toks_after_parse_mult Tok_Sub in
               let (toks2_after_parse_add, expr_after_parse_add) = parse_add toks2 in
               (toks2_after_parse_add, Sub (expr_after_parse_mult, expr_after_parse_add))
  | _ -> (toks_after_parse_mult, expr_after_parse_mult)

and parse_mult toks =
  let (toks_after_parse_pow, expr_after_parse_pow) = parse_pow toks in
  match (lookahead toks_after_parse_pow) with
  | Tok_Mult -> let toks2 = match_token toks_after_parse_pow Tok_Mult in
                let (toks2_after_parse_mult, expr_after_parse_mult) = parse_mult toks2 in
                (toks2_after_parse_mult, Mult (expr_after_parse_pow, expr_after_parse_mult))
  | Tok_Div -> let toks2 = match_token toks_after_parse_pow Tok_Div in
               let (toks2_after_parse_mult, expr_after_parse_mult) = parse_mult toks2 in
               (toks2_after_parse_mult, Div (expr_after_parse_pow, expr_after_parse_mult))
  | _ -> (toks_after_parse_pow, expr_after_parse_pow)

and parse_pow toks = 
  let (toks_after_parse_un, expr_after_parse_un) = parse_un toks in
  match (lookahead toks_after_parse_un) with
  | Tok_Pow -> let toks2 = match_token toks_after_parse_un Tok_Pow in
               let (toks2_after_parse_pow, expr_after_parse_pow) = parse_pow toks2 in
               (toks2_after_parse_pow, Pow (expr_after_parse_un, expr_after_parse_pow))
  | _ -> (toks_after_parse_un, expr_after_parse_un)

and parse_un toks =
  match (lookahead toks) with
  | Tok_Not -> let toks2 = match_token toks Tok_Not in
               let (toks2_after_parse_un, expr_after_parse_un) = parse_un toks2 in
               (toks2_after_parse_un, Not (expr_after_parse_un))
  | _ -> let (toks_after_parse_pri, expr_after_parse_pri) = parse_pri toks in
         (toks_after_parse_pri, expr_after_parse_pri)

and parse_pri toks =
  match (lookahead toks) with
  | Tok_Int i -> let toks2 = match_token toks (Tok_Int i) in
                 (toks2, Int i)
  | Tok_Bool b -> let toks2 = match_token toks (Tok_Bool b) in
                 (toks2, Bool b)
  | Tok_ID id -> let toks2 = match_token toks (Tok_ID id) in
                 (toks2, ID id)
  | Tok_LParen -> let toks2 = match_token toks Tok_LParen in
                  let (toks2_after_parse_expr, expr) = parse_expr toks2 in
                  let toks3_after_parse_expr = match_token toks2_after_parse_expr Tok_RParen in
                  (toks3_after_parse_expr, expr)
  | _ -> raise (InvalidInputException "parse expression failed")

(*
Stmt -> StmtOptions Stmt | ε
StmtOptions -> DeclareStmt | AssignStmt | PrintStmt | IfStmt | ForStmt | WhileStmt
DeclareStmt -> BasicType ID ;
BasicType -> int | bool
AssignStmt -> ID = Expr ;
PrintStmt -> printf ( Expr ) ;
IfStmt -> if ( Expr ) { Stmt } ElseBranch
ElseBranch -> else { Stmt } | ε
ForStmt -> for ( ID from Expr to Expr ) { Stmt }
WhileStmt -> while ( Expr ) { Stmt }
*)

let rec parse_stmt toks : stmt_result =
  match lookahead toks with
  | Tok_Int_Type -> let toks2 = match_token toks Tok_Int_Type in
                    (match lookahead toks2 with Tok_ID id ->  (* need this or id is unbound?*)
                    let toks3 = match_token toks2 (Tok_ID id) in
                    let toks4 = match_token toks3 Tok_Semi in
                    let (toks4_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks4 in
                    (toks4_after_parse_stmt, Seq(Declare(Int_Type, id), expr_after_parse_stmt))
                    |_ -> raise (InvalidInputException("not an id")))

  | Tok_Bool_Type -> let toks2 = match_token toks Tok_Bool_Type in
                     (match lookahead toks2 with Tok_ID id ->
                     let toks3 = match_token toks2 (Tok_ID id) in
                     let toks4 = match_token toks3 Tok_Semi in
                     let (toks4_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks4 in
                     (toks4_after_parse_stmt, Seq(Declare(Bool_Type, id), expr_after_parse_stmt))
                     |_ -> raise (InvalidInputException("not an id")))

  | Tok_ID id -> let toks2 = match_token toks (Tok_ID id) in
              let toks3 = match_token toks2 Tok_Assign in
              let (toks3_after_parse_expr, expr) = parse_expr toks3 in
              let toks4_after_parse_expr = match_token toks3_after_parse_expr Tok_Semi in
              let (toks4_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks4_after_parse_expr in
              (toks4_after_parse_stmt, Seq(Assign(id, expr), expr_after_parse_stmt))

  | Tok_Print -> let toks2 = match_token toks Tok_Print in
                 let toks3 = match_token toks2 Tok_LParen in
                 let (toks3_after_parse_expr, expr) = parse_expr toks3 in
                 let toks4_after_parse_expr = match_token toks3_after_parse_expr Tok_RParen in
                 let toks5_after_parse_expr = match_token toks4_after_parse_expr Tok_Semi in
                 let (toks5_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks5_after_parse_expr in
                 (toks5_after_parse_stmt, Seq(Print(expr), expr_after_parse_stmt))

  | Tok_If -> let toks2 = match_token toks Tok_If in
              let toks3 = match_token toks2 Tok_LParen in
              let (toks3_after_parse_expr, expr) = parse_expr toks3 in
              let toks4_after_parse_expr = match_token toks3_after_parse_expr Tok_RParen in
              let toks5_after_parse_expr = match_token toks4_after_parse_expr Tok_LBrace in
              let (toks5_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks5_after_parse_expr in
              let toks6_after_parse_stmt = match_token toks5_after_parse_stmt Tok_RBrace in
              (match lookahead toks6_after_parse_stmt with
              | Tok_Else -> let toks7_after_parse_stmt = match_token toks6_after_parse_stmt Tok_Else in
                            let toks8_after_parse_stmt = match_token toks7_after_parse_stmt Tok_LBrace in
                            let (toks8_after_parse_stmt2, expr_after_parse_stmt2) = parse_stmt toks8_after_parse_stmt in
                            let toks9_after_parse_stmt2 = match_token toks8_after_parse_stmt2 Tok_RBrace in
                            let (toks9_after_parse_stmt3, expr_after_parse_stmt3) = parse_stmt toks9_after_parse_stmt2 in
                            (toks9_after_parse_stmt3, Seq(If(expr, expr_after_parse_stmt, expr_after_parse_stmt2), expr_after_parse_stmt3))
              | _ -> let (toks6_after_parse_stmt2, expr_after_parse_stmt2) = parse_stmt toks6_after_parse_stmt in
                            (toks6_after_parse_stmt2, Seq(If(expr, expr_after_parse_stmt, NoOp), expr_after_parse_stmt2)))

  | Tok_For -> let toks2 = match_token toks Tok_For in
               let toks3 = match_token toks2 Tok_LParen in
               (match lookahead toks3 with Tok_ID id ->
               let toks4 = match_token toks3 (Tok_ID id) in
               let toks5 = match_token toks4 Tok_From in
               let (toks5_after_parse_expr, expr) = parse_expr toks5 in
               let toks6_after_parse_expr = match_token toks5_after_parse_expr Tok_To in
               let (toks6_after_parse_expr2, expr2) = parse_expr toks6_after_parse_expr in
               let toks7_after_parse_expr2 = match_token toks6_after_parse_expr2 Tok_RParen in
               let toks8_after_parse_expr2 = match_token toks7_after_parse_expr2 Tok_LBrace in
               let (toks8_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks8_after_parse_expr2 in
               let toks9_after_parse_stmt = match_token toks8_after_parse_stmt Tok_RBrace in
               let (toks9_after_parse_stmt2, expr_after_parse_stmt2) = parse_stmt toks9_after_parse_stmt in
               (toks9_after_parse_stmt2, Seq(For(id, expr, expr2, expr_after_parse_stmt), expr_after_parse_stmt2))
               |_ -> raise(InvalidInputException "not an id"))

  | Tok_While -> let toks2 = match_token toks Tok_While in
                 let toks3 = match_token toks2 Tok_LParen in
                 let (toks3_after_parse_expr, expr) = parse_expr toks3 in
                 let toks4_after_parse_expr = match_token toks3_after_parse_expr Tok_RParen in
                 let toks5_after_parse_expr = match_token toks4_after_parse_expr Tok_LBrace in
                 let (toks5_after_parse_stmt, expr_after_parse_stmt) = parse_stmt toks5_after_parse_expr in
                 let toks6_after_parse_stmt = match_token toks5_after_parse_stmt Tok_RBrace in
                 let (toks6_after_parse_stmt2, expr_after_parse_stmt2) = parse_stmt toks6_after_parse_stmt in
                 (toks6_after_parse_stmt2, Seq(While(expr, expr_after_parse_stmt), expr_after_parse_stmt2))
  
  | _ -> (toks, NoOp)

let parse_main toks : stmt =
  let toks2 = match_token toks Tok_Int_Type in
  let toks3 = match_token toks2 Tok_Main in
  let toks4 = match_token toks3 Tok_LParen in
  let toks5 = match_token toks4 Tok_RParen in
  let toks6 = match_token toks5 Tok_LBrace in
  let (remaining_toks, expr) = parse_stmt toks6 in
  let remaining_toks2 = match_token remaining_toks Tok_RBrace in
  if remaining_toks2 <> [EOF] then raise(InvalidInputException "still tokens left")
  else expr
