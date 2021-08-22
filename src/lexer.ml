open TokenTypes

let tokenize input =
  let len = String.length input in
  let rec helper pos =
    if pos >= len then
      [EOF]
    (*matches TokID*)
    else if (Str.string_match (Str.regexp "[ \n\t]+") input pos) then
      let match_len = String.length (Str.matched_string input) in
      helper (pos + match_len)
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then
      (*don't count those that perfectly match keyword*)
      let match_str = Str.matched_string input in
      let match_len = String.length match_str in
      (*match bool*)
      if (Str.string_match (Str.regexp "true\\|false") match_str 0) then
        let match_bool = Str.matched_string match_str in
        let match_len_b = String.length match_bool in
        Tok_Bool (bool_of_string match_bool) :: (helper (pos + match_len_b))
      (*match other keywords*)
      else if (Str.string_match (Str.regexp "^printf$") match_str 0) then
        Tok_Print :: (helper (pos + 6))
      else if (Str.string_match (Str.regexp "^while$") match_str 0) then
        Tok_While :: (helper (pos + 5))
      else if (Str.string_match (Str.regexp "^from$") match_str 0) then
        Tok_From :: (helper (pos + 4))
      else if (Str.string_match (Str.regexp "^else$") match_str 0) then
        Tok_Else :: (helper (pos + 4))
      else if (Str.string_match (Str.regexp "^bool$") match_str 0) then
        Tok_Bool_Type :: (helper (pos + 4))
      else if (Str.string_match (Str.regexp "^main$") match_str 0) then
        Tok_Main :: (helper (pos + 4))
      else if (Str.string_match (Str.regexp "^int$") match_str 0) then
        Tok_Int_Type :: (helper (pos + 3))
      else if (Str.string_match (Str.regexp "^for$") match_str 0) then
        Tok_For :: (helper (pos + 3))
      else if (Str.string_match (Str.regexp "^if$") match_str 0) then
        Tok_If :: (helper (pos + 2))
      else if (Str.string_match (Str.regexp "^to$") match_str 0) then
        Tok_To :: (helper (pos + 2))
      (*otherwise match to ID*)
      else
        Tok_ID match_str :: (helper (pos + match_len))
    (*match int before sub*)
    else if (Str.string_match (Str.regexp "-?[0-9]+") input pos) then
      let match_int = Str.matched_string input in
      let match_len = String.length match_int in
      Tok_Int (int_of_string match_int) :: (helper (pos + match_len))
    (*must escape - \^$.|?*+()[{ *)
    else if (Str.string_match (Str.regexp "==") input pos) then
      Tok_Equal :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "!=") input pos) then
      Tok_NotEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp ">=") input pos) then
      Tok_GreaterEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "<=") input pos) then
      Tok_LessEqual :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "||") input pos) then
      Tok_Or :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "&&") input pos) then
      Tok_And :: (helper (pos + 2))
    else if (Str.string_match (Str.regexp "=") input pos) then
      Tok_Assign :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ">") input pos) then
      Tok_Greater :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "<") input pos) then
      Tok_Less :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "!") input pos) then
      Tok_Not :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ";") input pos) then
      Tok_Semi :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\+") input pos) then
      Tok_Add :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "-") input pos) then
      Tok_Sub :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\*") input pos) then
      Tok_Mult :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "/") input pos) then
      Tok_Div :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "\\^") input pos) then
      Tok_Pow :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "(") input pos) then
      Tok_LParen :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp ")") input pos) then
      Tok_RParen :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "{") input pos) then
      Tok_LBrace :: (helper (pos + 1))
    else if (Str.string_match (Str.regexp "}") input pos) then
      Tok_RBrace :: (helper (pos + 1))
    else
      raise (InvalidInputException "failed lexer")
  in helper 0


(*helper function*)
let bool_of_string str = if str = "true" then true else false