open Stdlib
open Helpers
(** A name, after processing *)
type idx = Operator of string | Name of string | Var of string | Label of string | Numbered of string | Long of idx list
type name_context = Module | PatternVar | ModuleType | Value | TypeVar | ValueType | PatternHead | PatternOther | Constructor | Label

exception WrongTypeContext of name_context
exception WrongTypeName
let constructor_names = ["Row"; "Case"; "AND"; "BAR"; "DO"; "END"; "FUN"; "IN"; "OF"; "THEN"; "OP"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "IN"; "END"; "FUN"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "AS"; "DO"; "END"; "FUN"; "IN"; "OF"; "THEN"; "VAL"; "WITH"; "AND"; "AS"; "DO"; "END"; "FUN"; "IN"; "OF"; "STAR"; "THEN"; "VAL"; "WITH"; "END"; "FUN"; "IN"; "OPEN"; "STAR"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "IN"; "OPEN"; "STAR"; "TYPE"; "AND"; "END"; "FUN"; "IN"; "OPEN"; "STAR"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "WITH"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "WITH"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "DO"; "END"; "FUN"; "OF"; "THEN"; "AND"; "DO"; "END"; "FUN"; "OF"; "THEN"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "AND"; "DO"; "END"; "FUN"; "OF"; "THEN"; "AND"; "DO"; "END"; "FUN"; "OF"; "THEN"; "AND"; "DO"; "END"; "FUN"; "IN"; "OF"; "THEN"; "AND"; "DO"; "END"; "FUN"; "IN"; "OF"; "THEN"; "END"; "FUN"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "FUN"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "IN"; "OPEN"; "TYPE"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "WITH"; "AND"; "END"; "FUN"; "OPEN"; "TYPE"; "Some"; "Prop"; "Mor"; "What"; "O_M"; "FGN"; "Impl"; "All"; "And"; "In"; "Dc"; "Dec"; "BDec"; "Impl"; "All"; "And"; "In"; "Dc"; "Dec"; "BDec"; "App"; "Lam"; "Pi"; "TPi"; "KPi"; "AElt"; "Skip"; "SOME"; "Decs"; "Bar"; "Alt"; "Plus"; "And"; "Pair"; "Inx"; "Lam"; "Par"; "Case"; "Let"; "New"; "Rec"; "Id"; "Pi"; "Fn"; "App"; "Dot"; "Of"; "Decs"; "Bar"; "Plus"; "And"; "Pair"; "Inx"; "Lam"; "Case"; "Let"; "Par"; "New"; "Rec"; "Alt"; "Id"; "Pi"; "Fn"; "App"; "Dot"; "Of"; "Type"; "Brk"; "Hbx"; "Vbx"; "Hvx"; "Hov"; "Open"; "Use"; "Open"; "Use"; "NDec"; "JAnd"; "JOf"; "JAnd"; "JOf"; "BVar"; "Pi"; "Lam"; "Root"; "EClo"; "App"; "SClo"; "Comp"; "Def"; "BVar"; "Pi"; "Lam"; "Root"; "EClo"; "App"; "SClo"; "Comp"; "Def"; "FV"; "LV"; "PV"; "Next"; "LVar"; "CVar"; "Next"; "LVar"; "CVar"; "Decl"; "Pi"; "Root"; "Lam"; "EVar"; "EClo"; "AVar"; "NVar"; "Proj"; "Def"; "FVar"; "App"; "SClo"; "Dot"; "Exp"; "Axp"; "BDec"; "ADec"; "NDec"; "LVar"; "Inst"; "Eqn"; "Decl"; "Pi"; "Root"; "Lam"; "EVar"; "EClo"; "AVar"; "NVar"; "Proj"; "Def"; "FVar"; "App"; "SClo"; "Dot"; "Exp"; "Axp"; "BDec"; "ADec"; "NDec"; "LVar"; "Inst"; "BClo"; "Eqn"; "Lex"; "Leq"; "Eq"; "LE"; "LT"; "Lex"; "Leq"; "Eq"; "LE"; "LT"; "Conj"; "Base"; "All"; "Ex"; "And"; "FClo"; "FVar"; "PDec"; "Lam"; "New"; "Rec"; "Case"; "PClo"; "Let"; "EVar"; "Var"; "SClo"; "Dot"; "Prg"; "Exp"; "Conj"; "Base"; "All"; "Ex"; "And"; "FClo"; "FVar"; "PDec"; "Lam"; "New"; "Rec"; "Case"; "PClo"; "Let"; "EVar"; "Var"; "SClo"; "Dot"; "Prg"; "Exp"; "Add"; "BV"; "Ex"; "And"; "Inx"; "Rec"; "Let"; "Case"; "Pair"; "New"; "App"; "PApp"; "Left"; "Ex"; "And"; "Inx"; "Rec"; "Let"; "Case"; "Pair"; "New"; "App"; "PApp"; "Left"; "Phi"; "Lex"; "All"; "And"; "Lex"; "All"; "And"; "Star"; "Mapp"; "Star"; "Mapp"; "Univ"; "FGN"; "Lex"; "Leq"; "Eq"; "LE"; "LT"; "Lex"; "Leq"; "Eq"; "LE"; "LT"; "Body"; "Arg"; "Body"; "Arg"; "Fill"; "Fix"; "Elim"; "All"; "And"; "Exp"; "Col"; "Exp"; "Kill"; "Col"; "Exp"; "Kill"; "Expr"; "Red"; "Red"; "ONE"; "TWO"; "Mapp"; "And"; "Leq"; "Eq"; "Pi"; "All"; "And"; "Leq"; "Eq"; "Pi"; "Leq"; "Lex"; "Leq"; "Mark"; "Seq"; "Star"; "Plus"; "Seq"; "Star"; "Plus"; "Row"; "Case"]
let ocaml_reserved_names = ["and";"as";"assert";"asr";"begin";"class ";"constraint";"do";"done";"downto";"else";"end ";"exception";"external";"false";"for";"fun";"function ";"functor";"if";"in";"include";"inherit";"initializer ";"land";"lazy";"let";"lor";"lsl";"lsr ";"lxor";"match";"method";"mod";"module";"mutable ";"new";"nonrec";"object";"of";"open";"or ";"private";"rec";"sig";"struct";"then";"to ";"true";"try";"type";"val";"virtual";"when ";"while";"with" ]
let ocaml_id_chars = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z';'_';'\'';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9']
  (** Basic processing of all names *)
let all_process (name:string) = 
  let name2 = if String.ends_with "_" name then name ^ "_" else name in 
  let name3 = if String.ends_with "'" name2 then name2 ^ "'" else name2 in
  let name4 = if List.mem name3 ocaml_reserved_names then name3 ^ "_" else name3 in
  name4
(** Checks if a pattern variable is supposed to be a variable or constructor 
  How this is decided:
  {ul
  {- If the name is already lowercase, it is a variable}
  {- If the name is in the list of known constructor names, it is {i not} a variable}
  {- If the name is shorter than 4 characters, it is a variable}
  {- Otherwise, it is a constructor}
  }

  {b WARNING}: This function should only be called on pattern tails, pattern heads are automatically constructors.
  *)
let pattern_is_var (name:string) : bool = 
  if (String.length name = 0) then true
  else if Char.lowercase_ascii (String.get name 0) = String.get name 0 then true
  else if List.mem name constructor_names then false
  else if String.length name < 4 then true
  else false

let pattern_is_var_idx (name:Ast.idx) : bool = match name with 
  | Ast.IdxIdx s -> pattern_is_var s
  | Ast.IdxLab l -> pattern_is_var l
  | _ -> false
exception EmptyList
let rec lait = function 
  | (x :: []) -> ([], x)
  | (x :: xs) -> let (ys , y) = lait xs in (x :: ys , y)
  | [] -> raise EmptyList
module type PROCESS = sig 
  type t 
  type res 
  val process : name_context -> t -> res
end
let process_idx (context :  name_context) (name:string) : string = match context with 
  | Module -> String.capitalize_ascii (all_process name)
  | ModuleType -> String.capitalize_ascii (all_process name)
  | Value -> String.uncapitalize_ascii (all_process name)
  | TypeVar -> String.uncapitalize_ascii (all_process name)
  | ValueType -> String.uncapitalize_ascii (all_process name)
  | PatternHead -> if ((List.mem name constructor_names) || (String.length name > 4)) then String.capitalize_ascii (all_process name) else (all_process name)
  | PatternOther -> String.uncapitalize_ascii (all_process name)
  | Constructor -> String.capitalize_ascii (all_process name)
  | Label -> all_process name
  | PatternVar -> String.uncapitalize_ascii (all_process name)


let rec process_name ~(context:name_context) (name:Ast.idx) : idx = match context, name with 
  | _ , Ast.IdxIdx op -> if List.mem (String.get op 0) ocaml_id_chars then Name (process_idx context op) else Operator op
  | TypeVar , Ast.IdxVar v -> Var (String.uncapitalize_ascii (all_process v))
  | _ , Ast.IdxLab l -> Label (all_process l)
  | _ , Ast.IdxNum n -> Numbered n
  | _ , Ast.IdxLong lst -> let (lst', fin) = lait lst in Long ((List.map (process_name ~context:Module) lst') @ [process_name ~context:Module fin])
  | ctx , _ -> raise (WrongTypeContext ctx)

let process_type_var name' : Parsetree.core_type = let name = process_name ~context:TypeVar name' in match name with 
  | Var n -> Builder.ptyp_var n
  | _ -> raise WrongTypeName

let rec idx_to_string (idx : idx) : string = match idx with 
  | Operator s -> s
  | Name s -> s
  | Var s -> "'" ^ s
  | Label s -> s
  | Numbered s -> s
  | Long lst -> String.concat "." (List.map idx_to_string lst)
let rec process_longid (name : idx) : Longident.t = match name with 
  | Long [idx] -> Longident.Lident (idx_to_string idx)
  | Long [] -> raise EmptyList
  | Long lst -> let (lst', fin) = lait lst in 
    Longident.Ldot (process_longid (Long lst') , idx_to_string fin)
  | _ -> Longident.Lident (idx_to_string name)
(** Parse type expressions *)
