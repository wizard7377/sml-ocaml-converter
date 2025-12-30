open Process_names
type res 
include Helpers

let ghost (v : 'a) : 'a Location.loc = Location.mkloc v Location.none
let process_sml ~(prog:Ast.prog): res = assert false


let rec process_type_value (ty : Ast.typ) : Parsetree.core_type = match ty with 
  TypVar name -> process_type_var name
  | TypCon (args, head) -> let 
    head' = process_name ~context:ValueType head in 
    let args' = List.map process_type_value args in
    Builder.ptyp_constr (ghost (process_longid head')) args'
  | TypPar ty -> process_type_value ty
  | TypFun (ty1, ty2) -> let ty1', ty2' = process_type_value ty1, process_type_value ty2 in
    Builder.ptyp_arrow Nolabel ty1' ty2'
  | TypTuple tys -> Builder.ptyp_tuple (List.map process_type_value tys)
  | TypRecord fields -> let fields' = List.flatten (List.map process_object_field_type fields) in
    Builder.ptyp_object fields' Closed



and process_object_field_type (field : Ast.typ_row) : Parsetree.object_field list = match field with 
  | Ast.TypRow (name, ty, rest) -> let here : Parsetree.object_field = Builder.otag (ghost (process_name ~context:Label name |> idx_to_string)) (process_type_value ty) in
    begin match rest with 
    | Some rest' -> here :: (process_object_field_type rest')
    | None -> [here]
    end
let rec process_type (ty : Ast.typ) : Parsetree.core_type = process_type_value ty
  
let rec process_con (con : Ast.con) : Parsetree.constant = match con with 
  | ConInt i -> assert false 
  | ConWord w -> assert false
  | ConFloat r -> assert false
  | ConChar c -> assert false
  | ConString s -> assert false

let rec process_exp (exp : Ast.exp) : Parsetree.expression = match exp with
  | ExpCon c -> Builder.pexp_constant (process_con c)
  | ExpApp (e1, e2) -> let e1' = process_exp e1 in
    let e2' = process_exp e2 in
    Builder.pexp_apply e1' [(Nolabel, e2')]
  | ExpIdx idx -> Builder.pexp_ident (ghost (process_longid (process_name ~context:Value idx)))
  | InfixApp (e1, op, e2) -> let op' = process_name ~context:Value op in
      Builder.pexp_apply (Builder.pexp_ident (ghost (process_longid op'))) [(Nolabel, process_exp e1); (Nolabel, process_exp e2)]
  | LetExp (bindings, body) -> assert false
  | _ -> assert false
  

let rec process_pat (pat : Ast.pat) : Parsetree.pattern = assert false

let rec procress_dec (dec : Ast.dec) : Parsetree.value_binding list = assert false

let rec proccess_str (str : Ast.str) : Parsetree.structure_item list = assert false

let rec process_sign (sign : Ast.sign) : Parsetree.signature_item list = assert false
let%test_unit "process basic type" = (let _ = process_type (Ast.TypVar (IdxVar "a")) in ())

