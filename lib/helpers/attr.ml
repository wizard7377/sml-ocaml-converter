type atag = string
type attr = Parsetree.attribute
type cite = Parsetree.payload
type 'a citer = 'a -> attr -> 'a

exception CommentNotFound

module Help = Ppxlib.Ast_helper

let expression : Parsetree.expression citer = Help.Exp.attr
let pattern : Parsetree.pattern citer = Help.Pat.attr
let core_type : Parsetree.core_type citer = Help.Typ.attr
let module_type : Parsetree.module_type citer = Help.Mty.attr
let module_expr : Parsetree.module_expr citer = Help.Mod.attr

let core_type : Parsetree.core_type citer =
 fun ct attr ->
  { ct with Parsetree.ptyp_attributes = attr :: ct.Parsetree.ptyp_attributes }

let type_declaration : Parsetree.type_declaration citer =
 fun node attr ->
  {
    node with
    Parsetree.ptype_attributes = attr :: node.Parsetree.ptype_attributes;
  }

let value_binding : Parsetree.value_binding citer =
 fun node attr ->
  { node with Parsetree.pvb_attributes = attr :: node.Parsetree.pvb_attributes }

let row_field : Parsetree.row_field citer =
 fun node attr ->
  { node with Parsetree.prf_attributes = attr :: node.Parsetree.prf_attributes }

let object_field : Parsetree.object_field citer =
 fun node attr ->
  { node with Parsetree.pof_attributes = attr :: node.Parsetree.pof_attributes }

let value_description : Parsetree.value_description citer =
 fun node attr ->
  {
    node with
    Parsetree.pval_attributes = attr :: node.Parsetree.pval_attributes;
  }

let label_declaration : Parsetree.label_declaration citer =
 fun node attr ->
  { node with Parsetree.pld_attributes = attr :: node.Parsetree.pld_attributes }

let constructor_declaration : Parsetree.constructor_declaration citer =
 fun node attr ->
  { node with Parsetree.pcd_attributes = attr :: node.Parsetree.pcd_attributes }

let type_extension : Parsetree.type_extension citer =
 fun node attr ->
  {
    node with
    Parsetree.ptyext_attributes = attr :: node.Parsetree.ptyext_attributes;
  }

let exception_constructor : Parsetree.extension_constructor citer =
 fun node attr ->
  {
    node with
    Parsetree.pext_attributes = attr :: node.Parsetree.pext_attributes;
  }

let exception_declaration : Parsetree.type_exception citer =
 fun node attr ->
  {
    node with
    Parsetree.ptyexn_attributes = attr :: node.Parsetree.ptyexn_attributes;
  }

let class_type : Parsetree.class_type citer =
 fun node attr ->
  {
    node with
    Parsetree.pcty_attributes = attr :: node.Parsetree.pcty_attributes;
  }

let class_type_field : Parsetree.class_type_field citer =
 fun node attr ->
  {
    node with
    Parsetree.pctf_attributes = attr :: node.Parsetree.pctf_attributes;
  }

let class_infos : 'a Parsetree.class_infos citer =
 fun node attr ->
  { node with Parsetree.pci_attributes = attr :: node.Parsetree.pci_attributes }

let class_expr : Parsetree.class_expr citer =
 fun node attr ->
  { node with Parsetree.pcl_attributes = attr :: node.Parsetree.pcl_attributes }

let class_field : Parsetree.class_field citer =
 fun node attr ->
  { node with Parsetree.pcf_attributes = attr :: node.Parsetree.pcf_attributes }

let module_declaration : Parsetree.module_declaration citer =
 fun node attr ->
  { node with Parsetree.pmd_attributes = attr :: node.Parsetree.pmd_attributes }

let module_type_declaration : Parsetree.module_type_declaration citer =
 fun node attr ->
  {
    node with
    Parsetree.pmtd_attributes = attr :: node.Parsetree.pmtd_attributes;
  }

let module_substitution : Parsetree.module_substitution citer =
 fun node attr ->
  { node with Parsetree.pms_attributes = attr :: node.Parsetree.pms_attributes }

let open_infos : 'a Parsetree.open_infos citer =
 fun node attr ->
  {
    node with
    Parsetree.popen_attributes = attr :: node.Parsetree.popen_attributes;
  }

let include_infos : 'a Parsetree.include_infos citer =
 fun node attr ->
  {
    node with
    Parsetree.pincl_attributes = attr :: node.Parsetree.pincl_attributes;
  }

let value_binding : Parsetree.value_binding citer =
 fun node attr ->
  { node with Parsetree.pvb_attributes = attr :: node.Parsetree.pvb_attributes }

let module_binding : Parsetree.module_binding citer =
 fun node attr ->
  { node with Parsetree.pmb_attributes = attr :: node.Parsetree.pmb_attributes }
