(* Format_utils: Types, combinators, and helper functions for pretty-printing *)

type space_formatter = (unit, Stdlib.Format.formatter, unit) format
type 'a t = 'a Fmt.t

let ( ++ ) = Fmt.( ++ )

(* ========== Utility Functions ========== *)

let top_sep : _ Fmt.t =
 fun f x -> (Fmt.cut ++ Fmt.const Fmt.string ";;" ++ Fmt.cut) f x

let use (fmt : 'a -> unit t) : 'a t = fun f x -> fmt x f ()
let opt_val t ~default = match t with None -> default | Some x -> x
let sp = Fmt.sp

let list ?(sep = Fmt.sp) ?(first = Fmt.nop) ?(last = Fmt.nop) fu f = function
  | [] -> ()
  | [ x ] -> fu f x
  | x :: xs ->
      first f ();
      fu f x;
      List.iter
        (fun x ->
          sep f ();
          fu f x)
        xs;
      last f ()

let alts (f : 'a t) : 'a list t =
  Fmt.vbox (Fmt.list ~sep:(Fmt.cut ++ Fmt.const Fmt.string "|" ++ Fmt.cut) f)

let option ?(first = Fmt.nop) ?(last = Fmt.nop) fu f = function
  | None -> ()
  | Some x ->
      first f ();
      fu f x;
      last f ()

let fstr (s : string) : _ Fmt.t = fun f x -> Fmt.const Fmt.string s f x
let paren b fu f x = if b then (Fmt.parens fu) f x else fu f x

(* ========== Combinator Helpers ========== *)
let block (fmt : 'a t) : 'a t =
 fun f x -> (fstr "begin" ++ sp ++ Fmt.box fmt ++ sp ++ fstr "end") f x

(* Wrapping combinators *)
let parens (fmt : 'a t) : 'a t = fun f x -> Fmt.parens fmt f x
let braces (fmt : 'a t) : 'a t = fun f x -> Fmt.braces fmt f x
let brackets (fmt : 'a t) : 'a t = fun f x -> Fmt.brackets fmt f x
let bracks (fmt : 'a t) : 'a t = fun f x -> (fstr "[|" ++ fmt ++ fstr "|]") f x
let angles (fmt : 'a t) : 'a t = fun f x -> (fstr "<" ++ fmt ++ fstr ">") f x

(* Conditional wrapping *)
let parens_if cond fmt = if cond then parens fmt else fmt

(* Keyword and operator helpers *)
let kwd s : _ Fmt.t = fun f x -> fstr (s ^ " ") f x
let op s : _ Fmt.t = fun f x -> fstr (" " ^ s ^ " ") f x
let sep s : _ Fmt.t = fun f x -> fstr s f x

(* Common separators *)
let comma : _ Fmt.t = fun f x -> Fmt.comma f x
let semi : _ Fmt.t = fun f x -> fstr ";" f x
let space : _ Fmt.t = fun f x -> Fmt.sp f x
let cut : _ Fmt.t = fun f x -> Fmt.cut f x

(* Boxing helpers *)
let box n (fmt : 'a t) : 'a t = fun f x -> Fmt.box ~indent:n fmt f x
let hbox (fmt : 'a t) : 'a t = fun f x -> Fmt.hbox fmt f x
let vbox (fmt : 'a t) : 'a t = fun f x -> Fmt.vbox fmt f x
let hvbox (fmt : 'a t) : 'a t = fun f x -> Fmt.hvbox fmt f x

