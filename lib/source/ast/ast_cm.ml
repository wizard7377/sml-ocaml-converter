type entry = Source of string | Depend of string [@@deriving show, eq, ord]

type package = { header : header; groups : package list; entries : entry list }
[@@deriving show, make, eq, ord]

and header = {
  name : string;
  version : string option;
  author : string option;
  description : string option;
}
[@@deriving show, make, eq, ord]
