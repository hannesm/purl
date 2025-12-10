type t = private {
  typ : string;
  namespace : string list;
  name : string;
  version : string option;
  qualifiers : (string * string) list;
  subpath : string list;
}

val to_string : t -> string

val of_string : string -> (t, string) result

val make : string -> ?namespace:string -> string -> ?version:string -> ?qualifiers:string -> ?subpath:string -> unit -> (t, string) result

val v_exn : string -> string list -> string -> string option -> (string * string) list -> string list -> t

val equal : t -> t -> bool
