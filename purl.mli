type t = private {
  typ : string;
  namespace : string list;
  name : string;
  version : string option;
  qualifiers : (string * string) list;
  subpath : string list;
}
(** The type of a package URL. The namespace, version, qualifiers, and subpath
    are optional. The scheme is always "pkg:". *)

val to_string : t -> string
(** [to_string purl] provides the canonical representation of [purl]. *)

val of_string : string -> (t, string) result
(** [of_string str] decodes the string and either returns an error or a [purl]. *)

val make : string -> ?namespace:string -> string -> ?version:string -> ?qualifiers:string -> ?subpath:string -> unit -> (t, string) result
(** [make typ ~namespace name ~version ~qualifiers ~subpath ()] returns either a
    [purl] or an error. An error is returned if a parameter does not conform to
    the purl specification, i.e. if the name is empty. *)

val equal : t -> t -> bool
(** [equal a b] checks whether [a] and [b] are equal. *)
