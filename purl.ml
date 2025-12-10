let valid_key = function
  | 'a' .. 'z' | '0' .. '9' | '.' | '-' | '_' -> true
  | _ -> false

let alpha = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let alphanum = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let punctation = function
  | '.' | '-' | '_' | '~' -> true
  | _ -> false

module Utils = struct
  let int_of_hex_char = function
    | '0' .. '9' as c -> Char.code c - 48
    | 'A' .. 'F' as c -> Char.code c - 55
    | 'a' .. 'f' as c -> Char.code c - 87
    | _ -> invalid_arg "not a hex char"

  let percent_decode str =
    let l = String.length str in
    let b = Buffer.create l in
    let rec scan s idx =
      if idx = l then begin
        Buffer.add_substring b str s (idx - s);
        Ok ()
      end else if str.[idx] = '%' then begin
        Buffer.add_substring b str s (idx - s);
        if idx + 2 > l then
          Error "bad percent encoding (bad length)"
        else
          let high = idx + 1
          and low = idx + 2
          in
          match int_of_hex_char str.[high], int_of_hex_char str.[low] with
          | exception _ ->
            Error "bad percent encoding (invalid hex encoding)"
          | highbits, lowbits ->
            let char = Char.chr (highbits lsl 4 + lowbits) in
            if alphanum char || punctation char then
              Error "bad percent encoding (alphanumeric or punctation)"
            else begin
              Buffer.add_char b char;
              scan (low + 1) (low + 1)
            end
      end else scan s (idx + 1)
    in
    match scan 0 0 with
    | Ok () -> Ok (Buffer.contents b)
    | Error _ as e -> e

  let percent_encode str =
    let l = String.length str in
    let b = Buffer.create l in
    let rec scan s idx =
      if idx = l then begin
        Buffer.add_substring b str s (idx - s)
      end else begin
        let char = str.[idx] in
        if alphanum char || punctation char || char = ':' then
          scan s (idx + 1)
        else begin
          if idx > s then Buffer.add_substring b str s (idx - s);
          Buffer.add_string b (Printf.sprintf "%%%02X" (Char.code char));
          scan (idx + 1) (idx + 1)
        end
      end
    in
    scan 0 0;
    Buffer.contents b


  let trim_char_really char s =
    let l = String.length s in
    let i = ref 0 in
    while !i < l && String.unsafe_get s !i = char do
      incr i
    done;
    let j = ref (l - 1) in
    while !j >= !i && String.unsafe_get s !j = char do
      decr j
    done;
    if !j >= !i then
      String.sub s !i (!j - !i + 1)
    else
      ""

  let trim_char char s =
    if s = "" then s
    else if String.unsafe_get s 0 = char ||
            String.unsafe_get s (String.length s - 1) = char
    then
      trim_char_really char s
    else
      s

  let split_from_right char s =
    match String.rindex_opt s char with
    | None -> s, None
    | Some idx ->
      String.sub s 0 idx,
      Some (String.sub s (idx + 1) (String.length s - idx - 1))

  let split_from_left char s =
    match String.index_opt s char with
    | None -> None, s
    | Some idx ->
      Some (String.sub s 0 idx),
      String.sub s (idx + 1) (String.length s - idx - 1)
end

let scheme = "pkg"

type t = {
  typ : string;
  namespace : string list;
  name : string;
  version : string option;
  qualifiers : (string * string) list;
  subpath : string list;
}

let to_string { typ ; namespace ; name ; version ; qualifiers ; subpath } =
  let buf = Buffer.create 100 in
  Buffer.add_string buf scheme;
  Buffer.add_char buf ':';
  Buffer.add_string buf typ;
  Buffer.add_char buf '/';
  (match namespace with
   | [] -> ()
   | segs ->
     List.iter (fun s ->
         let enc = Utils.percent_encode s in
         Buffer.add_string buf enc;
         Buffer.add_char buf '/')
       segs);
  Buffer.add_string buf name;
  (match version with
   | None -> ()
   | Some v ->
     Buffer.add_char buf '@';
     Buffer.add_string buf (Utils.percent_encode v));
  (match qualifiers with
   | [] -> ()
   | qs ->
     Buffer.add_char buf '?';
     let qs' =
       List.map (fun (k, v) -> k ^ "=" ^ Utils.percent_encode v)
         qs |> List.sort String.compare
     in
     Buffer.add_string buf (String.concat "&" qs'));
  (match subpath with
   | [] -> ()
   | s ->
     Buffer.add_char buf '#';
     Buffer.add_string buf
       (String.concat "/" (List.map Utils.percent_encode s)));
  Buffer.contents buf


let typ typ =
  let valid typ =
    String.for_all (fun c -> alphanum c || c = '.' || c = '-') typ &&
    String.length typ > 0 && alpha (String.get typ 0)
  in
  if valid typ then
    Ok (String.lowercase_ascii typ)
  else
    Error "invalid typ"

let namespace ns =
  let ( let* ) = Result.bind in
  let segment s =
    match Utils.percent_decode s with
    | Ok s ->
      if
        String.for_all (fun c -> c <> '/') s &&
        String.length s > 0
      then
        Ok s
      else
        Error "invalid segment in namespace"
    | Error _ as e -> e
  in
  let trimmed_ns = Utils.trim_char '/' ns in
  List.fold_left (fun acc s ->
      let* acc = acc in
      let* s' = segment s in
      Ok (s' :: acc))
    (Ok []) (String.split_on_char '/' trimmed_ns) |> Result.map List.rev

let name name =
  let ( let* ) = Result.bind in
  let trimmed_name = Utils.trim_char '/' name in
  let* r = Utils.percent_decode trimmed_name in
  if String.length r > 0 then Ok r else Error "empty name"

let version v = Utils.percent_decode v

let qualifiers q =
  let qs = String.split_on_char '&' q in
  let ( let* ) = Result.bind in
  let qualifier q =
    let* k, v =
      match String.split_on_char '=' q with
      | [] -> Error "invalid qualifier (missing =)"
      | k :: [] -> Ok (k, "")
      | k :: v -> Ok (k, String.concat "=" v)
    in
    if
      String.for_all valid_key k &&
      String.length k > 0 && alpha (String.unsafe_get k 0)
    then
      let* v = Utils.percent_decode v in
      Ok (k, v)
    else
      Error "invalid key for qualifier"
  in
  List.fold_left (fun acc q ->
      let* acc = acc in
      let* (k, v) = qualifier q in
      if v = "" then
        Ok acc
      else
        match List.assoc_opt k acc with
        | None -> Ok ((k, v) :: acc)
        | Some _ -> Error "key exists twice")
    (Ok []) qs |> Result.map List.rev

let subpath s =
  let trimmed = Utils.trim_char '/' s in
  let segment s =
    match Utils.percent_decode s with
    | Ok s ->
      if String.for_all (fun c -> not (c = '/')) s then
        Ok s
      else
        Error "bad segment in subpath"
    | Error _ as e -> e
  in
  let ( let* ) = Result.bind in
  List.fold_left (fun acc s ->
      let* acc = acc in
      let* segment = segment s in
      if segment = "" || segment = "." || segment = ".." then
        Ok acc
      else
        Ok (segment :: acc))
    (Ok []) (String.split_on_char '/' trimmed) |> Result.map List.rev

let of_string s =
  let ( let* ) = Result.bind in
  let remainder, sub_str = Utils.split_from_right '#' s in
  let* subpath = match sub_str with
    | None -> Ok []
    | Some s -> subpath s
  in
  let remainder, qs_str = Utils.split_from_right '?' remainder in
  let* qualifiers = match qs_str with
    | None -> Ok []
    | Some s -> qualifiers s
  in
  let scheme_str, remainder = Utils.split_from_left ':' remainder in
  let* scheme_str = Option.to_result ~none:"missing scheme" scheme_str in
  let scheme' = String.lowercase_ascii scheme_str in
  let trimmed_remainder = Utils.trim_char '/' remainder in
  let typ_str, remainder = Utils.split_from_left '/' trimmed_remainder in
  let* typ_str = Option.to_result ~none:"missing type" typ_str in
  let* typ = typ typ_str in
  let remainder, version_str = Utils.split_from_right '@' remainder in
  let* version = Option.fold ~none:(Ok None) ~some:(function Error _ as e -> e | Ok x -> Ok (Some x)) (Option.map version version_str) in
  let trimmed_remainder = Utils.trim_char '/' remainder in
  let remainder, name_str = Utils.split_from_right '/' trimmed_remainder in
  let name_str' = Option.value ~default:remainder name_str in
  let* name = name name_str' in
  let* namespace =
    if name_str = None then Ok [] else namespace remainder
  in
  if String.equal scheme scheme' then
    Ok { typ ; namespace ; name ; version ; qualifiers ; subpath }
  else
    Error "bad scheme"

let make typ' ?namespace:ns name' ?version:v ?qualifiers:qs ?subpath:sub () =
  let ( let* ) = Result.bind in
  let* typ = typ typ' in
  let* namespace = Option.value ~default:(Ok []) (Option.map namespace ns) in
  let* name = name name' in
  let* version =
    Option.fold
      ~none:(Ok None)
      ~some:(function Error _ as e -> e | Ok x -> Ok (Some x))
      (Option.map version v)
  in
  let* qualifiers = Option.value ~default:(Ok []) (Option.map qualifiers qs) in
  let* subpath = Option.value ~default:(Ok []) (Option.map subpath sub) in
  Ok { typ ; namespace ; name ; version ; qualifiers ; subpath }

let v_exn typ namespace name version qualifiers subpath =
  { typ ; namespace ; name ; version ; qualifiers ; subpath }

let equal a b = String.equal (to_string a) (to_string b)
