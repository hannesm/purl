let purl_t =
  let module M = struct
    type t = Purl.t
    let pp ppf purl = Format.pp_print_string ppf (Purl.to_string purl)
    let equal = Purl.equal
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let test_one str canonical exp () =
  Alcotest.(check (result purl_t string) ("example from README " ^ str) (Ok exp)
              (Purl.of_string str));
  if canonical then
    Alcotest.(check string "building example from README" str
                (Purl.to_string exp))

let make_exn typ ?namespace name ?version ?qualifiers ?subpath () =
  Result.get_ok (Purl.make typ ?namespace name ?version ?qualifiers ?subpath ())

let readme_tests =
  (* from https://github.com/package-url/purl-spec README.md at 0c3bc11 *)
  List.map (fun (str, canonical, exp) ->
      str, `Quick, test_one str canonical exp)
    [
      ("pkg:bitbucket/birkenfeld/pygments-main@244fd47e07d1014f0aed9c", true,
       make_exn "bitbucket" ~namespace:"birkenfeld" "pygments-main"
         ~version:"244fd47e07d1014f0aed9c" ());
      ("pkg:deb/debian/curl@7.50.3-1?arch=i386&distro=jessie", true,
       make_exn "deb" ~namespace:"debian" "curl" ~version:"7.50.3-1"
         ~qualifiers:"arch=i386&distro=jessie" ());
      ("pkg:docker/cassandra@sha256:244fd47e07d1004f0aed9c", true,
       make_exn "docker" "cassandra" ~version:"sha256:244fd47e07d1004f0aed9c" ());
      ("pkg:docker/customer/dockerimage@sha256:244fd47e07d1004f0aed9c?repository_url=gcr.io", true,
       make_exn "docker" ~namespace:"customer" "dockerimage" ~version:"sha256:244fd47e07d1004f0aed9c"
         ~qualifiers:"repository_url=gcr.io" ());
      ("pkg:gem/jruby-launcher@1.1.2?platform=java", true,
       make_exn "gem" "jruby-launcher" ~version:"1.1.2"
         ~qualifiers:"platform=java" ());
      ("pkg:gem/ruby-advisory-db-check@0.12.4", true,
       make_exn "gem" "ruby-advisory-db-check" ~version:"0.12.4" ());
      ("pkg:github/package-url/purl-spec@244fd47e07d1004f0aed9c", true,
       make_exn "github" ~namespace:"package-url" "purl-spec" ~version:"244fd47e07d1004f0aed9c" ());
      ("pkg:golang/google.golang.org/genproto#googleapis/api/annotations", true,
       make_exn "golang" ~namespace:"google.golang.org" "genproto" ~subpath:"googleapis/api/annotations" ());
      ("pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?packaging=sources", true,
       make_exn "maven" ~namespace:"org.apache.xmlgraphics" "batik-anim" ~version:"1.9.1"
         ~qualifiers:"packaging=sources" ());
      ("pkg:maven/org.apache.xmlgraphics/batik-anim@1.9.1?repository_url=repo.spring.io/release", false (* repository_url not percent-encoded *),
        make_exn "maven" ~namespace:"org.apache.xmlgraphics" "batik-anim" ~version:"1.9.1"
          ~qualifiers:"repository_url=repo.spring.io/release" ());
      ("pkg:npm/%40angular/animation@12.3.1", true,
       make_exn "npm" ~namespace:"@angular" "animation" ~version:"12.3.1" ());
      ("pkg:npm/foobar@12.3.1", true,
       make_exn "npm" "foobar" ~version:"12.3.1" ());
      ("pkg:nuget/EnterpriseLibrary.Common@6.0.1304", true,
       make_exn "nuget" "EnterpriseLibrary.Common" ~version:"6.0.1304" ());
      ("pkg:pypi/django@1.11.1", true,
       make_exn "pypi" "django" ~version:"1.11.1" ());
      ("pkg:rpm/fedora/curl@7.50.3-1.fc25?arch=i386&distro=fedora-25", true,
       make_exn "rpm" ~namespace:"fedora" "curl" ~version:"7.50.3-1.fc25"
         ~qualifiers:"arch=i386&distro=fedora-25" ());
      ("pkg:rpm/opensuse/curl@7.56.1-1.1.?arch=i386&distro=opensuse-tumbleweed", true,
         make_exn "rpm" ~namespace:"opensuse" "curl" ~version:"7.56.1-1.1."
           ~qualifiers:"arch=i386&distro=opensuse-tumbleweed" ());
    ]

module String_map = Map.Make (String)

let smap : ?kind:string -> 'a Jsont.t -> 'a String_map.t Jsont.t =
  fun ?kind t ->
  Jsont.Object.map ?kind Fun.id
  |> Jsont.Object.keep_unknown (Jsont.Object.Mems.string_map t) ~enc:Fun.id
  |> Jsont.Object.finish

type json_purl = {
  typ : string option;
  namespace : string option;
  name : string option;
  version : string option;
  qualifiers : string String_map.t option;
  subpath : string option;
}

let make_json_purl typ namespace name version qualifiers subpath =
  { typ ; namespace ; name ; version ; qualifiers ; subpath }

(* TODO: I've no clue how to handle the input, so now I use the 'unknown' escape
   hatch. The test_type field defines which input it is -- when `Parse or
   `Roundtrip, it is `Data of string, when `Build it is the json. *)
type json_test = {
  description : string;
  test_type : [ `Parse | `Build | `Roundtrip ];
  (*  input : string (* [ `Data of string | `Build of json_purl ]*);*)
  expected_output : string option;
  expected_failure : bool;
  unknown : Jsont.json;
}

let make_json_test description test_type expected_output expected_failure unknown =
  { description ; test_type ; expected_output ; expected_failure ; unknown }

let purl_jsont =
  Jsont.Object.map ~kind:"purl" make_json_purl
  |> Jsont.Object.mem "type" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { typ ; _ } -> typ)
  |> Jsont.Object.mem "namespace" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { namespace ; _ } -> namespace)
  |> Jsont.Object.mem "name" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { name ; _ } -> name)
  |> Jsont.Object.mem "version" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { version ; _ } -> version)
  |> Jsont.Object.mem "qualifiers" Jsont.(option (smap string)) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { qualifiers ; _ } -> qualifiers)
  |> Jsont.Object.mem "subpath" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { subpath ; _ } -> subpath)
  |> Jsont.Object.finish

let json_test_jsont =
  let assoc = ["parse", `Parse; "build", `Build; "roundtrip", `Roundtrip ] in
  Jsont.Object.map ~kind:"test" make_json_test
  |> Jsont.Object.mem "description" Jsont.string ~enc:(fun { description ; _ } -> description)
  |> Jsont.Object.mem "test_type" (Jsont.enum ~kind:"typ" assoc) ~enc:(fun { test_type ; _ } -> test_type)
  (*  |> Jsont.Object.mem "input" (Jsont.any ~dec_string:Jsont.string ~dec_object:purl_jsont ()) ~enc:(fun { input ; _ } -> input)*)
  |> Jsont.Object.mem "expected_output" Jsont.(option string) ~dec_absent:None ~enc_omit:Option.is_none ~enc:(fun { expected_output ; _ } -> expected_output)
  |> Jsont.Object.mem "expected_failure" Jsont.bool ~enc:(fun { expected_failure ; _ } -> expected_failure)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun { unknown ; _ } -> unknown)
  |> Jsont.Object.finish

let make_tests schema tests =
  (schema, tests)

let json_tests_jsont =
  Jsont.Object.map ~kind:"test" make_tests
  |> Jsont.Object.mem "$schema" Jsont.string ~enc:(fun (schema, _) -> schema)
  |> Jsont.Object.mem "tests" (Jsont.list json_test_jsont) ~enc:(fun (_, tests) -> tests)
  |> Jsont.Object.finish

let err_str =
  let module M = struct
    type t = string
    let pp ppf data = Format.pp_print_string ppf data
    let equal _ _ = true
  end in (module M: Alcotest.TESTABLE with type t = M.t)

let execute_build description expected_failure expected_output purl =
  match purl.typ, purl.name with
  | None, _ | _, None when expected_failure -> ()
  | None, _ | _, None -> assert false
  | Some typ, Some name ->
    let qualifiers =
      Option.map
        (fun x ->
           String.concat "&"
             (List.map (fun (k, v) -> k ^ "=" ^ v) (String_map.bindings x)))
        purl.qualifiers
    in
    let purl = Purl.make typ ?namespace:purl.namespace name ?version:purl.version ?qualifiers ?subpath:purl.subpath () in
    if expected_failure then
      Alcotest.(check (result purl_t err_str) description (Error "") purl)
    else
      match purl with
      | Ok purl -> Alcotest.(check string description (Option.get expected_output) (Purl.to_string purl))
      | Error msg -> Alcotest.fail msg

let execute_parse description expected_failure expected_output d =
  match Purl.of_string d with
  | Error _ when expected_failure -> ()
  | Error msg -> Alcotest.fail msg
  | Ok purl ->
    print_endline ("input " ^ d);
    Alcotest.(check string description (Option.get expected_output) (Purl.to_string purl))

let execute_roundtrip description expected_failure expected_output d =
  match Purl.of_string d with
  | Error _ when expected_failure -> ()
  | Error msg -> Alcotest.fail msg
  | Ok purl ->
    print_endline ("input " ^ d);
    Alcotest.(check string description (Option.get expected_output) (Purl.to_string purl))

let execute_test test =
  let input = match test.unknown with
    | Jsont.Object els ->
      (match List.find_opt (fun ((name, _meta), _value) -> String.equal name "input") (fst els) with
       | Some (_, value) -> value
       | None -> assert false)
    | _ -> assert false
  in
  let input =
    match test.test_type with
    | `Build ->
      (match Jsont.Json.decode purl_jsont input with
       | Ok purl -> `Purl purl
       | Error msg -> print_endline ("failed: " ^ msg); assert false)
    | _ -> `Data (Result.get_ok (Jsont.Json.decode Jsont.string input))
  in
  match test.test_type, input with
  | `Build, `Purl purl ->
    execute_build test.description test.expected_failure test.expected_output purl
  | `Parse, `Data d ->
    execute_parse test.description test.expected_failure test.expected_output d
  | `Roundtrip, `Data d ->
    execute_roundtrip test.description test.expected_failure test.expected_output d
  | _ -> assert false

let with_infile file f =
  let process file ic = try Ok (f (Bytesrw.Bytes.Reader.of_in_channel ic)) with
  | Sys_error e -> Error (Format.sprintf "@[<v>%s:@,%s@]" file e)
  in
  try match file with
  | "-" -> process file In_channel.stdin
  | file -> In_channel.with_open_bin file (process file)
  with Sys_error e -> Error e


let json_test file () =
  match
    with_infile file @@ fun r ->
    match Jsont_bytesrw.decode ~file json_tests_jsont r with
    | Ok data ->
      List.iteri
        (fun i t -> print_endline (string_of_int i) ; execute_test t)
        (snd data);
    | Error err ->
      Alcotest.fail ("error processing " ^ file ^ ": " ^ err)
  with
  | Ok _ -> ()
  | Error err ->
    Alcotest.fail ("error processing with_infile " ^ file ^ ": " ^ err)

let json_tests () =
  let collect_dir dir =
    let open Unix in
    let dh = opendir dir in
    let next () = try Some (readdir dh) with End_of_file -> None in
    let rec doone acc = function
      | Some "." | Some ".." -> doone acc (next ())
      | Some s when not (Sys.is_directory (Filename.concat dir s)) ->
        doone (Filename.concat dir s :: acc) (next ())
      | Some _ -> doone acc (next ())
      | None -> acc
    in
    let res = doone [] (next ()) in
    closedir dh ;
    res
  in
  let files = collect_dir "data/" in
  List.map (fun file -> file, [ "tests", `Quick, json_test file ]) files

let suites = [
  "README", readme_tests ;
]

let () =
  let json_tests = json_tests () in
  Alcotest.run "purl tests" (suites @ json_tests)

