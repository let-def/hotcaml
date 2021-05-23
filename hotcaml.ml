let cwd = Sys.getcwd ()

let push_front xs x = xs := x :: !xs
let load_path = ref []
let packages = ref []
let entrypoints = ref []

let arg_specs = [
  "-I", Arg.String (push_front load_path),
  "<dir>  Add <dir> to the list of include directories";
  "-package", Arg.String (push_front packages),
  "<pkg>  Load findlib package <pkg>";
]

let () =
  Arg.parse arg_specs (push_front entrypoints)
    (Printf.sprintf
       "Usage: %s [-package findlib-package ...] source.ml ..."
       Sys.argv.(0))

let entrypoints = List.rev !entrypoints

let add_dir =
  let loaded = Hashtbl.create 7 in
  fun dir ->
    if not (Hashtbl.mem loaded dir) then (
      Hashtbl.add loaded dir ();
      Printf.ksprintf prerr_endline "Loading %S" dir;
      Topdirs.dir_directory dir;
    )

let () = push_front load_path cwd
let () = List.iter add_dir (List.rev !load_path)

let () =
  let crc_intfs = Symtable.init_toplevel() in
  Compmisc.init_path ();
  Env.import_crcs ~source:Sys.executable_name crc_intfs;
  Topfind.add_predicates ["byte"];
  Topfind.don't_load_deeply ["compiler-libs.toplevel"];
  ()

let () =
  let all = ref [] in
  let _ = Symtable.filter_global_map (fun ident ->
      all := ident :: !all;
      true
    ) (Symtable.current_state ())
  in
  (*List.iter prerr_endline !Topfind.predicates;*)
  begin try
      Topfind.load_deeply (List.rev !packages);
    with exn ->
      Format.printf "%a\n%!" Location.report_exception exn
  end;
  let added = ref [] in
  let _ =
    Symtable.filter_global_map (fun ident ->
        if not (List.mem ident !all) then
          added := ident :: !added;
        true
      ) (Symtable.current_state ())
  in
  List.iter (fun ident -> Format.printf "added %a\n" Ident.print ident)
    !added

(* ** *)

module File_id : sig
  type t
  val missing : t
  val identify : string -> t
  val same : t -> t -> bool
end = struct

  type t =
    | Missing
    | Found of Unix.stats

  let missing = Missing

  let identify path =
    match Unix.stat path with
    | exception Unix.Unix_error(Unix.ENOENT, "stat", _) -> Missing
    | stats -> Found stats

  let same t1 t2 =
    match t1, t2 with
    | Missing, Missing -> true
    | Missing, Found _ | Found _, Missing -> false
    | Found a, Found b ->
      a.st_dev = b.st_dev &&
      a.st_ino = b.st_ino &&
      a.st_ctime = b.st_ctime &&
      a.st_mtime = b.st_mtime &&
      a.st_size = b.st_size
end

module Hotpath : sig
  val refresh : unit -> unit
  val find : string -> string option
end = struct

  let path =
    ref (List.map (fun path -> path, File_id.missing, [||]) !load_path)

  let cache = Hashtbl.create 7

  let refresh () =
    Hashtbl.clear cache;
    path := List.map (fun (path, id, files) ->
        (*Printf.eprintf "refresh path %S\n" path;*)
        let id' = File_id.identify path in
        if File_id.same id id' then
          (path, id, files)
        else
          (path, id',
           try Sys.readdir path
           with Sys_error _ -> [||])
      ) !path

  let () = refresh ()

  let eq_mod_cap str1 str2 =
    (*Printf.eprintf "eq_mod_cap %S %S\n%!" str1 str2;*)
    str1 = str2 ||
    let len = String.length str1 in
    len = String.length str2 &&
    (try
       for i = 1 to len - 1 do
         if str1.[i] <> str2.[i] then raise Exit
       done;
       true
     with Exit -> false) &&
    Char.lowercase_ascii str1.[0] = Char.lowercase_ascii str2.[0]

  let find name =
    (*Printf.eprintf "find: %S\n%!" name;*)
    match Hashtbl.find cache name with
    | found -> found
    | exception Not_found ->
      let exception Found of string in
      let result =
        List.find_map (fun (path, _, files) ->
            (*Printf.eprintf "find: is %S in %S\n%!" name path;*)
            try
              Array.iter (fun name' ->
                  if eq_mod_cap name name' then raise (Found name')
                ) files;
              None
            with Found name -> Some (Filename.concat path name)
          ) !path
      in
      Hashtbl.add cache name result;
      result
end

module Hotfinder : sig
  type module_source = {
    name: string;
    interface: string option;
    interface_id: File_id.t;
    implementation: string option;
    implementation_id: File_id.t;
  }

  val find : string -> module_source
end = struct
  type module_source = {
    name: string;
    interface: string option;
    interface_id: File_id.t;
    implementation: string option;
    implementation_id: File_id.t;
  }

  let find name =
    if name = "" then invalid_arg "Finder.find: empty name";
    let raw_module_name, raw_file_name =
      if Filename.extension name = "" then
        name, name ^ ".ml"
      else
        Filename.chop_extension name, name
    in
    let name =
      String.capitalize_ascii (Filename.basename raw_module_name)
    in
    let raw_file_path =
      if Sys.file_exists raw_file_name then
        Some raw_file_name
      else
        match Hotpath.find raw_file_name with
        | found -> found
        | exception Not_found -> None
    in
    let implementation, interface =
      match raw_file_path with
      | None -> None, None
      | Some path ->
        let len = String.length path in
        if path.[len - 1] = 'i' then
          let ml_file = String.sub path 0 len in
          let ml_file =
            if Sys.file_exists ml_file
            then Some ml_file
            else None
          in
          ml_file, Some path
        else
          let mli_file = path ^ "i" in
          let mli_file =
            if Sys.file_exists mli_file
            then Some mli_file
            else None
          in
          Some path, mli_file
    in
    let identify = function
      | None -> File_id.missing
      | Some name -> File_id.identify name
    in
    let implementation_id = identify implementation in
    let interface_id = identify interface in
    { name;
      implementation; implementation_id;
      interface; interface_id;
    }
end

module Hotparser : sig

  type implementation = (Parsetree.structure option, exn) result
  val parse_implementation : string option -> implementation

  type interface = (Parsetree.signature option, exn) result
  val parse_interface : string option -> interface

  val compose : string -> implementation -> interface -> implementation

end = struct

  type implementation = (Parsetree.structure option, exn) result

  let parse_implementation = function
    | None -> Ok None
    | Some path ->
      match Pparse.parse_implementation ~tool_name:"hotcaml" path with
      | exception exn -> Error exn
      | tree -> Ok (Some tree)

  type interface = (Parsetree.signature option, exn) result

  let parse_interface = function
    | None -> Ok None
    | Some path ->
      match Pparse.parse_interface ~tool_name:"hotcaml" path with
      | exception exn -> Error exn
      | tree -> Ok (Some tree)

  let compose name implementation interface =
    Result.bind implementation @@ fun implementation ->
    Result.bind interface @@ fun interface ->
    Result.ok @@
    let open Ast_helper in
    let mod_name = Location.mknoloc (Some name) in
    let mod_expr = Option.map Mod.structure implementation in
    let mod_type = Option.map Mty.signature interface in
    match mod_expr, mod_type with
    | None, None -> None
    | Some impl, None ->
      Some [Str.module_ (Mb.mk mod_name impl)]
    | None, Some intf ->
      let lid = Location.mknoloc (Longident.Lident name) in
      let mod_expr = Mod.constraint_ (Mod.ident lid) intf in
      Some [Str.rec_module [Mb.mk mod_name mod_expr]]
    | Some impl, Some intf ->
      Some [Str.module_ (Mb.mk mod_name (Mod.constraint_ impl intf))]

end

module Hotdepend : sig
  val dependencies : Parsetree.structure -> Depend.String.Set.t
end = struct
  let dependencies str =
    Depend.free_structure_names := Depend.String.Set.empty;
    Depend.add_implementation Depend.String.Map.empty str;
    !Depend.free_structure_names
end

module Hotresolver : sig
  type program_unit = {
    source: Hotfinder.module_source;
    implementation: Hotparser.implementation;
    interface: Hotparser.interface;
    composed: Hotparser.implementation;
    depends: Depend.String.Set.t;
  }

  type program = program_unit Depend.String.Map.t

  val resolve : ?previous:program -> string list -> program
end = struct

  type program_unit = {
    source: Hotfinder.module_source;
    implementation: Hotparser.implementation;
    interface: Hotparser.interface;
    composed: Hotparser.implementation;
    depends: Depend.String.Set.t;
  }

  type program = program_unit Depend.String.Map.t

  let update_unit previous name =
    let previous = Depend.String.Map.find_opt name previous in
    let source = Hotfinder.find name in
    let implementation, impl_reused =
      match previous with
      | Some previous when
          File_id.same
            previous.source.implementation_id
            source.implementation_id
        ->
        previous.implementation, true
      | Some _ | None ->
        Hotparser.parse_implementation source.implementation, false
    in
    let interface, intf_reused =
      match previous with
      | Some previous when
          File_id.same
            previous.source.interface_id
            source.interface_id
        ->
        previous.interface, true
      | Some _ | None ->
        Hotparser.parse_interface source.interface, false
    in
    let composed, comp_reused = match previous with
      | Some previous when
          source.name = previous.source.name && impl_reused && intf_reused ->
        previous.composed, true
      | Some _ | None ->
        Hotparser.compose source.name implementation interface, false
    in
    let depends =
      match previous with
      | Some previous when comp_reused -> previous.depends
      | Some _ | None ->
        match composed with
        | Result.Ok (Some ast) -> Hotdepend.dependencies ast
        | Result.Ok None | Result.Error _ -> Depend.String.Set.empty
    in
    { source; implementation; interface; composed; depends }

  let empty = Depend.String.Map.empty

  let resolve ?(previous=empty) entrypoints =
    let rec load mod_name program =
      if Depend.String.Map.mem mod_name program then program else
        let p_unit = update_unit previous mod_name in
        let program = Depend.String.Map.add mod_name p_unit program in
        Depend.String.Set.fold load p_unit.depends program
    in
    List.fold_right load entrypoints empty
end

module Hotorder : sig
  val order : Hotresolver.program ->
    string list -> (string list, [`Cycle of string * string list]) Result.t
end = struct

  type ordered_set = string list * Depend.String.Set.t
  let empty : ordered_set = ([], Depend.String.Set.empty)
  let mem s (_, set : ordered_set ) = Depend.String.Set.mem s set
  let add s set : ordered_set = if mem s set then set else
      let (l, ss) = set in (s :: l, Depend.String.Set.add s ss)

  exception Cycle of string * string list

  let order program entrypoints =
    let rec visit stack name order =
      if mem name order then
        order
      else if mem name stack then
        raise (Cycle (name, fst stack))
      else
        let stack = add name stack in
        let p_unit = Depend.String.Map.find name program in
        let order =
          Depend.String.Set.fold (visit stack) p_unit.Hotresolver.depends order
        in
        add name order
    in
    try
      let order, _index =
        List.fold_left (fun order name -> visit empty name order)
          empty entrypoints
      in
      Ok (List.rev order)
    with Cycle (node, stack) ->
      Error (`Cycle (node, stack))

end

module Hottyper : sig
  type program_unit =
    | Present of {
        depends: program_unit Depend.String.Map.t;
        parsetree: Parsetree.structure;
        typedtree_env: Env.t;
        typedtree: Typedtree.structure;
        signature: Types.signature;
      }
    | Absent

  type program = (program_unit, exn) result Depend.String.Map.t

  val typecheck :
    ?previous:program -> Hotresolver.program -> string list -> program
end = struct

  type program_unit =
    | Present of {
        depends: program_unit Depend.String.Map.t;
        parsetree: Parsetree.structure;
        typedtree_env: Env.t;
        typedtree: Typedtree.structure;
        signature: Types.signature;
      }
    | Absent

  type program = (program_unit, exn) result Depend.String.Map.t

  let empty = Depend.String.Map.empty

  let initial_env = Compmisc.initial_env ()

  let just_typecheck env pstr =
    Typecore.reset_delayed_checks ();
    let (str, sg, sn, newenv) = Typemod.type_toplevel_phrase env pstr in
    let sg' = Typemod.Signature_names.simplify newenv sn sg in
    ignore (Includemod.signatures ~mark:Mark_positive env sg sg');
    Typecore.force_delayed_checks ();
    (env, str, sg)

  let typecheck_with_depends depends pstr =
    let env =
      Depend.String.Map.fold (fun _ dep env ->
          match dep with
          | Absent -> env
          | Present t-> Env.add_signature t.signature env
        ) depends initial_env
    in
    just_typecheck env pstr

  let typecheck_module previous source (program : program) mod_name =
    let s_unit = Depend.String.Map.find mod_name source in
    match s_unit.Hotresolver.composed with
    | Error _ -> None
    | Ok None -> Some (Ok Absent)
    | Ok (Some parsetree) ->
      match
        Depend.String.Set.fold (fun mod_name acc ->
            match Depend.String.Map.find mod_name program with
            | Ok t_unit -> Depend.String.Map.add mod_name t_unit acc
            | Error _ -> raise Not_found
          ) s_unit.Hotresolver.depends Depend.String.Map.empty
      with
      | exception Not_found -> None
      | depends ->
        Option.some @@
        match Depend.String.Map.find_opt mod_name previous with
        | Some (Ok (Present previous) as result) when
            previous.parsetree == parsetree &&
            Depend.String.Map.equal (==) previous.depends depends ->
          result
        | Some _ | None ->
          match typecheck_with_depends depends parsetree with
          | exception exn -> Error exn
          | (typedtree_env, typedtree, signature) ->
            Ok (Present { depends; parsetree;
                          typedtree_env; typedtree; signature;
                        })

  let typecheck ?(previous=empty) source modules =
    List.fold_left (fun program mod_name ->
        match typecheck_module previous source program mod_name with
        | None -> program
        | Some t_unit -> Depend.String.Map.add mod_name t_unit program
      ) empty modules

end

let () =
  let source_program = Hotresolver.resolve entrypoints in
  (*Depend.String.Map.iter (fun name p_unit ->
      Printf.printf "loaded %s\ndependencies:\n" name;
      Depend.String.Set.iter (Printf.printf "  %s\n") p_unit.Hotresolver.depends;
    ) source_program;
  Printf.printf "load order:\n";*)
  match Hotorder.order source_program entrypoints with
  | Error _ -> ()
  | Ok order ->
    (*List.iter (Printf.printf "  %s\n") (List.rev order);*)
    let types = Hottyper.typecheck source_program order in
    List.iter (fun name ->
        match Depend.String.Map.find_opt name types with
        | None -> ()
        | Some typ ->
          match typ with
          | Ok Hottyper.Absent -> () (*Printf.printf "%s is absent\n" name*)
          | Error exn ->
            Format.printf "%s failed: %a\n%!" name Location.report_exception exn
          | Ok (Hottyper.Present t) ->
            try
              (*Printf.printf "%s is present\n" name;*)
              let lam = Translmod.transl_toplevel_definition t.typedtree in
              let slam = Simplif.simplify_lambda lam in
              let init_code, fun_code = Bytegen.compile_phrase slam in
              let (code, reloc, events) =
                Emitcode.to_memory init_code fun_code
              in
              Symtable.patch_object code reloc;
              Symtable.check_global_initialized reloc;
              Symtable.update_global_table();
              (*let initial_bindings = !toplevel_value_bindings in*)
              let _bytecode, closure = Meta.reify_bytecode code [| events |] None in
              ignore (closure () : Obj.t)
            with exn ->
              Format.printf "%s compilation failed: %a\n%!" name Location.report_exception exn
      ) order
