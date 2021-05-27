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

module Hotlink = struct
  module type S = sig
    val is_hot_loaded : unit -> bool
    val is_hot_unloaded : unit -> bool

    val on_unload : (unit -> unit) -> unit
    val on_unload_or_at_exit : (unit -> unit) -> unit
  end

  let interface = "
    val is_hot_loaded : unit -> bool
    val is_hot_unloaded : unit -> bool

    val on_unload : (unit -> unit) -> unit
    val on_unload_or_at_exit : (unit -> unit) -> unit
  "

  type status = {
    mutable unloaded: bool;
    mutable on_unload : (unit -> unit) list;
  }

  let make () : status * (module S) =
    let status = {
      unloaded = false;
      on_unload = [];
    } in
    (status, (module struct
       let is_hot_loaded () = true
       let is_hot_unloaded () = status.unloaded
       let on_unload f =
         if status.unloaded then
           f ()
         else
           status.on_unload <- f :: status.on_unload
       let on_unload_or_at_exit = on_unload
     end))

  let unload name s =
    let fs = s.on_unload in
    s.unloaded <- true;
    s.on_unload <- [];
    List.iter (fun f ->
        try f ()
        with exn ->
          Printf.eprintf "Exception while unloading %s:\n%s\n"
            name (Printexc.to_string exn)
      ) fs
end
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
  Topfind.don't_load_deeply ["lwt"; "lwt.unix"; "compiler-libs.toplevel"];
  ()

let () =
  (*let all = ref [] in
  let _ = Symtable.filter_global_map (fun ident ->
      all := ident :: !all;
      true
    ) (Symtable.current_state ())
  in*)
  (*List.iter prerr_endline !Topfind.predicates;*)
  begin try
      Topfind.load_deeply (List.rev !packages);
    with exn ->
      Format.printf "%a\n%!" Location.report_exception exn
  end;
  (*let added = ref [] in
  let _ =
    Symtable.filter_global_map (fun ident ->
        if not (List.mem ident !all) then
          added := ident :: !added;
        true
      ) (Symtable.current_state ())
  in
  List.iter (fun ident -> Format.printf "added %a\n" Ident.print ident)
    !added*)

(* ** *)

module File_id : sig
  type t
  val missing : t
  val identify : string -> t
  val identify_option : string option -> t
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

  let identify_option = function
    | None -> missing
    | Some name -> identify name
end

module Hotpath : sig
  val find : string -> string option
  val changed : unit -> bool
end = struct
  let path =
    ref (List.map (fun path -> path, File_id.missing, [||]) !load_path)

  let cache = Hashtbl.create 7

  let changed () =
    let changed = ref false in
    path := List.map (fun (path, id, files) ->
        (*Printf.eprintf "refresh path %S\n" path;*)
        let id' = File_id.identify path in
        if File_id.same id id' then
          (path, id, files)
        else (
          changed := true;
          let files = try Sys.readdir path with Sys_error _ -> [||] in
          (path, id', files)
        )
      ) !path;
    if !changed then Hashtbl.clear cache;
    !changed

  let _ : bool = changed ()

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
    {
      name; implementation; interface;
      implementation_id = File_id.identify_option implementation;
      interface_id = File_id.identify_option interface;
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

module String = Depend.String

module Hotdepend : sig
  val dependencies : Parsetree.structure -> String.Set.t
end = struct
  let bound = String.Map.singleton "Hotlink"
      (Depend.Node (String.Set.empty, String.Map.empty))
  let dependencies str =
    Depend.free_structure_names := String.Set.empty;
    Depend.add_implementation bound str;
    !Depend.free_structure_names
end

module Hotresolver : sig
  type program_unit = {
    source: Hotfinder.module_source;
    implementation: Hotparser.implementation;
    interface: Hotparser.interface;
    composed: Hotparser.implementation;
    depends: String.Set.t;
  }

  type program = program_unit String.Map.t
  val empty : program

  val resolve : ?previous:program -> string list -> program
  val changed : program -> bool
end = struct

  type program_unit = {
    source: Hotfinder.module_source;
    implementation: Hotparser.implementation;
    interface: Hotparser.interface;
    composed: Hotparser.implementation;
    depends: String.Set.t;
  }

  type program = program_unit String.Map.t

  let update_unit previous name =
    let previous = String.Map.find_opt name previous in
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
        | Result.Ok None | Result.Error _ -> String.Set.empty
    in
    { source; implementation; interface; composed; depends }

  let empty = String.Map.empty

  let resolve ?(previous=empty) entrypoints =
    let rec load mod_name program =
      if String.Map.mem mod_name program then program else
        let p_unit = update_unit previous mod_name in
        let program = String.Map.add mod_name p_unit program in
        String.Set.fold load p_unit.depends program
    in
    List.fold_right load entrypoints empty

  let changed (program: program) = not (
      String.Map.for_all (fun _ {source; _} ->
          File_id.same source.implementation_id
            (File_id.identify_option source.implementation) &&
          File_id.same source.interface_id
            (File_id.identify_option source.interface)
        ) program
    )
end

module Hotorder : sig
  type t = string list
  val order : Hotresolver.program -> string list -> (t, string) Result.t
end = struct
  type t = string list

  type ordered_set = string list * String.Set.t
  let empty : ordered_set = ([], String.Set.empty)
  let mem s (_, set : ordered_set ) = String.Set.mem s set
  let add s set : ordered_set = if mem s set then set else
      let (l, ss) = set in (s :: l, String.Set.add s ss)

  exception Cycle of string * string list

  let order program entrypoints =
    let rec visit stack name order =
      if mem name order then
        order
      else if mem name stack then
        raise (Cycle (name, fst stack))
      else
        let stack = add name stack in
        let p_unit = String.Map.find name program in
        let order =
          String.Set.fold (visit stack) p_unit.Hotresolver.depends order
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
      let rec extract = function
        | [] -> assert false
        | node' :: xs ->
          if node = node'
          then [node]
          else node' :: extract xs
      in
      Error (
        Printf.sprintf "Cyclic dependency found: %s"
          (String.concat " -> " (node :: extract stack))
      )

end

module Hottyper : sig
  type program_unit =
    | Present of {
        depends: program_unit String.Map.t;
        parsetree: Parsetree.structure;
        typedtree_env: Env.t;
        typedtree: Typedtree.structure;
        signature: Types.signature;
      }
    | Absent

  type program = (program_unit, exn) result String.Map.t
  val empty : program

  val typecheck :
    ?previous:program -> Hotresolver.program -> string list -> program
end = struct

  type program_unit =
    | Present of {
        depends: program_unit String.Map.t;
        parsetree: Parsetree.structure;
        typedtree_env: Env.t;
        typedtree: Typedtree.structure;
        signature: Types.signature;
      }
    | Absent

  type program = (program_unit, exn) result String.Map.t

  let empty = String.Map.empty

  let initial_env = Compmisc.initial_env ()

  let hotlink =
    let lexbuf = Lexing.from_string Hotlink.interface in
    let sg = Parser.interface Lexer.token lexbuf in
    (Typemod.transl_signature initial_env sg).sig_type

  let hotlink_ident = Ident.create_local "Hotlink"

  let add_hotlink env =
    Env.add_module hotlink_ident Mp_present (Types.Mty_signature hotlink) env

  let just_typecheck env pstr =
    Typecore.reset_delayed_checks ();
    let (str, sg, sn, newenv) = Typemod.type_toplevel_phrase env pstr in
    let sg' = Typemod.Signature_names.simplify newenv sn sg in
    ignore (Includemod.signatures ~mark:Mark_positive env sg sg');
    Typecore.force_delayed_checks ();
    (env, str, sg)

  let typecheck_with_depends depends pstr =
    let env = add_hotlink initial_env in
    let env =
      String.Map.fold (fun _ dep env ->
          match dep with
          | Absent -> env
          | Present t-> Env.add_signature t.signature env
        ) depends env
    in
    just_typecheck env pstr

  let typecheck_module previous source (program : program) mod_name =
    let s_unit = String.Map.find mod_name source in
    match s_unit.Hotresolver.composed with
    | Error _ -> None
    | Ok None -> Some (Ok Absent)
    | Ok (Some parsetree) ->
      match
        String.Set.fold (fun mod_name acc ->
            match String.Map.find mod_name program with
            | Ok t_unit -> String.Map.add mod_name t_unit acc
            | Error _ -> raise Not_found
          ) s_unit.Hotresolver.depends String.Map.empty
      with
      | exception Not_found -> None
      | depends ->
        Option.some @@
        match String.Map.find_opt mod_name previous with
        | Some (Ok (Present previous) as result) when
            previous.parsetree == parsetree &&
            String.Map.equal (==) previous.depends depends ->
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
        | Some t_unit -> String.Map.add mod_name t_unit program
      ) empty modules

end

module Hoterrors : sig
  val validate_parse : Hotresolver.program -> Hotorder.t -> (unit, string) result
  val validate_types : Hottyper.program -> Hotorder.t -> (unit, string) result
end = struct
  let validate_parse program order =
    let first_error mod_name =
      let {Hotresolver. implementation; interface; _} =
        String.Map.find mod_name program
      in
      match implementation, interface with
      | (Result.Error exn, _) | (_, Result.Error exn) ->
        Some (Format.asprintf "%a" Location.report_exception exn)
      | _ -> None
    in
    match List.find_map first_error order with
    | None -> Ok ()
    | Some msg -> Error msg

  let validate_types (program: Hottyper.program) order =
    let first_error mod_name =
      match String.Map.find mod_name program with
      | Result.Ok _ -> None
      | Result.Error exn ->
        Some (Format.asprintf "%a" Location.report_exception exn)
    in
    match List.find_map first_error order with
    | None -> Ok ()
    | Some msg -> Error msg
end

module Hotloader : sig
  type program
  val empty : program
  val load : ?previous:program -> Hottyper.program -> Hotorder.t ->
    program * (unit, string) result
end = struct
  type link_map = (Hottyper.program_unit * Hotlink.status option) String.Map.t
  type program = (string * (Hottyper.program_unit * Hotlink.status option)) list
  let empty = []

  let unload_previous (program : Hottyper.program) (previous : program)
    : link_map
    =
    let still_loaded =
      List.fold_left (fun acc (name, (prev, link)) ->
          let need_unload =
            match String.Map.find_opt name program with
            | Some p_unit -> Result.get_ok p_unit != prev
            | None -> true
          in
          if need_unload then (
            begin match link with
              | None -> ()
              | Some link ->
                prerr_endline ("Unloading " ^ name);
                Hotlink.unload name link;
            end;
            acc
          ) else
            String.Map.add name (prev, link) acc
        ) String.Map.empty previous
    in
    still_loaded

  exception Failed of link_map * string

  let load_new loaded program order =
    let load_unit loaded name =
      if String.Map.mem name loaded then
        loaded
      else
        let p_unit = Result.get_ok (String.Map.find name program) in
        match p_unit with
        | Hottyper.Absent -> String.Map.add name (p_unit, None) loaded
        | Hottyper.Present t ->
          let status, link = Hotlink.make () in
          try
            let lam = Translmod.transl_toplevel_definition t.typedtree in
            let slam = Simplif.simplify_lambda lam in
            let init_code, fun_code = Bytegen.compile_phrase slam in
            Toploop.setvalue "Hotlink" (Obj.repr link);
            let (code, reloc, events) =
              Emitcode.to_memory init_code fun_code
            in
            Symtable.patch_object code reloc;
            Symtable.check_global_initialized reloc;
            Symtable.update_global_table();
            (*let initial_bindings = !toplevel_value_bindings in*)
            let _bytecode, closure = Meta.reify_bytecode code [| events |] None in
            ignore (closure () : Obj.t);
            prerr_endline ("Loaded " ^ name);
            String.Map.add name (p_unit, Some status) loaded
          with exn ->
            let message =
              Format.asprintf "%s compilation failed: %a\n%!"
                name Location.report_exception exn
            in
            Hotlink.unload name status;
            raise (Failed (loaded, message))
    in
    let get_program loaded order =
      List.map (fun name ->
          name,
          match String.Map.find_opt name loaded with
          | None -> Hottyper.Absent, None
          | Some (p_unit, link) -> p_unit, link
        ) order
    in
    let loaded, result =
      match List.fold_left load_unit loaded order with
      | loaded -> loaded, Ok ()
      | exception (Failed (loaded, msg)) -> loaded, Error msg
    in
    (get_program loaded order, result)

  let load ?(previous=(empty : program)) (program : Hottyper.program) order =
    let still_loaded = unload_previous program previous in
    load_new still_loaded program order

end


let quit = ref false

let () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> quit := true))

let () =
  Lwt.async_exception_hook :=
    (fun exn -> prerr_endline (Printexc.to_string exn))

let () =
  let source = ref Hotresolver.empty in
  let typed = ref Hottyper.empty in
  let loaded = ref Hotloader.empty in
  let process () =
    source := Hotresolver.resolve ~previous:!source entrypoints;
    Result.bind (Hotorder.order !source entrypoints) @@ fun order ->
    Result.bind (Hoterrors.validate_parse !source order) @@ fun () ->
    typed := Hottyper.typecheck ~previous:!typed !source order;
    Result.bind (Hoterrors.validate_types !typed order) @@ fun () ->
    let loaded', result = Hotloader.load ~previous:!loaded !typed order in
    loaded := loaded';
    result
  in
  let rec load () =
    begin match process () with
      | Ok () -> ()
      | Error msg -> prerr_endline msg
    end;
    wait_for_change ()

  and wait_for_change () =
    if !quit then
      Lwt.return_unit
    else if not (Hotpath.changed () || Hotresolver.changed !source) then
      Lwt.bind (Lwt_unix.sleep 0.05) wait_for_change
    else
      load ()
  in
  Lwt_main.run (load ())
