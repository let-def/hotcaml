let status () =
  Printf.eprintf "  is_hot_loaded: %b\n\
                 \  is_hot_unloaded: %b\n"
    (Hotlink.is_hot_loaded ())
    (Hotlink.is_hot_unloaded ())

let () =
  Printf.eprintf "Just loaded Test_unload\n";
  status ()

let () =
  Hotlink.on_unload (fun () ->
      Printf.eprintf "Just unloaded Test_unload\n";
      status ()
    )

