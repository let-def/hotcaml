# Hotcaml: an OCaml interpreter with watching and reloading

Hotcaml is an OCaml interpreter that starts from a source file and loads its dependencies.
When one of the source files changes and passes the typechecker, it is reloaded, along with all of its reverse dependencies.

To get started, clone the repository and type `make`. Two frontends are built: `hotcaml.exe` and `hotcaml_lwt.exe`.

## Starting hotcaml

An `hotcaml` invocation takes three kinds of arguments:
`hotcaml [ -package pkg ]* [ -I path ]* entrypoint.ml*`  

The `pkg`s should be valid `findlib` package names. They will be loaded in order during startup.

The `path`s are paths that will be looked up for dependencies.

Finally, the entrypoints are the actual code that we want to interpret.

Each entrypoint is loaded and interpreted in order. Dependencies of an entrypoint are looked up in the `path`s and then in the loaded packages.

Once execution of the entrypoints is done, the interpreter will watch the disk for changes. If one of the source files changes, it is reloaded and interpretation resumes from this module, followed by all of its reverse dependencies.

If one of the dependencies does not typecheck, reloading is postponed until all errors are solved.

## Synchronous and asynchronous frontends

Contrary to the normal execution of an OCaml program, modules can be loaded and unloaded, multiple times during the execution.

The synchronous `hotcaml` only looks for changes after execution finishes. This is not really convenient for interactive programs, where we might want to reload during execution rather than after.

`hotcaml_lwt` provides an asynchronous frontend: lwt threads continue to execute after loading, and modules can be reloaded concurrentlly.

## Observing reload process

The `Hotlink` module can be used to customize behavior of hot-loaded programs.

`Hotlink.is_hot_loaded () : bool` is true only when called from a module that has been hot-loaded.

`Hotlink.is_hot_unloaded () : bool` is true only when called from a module that was hot-loaded and has now been unloaded.

`Hotlink.on_unload : (unit -> unit) -> unit` allows to register a callback that will be invoked when an hot-loaded module is unloaded.

`Hotlink.on_unload_or_at_exit : (unit -> unit) -> unit` calls the callback either during unloading or when exiting the program.

### Cold execution

In a normal, "cold" execution, the `Hotlink` module exhibits a compatible, reasonable, behavior:

- `Hotlink.is_hot_loaded` and `Hotlink.is_hot_unloaded`  are always `false`
- `Hotlink.on_unload` does nothing
- `Hotlink.on_unload_or_at_exit` behaves like `at_exit`

This allows modules that are compatible with both classical OCaml and with hotcaml, and adjusts their behavior based on the situation.
