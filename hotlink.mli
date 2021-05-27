val is_hot_loaded : unit -> bool
val is_hot_unloaded : unit -> bool

val on_unload : (unit -> unit) -> unit
val on_unload_or_at_exit : (unit -> unit) -> unit
