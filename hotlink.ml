let is_hot_loaded () = false
let is_hot_unloaded () = false

let on_unload _f = ()
let on_unload_or_at_exit = at_exit
