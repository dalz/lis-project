val exec :
  ?denoise:bool ->
  on_step:(Executor_state.t -> Cmd.t -> unit) ->
  Executor_state.t ->
  Prog.t ->
  Prop.t Executor.status list
