val exec :
  on_step:(Executor_state.t -> Cmd.t -> unit) ->
  interactive:bool ->
  Executor_state.t ->
  Prog.t ->
  Prop.t Executor.status list
