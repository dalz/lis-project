val exec :
  on_step:(Executor_state.t -> unit) ->
  Executor_state.t ->
  Prog.t ->
  Prop.t Executor.status list
