val exec :
  on_step:(Executor_state.t -> unit) ->
  Executor_state.t ->
  Prog.t ->
  Norm_prop.t Executor.status list
