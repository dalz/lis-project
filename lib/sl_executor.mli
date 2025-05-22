val exec :
  on_step:(Executor_state.t -> unit) ->
  Executor_state.t ->
  Prog.t ->
  Executor_state.t Executor.status list
