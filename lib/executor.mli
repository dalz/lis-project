type 'a status =
  | Ok of 'a
  | Err of Executor_state.t
  | Stuck of Executor_state.t
  | Unreachable

type config = {
  bind :
    Executor_state.t status ->
    (Executor_state.t -> Executor_state.t status) ->
    Executor_state.t status;
  on_step : Executor_state.t -> unit;
  alloc_rule : Executor_state.t -> Ide.t -> Executor_state.t status;
}

val exec : config -> Executor_state.t -> Prog.t -> Executor_state.t status
