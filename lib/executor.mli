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
}

val exec : config -> Executor_state.t -> Prog.t -> Executor_state.t status
