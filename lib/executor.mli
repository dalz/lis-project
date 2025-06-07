type 'a status =
  | Ok of 'a
  | Err of Executor_state.t
  | Stuck of Executor_state.t

type config = {
  bind :
    Executor_state.t status ->
    (Executor_state.t -> Executor_state.t status list) ->
    Executor_state.t status list;
  on_step : Executor_state.t -> unit;
  alloc_rule : Executor_state.t -> Ide.t -> Executor_state.t status;
  choice_rule :
    Executor_state.t -> Prog.t -> Prog.t -> (Executor_state.t * Prog.t) list;
  iter_rule :
    (Executor_state.t -> Prog.t -> Executor_state.t status list) ->
    (Executor_state.t status list ->
    (Executor_state.t -> Executor_state.t status list) ->
    Executor_state.t status list) ->
    Executor_state.t ->
    Prog.t ->
    Executor_state.t status list;
}

val exec : config -> Executor_state.t -> Prog.t -> Executor_state.t status list
