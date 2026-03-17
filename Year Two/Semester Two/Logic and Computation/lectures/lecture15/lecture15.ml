module type StateMachine = sig
  type state
  type input
  val init : state
  val step : state -> input -> state
end
(* 
INIT := MACHINE(0.00, 10)
  MACHINE(money, items) :=
    insert25c.MACHINE(money + 0.25, items)
    + insert50c.MACHINE(money + 0.50, items)
    + vend.MACHINE(money - 1.00, items - 1) if money >= 1.00 and items >= 1
*)

module MoneyItemVendingMachine (* StateMachine *) = struct
  type state = { money: float; items: int }
  type input = Insert25c | Insert50c | Vend

  let init = { money= 0.0; items= 10}
  
  let step state input = 
    match input with
    | Insert25c -> { state with money = state.money +. 0.25 }
    | Insert50c -> { state with money = state.money +. 0.50 }
    | Vend when state.money >= 1.00 && state.items >= 1 -> 
      { money = state.money -. 1.00; items = state.items - 1}
    | Vend when state.money < 1.00 -> failwith "not enough money"
    | Vend when state.items < 1 -> failwith "out of stock"
    | Vend -> failwith "invalid state"
end

module MachineTrace (M : StateMachine) = struct
  type trace = (M.state * M.input * M.state) list

  
end