namespace DoorMachine

module StateMachine =
    type State =
        | Locked
        | Unlocked

    type Event =
        | Coin
        | Push


    type AllowedEvent =
        { EventInfo: Event
          RaiseEvent: unit -> EventResult }

    and EventResult =
        { CurrentState: State
          AllowedEvents: AllowedEvent array }

    let stateTransition state event =
        match (state, event) with
        | (_, Coin) -> Unlocked
        | (_, Push) -> Locked

    let getEventForState state =
        match state with
        | Locked -> [| Coin; Push |]
        | Unlocked -> [| Coin; Push |]

    let rec stateMachine event state =
        let newState = stateTransition state event
        let newEvents = getEventForState newState

        { CurrentState = newState
          AllowedEvents =
            newEvents
            |> Array.map (fun e ->
                let f () = stateMachine e newState

                { EventInfo = e
                  RaiseEvent = f

                }) }
