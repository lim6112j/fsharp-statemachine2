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

    let private stateTransition event =
        match event with
        | Coin -> Unlocked
        | Push -> Locked

    let private getEventForState state =
        match state with
        | Locked -> [| Coin; Push |]
        | Unlocked -> [| Coin; Push |]

    let rec stateMachine event =
        let newState = stateTransition event
        let newEvents = getEventForState newState

        { CurrentState = newState
          AllowedEvents =
            newEvents
            |> Array.map (fun e ->
                let f () = stateMachine e

                { EventInfo = e
                  RaiseEvent = f

                }) }
