namespace DoorMachine

module StateMachine =
    type State =
        | LockedClosed
        | LockedOpened
        | UnlockedOpened
        | UnlockedClosed

    type Event =
        | Open
        | Close
        | Lock
        | Unlock


    type AllowedEvent =
        { EventInfo: Event
          RaiseEvent: unit -> EventResult }

    and EventResult =
        { CurrentState: State
          AllowedEvents: AllowedEvent array }

    let stateTransition state event =
        match (state, event) with
        | (LockedClosed, Unlock) -> UnlockedClosed
        | (LockedClosed, _) -> LockedClosed
        | (LockedOpened, Close) -> LockedClosed
        | (LockedOpened, Unlock) -> UnlockedOpened
        | (LockedOpened, _) -> LockedOpened
        | (UnlockedOpened, Close) -> UnlockedClosed
        | (UnlockedOpened, Lock) -> LockedOpened
        | (UnlockedOpened, _) as s -> fst s
        | (UnlockedClosed, Lock) as s -> LockedClosed
        | (UnlockedClosed, Open) as s -> UnlockedOpened
        | (UnlockedClosed, _) as s -> fst s

    let getEventForState state =
        match state with
        | LockedClosed -> [| Unlock |]
        | LockedOpened -> [| Close; Unlock |]
        | UnlockedOpened -> [| Close; Lock |]
        | UnlockedClosed -> [| Lock; Open |]

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
