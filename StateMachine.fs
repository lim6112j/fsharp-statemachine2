namespace DoorMachine

module StateMachine =
    type State =
        | Start
        | State1
        | State2
        | State3
        | End

    type Event =
        | Started
        | Succeeded
        | Aborted
        | DeepResume
        | Pause
        | Failed
        | Save
        | Stop


    type AllowedEvent =
        { EventInfo: Event
          RaiseEvent: unit -> EventResult }

    and EventResult =
        { CurrentState: State
          AllowedEvents: AllowedEvent array }

    let private stateTransition event state =
        match event, state with
        | Started, Start -> State1
        | Succeeded, State1 -> State2
        | Aborted, State1 -> End
        | Succeeded, State2 -> State3
        | DeepResume, State2 -> State3
        | Aborted, State2 -> End
        | Save, State3 -> End
        | Failed, State3 -> State3
        | Aborted, State3 -> End
        | Pause, State3 -> State2
        | _, _ -> failwith ($"no mathcing event for event : {event}, state: {state}")

    let private getEventForState state =
        match state with
        | Start -> [| Started |]
        | State1 -> [| Succeeded; Aborted |]
        | State2 -> [| Succeeded; Aborted; DeepResume |]
        | State3 -> [| Save; Aborted; Failed |]
        | End -> [||]

    let getAllowedEvents state = state.AllowedEvents

    let rec stateMachine event state =
        let newState = stateTransition event state
        let newEvents = getEventForState newState
        printfn "current event : %A, state: %s" newEvents (newState.ToString())

        { CurrentState = newState
          AllowedEvents =
            newEvents
            |> Array.map (fun e ->
                let f () = stateMachine e newState

                { EventInfo = e
                  RaiseEvent = f

                }) }
