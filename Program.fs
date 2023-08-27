namespace DoorMachine

open DoorMachine.StateMachine

module Program =
    let allowedEventsForCurrentState = stateMachine Started Start |> getAllowedEvents
    let newEvent = Save

    let f x =
        fun (n: AllowedEvent) -> x = n.EventInfo

    let result = allowedEventsForCurrentState |> Array.filter (f newEvent)
    printfn $"{allowedEventsForCurrentState}\n"
    printfn $"{result.Length}"
