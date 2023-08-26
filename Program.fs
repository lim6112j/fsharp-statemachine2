namespace DoorMachine

open DoorMachine.StateMachine

module Program =
    let state = UnlockedOpened
    let result = stateMachine Close state
    let result2 = result.AllowedEvents[0].RaiseEvent()
    printfn $"{result2}"
