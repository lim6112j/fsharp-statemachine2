﻿namespace DoorMachine

open DoorMachine.StateMachine

module Program =
    let state = Unlocked
    let result = stateMachine Coin
    let result2 = result.AllowedEvents.[0].RaiseEvent()
    printfn $"{result2}"
