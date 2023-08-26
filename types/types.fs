namespace DomainOriented

module Domain =
    type Turnstile =
        | Unlocked of string
        | Locked of string

    type Action =
        | Coin of string
        | Push of string
