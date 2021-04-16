namespace Extensions

open Elmish
open System

module Cmd =
    let fromAsync (operation: Async<'msg>) : Cmd<'msg> =
        let delayedCmd (dispatch: 'msg -> unit) : unit =
            let delayedDispatch = async {
                let! msg = operation
                dispatch msg
            }

            Async.StartImmediate delayedDispatch

        Cmd.ofSub delayedCmd

module Async =
    let map (fnc: 'a -> 'b) (asyncInput: Async<'a>) : Async<'b> = async {
        let! u = asyncInput
        let output = fnc u
        return output  
    }

module DateTime =
    let getFromUnixSeconds (unixtime: int) =
        DateTimeOffset.FromUnixTimeSeconds(int64 unixtime).ToLocalTime()