#load "Header.fsx"

open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let inc = Event<unit>()
let dec = Event<unit>()

module Alt =
    let ofEvent (e: IEvent<'a>) =
        let res = ivar()
        job { 
            let! eres = Async.AwaitEvent e 
            do! IVar.fill res eres
        } |> Job.start >>.
        Alt.delay <| fun _ -> res 
         
let guiWorkflow = job {
    let rec counter n =
        Alt.choose [ Alt.ofEvent inc >>=? counter (n + 1)
                     Alt.ofEvent dec >>=? counter (n - 1) ]
    return! counter 0 }