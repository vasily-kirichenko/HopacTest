#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let uidServer = Job.delay <| fun _ ->
    let channel = ch()
    let rec loop value =
        Ch.give channel value >>. loop (value + 1)
    Job.start (loop 0) >>% channel

let server = run uidServer 
run (Ch.take server)