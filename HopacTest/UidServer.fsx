#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let uidServer = Job.delay <| fun _ ->
    let channel = ch()
    Job.iterateServer 0 (fun value ->
        Ch.give channel value >>% value + 1) >>% 
    channel

let server = run uidServer

seq { 1..1000000 }
|> Seq.map (fun _ -> Ch.take server)
|> Job.conCollect
|> run
|> Seq.sumBy (fun x -> int64 x)
 
run (Job.conCollect [ Ch.take server; Ch.take server ])