#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"

open System
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

let simpleUidServer = Job.delay <| fun _ ->
    let channel = ch()
    Job.iterateServer 0 (fun value ->
        Ch.give channel value >>% value + 1) >>% 
    channel

let uidServerWithNack: Ch<int * Alt<unit> * Ch<int>> = 
    let reqCh = ch()
    server << Job.iterate 0 <| fun oldValue ->
        reqCh >>= fun (n, nack, replyCh) ->
            let newValue = oldValue + n
            (replyCh <-? newValue >>%? newValue) <|>
            (nack >>%? oldValue)
    reqCh 

let incrementBy n = Alt.withNack <| fun nack ->
    let replyCh = ch()
    uidServerWithNack <-+ (n, nack, replyCh) >>% upcast replyCh

seq { 1..1000000 }
|> Seq.map (fun _ -> (incrementBy 1) <|> (incrementBy 2))
|> Job.conCollect
|> run
|> Seq.sumBy (fun x -> int64 x)
 
//run (Job.conCollect [ Ch.take server; Ch.take server ])

//let server = run simpleUidServer
//
//seq { 1..1000000 }
//|> Seq.map (fun _ -> Ch.take server)
//|> Job.conCollect
//|> run
//|> Seq.sumBy (fun x -> int64 x)
// 
//run (Job.conCollect [ Ch.take server; Ch.take server ])