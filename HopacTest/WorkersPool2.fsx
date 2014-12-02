#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"
#r "packages/ExtCore.0.8.43/lib/net40/ExtCore.dll"

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type Pool<'msg, 'res>(degreeOfParallelism: int, source: Alt<'msg>, worker: 'msg -> Job<Choice<'res, 'msg * exn>>) =
    let degreeOfParallelism = MVar.Now.createFull degreeOfParallelism
    let dopChanged = ch<unit>()
    let workDone = ch<Choice<'res, 'msg * exn>>()
    let failedMessages = mb()
    let getMessage workerCount = 
        let get() = Alt.guard (job {
            let! dop = MVar.read degreeOfParallelism 
            return if workerCount < dop then source <|>? failedMessages else Alt.never() })

        Alt.choose [ get()
                     Ch.Alt.take dopChanged >>.? get() ]
    
    let pool = Job.iterateServer 0 <| fun workerCount ->
        Alt.select [ getMessage workerCount >>=? fun msg -> 
                         job { let! res = worker msg
                               return! Ch.give workDone res } |> Job.start >>% workerCount + 1
                     workDone >>=? fun res ->
                        match res with
                        | Choice1Of2 _ -> Job.result (workerCount - 1)
                        | Choice2Of2 (msg, _) -> Mailbox.send failedMessages msg >>% workerCount - 1
                   ]
    do start pool
    
    member __.SetDegreeOfParallelism value = 
        MVar.take degreeOfParallelism >>. MVar.fill degreeOfParallelism value >>. Ch.give dopChanged () |> start

module Test =
    open System

    let mb = mb<int>()
    
    let pool = Pool<int, unit>(3, mb, (fun msg -> job {
                   printfn "[worker] Received %A. Sleeping..." msg
                   do! Timer.Global.sleep (TimeSpan.FromMinutes 10.)
                   return Choice1Of2() }))

    start (Mailbox.send mb 1)


    //pool.SetCapacity 4
    //pool.Add 20

//    for i in 1..10000000 do pool.Add i


