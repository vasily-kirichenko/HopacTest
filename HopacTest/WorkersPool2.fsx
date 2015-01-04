#load "Header.fsx"

open Hopac 
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes

type Pool<'msg, 'res, 'error>(degreeOfParallelism: int, source: Alt<'msg>, worker: 'msg -> Job<Choice<'res, 'msg * 'error>>) =
    let degreeOfParallelism = MVar.Now.createFull degreeOfParallelism
    let dopChanged = ch<unit>() 
    let workDone = ch<Choice<'res, 'msg * 'error>>()
    let failedMessages = mb()
    
    let getMessage workerCount = 
        let get() = Alt.guard (job {
            let! dop = MVar.read degreeOfParallelism
            printfn "Dop read: %d. Worker count: %d" dop workerCount
            return 
                if workerCount < dop then
                    printfn "Getting from source..."
                    source <|>? failedMessages 
                else
                    printfn "Alt.never"
                    Alt.never() })
             
        get() <|>? (dopChanged >>=? fun _ -> printfn "Dop changed!"; get())
    
    let pool = Job.iterateServer 0 <| fun workerCount ->
        Alt.choose [ getMessage workerCount >>=? fun msg ->
                        Job.start (worker msg >>= Ch.send workDone) >>% workerCount + 1
                     workDone >>=? fun r ->
                        printfn "Work done: %A (worker count = %d)" r workerCount
                        match r with
                        | Choice1Of2 _ -> Job.result (workerCount - 1)
                        | Choice2Of2 (msg, _) -> failedMessages <<-+ msg >>% workerCount - 1 
                   ]
    do start pool
    
    member __.SetDegreeOfParallelism value =
        degreeOfParallelism >>. MVar.fill degreeOfParallelism value >>. (dopChanged <-+ ()) |> run

module Test =
    open System
    open Hopac.Extensions

    let mb = mb<int>()
    
    let pool = Pool<int, unit, exn>(3, mb, (fun msg -> job {
                   printfn "[worker] Received %A. Sleeping..." msg
                   do! Timer.Global.timeOut (TimeSpan.FromSeconds 1.)
                   return Choice1Of2() }))

    [1..1000] |> Seq.Con.iterJob (Mailbox.send mb) |> run
    

    pool.SetDegreeOfParallelism 4
    //pool.Add 20

//    for i in 1..10000000 do pool.Add i



