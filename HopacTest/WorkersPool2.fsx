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
            return if workerCount < dop then source <|>? failedMessages else Alt.never() })

        get() <|>? (dopChanged >>.? get())
    
    let pool = Job.iterateServer 0 <| fun workerCount ->
        Alt.choose [ getMessage workerCount >>=? fun msg -> 
                         worker msg >>= Ch.give workDone |> Job.start >>% workerCount + 1
                     workDone >>=? function
                        | Choice1Of2 _ -> Job.result (workerCount - 1)
                        | Choice2Of2 (msg, _) -> failedMessages <<-+ msg >>% workerCount - 1
                   ]
    do start pool
    
    member __.SetDegreeOfParallelism value = 
        degreeOfParallelism >>. MVar.fill degreeOfParallelism value >>. (dopChanged <-- ()) |> start

module Test =
    open System

    let mb = mb<int>()
    
    let pool = Pool<int, unit, exn>(3, mb, (fun msg -> job {
                   printfn "[worker] Received %A. Sleeping..." msg
                   do! Timer.Global.timeOut (TimeSpan.FromMinutes 10.)
                   return Choice1Of2() }))

    job {
        for i in 1..3 do 
            do! mb <<-+ i
    } |> run
    

    pool.SetDegreeOfParallelism 100
    //pool.Add 20

//    for i in 1..10000000 do pool.Add i



