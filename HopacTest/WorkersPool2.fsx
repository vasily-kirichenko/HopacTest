#load "Header.fsx"

open Hopac 
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open Hopac.Infixes

type Failure<'a> =
    | Recoverable of 'a 
    | Fatal of 'a

type Pool<'msg, 'res, 'error>(degree: int, source: Alt<'msg>, worker: 'msg -> Job<Choice<'res, 'msg * Failure<'error>>>) =
    let setDegree = ch<int>() 
    let workDone = ch<Choice<'res, 'msg * Failure<'error>>>()
    let failedMessages = mb()
     
    let pool = Job.iterateServer (degree, 0)  <| fun (degree, usage) ->
        (setDegree |>>? fun degree -> degree, usage) <|>? 
        (workDone |>>? fun _ -> degree, usage - 1) <|>?
        (if usage < degree then
            source <~>? failedMessages 
            >>=? fun msg -> 
            job {
                let! result = worker msg 
                match result with
                | Choice2Of2 (msg, Recoverable _) -> do! failedMessages <<-+ msg
                | Choice2Of2 (_, Fatal _)
                | Choice1Of2 _ -> () 
                do! workDone <-- result }
            |> Job.queue
            >>% (degree, usage + 1)
         else Alt.never()) 
    do start pool
    member __.SetDegree value = setDegree <-+ value |> run

module Test =
    open System
    open Hopac.Extensions

    let mb = mb<int>()
    
    let pool = Pool<int, int, string>(2, mb, (fun msg -> job {
                   printfn "[worker] Received %A. Sleeping..." msg
                   do! Timer.Global.timeOut (TimeSpan.FromSeconds 1.)
                   return 
                       if msg % 10 = 0 
                       then Choice2Of2 (msg, Fatal (sprintf "Fatal for %d" msg))
                       elif msg % 3 = 0 
                       then Choice2Of2 (msg, Recoverable (sprintf "Recoverable for %d" msg))
                       else Choice1Of2 msg }))

    [1..10] |> Seq.Con.iterJob (Mailbox.send mb) |> run
    

    pool.SetDegree 5
    //pool.Add 20

//    for i in 1..10000000 do pool.Add i



