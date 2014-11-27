#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"
#r "packages/ExtCore.0.8.43/lib/net40/ExtCore.dll"

open System
open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
open System.Threading
open ExtCore

type Worker<'a> =
    { Name: string
      Mailbox: Mailbox<'a>
      Stop: IVar<unit> }

module Worker =
    let create w = Job.delay <| fun _ ->
        let rec server() =
            Alt.select [ IVar.Alt.read w.Stop >>=? fun _ ->
                            printfn "[%s] stopping" w.Name
                            Job.result()
                         Mailbox.Alt.take w.Mailbox >>=? fun msg -> 
                            Console.WriteLine( 
                                sprintf "[%s, TID %d] received %A" w.Name Thread.CurrentThread.ManagedThreadId msg)
                            server() ]

        Job.start (server())

    let stop w = IVar.fill w.Stop ()

type Pool<'a>(initCapacity: int) =
    let mailbox = mb<'a>()
    let stop = ivar<unit>()
    let capacity = mvar<int>()
    
    let worker() = 
        { Name = sprintf "worker %s" (Guid.NewGuid().ToString())
          Mailbox = mailbox
          Stop = ivar() }

    let startWorkers count =
        let newWorkers = List.init count (fun _ -> worker())
        newWorkers |> List.map Worker.create |> Job.conIgnore |> start |> ignore
        newWorkers  

    let setCapacity workers c =
        match c - List.length workers with
        | 0 -> workers
        | x when x > 0 -> workers @ startWorkers x
        | x -> 
            let victims, rest = workers |> List.take -x
            victims |> List.map Worker.stop |> Job.conIgnore |> start |> ignore
            rest

    let pool() = Job.delay <| fun _ ->
        let rec server workers =
            Alt.select [ IVar.Alt.read stop >>=? fun _ -> printfn "Stopping the pool..."; Job.result()
                         MVar.Alt.take capacity >>=? fun c -> 
                            printfn "Changing capacity from %d to %d..." (List.length workers) c
                            server (setCapacity workers c) ]
        Job.start (server (startWorkers initCapacity))

    do start (pool()) 
    member __.Add(msg: 'a) = Mailbox.send mailbox msg |> start
    member __.SetCapacity v = MVar.fill capacity v |> start

    interface IDisposable with
        member __.Dispose() = IVar.fill stop () |> start

let pool = new Pool<int>(3)
pool.Add 1
pool.SetCapacity 5
//pool.Add 20

seq { 1..1000 } |> Seq.iter (fun i -> pool.Add i)

(pool :> IDisposable).Dispose()
