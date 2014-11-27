#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.Core.dll"
#r "packages/Hopac.0.0.0.31/lib/net45/Hopac.dll"

open System
open Hopac
open Hopac.Alt.Infixes

type Worker<'a> =
    { Name: string
      Mailbox: Mailbox<'a>
      Stop: IVar<unit> }
    member x.Start() = Job.delay <| fun _ ->
        let rec server() =
            Alt.select [ IVar.Alt.read x.Stop >>=? fun _ ->
                            printfn "[%s] stopping" x.Name
                            Job.result()
                         Mailbox.Alt.take x.Mailbox >>=? (fun msg -> 
                             printfn "[%s] received %A" x.Name msg 
                             server()) ]
        Job.start (server())

type Pool<'a>(size: int) =
    let mailbox = Mailbox.Now.create<'a>()
    let workers = List.init size <| fun i -> 
            { Name = sprintf "worker #%d" i
              Mailbox = mailbox
              Stop = ivar() }
    member __.Run() =
        workers |> List.map (fun x -> x.Start()) |> Job.conIgnore |> run
    member __.Add(msg: 'a) = Mailbox.send mailbox msg |> run 
    interface IDisposable with
        member __.Dispose() =
            workers |> List.map (fun x -> IVar.fill x.Stop ()) |> Job.conIgnore |> run

let pool = new Pool<int>(3)
pool.Run()
pool.Add 1
pool.Add 20
(pool :> IDisposable).Dispose()
