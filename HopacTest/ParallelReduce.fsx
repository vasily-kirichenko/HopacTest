#load "Header.fsx"

//#r @"packages\FSharpx.Core.1.8.41\lib\40\FSharpx.Core.dll"
//#load @"packages\FSharp.Charting.0.90.6\FSharp.Charting.fsx"
open Hopac
open Hopac.Infixes
open Hopac.Job.Infixes
open Hopac.Alt.Infixes
//open FSharp.Charting
open System.Threading.Tasks
open System

let reduceParallelTasks<'a> f (ie: 'a array) = 
    let rec reduceRec f (ie: 'a array) = 
        match ie.Length with
        | 1 -> ie.[0]
        | 2 -> f ie.[0] ie.[1]
        | len -> 
            let h = len / 2
            let o1 = Task.Run(fun _ -> reduceRec f ie.[0..h - 1])
            let o2 = Task.Run(fun _ -> reduceRec f ie.[h..])
            f o1.Result o2.Result
    match ie.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> reduceRec f ie

let reduceParallelAsync<'a> f (ie: 'a array) = 
    let rec reduceRec f (ie: 'a array) = 
        async { 
            match ie.Length with
            | 1 -> return ie.[0]
            | 2 -> return f ie.[0] ie.[1]
            | len -> 
                let h = len / 2
                let! o1a = Async.StartChild <| reduceRec f ie.[0..h - 1]
                let! o2 = reduceRec f ie.[h..]
                let! o1 = o1a
                return f o1 o2
        }
    match ie.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> Async.RunSynchronously <| reduceRec f ie

let reduceParallelHopac<'a> f (a: 'a array) = 
    let rec reduceRec (f, ie: 'a array) = 
        match ie.Length with
        | 1 -> Job.result ie.[0]
        | 2 -> Job.result (f ie.[0] ie.[1])
        | len -> 
            let h = len / 2
            reduceRec (f, ie.[0..h - 1]) <*> Job.delayWith reduceRec (f, ie.[h..]) |>> fun (x, y) -> f x y
    match a.Length with
    | 0 -> failwith "Sequence contains no elements"
    | _ -> run <| reduceRec (f, a)

let cleanup() =
  for _ in 1..5 do
    GC.Collect ()
    Threading.Thread.Sleep 50
     
let a = [| 1L..50000000L |]

Array.reduce (+) a
cleanup()
reduceParallelTasks (+) a
cleanup()
reduceParallelHopac (+) a
cleanup()
reduceParallelAsync (+) a

// --------------

let (>>=) m f = async.Bind(m, f)
let leftIdentity f x = async.Return x >>= f
leftIdentity (fun x -> async.Return (x * x)) 2 |> Async.RunSynchronously
async { return 1 } >>= async.Return |> Async.RunSynchronously
// (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
let left m f g = (m >>= f) >>= g
let right m f g = m >>= (fun x -> f x >>= g)

let l = left (async { return 2 }) (fun x -> async.Return (x + 10)) (fun x -> async.Return (x * 5)) |> Async.RunSynchronously
let r = right (async { return 2 }) (fun x -> async.Return (x + 10)) (fun x -> async.Return (x * 5)) |> Async.RunSynchronously
