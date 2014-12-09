#load "Header.fsx"

open Hopac
open Hopac.Job.Infixes

type Msg<'a> = Get | Put of 'a

type Cell<'a> =
    { ReqCh: Ch<Msg<'a>>
      RespCh: Ch<'a> }

let cell initValue = Job.delay <| fun _ ->
    let cell = { ReqCh = ch(); RespCh = ch() }
    let rec loop v =
        Ch.take cell.ReqCh >>= function
        | Get -> Ch.give cell.RespCh v >>. loop v
        | Put v' -> loop v'
    Job.start (loop initValue) >>% cell

let get cell = Ch.give cell.ReqCh Get >>. Ch.take cell.RespCh
let put cell v = Ch.give cell.ReqCh (Put v)

let c = run (cell 0)
run (get c)
run (put c 2)