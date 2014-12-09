#load "Header.fsx"

open Hopac
open Hopac.Job.Infixes
open Hopac.Alt.Infixes

type Cell<'a> =
    { GetCh: Ch<'a>
      PutCh: Ch<'a> }

let cell initValue = Job.delay <| fun _ ->
    let cell = { GetCh = ch(); PutCh = ch() }
    let rec loop v =
        Alt.select [ Ch.Alt.give cell.GetCh v >>=? fun _ -> loop v
                     Ch.Alt.take cell.PutCh >>=? loop ]
    Job.start (loop initValue) >>% cell

let get cell = Ch.take cell.GetCh
let put cell v = Ch.give cell.PutCh v

let c = run (cell 0)
run (get c)
run (put c 2)
