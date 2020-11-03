let () = print_endline ""

open L

let rec synth names consts (rez : L.injected) =
  let open L in
  let open OCanren in
  conde
    [
      fresh x (rez === var x) (names x);
      fresh c (rez === const c) (consts c);
      fresh (l r)
        (rez === mul l r)
        (synth names consts l) (synth names consts r);
      fresh (l r)
        (rez === add l r)
        (synth names consts l) (synth names consts r);
    ]

let tests : _ list =
  let open OCanren in
  let s = run q (synth (fun n -> n === !!"x") (fun c -> c === !!1)) Fun.id in

  (* let (_ : int) = (Stream.hd s)#prjc L.prjc in *)
  let last = 200000 in
  let indexes =
    List.sort compare
      [
        (* 1000;
           1500;
           2000;
           2500;
           3000;
           3500;
           4000;
           4500;
           5000;
           6000;
           7000;
           8000; *)
        90000;
        120000;
        last;
      ]
  in
  let rec go acc i s =
    if Stream.is_empty s then assert false
    else if i > last then List.rev acc
    else if List.mem i indexes then
      go ((Stream.hd s)#prj :: acc) (i + 1) (Stream.tl s)
    else go acc (i + 1) (Stream.tl s)
  in
  go [] 0 s

let __ () =
  tests
  |> List.iter (fun p ->
         let s1 = Format.asprintf "%a" L.pp p in
         let s2 = Format.asprintf "%a" V.pp p in
         if s1 <> s2 then (
           Format.printf "%s\n%s\n%!" s1 s2;
           failwith "different answers" )
         else Format.printf "%a\n%!" (GT.fmt L.ground) p)

let pp_GT x =
  let (_ : string) = Format.asprintf "%a" L.pp x in
  ()

let pp_Visitors x =
  let (_ : string) = Format.asprintf "%a" V.pp x in
  ()

let test1 e =
  let open Benchmark in
  let res =
    throughputN ~style:Nil ~repeat:1 1
      [ ("GT", pp_GT, e); ("Default", pp_Visitors, e) ]
  in
  tabulate res

let () = List.iter test1 tests
