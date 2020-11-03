type ('self, 'int, 'name) t =
  | Const of 'int
  | Mul of 'self * 'self
  | Add of 'self * 'self
  | Var of 'name
[@@deriving gt ~options:{ gmap; fmt }]

module E = OCanren.Fmap3 (struct
  type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c) t

  let fmap f = GT.gmap t f
end)

class ['self, 'int, 'name, 'extra] my_fmt_t fself fint fname fself_ =
  object
    inherit ['self, 'int, 'name, 'extra] fmt_t_t fself fint fname fself_

    method c_Const ppf _ n = Format.fprintf ppf "%a" fint n

    method c_Var ppf _ = Format.fprintf ppf "%a" fname

    method c_Mul ppf _ l r = Format.fprintf ppf "(%a*%a)" fself l fself r

    method c_Add ppf _ l r = Format.fprintf ppf "(%a+%a)" fself l fself r
  end

let t =
  {
    t with
    GT.plugins =
      object
        method fmt f1 f2 f3 = GT.transform t (new my_fmt_t f1 f2 f3)

        method gmap eta = GT.gmap t eta
      end;
  }

type ground = (ground, GT.int, GT.string) t [@@deriving gt ~options:{ fmt }]

type logic =
  (logic, GT.int OCanren.logic, GT.string OCanren.logic) t OCanren.logic
[@@deriving gt ~options:{ fmt }]

type injected = (ground, logic) OCanren.injected

let rec prjc env (x : injected) =
  E.prjc prjc
    (OCanren.prjc (fun _ _ -> assert false))
    (OCanren.prjc (fun _ _ -> assert false))
    (fun _ _ -> assert false)
    env x

let var s = OCanren.inj @@ E.distrib @@ Var s

let mul a b = OCanren.inj @@ E.distrib @@ Mul (a, b)

let add a b = OCanren.inj @@ E.distrib @@ Add (a, b)

let const c = OCanren.inj @@ E.distrib @@ Const c

let pp ppf (x : ground) = GT.fmt ground ppf x
