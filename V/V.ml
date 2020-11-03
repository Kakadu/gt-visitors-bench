type ('self, 'int, 'name) t = ('self, 'int, 'name) L.t =
  | Const of 'int
  | Mul of 'self * 'self
  | Add of 'self * 'self
  | Var of 'name
[@@deriving
  visitors { variety = "iter"; name = "iter_t_t"; polymorphic = false }]

type ground = (ground, GT.int, GT.string) t

(* [@@deriving
  visitors { variety = "iter"; name = "iter_ground_t"; polymorphic = true }] *)

class ['self] my_pp =
  object (self : 'self)
    inherit [_] iter_t_t

    (* inherit [_] iter_ground_t *)
    method visit_'self ppf x = self#visit_t ppf x

    method visit_'int ppf = Format.fprintf ppf "%d"

    method visit_'name ppf = Format.fprintf ppf "%S"

    method visit_Mul ppf l r =
      Format.fprintf ppf "(%a*%a)" self#visit_t l self#visit_t r

    method visit_Add ppf l r =
      Format.fprintf ppf "(%a+%a)" self#visit_t l self#visit_t r
  end

let pp =
  let c = new my_pp in
  let rec fself ppf (x : ground) = c#visit_t ppf x in
  fself
