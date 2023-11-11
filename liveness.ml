open Imp
open Nimp

module VSet = Set.Make(String)

(* returns the set of variables accessed by the expression [e] *)
let rec use_expr e =
  match e with
    | Cst(_) | Bool(_) -> VSet.empty
    | Var(s) -> VSet.add s VSet.empty
    | Binop(_, e1, e2) -> VSet.union (use_expr e2) (use_expr e1)
    | Call(s, expList) ->
      List.fold_left (fun acc e ->
        VSet.union (use_expr e) acc
      ) VSet.empty expList;;

let liveness fdef =
  let n = max_instr_list fdef.code in
  let live = Array.make (n+1) VSet.empty in
  (* returns the set of variable that live in entry to the numbered 
     instruction [i], assuming a set of live variables [lv_out] on 
     exit of [i] *)
  let rec lv_in_instr nb i lv_out =
    let newSet = (match i with
      | Putchar(e) | Expr(e) -> VSet.union (use_expr e) lv_out
      | Return(e) -> use_expr e
      | While(e, s) -> lv_in_list s (VSet.union (use_expr e) (lv_in_list s lv_out))
      | Set(x, e) -> VSet.union (use_expr e) (VSet.remove x lv_out)
      | If (e, s1, s2) -> 
        VSet.union (use_expr e) (VSet.union (lv_in_list s1 lv_out) (lv_in_list s2 lv_out))
    ) in
    live.(nb) <- newSet;
    newSet

  (* the same for a sequence, and records in [live] the live sets computed
     on entry to each analyzed instruction *)
  and lv_in_list l lv_out = 
    List.fold_right (fun nInstr acc ->
      let nb = nInstr.nb in
      let instr = nInstr.instr in

      let lv_out = lv_in_instr nb instr acc in
      lv_out
    ) l lv_out
  in
  let _ = lv_in_list fdef.code VSet.empty in
  live

let liveness_intervals_from_liveness fdef =
  let live = liveness fdef in
  (* for each variable [x], create the smallest interval that contains all
     the numbers of instructions where [x] is live *)

  List.fold_left(fun acc localName ->
    let first = ref (-1) in
    let last = ref (-1) in

    Array.iteri (fun index set ->
      let contains = VSet.exists (fun s -> s = localName) set in

      if contains then begin
        if (!first) = -1 then
          first := index;

        last := index;
      end
    ) live;

    (localName, !first, !last) :: acc
  ) [] fdef.locals
