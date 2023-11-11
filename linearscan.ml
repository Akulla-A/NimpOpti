open Imp
open Nimp

(* sort by ascending lower bound, and sort equals by ascending upper bound *)
let sort_2 l =
  List.stable_sort (fun (_, l1, _) (_, l2, _) -> l1 - l2) l
let sort_3 l =
  List.stable_sort (fun (_, _, h1) (_, _, h2) -> h1 - h2) l
let sort_intervals l =
  sort_2 (sort_3 l)

(* insert interval [i] in active list [l] 
   pre/post-condition: sorted by ascending upper bound *)
let rec insert_active i l =
  failwith "not implemented"

(* raw allocation information for a variable *)
type raw_alloc =
  | RegN  of int  (* index of the register *)
  | Spill of int  (* index of the spill *)

(* allocation of the local variables of a function [fdef] using linear scan
   algorithm, with [nb_regs] registers available for allocation ;
   return a raw allocation for each variable, as well as the maximum index of
   used registers, and the number of used stack slots (spills) *)
let lscan_alloc nb_regs fdef =
  let live_intervals = Liveness.liveness_intervals_from_liveness fdef in
  let alloc = Hashtbl.create (List.length fdef.locals) in
  let active = ref [] in
  let free = ref (List.init nb_regs (fun i -> i)) in
  let r_max = ref (-1) in (* maximum index of used register *)
  let spill_count = ref 0 in (* number of spilled variables *)
  (* free registers allocated to intervals that stop before timestamp a,
     returns remaining intervals *)

     (*
  let rec expire a l =
    failwith "not implemented"
  in*)
  (* for each interval i, in sorted order *)
  List.iter (fun i ->
      let xi, li, hi = i in

      if (li != (-1) || hi != (-1)) then
        (* free registers that expire before the lower bound of i *)
        (active := List.fold_left (fun acc r ->
          let activeReg = fst r in
          let activeLb = snd(snd r) in
    
          if activeLb < li then
            (free := activeReg :: !free;
            acc)
          else
            r :: acc
        ) [] !active;

        (* if there are available registers *)
        if (List.length !free) > 0 then begin
          (* ... then allocate one *)
          match !free with
            | hd :: tl ->
              active := (hd, (li, hi)) :: !active;
              free := tl;
              Hashtbl.add alloc xi (RegN hd, li, hi);
              r_max := max (!r_max) (List.length !active);
            | [] -> ();
        end
        else
          (* otherwise, may replace an already used register if this can
            make this register available again earlier *)
        
          Hashtbl.add alloc xi (Spill !spill_count, li, hi);
          spill_count := !spill_count + 1;)
      ) (sort_intervals live_intervals);

  alloc, !r_max, !spill_count
