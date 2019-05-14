type typ =
   | TyVar of typ var
   | TyArrow of typ * typ
   | TyProduct of typ list         
   | TyConstr of string * typ list

and 'a var =
  { stamp: int; (* for debug *)
    mutable value: 'a value }

and 'a value =
  | Unknown
  | Known of 'a

type typ_scheme =
  { params: (typ var) list;
    body: typ }

exception TypeConflict of typ * typ;;
exception TypeCircularity of typ * typ;;

let type_arrow t1 t2 = TyArrow (t1,t2)
let type_arrow2 t1 t2 t3 = TyArrow (t1,TyArrow (t2,t3))
let type_constr c ts = TyConstr(c, ts)
let type_pair t1 t2 = TyProduct [t1;t2]
let type_list t n = TyConstr("list", [t])
let type_nat = type_constr "nat" []
let type_bool = type_constr "bool" []
let type_unit = type_constr "unit" []
let no_type = type_constr "unknown" []
let type_product = function
    [] -> type_unit
  | [t] -> t
  | ts -> TyProduct ts

let rec type_repr = function
  | TyVar({value = Known ty1} as var) ->
      let ty = type_repr ty1 in
      var.value <- Known ty;
      ty
  | ty -> ty

let rec real_type ty = 
  match type_repr ty with
  | TyArrow (ty1,ty2) -> TyArrow (real_type ty1, real_type ty2)
  | TyProduct ts  -> TyProduct (List.map real_type ts)        
  | TyVar { value=Known ty'} -> ty'
  | ty -> ty

let occur_check var ty =
  let rec test t =
    match type_repr t with
    | TyVar var' ->
        if var == var' then raise(TypeCircularity(TyVar var, ty))
    | TyArrow (ty1,ty2) ->
        test ty1;
        test ty2
    | TyProduct ts ->
        List.iter test ts
    | TyConstr(constr, args) ->
        List.iter test args
  in test ty

let rec unify ty1 ty2 =
  let val1 = real_type ty1
  and val2 = real_type ty2 in
  if val1 == val2 then () else
  match (val1, val2) with
  | TyVar v1, TyVar v2 when v1==v2 -> 
      ()
  | TyVar var, ty ->
      occur_check var ty;
      var.value <- Known ty
  | ty, TyVar var ->
      occur_check var ty;
      var.value <- Known ty
  | TyArrow(ty1, ty2), TyArrow(ty1', ty2') ->
      unify ty1 ty1';
      unify ty2 ty2'
  | TyProduct ts1, TyProduct ts2 when List.length ts1 = List.length ts2 ->
      List.iter2 unify ts1 ts2
  (* | Tconstr({tc_name="bundle"}, [ty1'], [sz1]), Tproduct ts2 *)
  (* | Tproduct ts2, Tconstr({tc_name="bundle"}, [ty1'], [sz1]) ->
   *     (\* Note 2014-05-14, JS. Special case for unifying bundles and tuples *\)
   *     unify_size (val1,val2) sz1 (SzConst (List.length ts2));
   *     List.iter (unify ty1') ts2 *)
  | TyConstr(constr1, arguments1), TyConstr(constr2, arguments2)
    when constr1=constr2 && List.length arguments1 = List.length arguments2 ->
      List.iter2 unify arguments1 arguments2
  | _, _ ->
      raise (TypeConflict(val1, val2))

let new_stamp =
  let cnt = ref 0 in
  function () -> incr cnt; !cnt

let new_type_var () = TyVar {stamp = new_stamp(); value = Unknown}

let generalize env ty =
  (* Note : we use here a naive version in which generic variables are detected by
     simply checking whether they do not occur free in the englobing typing environment.
     A more efficient version would use binding levels, of course *)
  let vars_of tvars' ty = 
     (* Returns the list of type variables occuring in [t] but not in [tvars'] *)
    let tvars = ref [] in
    let rec scan_ty t =
      match type_repr t with
      | TyVar var ->
          if not (List.memq var !tvars) && not (List.memq var tvars') 
          then tvars := var :: !tvars
      | TyArrow (t1, t2) ->
          scan_ty t1;
          scan_ty t2
      | TyProduct ts ->
          List.iter scan_ty ts
      | TyConstr (constr, arguments) ->
          List.iter scan_ty arguments
    in
    scan_ty ty;
    !tvars in
  let free_tvars = 
    List.fold_left
      (fun tvs (_,ts) -> 
        let tvs' = vars_of ts.params ts.body in
        tvs @ tvs')
      []
      env in
  let gen_tvars = vars_of free_tvars ty in
  {params = List.rev gen_tvars; body = ty}

let trivial_scheme ty = {params = []; body = ty}

(* Type instanciation *)

let rec copy_type tvbs ty =
  let rec copy ty = 
    match type_repr ty with
    | TyVar var as ty ->
        begin try
          List.assq var tvbs
        with Not_found ->
            ty
        end
    | TyArrow (ty1, ty2) ->
        TyArrow (copy ty1, copy ty2)
    | TyProduct ts ->
        TyProduct (List.map copy ts)
    | TyConstr (c, args) ->
        TyConstr (c, List.map copy args) in
  copy ty

let full_type_instance ty_sch =
  match ty_sch.params with
  | [] -> ty_sch.body, []
  | tparams ->
      let unknown_ts = List.map (fun var -> (var, new_type_var())) tparams in
      copy_type unknown_ts ty_sch.body,
      unknown_ts

let type_instance ty_sch = fst (full_type_instance ty_sch)

let type_copy t = type_instance (generalize [] t)  (* tofix ? *)

let is_constr_type p t = match real_type t with
  | TyConstr (name,_) -> p name 
  | _ -> false

let is_unit_type = is_constr_type (fun n -> n="unit")
let is_nat_type = is_constr_type (fun n -> n="nat")
let is_bool_type = is_constr_type (fun n -> n="bool")

let list_of_types ty = match real_type ty with
  TyProduct ts -> List.map real_type ts
| TyConstr ("unit", []) -> []
| _ -> [real_type ty]
