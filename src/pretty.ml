
(*i*)
open Hashcons
open Mpa
open Bitv
open Format
open Term
(*i*)

type 'a  printer = Format.formatter -> 'a -> unit

let print_connectives = ref false

let rec list f fmt = function
    | [] -> ()
    | [x] -> f fmt x
    | x :: l -> f fmt x; Format.fprintf fmt ", " ; list f fmt l
	
let rec list_sep sep f = function
    | [] -> ()
    | [x] -> f x
    | x :: l -> f x; sep (); list_sep sep f l

	

		 (*s Pretty-printing constraints. *)
	
let rec pp_cnstrnt prec fmt = function
  | Top ->
      Format.fprintf fmt "full"
  | Bot ->
      Format.fprintf fmt "empty"
  | Sub(s,i) when Nonreals.is_empty s ->
      Interval.pp fmt i
  | Sub(s,i) when Interval.is_empty i ->
      pp_nonreals fmt s
  | Sub(s,i) ->
      Format.fprintf fmt "@[";
      pp_nonreals fmt s;
      Format.fprintf fmt "@ union@ ";
      Format.fprintf fmt "@]"

and pp_nonreals fmt s =
  let l = Nonreals.fold (fun x acc -> x :: acc) s [] in
  list_sep (fun () -> Format.fprintf fmt "@ union@ ") (pp_nonreal fmt) l

and pp_nonreal fmt = function
  | Boolean -> Format.fprintf fmt "boolean"
  | Predicate -> Format.fprintf fmt "predicate"
  | Cartesian -> Format.fprintf fmt "cartesian"
  | Bitvector -> Format.fprintf fmt "bitvector"
  | Other -> Format.fprintf fmt "other"   
	    

(*s For the printing function, we take into account the usual
    precedence rules (i.e. * binds tighter than +) to avoid
    printing unnecessary parentheses. To this end, we maintain
    the current operator precedence and print parentheses
    around an operator only if its precedence is less than
    the current precedence.

    Operator precedences:
      0  '|', '#', '=>'
      1  '&' '<=>'
      2  '=' '<>' '<' '>' '<=' '>='
      3  'union'
      4  'inter'
      5  'diff' 'symdiff'
      6  '-' '+'
      7  '*' '/'
      8 '~' 'compl'
  *)

let pp fmt t =
  let pr = fprintf fmt in
  let lpar prec op_prec = if prec > op_prec then pr "(" in 
  let rpar prec op_prec = if prec > op_prec then pr ")" in
  let rec pp_term prec t =
    match t.node with
      | Var x -> 
	  pp_var x
      | App ({node=Set _} as s,[x]) ->
	  pp_term prec x; pr "@ in@ "; pp_set prec s
      | App ({node=Update _} as a,[x]) ->
	  pp_set prec a; pr "["; pp_term prec x; pr "]"
      | App (f, []) ->
	  pp_term prec f
      | App (f,l) ->
	  pr "@["; pp_term prec f; pr "("; pp_terml l; pr ")@]"
      | Update (a,i,v) ->
	  pr "@["; pp_term prec a; pr "["; pp_term prec i; pr " := "; pp_term prec v; pr "]@]"  
      | Arith a -> 
	  pp_arith prec a
      | Tuple t ->
	  pp_tuple prec t
      | Set _ ->
	  pp_set prec t
      | Bool _ ->
	  pp_bool prec t
      | Bv _ ->
	  pp_bv prec t
	    
  and pp_var x =
    fprintf fmt "%s" x

  and pp_varl = function
    | [x]  -> pp_var x
    | x::l -> pp_var x; pr ", "; pp_varl l
    | []   -> ()

  and pp_tuple prec t =
    match t with
      | Tup l -> 
	  pr "@[("; pp_terml l; pr ")@]"
      | Proj (i,n,t) -> 
	  fprintf fmt "@[proj[%d,%d](" i n; pp_term 0 t; pr ")@]"

  and pp_arith prec a =
    match a with
      | Num q -> Mpa.Q.pp fmt q
      | Multq(q,x) ->
	  fprintf fmt "@[";
          Mpa.Q.pp fmt q;
	  fprintf fmt "*";
	  pp_term prec x;
	  fprintf fmt "@]"
      | Mult l ->
	  list_sep (fun () -> pr " * ") (pp_term prec) l
      | Add l ->
	  list_sep (fun () -> pr " + ") (pp_term prec) l
      | Div(x,y) ->
	  pp_binary prec (pp_term 7) "/" (x,y)

  and pp_bool prec b =
    if Bool.is_tt b then
      fprintf fmt "true"
    else if Bool.is_ff b then
      fprintf fmt "false"
    else if Bool.is_equal b then
      pp_binary prec (pp_term 1) "=" (Bool.d_equal b)
    else if Bool.is_diseq b then
      pp_binary prec (pp_term 1) "<>" (Bool.d_diseq b)    
    else if Bool.is_neg b then
      pp_unary prec (pp_term 8) "~" (Bool.d_neg b)
    else if !print_connectives && Bool.is_conj b then
      pp_binary prec (pp_term 4) "&" (Bool.d_conj b)
    else if !print_connectives && Bool.is_disj b then
      pp_binary prec (pp_term 3) "|" (Bool.d_disj b)
    else if !print_connectives && Bool.is_xor b then
      pp_binary prec (pp_term 3) "#" (Bool.d_xor b)
    else if !print_connectives && Bool.is_imp b then
      pp_binary prec (pp_term 2) "=>" (Bool.d_imp b)
    else if !print_connectives && Bool.is_iff b then
      pp_binary prec (pp_term 1) "<=>" (Bool.d_iff b)
    else 
      pp_ite "if" (pp_term 0) (Bool.d_ite b)

  and pp_bv prec b =
    match b.node with
      | Bv(Const c) -> 
	  Format.fprintf fmt "%s" (Bitv.to_string c)
      | Bv(BvToNat x) ->
	  fprintf fmt "@[bv2nat("; pp_term 0 x;  fprintf fmt ")@]"
      | Bv(Conc l) ->
	  list_sep (fun () -> pr " ++ ") pp_fixed l
      | Bv(Extr((n,x),i,j)) ->
	    fprintf fmt "@[extr[%d,%d](" i j; pp_fixed (n,x); fprintf fmt ")@]"
      | _ ->
	  if Bv.is_neg b then
	    begin
	      fprintf fmt "@[bvneg("; pp_fixed (Bv.d_neg b); fprintf fmt ")@]"
	    end
	  else if Bv.is_conj b then
	    let n,x,y = Bv.d_conj b in
	    begin
	      fprintf fmt "@[bvand("; pp_term 0 x; fprintf fmt ", "; pp_term 0 y; fprintf fmt ")@]"
	    end
	  else
	    let (n,x,y,z) = Bv.d_ite b in
	    pp_ite "bvite" (pp_term 0) (x,y,z)
	    
  and pp_fixed (n,x) =
      pp_term 0 x; fprintf fmt "@[[%d]]" n

  and pp_set prec s =
    match s.node with
      | Set(Empty _) ->
	  fprintf fmt "empty"
      | Set(Full _) ->
	  fprintf fmt "full"
      | Set(Finite ts) ->
	  fprintf fmt "@[{"; pp_terml (Term.Set.to_list ts); fprintf fmt "}@]"
      | Set(Cnstrnt c) ->
	  pp_cnstrnt prec fmt c
      | Set(SetIte _) when Sets.is_compl s ->
	  pp_unary prec (pp_term 8) "compl" (Sets.d_compl s)
      | Set(SetIte _) when !print_connectives && Sets.is_inter s ->
	  pp_binary prec (pp_term 4) "inter" (Sets.d_inter s)
      | Set(SetIte _) when !print_connectives && Sets.is_union s ->
	  pp_binary prec (pp_term 3) "union" (Sets.d_union s)
      | Set(SetIte _) when !print_connectives && Sets.is_sym_diff s ->
	  pp_binary prec (pp_term 3) "diff" (Sets.d_sym_diff s)
      | Set(SetIte _) when !print_connectives && Sets.is_sub s ->
	  pp_binary prec (pp_term 2) "sub" (Sets.d_sub s)
      | Set(SetIte _) when !print_connectives && Sets.is_equal s ->
	  pp_binary prec (pp_term 2) "sub" (Sets.d_equal s)
      | Set(SetIte(_,x,y,z)) ->
	  pp_ite "setite" (pp_term 0) (x,y,z)
      | _ -> assert false

	
  and pp_binary prec pp op (x,y) =
    fprintf fmt "@["; pp x; fprintf fmt "@ %s@ " op; pp y; fprintf fmt "@]"

  and pp_unary prec pp op x =
    fprintf fmt "@["; fprintf fmt "%s " op; pp x; fprintf fmt "@]"

  and pp_ite str pp (x,y,z) =
    pr "@["; fprintf fmt "%s " str;
    pp x;
    pr "@ then@ ";
    pp y;
    pr "@ else@ ";
    pp z;
    pr "@ end@]"
			    
  and pp_terml = function
    | [t]  -> pp_term 0 t
    | t::l -> pp_term 0 t; pr ","; pp_terml l
    | []   -> ()
	  
  in
  pp_term 0 t
    

let pp_eqn fmt (a,b) =
  Format.fprintf fmt "@[";
  pp fmt a;
  Format.fprintf fmt "@ =@ ";
  pp fmt b;
  Format.fprintf fmt "@]"

    
let pp_diseq fmt (a,b) =
  Format.fprintf fmt "@[";
  pp fmt a;
  Format.fprintf fmt "@ <>@ ";
  pp fmt b;
  Format.fprintf fmt "@]"

    
let term = pp
     
let eqn = pp_eqn

let cnstrnt = (pp_cnstrnt 0)

	    
let tset fmt s = 
  let rec loop = function
    | [] -> ()
    | [a] -> pp fmt a
    | a :: l -> pp fmt a; Format.fprintf fmt "@ ,@ "; loop l
  in
  Format.fprintf fmt "@[{"; loop (Term.Set.to_list s); Format.fprintf fmt "}@]"

    
let tmap p fmt m =
  let pp_assign (x,a) =
    Format.fprintf fmt "@["; pp fmt x; Format.fprintf fmt "@ |->@ "; p fmt a
  in
  let rec pp_assigns = function
    | [] -> ()
    | [a] -> pp_assign a
    | a :: l -> pp_assign a; Format.fprintf fmt "@ ,@ "; pp_assigns l
  in
    Format.fprintf fmt "@[["; pp_assigns (Map.to_list m); Format.fprintf fmt "]@]"
