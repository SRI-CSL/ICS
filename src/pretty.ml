
(*i*)
open Tools
open Hashcons
open Term
open Format
(*i*)

let pp_full_name = ref true

(*s Printing of a list *)

let rec list_sep sep f = function
  | [] -> ()
  | [x] -> f x
  | x :: l -> f x; sep (); list_sep sep f l

let list f = list_sep (fun () -> Format.print_string ", ") f

let pp_cnstrnt = function
  | Int -> printf "Int"
  | Real -> printf "Real"
  | Pos -> printf "Pos"
  | Neg -> printf "Neg"
  | Nonneg -> printf "Nonneg"
  | Nonpos -> printf "Nonpos"


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

let lpar prec op_prec = if prec > op_prec then printf "("
let rpar prec op_prec = if prec > op_prec then printf ")"

 
let rec pp_prop prec p = match p with
  | True -> printf "true"
  | False -> printf "false"
	(*
  | Ite (a,{node=Bool True},{node=Bool False}) ->
      printf "@[<hv 1>~";
      pp_term prec a;
      printf "@]"
  | Ite (a,{node=Bool False},{node=Bool True}) ->
      printf "@[<hv 1>~";
      lpar prec 8;
      pp_term 8 a;
      rpar prec 8;
      printf "@]"
  | Ite (a,b,{node=Bool False}) ->
      printf "@[<hv 1>";
      lpar prec 1;
      pp_term 1 a;
      printf "@ & @ "; 
      pp_term 1 b;
      rpar prec 1;
      printf "@]"
  | Ite (a,{node=Bool True},b) ->
      printf "@[<hv 1>";
      lpar prec 0;
      pp_term 0 a;
      printf "@ || @ ";
      pp_term 0 b;
      rpar prec 0;
      printf "@]"
  | Ite (a,b,{node=Bool True}) ->
      printf "@[<hv 1>";
      lpar prec 0;
      pp_term 0 a;
      printf "@ => @ ";
      pp_term 0 b;
      rpar prec 0;
      printf "@]"
	  *)
  | Ite (a,b,c) ->
      printf "@[<hv 1>if ";
      pp_term prec a;
      printf " then@ ";
      pp_term prec b;
      printf " else@ ";
      pp_term prec c;
      printf " end@]"
  | Forall (xl, p) ->
      printf "@[<hv 3> forall ";
      list pp_var xl;
      printf ".@ ";
      pp_term prec p
  | Exists (xl, p) ->
      printf "@[<hv 3> exists ";
      list pp_var xl;
      printf ".@ ";
      pp_term prec p

and pp_var x =
  print_string x
    
and pp_tuple prec t = match t with
  | Tup l -> 
      printf "@[<hv 1>("; pp_comma_list l; printf ")@]"
  | Proj (i,n,t) -> 
      printf "@[<hv 1>proj[%d,%d](" i n; pp_term 0 t; printf ")@]"

and pp_set prec s = match s with
  | Empty _ -> printf "empty"
  | Full _ -> printf "full"
  | SetIte (_,a,b,c) ->
      printf "@[<hv 1>setite ";
      pp_term prec a;
      printf " then@ ";
      pp_term prec b;
      printf " else@ ";
      pp_term prec c;
      printf " end@]"

and pp_bv prec v =
  match v with
    | Const bv -> 
	printf "@[<hv 1>0b";
	print_string (Bitv.to_string bv);
	printf "@]"
    | Extr (b,l,u) ->
	printf "@[<hv 1>";
	pp_fixed prec b;
	printf "[%d,%d]" l u;
	printf "@]"
    | Conc l ->
	printf "@[<hv 1>";
	pp_fixedl prec l;
	printf "@]"
    | BvIte (a,b,c) ->
	printf "@[<hv 1>bvif ";
	pp_fixed prec a;
	printf " then@ ";
	pp_fixed prec b;
	printf " else@ ";
	pp_fixed prec c;
	printf " end@]"

and pp_fixed prec (_,b) =
  pp_term prec b

and pp_fixedl prec bl =
  list_sep (fun () -> printf " ++ ") (pp_fixed prec) bl
  
and pp_term prec t =
  match t.node with
  | Var x -> 
      pp_var x
  | App (f, []) ->
       pp_term prec f
  | App (f,l) ->
      printf "@[<hv 1>";
      pp_term prec f;
      printf "(";
      list (pp_term prec) l;
      printf ")@]"
  | Update (a,i,v) ->
      printf "@[<hv 1>";
      pp_term prec a;
      printf "[";
      pp_term prec i;
      printf " := ";
      pp_term prec v;
      printf "]@]"
  | Equal (t1,t2) ->
      printf "@[<hv 1>";
      lpar prec 2;
      pp_term 2 t1;
      printf " = ";
      pp_term 2 t2;
      rpar prec 2;
      printf "@]"
  | Cnstrnt (c,t) ->
      printf "@[<hv 1>";
      pp_cnstrnt c;
      printf "(";
      pp_term 2 t;
      printf ")";
      printf "@]"
  | Arith a -> 
      pp_arith prec a
  | Tuple t ->
      pp_tuple prec t
  | Set s ->
      pp_set prec s
  | Bool p ->
      pp_prop prec p
  | Bv b ->
      pp_bv prec b

and pp_arith prec a =
  match a with
  | Num q -> Mpa.Q.pp q
  | Times l -> pp_times prec l
  | Plus l -> pp_plus prec l
 
and pp_plus prec l =
  assert (List.length l > 1);
  list_sep (fun () -> printf " + ") (pp_term prec) l
      
and pp_times prec l =
  assert (List.length l > 1);
  list_sep (fun () -> printf " * ") (pp_term prec) l
      
and pp_monomial prec m =
  list_sep (fun () -> printf "*") (pp_term prec) m.node
	
and pp_comma_list = function
  | [t]  -> pp_term 0 t
  | t::l -> pp_term 0 t; printf ","; pp_comma_list l
  | []   -> ()

let term a = pp_term 0 a
let bv b = pp_bv 0 b
		
let eqn (x,y) = term x; print_string " = "; term y

let diseq (x,y) = term x; print_string " <> "; term y


(* Pretty-printing a set of terms *)

let tset s =
  printf "{";
  let l = Tset.fold (fun x acc -> x :: acc) s [] in
  list term l; printf "}@?"


(*s Pretty-printing a map with terms as domain *)

let tmap pp =
  Tmap.iter (fun x y ->
	       term x; printf " |-> "; pp y; printf "\n@?")
