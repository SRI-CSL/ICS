
(*i
 * ICS - Integrated Canonizer and Solver
 * Copyright (C) 2001-2004 SRI International
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the ICS license as published at www.icansolve.com
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * ICS License for more details.
 *
 * Author: Harald Ruess
 i*)

(*i*)
open Ics
open Tools
open Format
(*i*)

  
(*s Stack of states *)
  
let states = ref(Stack.create())

let init () = Stack.push(Ics.state_init()) !states

let _ = init()

let current () = Stack.top !states

let push s = Stack.push s !states

let pop () = 
  if Stack.length !states <= 1 then () else
    let _ = Stack.pop !states in ()

let clear () =
  Stack.clear !states;
  init()

		
(*s Setting the level of trace output. The larger the natural [n], the more
    output is provided. *)

let verbose n =
  Ics.set_verbose n

(*s The command [process] introduces a new fact. The state
  is left unchanged is an inconsistency is discovered. *)
  
let process a =
  let s = current () in
  match Ics.process s a with
    | Ics.Consistent(s') ->
	push s';
	Format.printf "Ok.@."
    | Ics.Valid ->
	Format.printf "Redundant.@."
    | Ics.Inconsistent ->
	Format.printf "Inconsistent.@."

let check t =
  let s = current() in
  let status = Ics.check s t in
  if Ics.is_check_valid status then
    Format.printf "Valid!@."
  else if Ics.is_check_inconsistent status then
    Format.printf "Inconsistent!@."
  else 
    Format.printf "Satisfiable!@."


(*s Print states state on standard output. *)

let curr () =
  let s = current() in
  Ics.state_pp s
  
  
    (*s Querying the state. *)
    
let find th ma =
  let s = current() in
  match ma with
    | Some(a) -> 
      let b = Ics.state_find th s a in
      Format.printf "@["; Ics.term_pp b; Format.printf "@]@."
  | None -> 
      Ics.subst_pp (Ics.state_find_of th s)

let show () =
  let s = current() in
  Ics.state_pp s

let inconsistent () =
  let s = current() in
  if Ics.state_inconsistent s then
    Format.printf "@[Yes.@]"
  else
    Format.printf "@[No.@]"

let ctxt () =
  let s = current() in
  Ics.list_pp Ics.term_pp (Ics.state_ctxt_of s)


let cnstrnt a = 
  let s = current() in
  let c = Ics.cnstrnt s a in
  Ics.cnstrnt_pp c
	
	(*s Simplifiers *)

let sigma a =
  Format.printf "@["; Ics.term_pp a; Format.printf "@]@."
			 
     
let can a = 
  let s = current() in
  let b = Ics.can s a in
  Format.printf "@["; Ics.term_pp b; Format.printf "@]@."

let norm a =  
  let string_of_domain = function
    | Term.IntDom -> "int"
    | Term.BoolDom -> "bool"
    | Term.RatDom -> "real"
  in 
  let s = current() in
  let (xs,b) = Ics.norm s a in
  Format.printf "@["; Ics.term_pp b; Format.printf "@]@.";
  if not(Ics.terms_is_empty xs) then
    begin
      Format.printf "\nwith fresh vars: ";
      Ics.list_pp 
	(fun x ->
	   (assert (Ics.is_fresh x));
	   let (id,dom) = d_fresh x in
	   Format.printf "(%s,%s)" id (string_of_domain dom))
	(Ics.terms_to_list xs)
    end

    
    (*s Solver command [solve]. *)
    
let solve th (a,b) =
  let s = current() in
  let e' = (Ics.can s a, Ics.can s b) in
  match Ics.solve th s e' with
    | Some(rho) -> 
	Ics.list_pp Ics.eqn_pp rho
    | None -> 
	printf "Inconsistent.@."


   (*s Display use lists. *)

let use th ma =
  let s = current() in
  match ma with
    | Some(a) ->
	let ts = Ics.state_use th s a in
	Ics.terms_pp ts
    | None ->
	Ics.map_pp Ics.terms_pp (Ics.state_use_of th s)

   (*s Display set of equivalent terms *)

let ext a =
  let s = current() in
  let bs = Ics.state_ext s a in
  Ics.terms_pp bs

   (*s Display disequality lists. *)

let diseqs ma =
  let s = current() in
  match ma with
    | Some(a) ->
	let ts = Ics.state_diseqs s a in
	Ics.terms_pp ts
    | None ->
	Ics.map_pp Ics.terms_pp (Ics.state_diseqs_of s)

   (*s Groebner basis completion. *)

let groebner () = 
  let s = current() in
  match Ics.groebner s with
    | None -> 
	Format.printf "Inconsistent.@."
    | Some(s') ->
	push s';
	Format.printf "Ok.@."
	


   (*s Get solution for a term. *)

let solution al =
  let s = current() in
  let rhos = Ics.state_solutions s (Ics.terms_of_list al) in
  List.iter Ics.subst_pp rhos


(*s Get witnesses for a term. *)

let witness al =
  let s = current() in
  let rhos = Ics.state_solutions s (Ics.terms_of_list al) in
  List.iter Ics.subst_pp rhos
      
    
    (*s Order of terms. *)
    
let less (t1,t2) =
  Format.printf "%s.@."
    (if Ics.term_cmp t1 t2 < 0 then "Yes" else "No")

    
    (*s Reset, undoing, and dropping into byte-code interpreter *)
			 
let reset () =
  Ics.reset ();
  clear()
 
let undo n =
  match n with
    | None -> 
	pop ()
    | Some(n) ->
	assert(n >= 1);
	  for i = 0 to n - 1 do
	    pop()
	  done

let drop () =
  failwith "drop"

    
    (*s Help functions. *)

let help () =
  Format.printf "help commands.         Lists all commands.@.";
  Format.printf "help syntax.           Outlines term syntax.@."

let help_commands () =
  Format.printf "assert <term>.          Asserts <term> to the current context.@.";
  Format.printf "                        There are three different outcomes. In case, <term> is@.";
  Format.printf "                        found to be implied by the current context, `Valid.' is @.";
  Format.printf "                        returned, and if <term> is found to be inconsistent, @.";
  Format.printf "                        'Inconsistent.' is printed. Otherwise, <term> is@.";
  Format.printf "                        added to the current context@.";
  Format.printf "check <term>.           Check if <term> holds or is unsatisfiable@.";
  Format.printf "                        Returns 'Valid.' if <term> is redundant, 'Inconsistent.'@.";
  Format.printf "                        if it is inconsistent, otherwise silent. In contrast @.";
  Format.printf "                        to the assert command, the context is not updated.@.";
  Format.printf "sigma <term>.           Normal form using theory-specific normalizations.@.";
  Format.printf "                        only. In particular, no context information is used.@.";
  Format.printf "can <term>.             Computes canonical representative of <term> using.@.";
  Format.printf "                        context information.@.";
  Format.printf "solve <term> = <term>.  Solves equation on terms.@.";
  Format.printf "find [<term>].          Canonical representative of the equivalence class@.";
  Format.printf "                        of <term> as stored in the context.@.";
  Format.printf "                        If <term> is omitted, all finds are displayed.@.";
  Format.printf "cnstrnt [<term>].       Type interpretation of <term> wrt. current context.@.";
  Format.printf "                        If <term> is omitted, then all constraints are shown.@.";
  Format.printf "<term> << <term>.       Check term ordering.@.";
  Format.printf "verbose <int>.          Sets the verbose level.@.";
  Format.printf "reset.                  Reset to empty context.@.";
  Format.printf "drop.                   Drop into Caml bytecode (back with Main.repl ();;).@."

let help_syntax () =
  Format.printf
"<term> ::= ident                                            
         | <term> '(' <term> ',' ... ',' <term> ')'      Function application
         | <term> '[' <term> ':=' <term> ']'             Function update
         | <prop> | <arith> | <tuple> | <array> |        Interpreted terms 
         | '(' <term> ')'                             

<arith> ::= <term> '+' <term>                            Addition
          | <term> '-' <term>                            Subtraction   
          | <term> '*' <term>                            Multiplication
          | const ['/' const]                            Rational constant    
          | '-' <term>                                   Unary minus              

<tuple> ::= '(' <term> ',' ... ',' <term> ')'              Tuple  
          | proj '[' int ',' int ']' '(' <term> ')'        Projection
    
<prop> ::= true             
         | false            
         | <atom>            
         | <term> '&' <term>        Conjunction
         | <term> '|' <term>        Disjunction
         | <term> '=>' <term>        Implication
         | <term> '<=>' <term>       Biimplication
         | '~' <term>                Negation

<atom> ::= <unary> '(' <term> ')'
         | <term> <binary> <term>    

<binary> ::= '=' | '<>' | '<' | '>' | '<=' | '>' | '>='

<unary> ::= integer | real @."


