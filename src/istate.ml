(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 1.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2001, 2002.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 * 
 * Author: Harald Ruess
 *)

open Name.Map

(** Global state. *)

let current = ref Context.empty
let symtab = ref (Symtab.empty())
let inchannel = ref Pervasives.stdin
let outchannel = ref Format.std_formatter
let eot = ref "" 
let counter = ref 0
let prompt = ref "ics> "

let batch = ref false

let set_prompt str =
  prompt := str

let set_eot str =
  eot := str

(** Some Translations. *)

module Inchannel = struct
  let of_string = function
    | "stdin" -> Pervasives.stdin
    | str -> Pervasives.open_in str

  let to_string inch =
    if inch == Pervasives.stdin then "stdin" else "<abst>"
	
end 

module Outchannel = struct
  let of_string = function
    | "stdout" -> Format.std_formatter
    | "stderr" -> Format.err_formatter
    | str -> Format.formatter_of_out_channel (Pervasives.open_out str)

  let to_string outch =
    if outch == Format.std_formatter then "stdout"
    else if outch == Format.err_formatter then "stderr"
    else "<abst>"
      
end 


(** Initialize. *)

let initialize pp ch inch outch =
  Pretty.flag := Pretty.Mode.Mixfix;
  Var.pretty := pp;
  eot := ch;
  inchannel := inch;
  outchannel := outch


let lexing () = Lexing.from_channel !inchannel

(** {6 Accessors} *)

let current_of () = !current
let symtab_of () = !symtab
let inchannel_of () = !inchannel
let outchannel_of () =  !outchannel
let prompt_of () = !prompt

let print_prompt () =
  Format.fprintf !outchannel "\n%s " !prompt;
  Format.pp_print_flush !outchannel ()


(** Entry from symbol table. *)
let entry_of n = 
  Symtab.lookup n !symtab

(** Type from the symbol table. *)
let type_of n =
  match Symtab.lookup n !symtab with
    | Symtab.Type(c) -> c
    | _ -> raise Not_found

let prop_of n =
  match Symtab.lookup n !symtab with
    | Symtab.Def(Symtab.Prop(p)) -> p
    | _ -> raise Not_found

(** Get context for name in symbol table *)
let context_of n = 
  match Symtab.lookup n !symtab with
    | Symtab.State(c) -> c
    | _ -> raise Not_found

(** Getting the width of bitvector terms from the signature. *)
let width_of a =
  match Bitvector.width a with
    | None when Term.is_var a -> 
	let n = Term.Var.name_of a in
	  (try
	    (match Symtab.lookup n !symtab with
	      | Symtab.Arity(i) -> Some(i)
	      | _ -> None)
	    with
		Not_found -> None)
    | res -> res


(** {6 Pretty-printing results} *)

let progress = ref false

module Out = struct

  let nothing () = 
    if !progress then Format.eprintf ".@?" else ()

  let nl () = 
    if !batch then nothing () else 
      Format.fprintf !outchannel "\n@?"

  let unit () =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":unit@?"

  let tt () =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":true@?"

  let ff () =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":false@?"

  let boolean = function
    | true -> tt()
    | false -> ff()

  let proofmode mode =
    if !batch then nothing () else
      Format.fprintf !outchannel ":%s@?" (Jst.Mode.to_string mode)
    
  let three res =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":%s@?"
	(Pretty.to_string Pretty.three res)
     
  let string str =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":string %s@?" str

  let none () = 
    if !batch then nothing () else 
      Format.fprintf !outchannel ":none@?"

  let trace sl =
    if !batch then nothing () else
      Format.fprintf !outchannel ":trace %s@?"
	(Pretty.to_string (Pretty.list Pretty.string) sl)

  let term0 a =
    if !batch then nothing () else 
      Format.fprintf !outchannel ":term %s@?" (Term.to_string a)

  let term (a, rho) =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt ":term "; Term.pp fmt a;
	if not(Jst.Mode.is_none()) then
	  begin
	    Format.fprintf fmt "\n:justification ";
	    Jst.pp fmt rho
	  end ;
	Format.fprintf fmt "@?"

  let dom (d, rho) =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt ":dom "; Dom.pp fmt d;
	if not(Jst.Mode.is_none()) then
	  begin
	    Format.fprintf fmt "\n:justification ";
	    Jst.pp fmt rho
	  end ;
	Format.fprintf fmt "@?"

  let fact (a, j) =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt "\n:atom "; 
	Atom.pp fmt a;
	if not(Jst.Mode.is_none()) then
	  begin
	    Format.fprintf fmt "\n:justification ";
	    Jst.pp fmt j
	  end;
	Format.fprintf fmt "@?"

  let ok n =
    Format.fprintf !outchannel ":ok ";
    Name.pp !outchannel n;
    Format.fprintf !outchannel "@?"

  let unsat j =
    let fmt = !outchannel in
      Format.fprintf fmt ":unsat @."; 
      if Jst.Mode.is_none() then () else Jst.pp fmt j;
      Format.fprintf fmt "@?" 

  let valid j =
    let fmt = !outchannel in
      Format.fprintf fmt ":valid ";
      if not(Jst.Mode.is_none()) then
	begin
	  Jst.pp fmt j
	end;
      Format.fprintf fmt "@?"
	
  let prop_unsat () =
    let fmt = !outchannel in
      Format.fprintf fmt ":unsat@?"
    
  let terms ts =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt ":terms "; 
	Pretty.set Term.pp fmt (Term.Var.Set.elements ts);
	Format.fprintf fmt "@?"

  let hypotheses hyps =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt "\n:atoms "; 
	Pretty.list Atom.pp fmt hyps;
	Format.fprintf fmt "@?"

 let splits atms =
    if !batch then nothing () else 
      let fmt = !outchannel in
	Format.fprintf fmt "\n:splits "; 
	Pretty.list Atom.pp fmt atms;
	Format.fprintf fmt "@?"

  let incomplete () = 
    if !batch then nothing () else 
      Format.fprintf !outchannel ":incomplete@?"

  let inconsistent () = 
    if !batch then nothing () else 
      Format.fprintf !outchannel ":unsat@?"

  let name n =
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel ":name ";
	Name.pp !outchannel n;
	Format.fprintf !outchannel "@?"
      end 

  let invalid msg =
    if !batch then nothing () else
      Format.fprintf !outchannel ":invalid %s @?" msg

  let assignment ml =
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel ":subst ";
	Term.Subst.pp !outchannel ml;
	Format.fprintf !outchannel "@?"
      end 

  let yes_or_no res =
    if !batch then nothing () else
      match res with
	| Jst.Three.X ->
	    Format.fprintf !outchannel ":unknown@?"
	| Jst.Three.Yes(rho) ->
	    Format.fprintf !outchannel ":yes ";
	    if not(Jst.Mode.is_none()) then
	      Jst.pp !outchannel rho;
	    Format.fprintf !outchannel "@?"
	| Jst.Three.No(rho) ->
	    Format.fprintf !outchannel ":no ";
	    if not(Jst.Mode.is_none()) then
	      Jst.pp !outchannel rho;
	    Format.fprintf !outchannel "@?"

  let prop_sat (n, rho) =
    Format.fprintf !outchannel ":sat ";
    Name.pp !outchannel n;
    Format.fprintf !outchannel "\n:model ";
    Prop.Assignment.pp !outchannel rho;
    Format.fprintf !outchannel "@?"

  let prop p = 
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel ":prop ";
	Prop.pp !outchannel p;
	Format.fprintf !outchannel "@?"
      end 

  let symtab tab =
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel ":symtab ";
	Symtab.pp !outchannel tab;
	Format.fprintf !outchannel "@?"
      end 

  let int n = 
    if !batch then nothing () else
      Format.fprintf !outchannel ":int %d@?" n

  let dom0 d =
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel ":dom ";
	Dom.pp !outchannel d;
	Format.fprintf !outchannel "@?"
      end 

  let cnstrnt0 c =
    if !batch then nothing () else
      begin
	Format.fprintf !outchannel "cnstrnt ";
	Var.Cnstrnt.pp !outchannel c;
	Format.fprintf !outchannel "@?"
      end

  let context ctxt =
     if !batch then nothing () else
       begin
	 Format.fprintf !outchannel ":state ";
	 Context.pp !outchannel ctxt;
	 Format.fprintf !outchannel "@?"
       end 

  let partition p =
     if !batch then nothing () else
       begin
	 Format.fprintf !outchannel ":partition ";
	 Partition.pp !outchannel p;
	 Format.fprintf !outchannel "@?"
       end 

  let error msg =
    let fmt = if !batch then Format.err_formatter else !outchannel in
      Format.fprintf fmt ":error %s@." msg

  let endmarker () =
    let fmt = !outchannel in
    let eot = !eot in
      if eot = "" then
	Format.pp_print_flush fmt ()
      else 
	begin
	  Format.fprintf fmt "\n%s" eot;
	  Format.pp_print_flush fmt ()
	end 

 let outch ch =
   if !batch then nothing () else
     Format.fprintf !outchannel ":outchannel %s@?" (Outchannel.to_string ch)

 let inch ch =
   if !batch then nothing () else
     Format.fprintf !outchannel ":inchannel %s@?" (Inchannel.to_string ch)

end


(** {6 Registration of commands} *)

let help_enabled = ref true

type help = 
  | All 
  | Command of string
  | Nonterminal of string

(** Registration of nonterminals for help command. *)
module Nonterminal = struct

  let registration = ref []

  let is_defined nt =
    List.mem_assoc nt !registration

  let register nt (alternatives: string list) description =
      begin
	assert(not(is_defined nt));
	registration := (nt, (alternatives, description)) :: !registration
      end 
      
  let definition_of nt =
    try
      List.assoc nt !registration
    with
	Not_found ->
	  raise(Invalid_argument
		  (Format.sprintf "No help available for nonterminal <%s>" nt))


  let rec pp fmt (nt, defs, descr) =
    Format.fprintf !outchannel "DEFINITION";
    List.iter
      (fun def -> 
	 Format.fprintf !outchannel "\n     <%s> ::= %s" nt def)
      defs;
    if descr <> "" then
      begin
	Format.fprintf !outchannel "\nDESCRIPTION";
	Format.fprintf !outchannel "\n%s" descr
      end
	
end


(** Commands are registered just for the purpose of producing
  consistent help texts for commands. *)
module Command = struct

  module Description = struct

    type description = {
      args : string;
      short : string;
      description : string;
      examples : (string * string) list;
      seealso : string
    }

    let mk_short args short = {
      args = args;
      short = short;
      description = "";
      examples = [];
      seealso = ""
    }

    let pp name fmt d =
      Format.fprintf !outchannel "NAME";
      Format.fprintf !outchannel "\n     %s - %s " name d.short;
      Format.fprintf !outchannel "\nSYNOPSIS";
      Format.fprintf !outchannel "\n     %s %s" name d.args;
      if d.description <> "" then
	Format.fprintf !outchannel "\nDESCRIPTION\n %s" d.description;
      if d.examples <> [] then
	 begin
	   Format.fprintf !outchannel "\n%s"
	     (if List.length d.examples <> 1 then "EXAMPLES" else "EXAMPLE");
	   List.iter
	     (fun (example, description) ->
		if description = "" then
		  Format.fprintf !outchannel "\n     %s" example
		else
		  Format.fprintf !outchannel "\n     %s - %s" example description)
	     d.examples
	 end;
      if d.seealso <> "" then
	Format.fprintf !outchannel "\nSEE ALSO\n %s" d.seealso;

  end

  open Description

  let descriptions = ref []

  let description_of name = 
    try
      List.assoc name !descriptions
    with
	Not_found -> raise (Invalid_argument (name ^ ": no such command@?"))

  let register name proc descr =
    if !help_enabled then
      begin
	assert(not(List.mem_assoc name !descriptions));
	descriptions := (name, descr) :: !descriptions;
      end;
    proc


  let rec help = function
    | All -> 
	helpall ()
    | Command(name) -> 
	help1 name
    | Nonterminal(nt) ->
	syntax nt

  and help1 name =
    Description.pp name !outchannel 
      (description_of name)
    

  and helpall () =
    let max = ref 10 in
      List.iter
	(fun (name, _) ->
	   let n = String.length name in
	     if n > !max then max := n)
	!descriptions;
      List.iter
	(fun (name, d) ->
	   let offset = ref (!max - String.length name) in
	     Format.fprintf !outchannel "\n%s" name;
	     while !offset >= 0 do 
	       offset := !offset - 1; Format.fprintf !outchannel " " 
	     done;
	     Format.fprintf !outchannel "%s" d.args)
	!descriptions

  and syntax nt = 
    let defn, descr = Nonterminal.definition_of nt in
      Nonterminal.pp !outchannel (nt, defn, descr)
 
     
end

open Command.Description
  
(** {6 Commands } *)

let do_help =
  Command.register "help"
    Command.help
    {args = "[<command>] | < <nonterminal> >"; 
     short = "Help about ICS interactor commands and syntactic categories.";   
     description = ""; 
     examples = [
       "help", "Display all commands"; 
       "help help", "Display this message"; 
       "help <term>", "Display definition of nonterminal <term>"; 
       "help assert", "Display description of command 'assert'"]; 
     seealso = ""}
    

    
let symtab_all () = Out.symtab !symtab
		  
let symtab1 n =
  try 
    (match Symtab.lookup n !symtab with
       | Symtab.Def(Symtab.Term(a)) -> Out.term0 a
       | Symtab.Def(Symtab.Prop(p)) -> Out.prop p
       | Symtab.Arity(n) -> Out.int n
       | Symtab.Type(c) -> Out.cnstrnt0 c
       | Symtab.State(s) -> Out.context s)
  with
      Not_found -> Out.none ()

let do_symtab =
  Command.register "symtab" 
    (function 
       | None -> symtab_all ()
       | Some(n) -> symtab1 n)
    {args = "[<ident>]";
     short = "Display symbol table entries"; 
     description = "
        [symtab] display the current symbol table, and
        [symtab x] displays the symbol table entry associated with [x].  
        Such an entry might either be a logical context entry, 
        a term definition, a definition of a proposition, or a signature entry 
        for domain restrictions of variables."; 
     examples = []; 
     seealso = ""}

  

(** Adding to symbol table *)

let do_def =
  Command.register "def"
    (fun (n, a) ->
       let e = Symtab.Def(a) in
	 symtab := Symtab.add n e !symtab;
	 Out.unit ())
    {args = "<ident> := <term>";
     short = "Extend symbol table with term definition"; 
     description = 
        "Extend the symbol table with a definition <ident> for a term <term>.
         In such a context, variables <ident> are macro-expanded to <term> 
         but different <term>s obtained from the same definition are structure-shared."; 
     examples = ["def x := y + z", ""]; 
     seealso = ""}


let do_prop =
  Command.register "prop"
    (fun (n, a) ->
       let e = Symtab.Def(Symtab.Prop(a)) in
	 symtab := Symtab.add n e !symtab;
	 Out.unit ())
    {args = "<ident> := <prop>";
     short = "Extend symbol table with definition for proposition"; 
     description = 
        "Extend the symbol table with a definition <ident> for a proposition <prop>.
         In such a context, variables <ident> are macro-expanded to <term> 
         but different <props>s obtained from the same definition are structure-shared."; 
     examples = []; 
     seealso = ""}
  
let do_sgn =
  Command.register "sig"
    (fun (n, a) ->
       let e = Symtab.Arity(a) in
	 symtab := Symtab.add n e !symtab;
	 Out.unit ())
    {args = "<ident> : <sig>";
     short = "Extend symbol table with signature declaraction"; 
     description = "
       Declare a variable to be interpreted over the set of 
       bitvectors of width [i] or the integers or the reals.
       Notice that only bitvector variables have to be declared before use,
       since context information is used for inferring parameters when applying
       infix bitvector operators."; 
     examples = [
        "sig x, y : int", "Declare [x] and [y] to be integer variables";
        "sig x : bitvector[5]", "Declare [x] to be a bitvector or length [5]"]; 
     seealso = ""}

let do_typ =
  Command.register "typ"
    (fun (nl, c) ->
       let e = Symtab.Type(c) in
	 List.iter
	   (fun n ->
	      symtab := Symtab.add n e !symtab)
	   nl;
	 Out.unit ())
    {args = "<ident>, ..., <ident> : <dom>";
     short = "Extend symbol table with type declarations"; 
     description = "" ; 
     examples = []; 
     seealso = ""}

(** Getting either current context or explicitly specified context. *)

let get_context = function
  | None -> !current
  | Some(n) -> context_of n

(** Set input and output channels. *)

let do_set_inchannel =
  (fun ch ->
     inchannel := ch;
     Out.unit ())

let do_set_outchannel fmt = 
  outchannel := fmt;
  Out.unit ()


(** Context. *)

let do_cmp =
  Command.register "cmp"
    (fun (a, b) -> Out.int (Term.cmp a b))
    {args = "<term> <term>";
     short = "Test term ordering."; 
     description = "" ; 
     examples = []; 
     seealso = ""}


let do_ctxt =
  Command.register "ctxt"
    (fun n ->
       let hyps = match n with 
	 | None -> Context.ctxt_of !current
	 | Some(n) -> Context.ctxt_of (context_of n)
       in
	 Out.hypotheses hyps)
    {args = "[@<ident>]";
     short = "Output logical context."; 
     description = 
          "'ctxt' returns the set of atoms asserted in the current 
           logical context, and 'ctxt@s' returns the set of asserted
           atoms in state 's' from the symbol table. These atoms are not necessarily 
           in canonical form."; 
     examples = []; 
     seealso = ""}

let do_show = 
  Command.register "show"
    (fun n th ->
       let s = match n with
	 | None -> !current
	 | Some(n) -> (context_of n) 
       in
	 match th with
	   | None -> Out.context s
	   | Some(th) -> 
	       (match th with
		  | None -> Partition.pp !outchannel (Context.partition_of s)
		  | Some(i) -> Combine.pp i !outchannel (Context.eqs_of s)))
    {args = "[@<ident>] [<th>]'";
     short = "Output partition and theory-specific equality sets."; 
     description = "" ; 
     examples = []; 
     seealso = ""}
      

(** Canonization w.r.t current state. *)

let do_can =
  Command.register "can"
    (fun a ->
       Out.term (Combine.can (Context.config_of !current) a))
    {args = "<term>";
     short = "Canonical term w.r.t. current context."; 
     description =
        "For a term 'a', 'can a' returns a term which is a canonical representative 
        of the equivalence class of [a] as induced by the atoms in the current 
        context. If proof generation is enabled, then also a justification of the
        equality between 'a' and 'can a' is returned. There are no side effects.";
     examples = [];
     seealso = "simplify"}
    

    
let do_sigma =
  Command.register "sigma"
    (fun a ->
       Out.term (Jst.Eqtrans.id a))
    {args = "<term>";
     short = "Theory-specific canonization"; 
     description = 
	"[sigma a] computes the normal form of a term using
         theory-specific canonizers for terms in interpreted 
         theories. This command leaves the current state unchanged."; 
     examples = []; 
     seealso = ""}

let do_simplify =
  Command.register "simplify"
    (fun a ->
       Out.fact (Combine.simplify (Context.config_of !current) a))
    {args = "<atom>";
     short = "Simplification of atoms."; 
     description = 
       "[simplify a] returns an atom equivalent to [b] in the current
        context. If proofmode is enabled, then, in addition, a justification
        of this equivalence is returned."; 
     examples = []; 
     seealso = ""}
	

(** Create a fresh name for a state. *)

let rec fresh_state_name () =
  counter:=  !counter + 1;
  let n = Name.of_string ("s" ^ (string_of_int !counter)) in
  try
    let _ = Symtab.lookup n !symtab in  (* make sure state name is really fresh. *)
    fresh_state_name ()
  with
      Not_found -> 
	n

(** Change current state. *)

let save_state arg =
  let n = match arg with
    | None -> fresh_state_name ()
    | Some(n) -> n
  in
  let e = Symtab.State !current in
    symtab := Symtab.add n e !symtab;
    n
  
let do_save =
  Command.register "save"
    (fun arg -> 
       Out.name (save_state arg))
    {args = "[<ident>]";
     short = "Save current state in symbol table."; 
     description = 
       "[save s] adds a symbol table entry [s] for the current logical state" ; 
     examples = []; 
     seealso = "symtab"}
    

let do_restore =
  Command.register "restore"
    (fun n -> 
       try
	 match Symtab.lookup n !symtab with
	   | Symtab.State(t) -> 
	       current := t;
	       Out.unit ()
	   | _ -> 
	       raise Not_found
	 with
	     Not_found -> Out.invalid (Pretty.to_string Name.pp n ^ ": not a state name"))
    {args = "<ident>";
     short = "Restore logical state "; 
     description = 
        "[restore s] updates the current logical state to be the 
         state named by [s] in the symbol table." ; 
     examples = []; 
     seealso = "symtab"}

let do_remove =  
  Command.register "remove"
    (fun n ->
       symtab := Symtab.remove n !symtab;
       Out.unit ())
    {args = "<ident>";
     short = "Remove logical state from symbol table"; 
     description = "[remove s] removes the symbol table entry for  name [s]"; 
     examples = []; 
     seealso = "symtab"}


let do_forget =
  Command.register "forget"
    (fun () ->
       current := Context.empty;
       Out.unit ())
    {args = "";
     short = "Clear out current state"; 
     description = 
       "Resets the current logical context to the empty 
        context. In contrast to 'reset', all other
        ICS data structures are left unchanged" ; 
     examples = []; 
     seealso = "reset"}

let do_trace =
  Command.register "trace"
    (fun levels ->
       List.iter Trace.add levels;
       Out.trace (Trace.get ()))
    {args = "<levels>";
     short = "Enable tracing";   
     description = "Tracing of various levels from the
      top-level rules of the Shostak integration down to
      updates of individual solution sets."; 
     examples = [
       "trace rule", "Trace top-level integration methods";
       "trace a", "Trace updates in linear arithmetic";
       "trace la", "Trace Simplex methods";
       "trace v", "Trace updates in variable partitioning"]; 
     seealso = "untrace, <levels>"}
    

let do_untrace =
  Command.register "untrace"
    (fun levels ->
       (match levels with
	  | None -> Trace.reset ()
	  | Some(strs) -> List.iter Trace.remove strs);
       Out.trace (Trace.get ()))
    {args = "[<levels>]";
     short = "Disable tracing"; 
     description = "The given trace levels are disabled.
       When no arguments are supplied, all tracing is disabled."; 
     examples = []; 
     seealso = "trace, reset, <levels>"}
    


(** Adding a new fact *)

let do_process1 = 
  Command.register "assert"
    (fun (n, a) ->
       let t = get_context n in
	 match Context.add t a with  (* Update state and install new name in symbol table *)
	   | Context.Status.Ok(t') -> 
	       current := t';
	       let n = save_state None in Out.ok n
	   | Context.Status.Valid(rho) ->  Out.valid rho
	   | Context.Status.Inconsistent(rho) ->
	       begin
		 Out.unsat rho;
		 if !batch then raise End_of_file else ()
	       end)
    {args = "[@<ident>]  <atom>,...,<atom>";
     short = "Add an atom to a context"; 
     description = "
         An <atom> is asserted to the specified context. If <ident> 
         is omitted, then the current context is used, and otherwise
         the context of name <ident> in the symbol table is extended.
         There are three possible outcomes. First, <atom> is inconsistent with respect 
         to the current context. In this case, assert leaves the current context
         unchanged and outputs ':unsat' on the standard
         output. <atom> is valid in the current context. Again, the
         the current context is left unchanged and ':valid' is output.
         Otherwise, in case <atom> has neither been shown to be valid
         nor inconsistent in the specified context, the current context is 
         modified to include new information obtained from <atom>.  
         In addition, a new name 'si' is generated for this context and a symbol 
         table entry is added for this name. The result is of the form ':ok si}'" ; 
     examples = ["
       ics> assert f(v) = v.
       :ok s1
       ics> assert f(u) = u - 1.
       :ok s2
       ics> assert u = v.
       :unsat", "The conjunction of these three assertions is inconsistent"]; 
     seealso = "symtab, split"}

let do_process (n, al) =
  let t = get_context n in
    match Context.addl t al with  (* Update state and install new name in symbol table *)
      | Context.Status.Ok(t') -> 
	  current := t';
	  let n = save_state None in Out.ok n
      | Context.Status.Valid(rho) ->  Out.valid rho
      | Context.Status.Inconsistent(rho) ->
	  begin
	    Out.unsat rho;
	    if !batch then raise End_of_file else ()
	  end


let do_valid =
  Command.register "valid"
  (fun (n, al) ->
     if Context.is_valid (get_context n) al then Out.tt () else Out.ff ())
  {args = "[@<ident>]  <atom>,...,<atom>";
   short = "Test if the conjunction of <atom>s is valid in context <ident>."; 
   description = "" ; 
   examples = []; 
   seealso = "assert, unsat"}


let do_unsat =
  Command.register "unsat"
    (fun (n, al) ->
       if Context.is_inconsistent (get_context n) al then 
	 Out.tt ()
       else 
	 Out.ff ())
    {args = "[@<ident>]  <atom>,...,<atom>";
     short = "Test if the conjunction of <atom>s is unsatisfiable in context <ident>."; 
     description = "" ; 
     examples = []; 
     seealso = "assert, valid"}


let do_model =
  Command.register "model"
    (fun (n, xs) ->
       try
	 let m = Combine.model (Context.config_of (get_context n)) xs in
	 let l = Term.Map.fold (fun x a acc -> (x, a) :: acc) m [] in
	   Out.assignment l
       with
	   Not_found -> Out.none ())
    {args = "[@<ident>]  <nameset>";
     short = "Assignment with domain <nameset>, extendable to a model."; 
     description = "" ; 
     examples = []; 
     seealso = ""}

let do_check_sat =
  Command.register "check"
    (fun n ->
       let s = get_context n in
	 match Context.check_sat s with
	   | None ->
	       Out.prop_unsat ()
	   | Some(splits', s') ->   
	       let sn = fresh_state_name() in
		 symtab := Symtab.add sn (Symtab.State(s')) !symtab;
		   Out.ok sn;
		   Out.splits splits')
    {args = "[@<ident>]";
     short = "Perform complete case-split.";
     description = "Check returns  ':unsat' if all case-splits fail and ':ok s'
       if one complete case split is found to be satisfiable. In the latter case,
       's' is a new name in the symbol table for the extension of the current
       context with all the case splits.";
     examples = []; 
     seealso = ""}


(** Accessors. *)

let do_diseq =
  Command.register "diseq"
    (fun (n, a) ->
       let s = get_context n in
       let (b, rho1) = Combine.can (Context.config_of s) a in    (* [rho1 |- a = b] *)
	 try
	   let ds = Partition.diseqs (Context.partition_of s) b in
	     D.Set.iter
	       (fun (x, rho2) ->        (* [rho2 |- x <> b] *)
		  let rho = Jst.dep2 rho2 rho1 in
		    Out.term (x, rho))
	       ds
	 with
	     Not_found -> Out.none ())
    {args = "[@<ident>]  <term>";
     short = "Return known disequalities."; 
     description = 
       "Returns a list of variables known to be disequal to <term> in
        the context <ident> or the current context if <ident> is not
        specified. In addition, in proof generation mode, justifications
        for each disequality are returned" ; 
     examples = []; 
     seealso = ""}


let do_dom =
  Command.register "dom"
    (fun (n, a) ->
       let s = get_context n in
	 try
	   let (d, rho) = Combine.dom (Context.config_of s) a in
	     Out.dom (d, rho)
	 with
	     Not_found -> Out.none ())
  {args = "[@<ident>]  <term>";
   short = "Return domain constraint for <term>."; 
   description = "" ; 
   examples = []; 
   seealso = ""}


let do_sup =
  Command.register "sup"
  (fun (n, a) ->
     let s = get_context n in
       try
	 let (b, rho) = Combine.maximize (Context.config_of s) a in
	   Out.term (b, rho)
       with
	   La.Unbounded -> Out.none ())
  {args = "[@<ident>]  <term>";
   short = "Maximize term"; 
   description = 
       "[sup a] returns a term [b] of the form [c0 - d1*x1 - ... - dn*xn] with
        [di > 0] and all the [xi] slack variables or it returns
        [:unbounded]. In the former case, [a] is maximized at [c0].
        Also, a justification of the equality [a = b] in the current context 
        is returned. " ; 
   examples = []; 
   seealso = ""}


let do_inf =
  Command.register "inf"
    (fun (n, a) ->
       let s = get_context n in
	 try
	   let (b, rho) = Combine.minimize (Context.config_of s) a in
	     Out.term (b, rho)
	 with
	     La.Unbounded -> Out.none ())
  {args = "[@<ident>] <term>";
   short = "Minimize term"; 
   description =
       "[inf a] returns a term [b] of the form [c0 + d1*x1 + ... + dn*xn] with
        [di > 0] and all the [xi] slack variables or it returns
        [:unbounded]. In the former case, [a] is minimized at [c0].
        Also, a justification of the equality [a = b] in the current 
        context is returned. " ; 
   examples = []; 
   seealso = ""}


let do_split =
  Command.register "split"
    (fun n ->
       let s = get_context n in
	 (try
	    let spl = Combine.split (Context.config_of s) in
	     Combine.Split.pp !outchannel spl
	  with
	      Not_found -> 
		Format.fprintf !outchannel ":none");
	 Format.fprintf !outchannel "@?")
    {args = "[@<ident>]";
     short = "Suggested case splits."; 
     description = "" ; 
     examples = []; 
     seealso = ""}


(** Applying maps. *)

let do_find =
  Command.register "find"
    (fun (n, th, x) ->
       let s = get_context n in
       let (b, rho) = match th with
	 | Some(i) -> Combine.find (Context.eqs_of s) i x
	 | None -> Partition.find (Context.partition_of s) x
       in
	 Out.term (b, rho))
    {args = "[@<ident>] <th> <term> ";
     short = "Return theory-specific interpretation for a variable."; 
     description = 
        "If the equality [x = t] is in the solution set [<th>] , say [a], 
         then [find a x] returns [t] and otherwise [x]. The addressing, 
         say, [find@s1 a x] may be used to address the solution set for the 
         arithmetic theory in the context [s1] in the symbol table. In case,
         addressing is omitted, the current context is investigated."; 
     examples = []; 
     seealso = ""}

let do_inv =
  Command.register "inv"
    (fun (n, i, b) -> 
       try
	 Out.term (Combine.inv (Context.config_of (get_context n)) b)
       with
	   Not_found -> Out.none ())
    {args = "[@<ident>] <th> <term> ";
     short = "Returns a variable for interpreted terms."; 
     description = "
        If the equality [x = t] is in the solution set <th>, say,
        [a], then [inv a x] returns [x] and otherwise [:none]. Thus, 
        whenever [inv] is defined, it is inverse to [find]. The addressing 
        [inv@s1 a x] may be used to address the solution set for, say, 
        the arithmetic theory in the context \texttt{s1} in the symbol table."; 
     examples = []; 
     seealso = ""}

      
let do_dep =
  Command.register "dep"
    (fun (n, i, a) -> 
       Out.terms (Combine.dep (Context.eqs_of (get_context n)) i a))
    {args = "[@<ident>] <th> <term> ";
     short = "Return the lhs dependencies for variable <term> in solution set <th>."; 
     description = "" ; 
     examples = []; 
     seealso = ""}


(** Solver. *)

let do_solve =
  Command.register "solve"
    (fun (i, e) -> 
       try
	 let al = Combine.solve i e in
	   Out.assignment al
       with
	 | Exc.Inconsistent -> Out.inconsistent ()
	 | Exc.Incomplete -> Out.incomplete ())
 {args = "<th> <term> = <term> ";
     short = "Solve a term equality"; 
     description = "
        Theory-specific solver for input equality. Returns either a solved 
        list of equalities with variables on the lhs which is, in the given theory,
        equivalent to the input equality or [:unsat] if the input equality is
        unsatisfiable.";
     examples = 
       ["solve a x + 2 = y - 3", "Solve equality in linear arithmetic";
        "solve p car(x) = cons(u, v)", "Solve in the pair theory";
        "solve bv x2 ++ 0b10 = 0b10 ++ x2", "Solve in bitvector theory"];
     seealso = ""}

 
(** Equality/disequality test. *)

let do_is_equal =
  Command.register "is_equal"
    (fun (a, b) ->
       Out.yes_or_no 
       (Jst.Rel2.apply 
	  (Combine.can (Context.config_of !current))
	  (Combine.is_equal_or_diseq (Context.config_of !current))
	  a b))
    {args = "<term> <term> ";
     short = "Test whether terms are equal or disequal."; 
     description = 
        "[is_equal a b] tests whether [a] and [b] are equal in the
         current logical state. If proof generation is enabled, a 
         justification is displayed." ; 
     examples = []; 
     seealso = ""}


(** Sat solver *)

let do_sat =
  Command.register "sat"
    (fun (n, p) ->
       let s = get_context n in
       match Prop.sat s p with
	 | None -> 
	     Out.prop_unsat ()
	 | Some(rho, s') -> 
	     let n = fresh_state_name () in
	       symtab := Symtab.add n (Symtab.State(s')) !symtab;
	       Out.prop_sat (n, rho))
    {args = "[@<ident>] <prop> ";
     short = "SAT Solver for propositional constraints."; 
     description = 
        "A satisfiability solver for propositional formulas over atoms.
         Returns [:unsat] if the formulas has been shown to be unsatisfiable or
         [:sat} together with an assignment to the Boolean variables and the
         truth values of the atoms in a satisfying assignment.  In addition, a name
         is added in the symbol table for the state corresponding to the conjunction of 
         the atoms in a satisfying assignment, but the current logical state is unchanged.";
     examples = 
       ["sat x | y | [z & ~x] # y", "Boolean SAT problem";
	"sat x > y & [y = 2 # ~[x <> 3]]", "Boolean constraint SAT problem"];
     seealso = ""}
      

(** Errors *)
	
let do_error = Out.error 

let do_parse_error n = 
  Out.error ("Parse error on line" ^ string_of_int n)

let do_quit =
  (fun n ->
     Tools.do_at_exit();
     Out.endmarker())

(** {6 Variables} *)


module Parameters = struct 

  type t = 
    | Compactify
    | Pretty
    | Statistics
    | Proofmode
    | Justifications
    | Inchannel
    | Outchannel
    | Eot
    | Prompt
    | IntegerSolve
    | Crossmultiply
    | Index
    | Clock

  let to_string = function
    | Compactify -> "compactify"
    | Pretty -> "pretty"
    | Statistics -> "statistics"
    | Proofmode -> "proofmode"
    | Justifications -> "justifications"
    | Inchannel -> "inchannel"
    | Outchannel -> "outchannel"
    | Eot -> "eot"
    | Prompt -> "prompt"
    | IntegerSolve -> "integersolve"
    | Crossmultiply -> "crossmultiply"
    | Index -> "index"
    | Clock -> "clock"

  let of_string = function
    | "compactify" -> Compactify
    | "pretty" -> Pretty
    | "statistics" -> Statistics
    | "proofmode" -> Proofmode
    | "justifications" -> Justifications
    | "inchannel" -> Inchannel
    | "outchannel" -> Outchannel
    | "eot" -> Eot
    | "prompt" -> Prompt
    | "integersolve" -> IntegerSolve
    | "crossmultiply" -> Crossmultiply
    | "index" -> Index
    | "clock" -> Clock
    | str -> raise(Invalid_argument (str ^ " : no such variable"))

  let get var =
    match var with
      | Compactify -> string_of_bool !V.garbage_collection_enabled
      | Pretty ->  Pretty.Mode.to_string !Pretty.flag
      | Statistics ->  string_of_bool !Prop.statistics
      | Proofmode ->  Jst.Mode.to_string (Jst.Mode.get())
      | Justifications -> string_of_bool !Fact.print_justification
      | Inchannel -> Inchannel.to_string !inchannel
      | Outchannel -> Outchannel.to_string !outchannel
      | Eot -> !eot
      | Prompt -> !prompt
      | Index -> string_of_bool !Eqs.pp_index
      | IntegerSolve -> string_of_bool !Arith.integer_solve
      | Crossmultiply -> string_of_bool !Atom.crossmultiply
      | Clock -> 
	  let times =  Unix.times() in
	  let utime =  times.Unix.tms_utime in
	  let systime = times.Unix.tms_stime in
	    Format.sprintf "utime = %f, systime = %f" utime systime
	
  let set var value =
    match var with
      | Compactify -> V.garbage_collection_enabled := bool_of_string value
      | Index -> Eqs.pp_index := bool_of_string value
      | Statistics -> Prop.statistics := bool_of_string value
      | Proofmode -> Jst.Mode.set (Jst.Mode.of_string value)
      | Justifications -> Fact.print_justification :=  bool_of_string value
      | Inchannel -> inchannel := Inchannel.of_string value
      | Outchannel -> outchannel := Outchannel.of_string value
      | Eot -> eot := value
      | Prompt -> prompt := value 
      | IntegerSolve -> Arith.integer_solve := bool_of_string value
      | Crossmultiply -> Atom.crossmultiply := bool_of_string value
      | Pretty -> Pretty.flag := Pretty.Mode.of_string value
      | Clock -> invalid_arg "Can not reset clock"
	

  let reset () = 
    set Compactify "true";
    set Statistics "false";
    set Proofmode "dep";
    set Justifications "false";
    set Inchannel "stdin";
    set Outchannel "stdout";
    set Eot "";
    set Prompt "ics> ";
    set IntegerSolve "true";
    set Crossmultiply "false";
    set Index "false";
    set Pretty "mixfix"

  let show fmt var =
    Format.fprintf fmt "\n%s = %s" (to_string var) (get var)

  let iter f =
    f Compactify;
    f Pretty;
    f Statistics;
    f Proofmode;
    f Justifications;
    f Inchannel;
    f Outchannel;
    f Eot;
    f Prompt;
    f Index;
    f IntegerSolve;
    f Crossmultiply


  let description = function
    | Compactify -> "Enables garbage collection of noncanonical, internal variables"
    | Pretty -> "Enables infix/mixfix printing and suppression of domain restrictions"
    | Statistics -> "Print statistics of propositional SAT solver"
    | Proofmode -> "Setting level of proofmode (No = no proofs, Yes = proofs, Dep = only dependencies"
    | Justifications -> "Enables printing of justifications"
    | Inchannel -> "Current value of input channel"
    | Outchannel -> "Current value of output channel"
    | Eot -> "End of terminal marker sent at end of every transmission"
    | Prompt -> "Value of prompt"
    | IntegerSolve -> "Enable integer solver"
    | Index -> "Enable printing of indices"
    | Clock -> "Current user and system time"
    | Crossmultiply -> "Enable crossmultiplication"
      
end


let do_get =
  Command.register "get"
    (fun var -> 
       try
	 Out.string (Parameters.get var);
	 Out.nl();
	 Out.unit()
       with

	   exc -> Out.error(Printexc.to_string exc))
    {args = "[<parameter>] ";
     short = "Get current value for <parameter>. "; 
     description = "" ; 
     examples = []; 
     seealso = "<parameter>"}


let do_set =
  Command.register "set"
    (fun (var, value) ->
       try
	 Parameters.set var value;
	 Out.unit()
       with
	   exc -> Out.error(Printexc.to_string exc))
    {args = "[<parameter> := <value>] ";
     short = "Set <value> for <parameter>."; description = "" ; examples = []; seealso = ""}


let do_show_vars =
  (fun () ->
     Parameters.iter (Parameters.show !outchannel);
     Format.fprintf !outchannel "\n";
     Out.unit ())

		   
(** Resetting all of the global state. *)

let do_reset = 
  Command.register "reset"
    (fun () -> 
       Tools.do_at_reset ();
       current := Context.empty;
       symtab := Symtab.empty();
       Parameters.reset ();
       counter := 0;
       Out.unit ())
    {args = "";
     short = "Reset ICS state."; 
     description = 
       "Reinitializes all internal data structures including
        setting the current logical context to the empty context
        and the symbol table is emptied out." ; 
     examples = []; 
     seealso = ""}



(** Only protect logical context. *)
let protect f a =
  let save_current = !current
  and save_symtab = !symtab 
  and save_counter = !counter in
    try
      let b = f a in
	current := save_current;
	symtab := save_symtab;
	counter := save_counter;
	b
    with
	exc ->
	  current := save_current;
	  symtab := save_symtab;
	  counter := save_counter;
	  raise exc




(** Sorting commands in alphabetic order. *)
let _ =
  Command.descriptions :=
    (List.sort
       (fun (n1, _) (n2, _) -> Pervasives.compare n1 n2)
       !Command.descriptions)


(** Register nonterminals. *)

let _ = 
  Nonterminal.register "term"
    ["<var>";
     "<app>";
     "(<term>)";
     "<arith>";
     "<array>";
     "<bv>";
     "<list>";
     "<apply>";
     "<propset>"]
    "A <term> is either a variable <var> or an application <app>,
     and an application is of the form 'f(a1,...,an)' with 'f' a
     function symbol and [ai] terms. Besides application in prefix notation,
     there are predefined infix and mixfix variations for
     function symbols of various builtin theories such as
     linear arithmetic terms (<arith>), functional arrays (<array>),
     bitvectors (<bv>), coproducts (<coproduct>), lists (<list>), and
     functional abstraction and application (<apply>), and propositional
     sets (<propset>)."

let _ = 
  Nonterminal.register "var"
    ["<ident>";
     "<ident>{<dom>}";
     "<ident>!<int>";
     "<ident>!<int>{<dom>}";
     "!<int>"]
    "An external variable is either an identifier or
     an identifier followed by a domain restriction. Such
     a restriction specifies the variable to be interpreted
     over the specified domain <dom>.  Notice that domain restrictions 
     may also be specified, once and forall, using type declarations
     in the symbol table.  For example, after declaring, [sig x : int],
     every occurrence of the external variable [x] is identified with
     the restricted form [x{int}]. Names of internal variables
     consist of an identifier and an index separated by '!'. 
     These internal names are usually generated by the ICS engine.
     As with external variables, the domain of variables can be restricted.
     Bound variables (as used, for example, for building terms in the
     theory <apply> of functional application and abstraction) are just
     deBruijn indices of the form '!' followed by an integer."

let _ = 
  Nonterminal.register "app"
    ["<funsym>(<term>,...,<term>)"; "<constsym>"]
    "A term application in prefix form is either of the 
     form 'f(a1,...,an)' with 'f' a function symbol or simply
     a constant 'c' with 'c' a constant symbol."

let _ = 
  Nonterminal.register "funsym"
    ["<ident>"; 
     "+"; "-"; "*"; "expt[<int>]"; 
     "cons"; "car"; "cdr";
     "inl"; "inr"; "outl"; "outr"; "out[<out>]"; "inj[<int>]";
     "conc[<int>,<int>]"; 
     "sub[<int>,<int>,<int>]";
     "$"; "lambda"]
    "A function symbol <ident> is assumed to be uninterpreted,
     and all other function symbol are interpreted in some
     theory <th>.  The function symbols '+', '-', '*', 'expt[n]'
     are interpreted in linear ('a') or nonlinear arithmetic 'nl'.
     'cons', 'car', and 'cdr' are the function symbols of the pair
     theory 'p', and 'inl', 'inr', 'outl', 'outr', 'inj', out' 
     are function symbols in the theory 'cop' of direct sums (or copairs).
     The indexed function symbols 'conc[i, j]' and 'sub[n, i, j]'
     of the bitvector theory 'bv' are interpreted as concatenation
     of a bitvector of length 'i' with a bitvector of length 'j'
     and extraction of a bits 'i' through 'j', respectively.
     The function symbol '$' in the theory 'app' denotes function
     application and 'lam' is used for functional abstraction."


let _ = 
 Nonterminal.register "constsym"
   ["<rat>"; "true"; "false"; "0b[1|0]*"]
   "A constant symbol is either a rational number of the form <rat>
    or a bitvector constant such as '0b1001'. 'true' and 'false'
    are just mnemonics for '0b1' and '0b0', respectively."

let _ = 
  Nonterminal.register "list"
    ["<term> :: <term>";
     "hd(<term>)";
     "tl(<term>)";
     "nil"]
    ""
let _ = 
  Nonterminal.register "apply"
    ["<term> $ <term>"]
    ""

let _ = 
  Nonterminal.register "arith"
    ["<term> + <term>";
     "<term> - <term>";
     "<term> * <term>";
     "-<term>";
     "<term> / <term>";
     "<term> ^ <int>"]
    ""

let _ = 
  Nonterminal.register "propset"
    ["empty";
     "full";
      "<term> union <term>";
     "<term> inter <term>";
     "compl <term>"]
    ""

let _ = 
  Nonterminal.register "array"
    ["create(<term>";
     "<term>[<term> := <term>]";
     "<term>[<term>]"]
    ""
let _ = 
  Nonterminal.register "bv"
    ["<term> ++ <term>";
     "<term>[<int>:<int>]"]
    "'a ++ b' denotes the concatenation of a bitvector term 'a'
     with a bitvector term 'b', and 'a[i:j]' the extraction of the
     bits 'i' through 'j' in a bitvector term 'a'. These mixfix
     operators assume that the width of 'a' and 'b' can be 
     computed from its nonbitvector parts.  Width of variables, 
     for example, can be declared using symbol table declarations
     such as 'sig x : bitvector[5]'"

let _ = 
  Nonterminal.register "ident"
    ["[A-Z | a-z][A-Z | a-z | ' |_ | 0-9]*"]
    "Identifiers are all the above minus keywords such as commands
     and identifiers that have been used up by other means"

let _ = 
  Nonterminal.register "int"
    ["[0-9]*"]
    "Natural numbers"


let _ = 
  Nonterminal.register "rat"
    ["<int>/<int> "]
    "Rational numbers"

let _ = 
  Nonterminal.register "prop"
    ["[<prop>]";
     "<ident>";
     "<prop> & <prop>";
     "<prop> | <prop>";
     "<prop> <=> <prop>";
     "<prop> => <prop>";
     "~<prop>";
     "if <prop> then <prop> else <prop> end"]
    "Propositions are either propositional variables of the form <ident>
     or built up from propositional connectives such as conjunction '&',
     disjunction '|', negation '~', implication '=>', biimpliciation '<=>',
     or the conditional construct."

let _ = 
  Nonterminal.register "atom"
    ["ff";
     "tt";
     "<term> = <term>";
     "<term> <> <term>";
     "<term> < <term>";
     "<term> > <term>";
     "<term> <= <term>";
     "<term> >= <term>";
     "<term> sub <term>"]
   "An atom is either a constant atom, an equality, a disequality,
    one of the arithmetic inequality constraints, or a subset constraint."

let _ = 
  Nonterminal.register "dom"
    ["int"; "real"]
    "The integer and real domains"

let _ = 
  Nonterminal.register "th"
    (Th.fold (fun i acc -> Th.to_string i :: acc) [])
    "Builtin equality theories"


let _ =
  Nonterminal.register "parameters"
    (let params = ref [] in
       Parameters.iter
	 (fun x ->  params := Parameters.to_string x :: !params);
       !params)
    (let str = ref "" in
       Parameters.iter
	 (fun x ->
	    let entry = Parameters.to_string x ^ "\t" ^ Parameters.description x ^ "\n" in
	    str := entry ^ !str);
       !str)

let _ = 
  Nonterminal.register "levels"
     (List.map fst !Trace.registered)
     (List.fold_right
	(fun (name, descr) acc ->
	   let entry = "     " ^ name ^ "\t" ^ descr ^ "\n" in
	     entry ^ acc)
	!Trace.registered "")
