(*
 * The contents of this file are subject to the ICS(TM) Community Research
 * License Version 2.0 (the ``License''); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.icansolve.com/license.html.  Software distributed under the
 * License is distributed on an ``AS IS'' basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied. See the License for the specific language
 * governing rights and limitations under the License.  The Licensed Software
 * is Copyright (c) SRI International 2003, 2004.  All rights reserved.
 * ``ICS'' is a trademark of SRI International, a California nonprofit public
 * benefit corporation.
 *)

module Inchannel = 
  Ref.Make(Tools.Inchannel)
    (struct
       type t = Tools.Inchannel.t
       let name = "inchannel"
       let default = stdin
       let description =  "Current value of input channel"
     end)

module Outchannel = 
  Ref.Make(Tools.Outchannel)
    (struct
       type t = Tools.Outchannel.t
       let name = "outchannel"
       let default = Format.std_formatter
       let description =  "Current value of output channel"
     end)

module Eot = 
  Ref.String
    (struct
       type t = string
       let name = "eot"
       let default = ""
       let description =  "End of terminal marker sent at end of every transmission"
     end)


let current = ref Context.empty

let previous = ref Context.empty

let batch = ref false


let progress = ref false

module Out = struct

  let nothing () = 
    if !progress then Format.eprintf ".@?" else ()

  let nl () = 
    if !batch then nothing () else 
      let out = Outchannel.get() in
      Format.fprintf out "\n@?"

  let start str =
    if !batch then nothing () else
      Format.fprintf (Outchannel.get()) ":%s @?" str

  let final () =
    Format.fprintf (Outchannel.get()) "@."

  let exc e = 
    Format.fprintf (Outchannel.get()) ":error %s@." 
      (Printexc.to_string e)

end

(** {6 Commands } *)

open Help

type help = 
  | All 
  | Command of string
  | Nonterminal of string

let do_help =
  Register.command "help"
    (function
       | All -> Help.commands ()
       | Command(name) -> Help.command name
       | Nonterminal(nt) -> Help.syntax nt)
    {args = "[<command>] | < <nonterminal> >"; 
     short = "Help about ICS interactor commands and syntactic categories.";   
     description = ""; 
     examples = [
       "help", "Display all commands"; 
       "help help", "Display this message"; 
       "help <term>", "Display definition of nonterminal <term>"; 
       "help assert", "Display description of command 'assert'"]; 
     seealso = ""}

let do_symtab =
  Register.command "symtab" 
    (function 
       | None -> 
	   Out.start "symtab";
	   Symtab.pp (Outchannel.get());
	   Out.final ()
       | Some(n) ->
	   try
	     let e = Symtab.lookup n in
	       Out.start "entry";
	       Symtab.Entry.pp (Outchannel.get()) e;
	       Out.final()
	   with
	       Not_found -> Out.start "none"; Out.final())
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


type define = 
  | Term of Name.t list * Term.t
  | Prop of Name.t list * Prop.t
  | Spec of Spec.t
  | Context of Context.t

let do_define =
  Register.command "def"
    (fun (n, def) ->
       Out.start "def";
       (match def with
	 | Term(xl, a) -> Symtab.Put.term n xl a
	 | Prop(xl, p) -> Symtab.Put.prop n xl p
	 | Spec(sp) -> Symtab.Put.spec n sp
	 | Context(ctxt) -> Symtab.Put.context n ctxt);
       Out.final())
    {args = "<ident> ['(' <ident>,...,<ident> ')'] := (<term> | <prop>)";
     short = "Extend symbol table with term definition"; 
     description = 
        "Extend the symbol table with a name <ident> and optional arguments
         [<args>] for a term <term>.
         In such a context, variables <ident> is macro-expanded to <term> 
         but different <term>s obtained from the same definition are structure-shared."; 
     examples = ["def x := y + z", ""]; 
     seealso = ""}




(** Getting either current context or explicitly specified context. *)

let context = function
  | None -> !current
  | Some(n) -> Symtab.Get.context n

let config n = Context.config (context n)


(** Context. *)

let do_cmp =
  Register.command "cmp"
    (fun (a, b) -> 
       Out.start "value";
       Format.fprintf (Outchannel.get()) "%d" (Term.compare a b);
       Out.final())
    {args = "<term> <term>";
     short = "Test term ordering."; 
     description = "" ; 
     examples = []; 
     seealso = ""}


let do_ctxt =
  Register.command "ctxt"
    (fun n ->
       let s = context n in
	 Out.start "ctxt";
	 Pretty.set Atom.pp (Outchannel.get()) (Context.ctxt s);
	 Out.final ())
    {args = "[@<ident>]";
     short = "Output logical context."; 
     description = 
          "'ctxt' returns the set of atoms asserted in the current 
           logical context, and 'ctxt@s' returns the set of asserted
           atoms in state 's' from the symbol table. These atoms are not 
           necessarily in canonical form."; 
     examples = []; 
     seealso = ""}

let do_config =
  Register.command "config"
    (fun n ->
       let cfg = Context.config (context n) in
       let out = Outchannel.get() in
	 Out.start "config"; Combine.Config.pp out cfg; Out.final ())
    {args = "[@<ident>]";
     short = "Output logical context."; 
     description = 
        "'config' returns the inference system configuration of the current context,
         and 'config@s' returns the configuration configuration for the context with 
         name [s] in the symbol table.";
     examples = []; 
     seealso = ""}

let do_status =
  Register.command "status"
    (fun n ->
       let st = Context.status (context n) in
	 Out.start "status";
	 Context.Status.pp (Outchannel.get()) st;
	 Out.final ())
    {args = "[@<ident>]";
     short = "Output status of a context."; 
     description = "";
     examples = []; 
     seealso = ""}

let do_undo = 
  Register.command "undo"
    (fun () ->
       current := !previous;
       previous := Context.empty)
    {args = "";
     short = "Undo last state modifying command. Another consecutive undo
              returns to original context.";
     description = "" ; 
     examples = [];
     seealso = ""}


(** Set to [Cmd.batch] in module {!Cmd}. *)
let cmdBatch = ref (fun _ -> 0)
    
let do_load = 
  Register.command "load"
    (fun (n, filename) ->
       let inch = Pervasives.open_in filename in
       let save_current = !current in   (* save variables changed by {!Cmd.batch}. *)
       let save_previous = !previous in
       let save_batch = !batch in
       let save_linenumber = !Tools.linenumber in
       let save_inch = Inchannel.get () in
	 try
	   Out.start ("loading " ^ filename); 
	   previous := !current;
	   current := context n;
	   let res = !cmdBatch inch in
	     batch := save_batch;
	     Tools.linenumber := save_linenumber;
	     Inchannel.set save_inch;
	     Format.fprintf (Outchannel.get()) "end %d" res;
	     Out.final ()
	 with
	     exc -> 
	       current := save_current;
	       previous := save_previous;
	       batch := save_batch;
	       Tools.linenumber := save_linenumber;
	       Inchannel.set save_inch;
	       raise exc)
    {args = "[@<ident>] <filename>";
     short = "Load commands from file.";
     description = "" ; 
     examples = [];
     seealso = ""}


let do_show = 
  Register.command "show"
    (fun (n, flag) -> 
       let fmt = Outchannel.get()
       and cfg = config n in
	 match flag with
	   | None -> Combine.Config.pp fmt cfg
	   | Some(None) -> Combine.Config.Print.shared fmt cfg
	   | Some(Some(i)) -> Combine.Config.Print.component i fmt cfg)
    {args = "[@<ident>] [<th>]'";
     short = "Output partition and theory-specific equality sets."; 
     description = "Display current logical state or parts of it. If parameter
                    'showdiff' is set to 'true', then only added information w.r.t.
                    the previous state is displayed." ; 
     examples = [("show", "Display logical state"); 
                  ("show v", "Display variable equalities only");
                  ("show la", "Display linear arithmetic context");
                  ("show@st", "Display context with name 'st'")]; 
     seealso = ""}
      

let do_sigma =
  Register.command "sigma"
    (fun a ->
       Out.start "term";
       Term.pp (Outchannel.get()) a;
       Out.final())
    {args = "<term>";
     short = "Theory-specific canonization"; 
     description = 
	"[sigma a] computes the normal form of a term using
         theory-specific canonizers for terms in interpreted 
         theories. The outcome is independent from the current
         logical state, and this command leaves the current state 
         unchanged."; 
     examples = []; 
     seealso = ""}

let do_can =
  Register.command "can"
    (fun (n, a) ->
       let cfg = Context.config (context n) in
       let e = Combine.Config.Can.justify cfg a in
       let b = e#rhs in
       let out = Outchannel.get () in
	 Out.start "term";
	 Term.pp out b;
	 Out.nl ();
	 Out.start "justification";
	 e#pp out;
	 Out.final ())
    {args = "[<ident>] <term>";
     short = "Canonical term w.r.t. current context."; 
     description =
        "For a term 'a', 'can a' returns a term which is a canonical representative 
        of the equivalence class of [a] as induced by the atoms in the current 
        context or the context with name [n]. If proof generation is enabled, 
        then also a justification of the equality between 'a' and 'can a' is returned. 
        There are no side effects.";
     examples = [];
     seealso = "simplify"}

let do_eval =
  Register.command "sigma"
    (fun (n, atm) ->
       let s = context n in
	 Out.start "value";
	 Atom.pp (Outchannel.get())(Context.eval s atm);
	 Out.final ())
    {args = "[@<ident>] <atom>";
     short = "Theory-specific canonization"; 
     description = "";
     examples = []; 
     seealso = ""}
	

(** Create a fresh name [n] for the current context [ctxt] and 
  install a binding [n |-> ctxt] into symbol table. *)
module Save = struct

  let counter = ref 0
  let _ = Tools.add_at_reset (fun () -> counter := 0)

  let rec fresh () =
    incr counter;
    let n = Name.of_string ("s" ^ (string_of_int !counter)) in
      if Symtab.in_dom n then fresh () else n 
	(* ensure name is really fresh. *)

  (** Save current context. *)
  let current arg =
    let n = match arg with
      | None -> fresh ()
      | Some(n) -> n
    in
      Symtab.Put.context n !current;
      n

  let context s = 
    let n = fresh () in
      Symtab.Put.context n s

end
  
let do_save =
  Register.command "save"
    (fun arg ->
       Out.start "name";
       Name.pp (Outchannel.get()) (Save.current arg);
       Out.final())
    {args = "[<ident>]";
     short = "Save current state in symbol table."; 
     description = 
       "[save] adds a symbol table entry [s] for the current logical state
        by generating a fresh name, whereas [save n] install the
        current state into the symbol table with key [n]. The
        current logical state is left unchange." ; 
     examples = []; 
     seealso = "symtab, restore"}
    
let do_restore =
  Register.command "restore"
    (fun n -> 
       let ctxt = Symtab.Get.context n in
	 previous := !current;
	 current := ctxt;
	 Out.start "unit"; Out.final())
    {args = "<ident>";
     short = "Restore logical state "; 
     description = 
        "[restore s] updates the current logical state to be the 
         state named by [s] in the symbol table." ; 
     examples = []; 
     seealso = "symtab"}

let do_remove =  
  Register.command "remove"
    (fun n ->
       Symtab.restrict n;
       Out.start "unit"; Out.final())
    {args = "<ident>";
     short = "Remove logical state from symbol table"; 
     description = "[remove s] removes the symbol table entry for  name [s]"; 
     examples = []; 
     seealso = "symtab"}


let do_forget =
  Register.command "forget"
    (fun () ->
       previous := !current;
       current := Context.empty;
       Out.start "unit"; Out.final())
    {args = "";
     short = "Clear out current state"; 
     description = 
       "Resets the current logical context to the empty 
        context. In contrast to 'reset', the symbol table,
        values of global variables, and other data structures 
        are left unchanged." ; 
     examples = []; 
     seealso = "reset"}

    
let do_process1 = 
  Register.command "assert"
    (fun (n, a) ->
       let s = context n in
	 try
	   let t = Context.add s a in
	     previous := !current;
	     current := t;
	     Out.start "ok";
	     Name.pp (Outchannel.get()) (Save.current None);
             Out.final()
	 with
	   | Judgement.Valid(rho) ->  
	       Out.start "valid"; rho#pp (Outchannel.get()); Out.final()
	   | Judgement.Unsat(rho) ->
	       Out.start "unsat"; rho#pp (Outchannel.get()); Out.final();
	       if !batch then raise End_of_file else ())
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


let do_process =
  (fun (n, al) ->
     let s = context n in
       try
	 let t = Context.addl s al in
	   previous := !current;
	   current := t;
	   Out.start "ok";
	   Name.pp (Outchannel.get()) (Save.current None);
           Out.final()
       with
	 | Judgement.Valid(rho) ->  
	     Out.start "valid";
	     rho#pp (Outchannel.get());
	     Out.final()
	 | Judgement.Unsat(rho) ->
	     Out.start "unsat";
	     rho#pp (Outchannel.get());
	     Out.final ();
	     if !batch then raise End_of_file else ())
 
let do_resolve =
  Register.command "resolve"
    (fun n ->
       let s = context n in
	 Context.resolve s;
	 Out.start "status";
	 Context.Status.pp (Outchannel.get()) (Context.status s);
	 Out.final())
    {args = "[@<ident>]";
     short = "Resolve status of context <ident>."; 
     description = 
        "The status of a context is either 'ok', 'sat', or 'unsat'.
         In status 'sat' satisfiability has been established by 
         the construction of a model for this context, and with status
         'unsat' comes a proof of unsatisfiability of this context. 
         The satisfiability or unsatisfiability of contexts with 
         status 'ok' have not been established, and the command
         'resolve' tries to resolve such 'ok' contexts to 'sat' 
         or 'unsat'.  Resolve may fail since the logic underlying
         ICS is indeed undecidable, but for the decidable fragment
         'resolve' always succeeds.";
     examples = []; 
     seealso = ""}


(** Applying maps. *)

let do_find =
  Register.command "find"
    (fun (n, th, x) ->
       let cfg = Context.config (context n) in
       let t, e = 
	 match th with
	   | Some(i) -> 
	       let e = Combine.Config.Find.justify i cfg x in
		 e#rhs, e
	   | None -> 
	       let v = Combine.Config.shared cfg in
	       let e = V.Config.justify v x in
		 assert(Term.eq x e#lhs);
		 e#rhs, e
       in
       let out = Outchannel.get() in
	 Out.start "term"; Term.pp out t; Out.nl();
	 Out.start "justification"; e#pp out; Out.final())
    {args = "[@<ident>] <th> <term> ";
     short = "Return theory-specific interpretation for a variable."; 
     description = 
        "If the equality [x = t] is in the equality set [<th>] , say [a], 
         then [find a x] returns [t] and otherwise [x]. The addressing, 
         say, [find@s1 a x] may be used to address the solution set for the 
         arithmetic theory in the context [s1] in the symbol table. In case,
         addressing is omitted, the current context is investigated.  In addition,
         a justification for the equality 'x = t' is returned."; 
     examples = []; 
     seealso = ""}

let do_inv =
  Register.command "inv"
    (fun (n, i, t) -> 
       let cfg = Context.config (context n) in
	 try
	   let e = Combine.Config.Inv.justify cfg t in
	   let x = e#lhs in
	   let out = Outchannel.get() in
	     Out.start "term"; Term.pp out x; Out.nl();
	     Out.start "justification"; e#pp out; Out.final()
	 with
	     Not_found -> Out.start "none"; Out.final())
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
  Register.command "dep"
    (fun (n, i, y) -> 
       let cfg = Context.config (context n) in
       let xs = 
	 try 
	   Combine.Config.dep i cfg y 
	 with 
	     Not_found -> Dep.Set.empty ()
       in
	 Out.start "terms"; Dep.Set.pp (Outchannel.get()) xs; Out.final())
    {args = "[@<ident>] <th> <term> ";
     short = "Return the lhs dependencies for variable <term> in equality set <th>."; 
     description = "" ; 
     examples = []; 
     seealso = ""}


(** Solver. *)

let do_solve =
  Register.command "solve"
    (fun (a, b) -> 
       try
	 let al = Term.solve a b in
	   Out.start "solved";
	   Term.Subst.pp (Outchannel.get()) al;
	   Out.final()
       with
	 | Exc.Inconsistent ->
	     Out.start "unsat"; Out.final()
	 | Exc.Incomplete ->
	     Out.start "incomplete"; Out.final())
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

 

(** Sat solver *)

let do_sat (n, p) = failwith "to do"
(*
  Register.command "sat"
    (fun (n, p) ->
       let s = context n in
         match Prop.sat s p with
	   | None -> failwith "to do"
           | Some _ -> failwith "to do")
     { short = "SAT Solver for propositional constraints."; 
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
*)
      

(** Errors *)

let do_quit =
  (fun n ->
     Tools.do_at_exit();
     Out.start "exit"; Out.final();
     exit n)

let do_get =
  Register.command "get"
    (function
       | None -> 
	   let out = Outchannel.get() in
	     Out.start "parameters";
	     Ref.iter 
	       (fun n -> 
		  let v = Ref.get n in
		    Format.fprintf out "\n"; 
		    Name.pp out n; 
		    Format.fprintf out "\t%s" v);
	     Out.final()
       | Some(n) -> 
	   let v = Ref.get n in
	     Out.start "value"; 
	     Format.fprintf (Outchannel.get()) "%s" v;
	     Out.final())
    {args = "[<parameter>] ";
     short = "Get current value for <parameter>. "; 
     description = "" ; 
     examples = []; 
     seealso = "set"}


let do_set =
  Register.command "set"
    (fun (n, v) ->
       Ref.set n v;
       Out.start "unit"; Out.final())
    {args = "[<parameter> := <value>] ";
     short = "Set <value> for <parameter>."; 
     description = "" ; 
     examples = []; 
     seealso = "get"}


let do_register =
  Register.command "register"
    (fun sp -> 
       let module Sp: Spec.SPEC = struct
	 let th = sp.Spec.th
	 let signature = sp.Spec.signature
         module Axs = struct
	   let chains = sp.Spec.chains
	   let rewrites = sp.Spec.rewrites
	 end
       end
       in
       let module Unit = Spec.Register(Spec.Make(Sp)) in
	 Out.start "unit"; Out.final())
    {args = ".";
     short = "Registering a new theory."; 
     description = "" ; 
     examples = [
       "register 
     theory crypt
         description 
           \"theory of encryption and decryption\"
         signature 
            enc, dec
         axioms
            ==> enc(dec(x)) = x,
            ==> dec(enc(x)) = x
     end.",
        "Defines a new theory [crypt], defines new symbols, 
         and installs corresponding inference system."]; 
     seealso = ""}

		   
(** Resetting all of the global state. *)

let do_reset = 
  Register.command "reset"
    (fun () -> 
       Tools.do_at_reset ();
       Ref.reset ();
       Symtab.reset ();
       previous := Context.empty;
       current := Context.empty;
       batch := false;
       Out.start "unit"; Out.final ())
    {args = "";
     short = "Reset ICS state."; 
     description = 
       "Reinitializes all internal data structures including
        setting the current logical context to the empty context
        and the symbol table is emptied out." ; 
     examples = []; 
     seealso = ""}



(*
(** Sorting commands in alphabetic order. *)
let _ =
  Command.descriptions :=
    (List.sort
       (fun (n1, _) (n2, _) -> Pervasives.compare n1 n2)
       !Command.descriptions)
*)


(** Register nonterminals. *)

let _ = 
  Register.nonterminal "term"
    ["<var>";
     "<app>";
     "(<term>)";
     "<arith>";
     "<array>";
     "<bv>";
     "<list>";
     "<apply>";
     "<product>";
     "<coproduct>";
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
  Register.nonterminal "var"
    ["<ident>";
     "<ident>{<dom>}";
     "<ident>!<intconst>";
     "<ident>!<intconst>{<dom>}"]
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
     As with external variables, the domain of variables can be restricted."

let _ = 
  Register.nonterminal "app"
    ["<funsym>[(<term>,...,<term>)]"]
    "A term application in prefix form is either of the 
     form 'f(a1,...,an)' with 'f' a function symbol or simply
     a constant 'c' with 'c' a constant symbol."

let _ = 
  Register.nonterminal "funsym"
    ["<ident>"]
    "A function symbol <ident> is assumed to be uninterpreted,
     and all other function symbol are interpreted in some
     theory <th>. "

let _ =
  Register.nonterminal "product"
    ["cons(<term>, <term<.)"; "car(<term>"; "cdr(<term>)"]
    "'cons', 'car', and 'cdr' are the function symbols of the
     product rtheory 'p'."

let _ = 
  Register.nonterminal "coproduct"
    ["inl(<term>)";
     "inr(<term>)";
     "outl(<term>)";
     "outr(<term>)"]
    "'inl', 'inr', 'outl', 'outr', 'inj', out' 
     are function symbols in the theory 'cop' of direct sums (or coproducts)."


let _ = 
  Register.nonterminal "list"
    ["<term> :: <term>";
     "hd(<term>)";
     "tl(<term>)";
     "nil"]
    ""
let _ = 
  Register.nonterminal "apply"
    ["S"; "K"; "I"; "C"; 
     "<term> $ <term>";
     "lambda x1,...,xn: <term>"]
    "Terms in combinatory logic are either one of the 
     combinators 'S', 'K', 'I', 'C' or an application of
     such a term to an argument term. Lambda abstraction
     is a macro for encoding variable abstraction in terms
     of combinatory logic. Notice that the body of lambda
     abstraction often needs parentheses as, for example
     'lambda x: x $ y' is parsed as '(lambda x: x) $ y'."

let _ = 
  Register.nonterminal "arith"
    ["<rat>";
     "<term> + <term>";
     "<term> - <term>";
     "<term> * <term>";
     "-<term>";
     "<term> ^ <intconst>"]
    " The function symbols '+', '-', '*',
     are interpreted in linear ('a') or nonlinear arithmetic 'nl'."

let _ = 
  Register.nonterminal "propset"
    ["empty";
     "full";
      "<term> union <term>";
     "<term> inter <term>";
     "compl <term>"]
    ""

let _ = 
  Register.nonterminal "array"
    ["create(<term>)";
     "<term>[<term> := <term>]";
     "<term>[<term>]"]
    ""
let _ = 
  Register.nonterminal "bv"
    ["0b[0|1]*";
     "<term> ++ <term>";
     "<term>[<intconst>:<intconst>]"]
    "Constant bitvectors are preceded by '0b' and 
     'true' and 'false' are just mnemonics for the constant bitvectors
     '0b1' and '0b0' of length 1, respectively.
     'a ++ b' denotes the concatenation of a bitvector term 'a'
     with a bitvector term 'b', and 'a[i:j]' the extraction of the
     bits 'i' through 'j' in a bitvector term 'a'. These mixfix
     operators assume that the width of 'a' and 'b' can be 
     computed from its nonbitvector parts.  Width of variables, 
     for example, can be declared using symbol table declarations
     such as 'sig x : bitvector[5]'"

let _ = 
  Register.nonterminal "ident"
    ["[A-Z | a-z][A-Z | a-z | ' |_ | 0-9]*"]
    "Identifiers are all the above minus keywords such as commands
     and identifiers that have been used up by other means."

let _ = 
  Register.nonterminal "int"
    ["[0-9]*"]
    "Natural numbers."


let _ = 
  Register.nonterminal "rat"
    ["<int>/<int> "]
    "Rational numbers."

let _ = 
  Register.nonterminal "prop"
    ["[<prop>]";
     "<ident>";
     "<propfml> & <propfml>";
     "<propfml> | <propfml>";
     "<propfml> <=> <propfml>";
     "<propfml> => <propfml>";
     "~<propfml>";
     "if <propfml> then <propfml> else <propfml> end"]
    "Propositions are either propositional variables of the form <ident>
     or built up from propositional connectives such as conjunction '&',
     disjunction '|', negation '~', implication '=>', biimpliciation '<=>',
     or the conditional construct. Notice that, in contrast to atoms,
     propositions are grouped using square brackets. Structure sharing
     is obtained by using the <prop> command for defining names for
     propositional formulas. For example, the command 'prop p := x & [y | z].'
     defines a name 'p' for the formula on the right-hand side. An identifier
     in a propositional formula is first tried to be expanded to such 
     a definitional right-hand side and if there is no such definition,
     an identifier is treated as a propositional variable."

let _ = 
  Register.nonterminal "atom"
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
  Register.nonterminal "dom"
    ["int"; "real"]
    "The integer and real domains"

(**
let _ =
  Register.nonterminal "th"
    (Theory.fold (fun i acc -> Theory.to_string i :: acc) [])
    "Builtin equality theories"
*)


(*
let _ =
  Register.nonterminal "parameters"
*)
