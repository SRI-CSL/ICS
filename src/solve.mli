

(*s The solver for the combined theory.
    If neither [a] nor [b] contain uninterpreted terms
    (including variables),
    [solve(a,b)] returns [true] if the arguments have identical
    canonical forms and [false] if not. If [a] has no
    uninterpreted subterms, [a] and [b] are exchanged.  Using 
    the adequate component solver for [a] (according to theory of
    top-level symbol of [a]) the equality [a = b] is solved. Each
    equality in the resulting conjunction is then recursively replaced
    by its solution until no left side of an equality occurs in a
    right side except as a proper subterm of an uninterpreted term.

    E.g. the equation
      $$4 + \mathit{car}(x + 1) = \mathit{cdr}(x + 2) + 3$$
    is solved as
      $$x = \mathit{cons}(\mathit{cdr}(x + 2) - 2, d) -1$$
    by first solving for [car(x + 1)] using the arithmetic
    solver, then solving for [x + 1] using the tuple solver, followed
    by solving for [x] using the arithmetic solver again.

    Notice that this is a solved form, since [x] on the right-hand side
    does only occur as a proper subterm of an uninterpreted term, namely
    [x + 2].

    The solver is destructive in that newly generated constraints are
    added to the argument state.

 *)

val is_solvable : Term.t -> bool

val solve : Term.t option -> State.t -> Term.eqn -> Subst.t

  (*s [solution s a] yields, if successful, a term [b] such that [a = b] can
    be derived from context [s] and [a] does not occur interpreted in [b].
    This function is restricted to [a] which satisfy the [is_solvable] predicate. *)

val solution : State.t -> Term.t -> Term.t option
