
type 'a  printer = Format.formatter -> 'a -> unit

val list : 'a printer -> 'a list printer
val term : Term.t printer
val eqn : (Term.t * Term.t) printer
val cnstrnt : Cnstrnt.t printer
val tset : Term.Set.t printer
val tmap : 'a printer -> 'a Term.Map.t printer
