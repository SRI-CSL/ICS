/* Copyright (c) SRI International 2002. */
/***
   PURPOSE
     
   NOTES
     
   HISTORY
     demoura - May 6, 2002: Created.
***/

#include"LPSolver.h"

// #undef RULE_TRACE
// #define RULE_TRACE(code) { code }
// #define ctrace cout

int LPSolver::propagate_rich_constraints() {
	SOLVER_TRACE(ctrace << "propagating constraints...\n";);
	assert(check_invariant());
	while (!to_process.is_empty()) {
		unsigned int formula_idx = to_process_queue_pop();
		LPInternalFormulaInfo & info = internal_formulas[formula_idx];
		if (assignments[formula_idx] != 0 || absolute(info.next) != formula_idx) {
			// A formula only need to be processed if it was assigned, or it is a member of a equivalence class.	

			// 1. propagate logic constraints downward.
			const LPFormula * formula = formula_manager->get_formula(formula_idx);
			if (!formula->is_atomic()) {
				int result = propagate_logic_constraint_downward(formula);
				if (result != 0)
					return result;
			}

			// 2. propagate logic constraints upward.
			unsigned int num_occurrences = info.positive_occurrences.get_size();
			// Idea: Is it useful to propagate constraints upwards if a given occurrence is inactive?
			//       That is, should I filter inactive occurrences?
			//       Maybe, I should add a flag signaling whether this kind of propagation is enable or not.
			for (unsigned int i = 0; i < num_occurrences; i++) {
				unsigned int occurrence_idx = info.positive_occurrences.get(i);
				int result = propagate_logic_constraint_upward(formula_idx, occurrence_idx, false);
				if (result != 0)
					return result;
			}
			num_occurrences = info.negative_occurrences.get_size();
			for (unsigned int i = 0; i < num_occurrences; i++) {
				unsigned int occurrence_idx = info.negative_occurrences.get(i);
				int result = propagate_logic_constraint_upward(formula_idx, occurrence_idx, true);
				if (result != 0)
					return result;
			}
		}
	}
	assert(to_process.is_empty());
	to_process.reset();
	return 0;
}

int LPSolver::propagate_logic_constraint_downward(const LPFormula * formula)
{
	switch (formula->get_kind()) {
	case LP_OR:
		return propagate_or_constraint_downward(formula);
	case LP_IFF:
		return propagate_iff_constraint_downward(formula);
	case LP_ITE:
		return propagate_ite_constraint_downward(formula);
	case LP_EXISTS:
		return propagate_exists_constraint_downward(formula);
	case LP_EQ: /* BUG... atomic formula */
	case LP_PROPOSITION: /* BUG... atomic formula */
	default: /* BUG... unidentified alternative */
		assert(false);
	}
}

#define ASSIGN(RHS,LHS,FORMULA) { if (!assign_formula(RHS, LHS, 0)) return FORMULA; } ((void) 0)

#define RULE_OR_TRACE() RULE_TRACE(const LPFormula * tmp = formula_manager->get_formula(parent_idx);								\
																	 unsigned int n = tmp->get_num_arguments();																				\
																	 for (unsigned int i = 0; i < n; i++) {																						\
																		 ctrace << "\t\ta" << (i+1) << ": ";																							\
																		 formula_manager->dump_formula(ctrace, tmp->get_argument(i));											\
																		 ctrace << " == ";																																\
																		 formula_manager->dump_formula(ctrace, canonical_formula(tmp->get_argument(i)));	\
																		 ctrace << "\n";																																	\
																	 }																																								\
																	 ctrace << "\t\tor(...): ";																													\
																	 formula_manager->dump_formula(ctrace, parent_idx);																	\
																	 ctrace << " == ";																																	\
																	 formula_manager->dump_formula(ctrace, canonical_formula(parent_idx));							\
																	 ctrace << endl;)

#define RULE_IFF_TRACE() RULE_TRACE(ctrace << "\t\ta: ";																											\
																			 formula_manager->dump_formula(ctrace, lhs);														\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(lhs));					\
																			 ctrace << "\n";																												\
																			 ctrace << "\t\tb: ";																										\
																			 formula_manager->dump_formula(ctrace, rhs);														\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(rhs));					\
																			 ctrace << "\n\t\tiff(a,b): ";																					\
																			 formula_manager->dump_formula(ctrace, parent_idx);											\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(parent_idx));	\
																			 ctrace << "\n";)

#define RULE_ITE_TRACE() RULE_TRACE(ctrace << "\t\tc: ";																											\
																			 formula_manager->dump_formula(ctrace, c);															\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(c));						\
																			 ctrace << "\n";																												\
																			 ctrace << "\t\tt: ";																										\
																			 formula_manager->dump_formula(ctrace, t);															\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(t));						\
																			 ctrace << "\n";																												\
																			 ctrace << "\t\te: ";																										\
																			 formula_manager->dump_formula(ctrace, e);															\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(e));						\
																			 ctrace << "\n";																												\
																			 ctrace << "\t\tite(c,t,e): ";																					\
																			 formula_manager->dump_formula(ctrace, parent_idx);											\
																			 ctrace << " == ";																											\
																			 formula_manager->dump_formula(ctrace, canonical_formula(parent_idx));	\
																			 ctrace << "\n";)


int LPSolver::propagate_or_constraint_downward(const LPFormula * parent)
{
	assert(parent->is_or());
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	int can = canonical_formula(parent_idx);
	unsigned int n = parent->get_num_arguments();
	// Rule: parent == false --->  a_1 == false, ..., a_n == false
	if (can == -1) {
		RULE_TRACE(ctrace << "  [down] or(a1,...,an) == false ---> a1 == false, ..., an == false\n";);
		RULE_OR_TRACE(); 
		for (unsigned int i = 0; i < n; i++) {
			ASSIGN(parent->get_argument(i), -1, parent_idx); 
		}
		RULE_OR_TRACE(); 
	}
	else {
		// Rule: parent == -a_n --->  a_n == false, parent == true
		// Note: I don't need to assign parent, since parent is automatically assigned because parent and -a_n
		// are in the same equivalence class
		int unknown_can = 0;
		bool all_children_are_false = true;
		bool is_target = true; // true, if unit rule can be applied
		for (unsigned int i = 0; i < n; i++) {
			int can_arg = canonical_formula(parent->get_argument(i));
			if (can == -can_arg) { 
				RULE_TRACE(ctrace << "  [down] or(a1,...,an) == -ai ---> ai == false, or(...) == true\n";);
				RULE_OR_TRACE(); 
				ASSIGN(can_arg, -1, parent_idx); can = 1; /* update "can" to reflect parent == true */ 
				RULE_OR_TRACE(); 
			}
			else if (can_arg == 1) { all_children_are_false = false; is_target = false; }
			else if (can_arg == -1) { /* do nothing */ }
			else {
				assert(is_unknown(can_arg));
				all_children_are_false = false;
				if (unknown_can != 0 && unknown_can != can_arg ) { is_target = false; }
				else { 
					assert(unknown_can == 0 || unknown_can == can_arg);
					unknown_can = can_arg;
				}
			}
		}

		if (all_children_are_false) {
			// inconsistency detected!!!
			DBG_CODE(for (unsigned int i = 0; i < n; i++) {
								 if (get_formula_value(parent->get_argument(i)) != -1)
									 assert(false);
							 });
			RULE_TRACE(ctrace << "  [down] a1 == false,...,an == false ---> or(...) == false\n";);
			RULE_OR_TRACE(); 
			ASSIGN(parent_idx, -1, parent_idx);
//  			SOLVER_TRACE(ctrace << "  conflict detected in or-node, all children are false " << parent_idx << " [";
//  									 formula_manager->dump_formula(ctrace, parent_idx);
//  									 ctrace << "]\n";);
			RULE_OR_TRACE(); 
		}

		// assert(IMPLY(all_children_are_false, !is_target));

		// check if it is an unit rule
		if (can == 1 && is_target && unknown_can != 0) {
			DBG_CODE(int unknown_can_aux = 0; 
							 for (unsigned int i = 0; i < n; i++) {
								 int can_arg = canonical_formula(parent->get_argument(i));
								 if (can_arg == 1) assert(false); /* bug detected... */
								 else if (can_arg == -1) { /* do nothing */ }
								 else {
									 assert(is_unknown(can_arg));
									 if (unknown_can_aux != 0 && unknown_can_aux != can_arg)
										 assert(false); /* bug detected... */
									 else {
										 assert(can_arg == unknown_can);
										 unknown_can_aux = can_arg;
									 }
								 }
							 }
							 assert(unknown_can_aux != 0););
			RULE_TRACE(ctrace << "  [down] UNIT, or(a1,...,an) == true, all but one ai is false ---> unknown ai == true\n";);
			RULE_OR_TRACE(); 
			ASSIGN(unknown_can, 1, parent_idx);
			RULE_OR_TRACE(); 
		}
	}
	
	return 0;
}

int LPSolver::propagate_iff_constraint_downward(const LPFormula * parent)
{
	assert(parent->is_iff());
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	int can = canonical_formula(parent_idx);
	LPFormulaId lhs = parent->get_iff_lhs();
	LPFormulaId rhs = parent->get_iff_rhs();
	
	// Rule: parent == true ---> rhs == lhs
	if (can == 1) { 
		RULE_TRACE(ctrace << "  [down] iff(a,b) == true ---> a == b\n";);
		RULE_IFF_TRACE();
		ASSIGN(rhs, lhs, parent_idx); 
		RULE_IFF_TRACE();
	}
	// Rule: parent == false ---> rhs == -lhs
	else if (can == -1) { 
		RULE_TRACE(ctrace << "  [down] iff(a,b) == false ---> a == -b\n";);
		RULE_IFF_TRACE();
		ASSIGN(rhs, -lhs, parent_idx); 
		RULE_IFF_TRACE();
	}
	else {
		int can_lhs = canonical_formula(lhs);
		int can_rhs = canonical_formula(rhs);
		// Rule: parent == lhs ---> rhs == true
		if (can == can_lhs) { 
			RULE_TRACE(ctrace << "  [down] iff(a,b) == a ---> b == true\n";);
			RULE_IFF_TRACE();
			ASSIGN(rhs, 1, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == -lhs ---> rhs == false
		else if (can == -can_lhs) {	
			RULE_TRACE(ctrace << "  [down] iff(a,b) == -a ---> b == false\n";);
			RULE_IFF_TRACE();
			ASSIGN(rhs, -1, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == rhs ---> lhs == true
		else if (can == can_rhs) { 
			RULE_TRACE(ctrace << "  [down] iff(a,b) == b ---> a == true\n";);
			RULE_IFF_TRACE();
			ASSIGN(lhs, 1, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == -rhs ---> lhs == false
		else if (can == -can_rhs) { 
			RULE_TRACE(ctrace << "  [down] iff(a,b) == -b ---> a == false\n";);
			RULE_IFF_TRACE();
			ASSIGN(lhs, -1, parent_idx); 
			RULE_IFF_TRACE();
		}
	}

	return 0;
}

int LPSolver::propagate_ite_constraint_downward(const LPFormula * parent)
{
	assert(parent->is_ite());
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	int can = canonical_formula(parent_idx);
	LPFormulaId c = parent->get_cond();
	LPFormulaId t = parent->get_then();
	LPFormulaId e = parent->get_else();
	if (can == 1) { // the parent is true
		// Rule:  parent == true, then == false --->  cond == false,  else == true
		if (canonical_formula(t) == -1) { 
			RULE_TRACE(ctrace << "  [down] ite(c,t,e) == true, t == false ---> c == false, e = true\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, -1, parent_idx);	ASSIGN(e, 1, parent_idx); 
		}
		// Rule:  parent == true, else == false --->  cond == true,  then == true
		else if (canonical_formula(e) == -1) { 
			RULE_TRACE(ctrace << "  [down] ite(c,t,e) == true, e == false ---> c == true, t = true\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, 1, parent_idx);	ASSIGN(t, 1, parent_idx); 
		}
	}
	else if (can == -1) { 
		int can_t = canonical_formula(t);
		int can_e = canonical_formula(e);
		// Rule: parent == false, else == true --->  cond == true, then == false
		if (can_e == 1) { 
			RULE_TRACE(ctrace << "  [down] ite(c,t,e) == false, e == true ---> c == true, t == false\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, 1, parent_idx);	
			ASSIGN(t, -1, parent_idx);
		}
		// Rule: parent == false, then == true  --->  cond == false, else == false
		else if (can_t == 1) { 
			RULE_TRACE(ctrace << "  [down] ite(c, t, e) == false, t == true ---> c == false, e == false\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, -1, parent_idx); 
			ASSIGN(e, -1, parent_idx);
		}
	}
	
	return 0;
}

int LPSolver::propagate_exists_constraint_downward(const LPFormula * formula)
{
	// TODO
	feature_not_implemented_yet();
	return 0;
}

int LPSolver::propagate_logic_constraint_upward(unsigned int formula_idx, unsigned int occurrence_idx, bool negative_occurrence)
{
	const LPFormula * occurrence = formula_manager->get_formula(occurrence_idx);
	switch (occurrence->get_kind()) {
	case LP_OR:
		return propagate_or_constraint_upward(formula_idx, occurrence, negative_occurrence);
	case LP_IFF:
		return propagate_iff_constraint_upward(formula_idx, occurrence);
	case LP_ITE:
		return propagate_ite_constraint_upward(formula_idx, occurrence);
	case LP_EXISTS:
		return propagate_exists_constraint_upward(formula_idx, occurrence);
	case LP_EQ: /* BUG... atomic formula */
	case LP_PROPOSITION: /* BUG... atomic formula */
	default: /* BUG... unidentified alternative */
		assert(false);
	}
}

int LPSolver::propagate_or_constraint_upward(unsigned int formula_idx, const LPFormula * parent, bool negative_occurrence)
{
	assert(parent->is_or());
	unsigned int n = parent->get_num_arguments();
	int child = negative_occurrence ? -formula_idx : formula_idx;
	DBG_CODE(bool found = false;
					 for(unsigned i = 0; i < n; i++) {
						 if (parent->get_argument(i) == child) { found = true; break; }
					 }
					 assert(found););
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	int can_child = canonical_formula(child);
	// Rule: child == true ---> parent == true
	if (can_child == 1) { 
		RULE_TRACE(ctrace << "  [up] a == true ---> or(...,a,...) == true\n";);
		RULE_OR_TRACE();
		ASSIGN(parent_idx, 1, parent_idx);
		RULE_OR_TRACE(); 
	}
	// Rule: child_1 == false && ... && child_(i-1) == false && child_(i+1) == false && ... && child_n == false ---> parent == child_i
	else if (can_child == -1) {
		// little optimization, if the parent is false, then let it handle the constraint propagation...
		// this simple modification optimizes a lot the verification, specially in huge problems without structure (e.g. problems in CNF format)
		if (get_formula_value(parent_idx) == -1) 
			return 0;

		bool apply_rule = true;
		int unknown_can = 0; 
		for (unsigned i = 0; i < n; i++) {
			int can_arg = canonical_formula(parent->get_argument(i));
			if (can_arg == 1) { /* rule cannot be applied */ apply_rule = false; break; }
			else if (can_arg == -1) { /* do nothing */ }
			else {
				assert(is_unknown(can_arg));
				if (unknown_can != 0 && unknown_can != can_arg ) { /* rule cannot be applied */ apply_rule = false; break; }
				else { 
					assert(unknown_can == 0 || unknown_can == can_arg);
					unknown_can = can_arg;
				}
			}
		}
		if (apply_rule) {
			RULE_TRACE(ctrace << "  [up] n-1 children are false, so or(a1,...,ai,...,an) = ai\n";);
			RULE_OR_TRACE();
			if (unknown_can != 0) {
				ASSIGN(parent_idx, unknown_can, parent_idx);
			}
			else {
				ASSIGN(parent_idx, -1, parent_idx); // all children are equal to false, so the parent is also false!
			}
			RULE_OR_TRACE(); 
		}
	}
	else {
		assert(is_unknown(can_child));
		// Rule 1: child_1 == ... == child_n ---> parent == child_1
		// Rule 2: Exists i,j. child_i == -child_j ---> parent == true
		bool can_apply_rule_1 = true;
		for (unsigned i = 0; i < n; i++) {
			int can_arg = canonical_formula(parent->get_argument(i));
			if (can_child == can_arg) { /* do nothing */ }
			else if (can_child == -can_arg) { 
				// applying rule 2...
				RULE_TRACE(ctrace << "  [up] exists ai, aj such that ai = -aj ---> or(...,ai,...,aj,...) == true\n";);
				RULE_OR_TRACE();
				ASSIGN(parent_idx, 1, parent_idx);
				RULE_OR_TRACE(); 
				return 0;
			}
			else { 
				can_apply_rule_1 = false;
				break;
			}
		}
		if (can_apply_rule_1) {
			RULE_TRACE(ctrace << "  [up] a1 == ... == an ---> or(a1,...,an) == a1\n";);
			RULE_OR_TRACE();
			ASSIGN(parent_idx, can_child, parent_idx);
			RULE_OR_TRACE(); 
		}
	}
	return 0;
}

int LPSolver::propagate_iff_constraint_upward(unsigned int formula_idx, const LPFormula * parent)
{
	assert(absolute(parent->get_iff_lhs()) == formula_idx ||
				 absolute(parent->get_iff_rhs()) == formula_idx);
	assert(parent->is_iff());
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	LPFormulaId lhs = parent->get_iff_lhs();
	LPFormulaId rhs = parent->get_iff_rhs();
	int can_lhs = canonical_formula(lhs);
	int can_rhs = canonical_formula(rhs);
	int can_parent = canonical_formula(parent_idx);

	if (absolute(lhs) == formula_idx) {
		// the lhs was modified...
		// Rule: lhs == true ---> parent == rhs
		if (can_lhs == 1) {	
			RULE_TRACE(ctrace << "  [up] a == true ---> iff(a,b) == b\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, rhs, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: lhs == false ---> parent == -rhs
		else if (can_lhs == -1) { 
			RULE_TRACE(ctrace << "  [up] a == false ---> iff(a,b) == -b\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, -rhs, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == lhs ---> rhs == true
		else if (can_parent == can_lhs) { 
			RULE_TRACE(ctrace << "  [up] iff(a,b) == a ---> b == true\n";);
			RULE_IFF_TRACE();
			ASSIGN(rhs, 1, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == -lhs ---> rhs == false
		else if (can_parent == -can_lhs) { 
			RULE_TRACE(ctrace << "  [up] iff(a,b) == -a ---> b == false\n";);
			RULE_IFF_TRACE();
			ASSIGN(rhs, -1, parent_idx); 
			RULE_IFF_TRACE();
		}
	}
	if (absolute(rhs) == formula_idx) {
		// the rhs was modified...
		// Rule: rhs == true ---> parent == lhs
		if (can_rhs == 1) {	
			RULE_TRACE(ctrace << "  [up] b == true ---> iff(a,b) == a\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, lhs, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: rhs == false ---> parent == -lhs
		else if (can_rhs == -1) { 
			RULE_TRACE(ctrace << "  [up] b == false ---> iff(a,b) == -a\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, -lhs, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: parent == rhs ---> lhs == true
		else if (can_parent == can_rhs) { 
			ASSIGN(lhs, 1, parent_idx); 
			RULE_IFF_TRACE();
			RULE_TRACE(ctrace << "  [up] iff(a,b) == b ---> a == true\n";);
			RULE_IFF_TRACE();
		}
		// Rule: parent == -rhs ---> lhs == false
		else if (can_parent == -can_rhs) { 
			RULE_TRACE(ctrace << "  [up] iff(a,b) == -b ---> a == false\n";);
			RULE_IFF_TRACE();
			ASSIGN(lhs, -1, parent_idx); 
			RULE_IFF_TRACE();
		}
	}
	if (absolute(lhs) == formula_idx || absolute(rhs) == formula_idx) {
		// the (lhs or rhs) was modified...
		// Rule: lhs == rhs ---> parent == true
		if (can_lhs == can_rhs) {	
			RULE_TRACE(ctrace << "  [up] a == b ---> iff(a,b) == true\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, 1, parent_idx); 
			RULE_IFF_TRACE();
		}
		// Rule: lhs == -rhs ---> parent == false
		if (can_lhs == -can_rhs) { 
			RULE_TRACE(ctrace << "  [up] a == -b ---> iff(a,b) == false\n";);
			RULE_IFF_TRACE();
			ASSIGN(parent_idx, -1, parent_idx); 
			RULE_IFF_TRACE();
		}
	}
	return 0;
}

int LPSolver::propagate_ite_constraint_upward(unsigned int formula_idx, const LPFormula * parent)
{
	assert(parent->is_ite());
	unsigned int parent_idx = formula_manager->get_formula_id(parent);
	LPFormulaId c = parent->get_cond();
	LPFormulaId t = parent->get_then();
	LPFormulaId e = parent->get_else();
	assert(formula_idx == absolute(c) ||
				 formula_idx == absolute(t) ||
				 formula_idx == absolute(e));
 	if (absolute(c) == formula_idx) {
		// the condition was modified...
		int can_c = canonical_formula(c);
		// Rule: cond == true ---> parent == then
		if (can_c == 1) {
			RULE_TRACE(ctrace << "  [up] c == true ---> ite(c,t,e) == t\n";);
			RULE_ITE_TRACE();
			ASSIGN(parent_idx, t, parent_idx); 
		}
		// Rule: cond == false ---> parent == else
		else if (can_c == -1) {	
			RULE_TRACE(ctrace << "  [up] c == false ---> ite(c,t,e) == e\n";);
			RULE_ITE_TRACE();
			ASSIGN(parent_idx, e, parent_idx); 
		}
		return 0;
	}
	int can_t = canonical_formula(t);
	int can_e = canonical_formula(e);
	int can_parent = canonical_formula(parent_idx);
	if (absolute(t) == formula_idx) {
		// the then part was modified...
		// Rule:  parent == true, then == false --->  cond == false,  else == true
		if (can_parent == 1 && can_t == -1) {	
			RULE_TRACE(ctrace << "  [up] ite(c,t,e) == true, t == false ---> c == false, e == true\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, -1, parent_idx); ASSIGN(e, 1, parent_idx); 
		}
	}
	if (absolute(e) == formula_idx) {
		// the else part was modified...
		// Rule:  parent == true, else == false --->  cond == true,  then == true
		if (can_parent == 1 && can_e == -1) { 
			RULE_TRACE(ctrace << "  [up] ite(c,t,e) == true, e == false ---> c == true, t == true\n";);
			RULE_ITE_TRACE();
			ASSIGN(c, 1, parent_idx);	ASSIGN(t, 1, parent_idx);	
		}
	}
	if (absolute(t) == formula_idx || absolute(e) == formula_idx) { 
		// the (then or else) part was modified...
		// Rule: then == else ---> parent == then
		if (can_t == can_e) { 
			RULE_TRACE(ctrace << "  [up] t == e ---> ite(c, t, e) == t\n";);
			RULE_ITE_TRACE();
			ASSIGN(parent_idx, t, parent_idx); 
		}
		else if (can_parent == -1) {
			// Rule: parent == false, else == true --->  cond == true, then == false
			if (can_e == 1) { 
				RULE_TRACE(ctrace << "  [up] ite(c, t, e) == false, e == true ---> c == true, t == false\n";);
				RULE_ITE_TRACE();
				ASSIGN(c, 1, parent_idx); 
				ASSIGN(t, -1, parent_idx);
			}
			// Rule: parent == false, then == true  --->  cond == false, else == false
			else if (can_t == 1) {	
				RULE_TRACE(ctrace << "  [up] ite(c, t, e) == false, t == true ---> c == false, e == false\n";);
				RULE_ITE_TRACE();
				ASSIGN(c, -1, parent_idx); 
				ASSIGN(e, -1, parent_idx);
			}
		}
	}
	return 0;
}

int LPSolver::propagate_exists_constraint_upward(unsigned int formula_idx, const LPFormula * parent)
{
	// TODO
	feature_not_implemented_yet();
	return 0;
}

int LPSolver::propagate_clausal_constraints() {
	clock_t start = clock();
	SOLVER_TRACE(ctrace << "propagating clausal constraints...\n";);
	assert(check_invariant());
	while (!to_process.is_empty()) {
		unsigned int formula_idx = to_process_queue_pop();
		if (assignments[formula_idx] != 0) {
			// A formula only need to be processed if it was assigned
			int result = propagate_clausal_constraints(formula_idx);
			if (result != 0) {
				clock_t end = clock();
				constraint_propagation_time += ((double) (end - start)) / CLOCKS_PER_SEC;
				return result;
			}
		}
	}
	assert(to_process.is_empty());
	to_process.reset();
	clock_t end = clock();
	constraint_propagation_time += ((double) (end - start)) / CLOCKS_PER_SEC;
	SOLVER_TRACE(ctrace << "end of propagating clausal constraints with success...\n";);
	return 0;
}

#define KEEP_CLAUSE() {													\
			v.set(j, clause_idx);											\
			j++;																			\
} ((void) 0)

#define ADJUST_CLAUSE_VECTOR() {																		\
	for (unsigned int k = (must_keep ? i : i+1); k < n; k++, j++)													\
		v.set(j, v.get(k));																							\
	v.resize(j);																											\
} ((void) 0)

#define ASSIGN_WATCH_LITERAL(LITERAL) {									\
 	if (!fast_assign_formula(LITERAL, 1, -clause_idx)) {	\
    ADJUST_CLAUSE_VECTOR();															\
 		return -clause_idx;																	\
 	}																											\
} ((void) 0)

int LPSolver::propagate_clausal_constraints(unsigned int formula_idx) {
	LPInternalFormulaInfo & info = internal_formulas[formula_idx];
	int can = canonical_formula(formula_idx);
	if (can == 1) { 
		// true... then only negative occurrences are affected...
		return propagate_clausal_constraints(formula_idx, info.clausal_negative_occurrences);
	}
	else if (can == -1) {
		// false... then only positive occurrences are affected...
		return propagate_clausal_constraints(formula_idx, info.clausal_positive_occurrences);
	}
	else {
		assert(false);
		return 0;
	}
}

int LPSolver::propagate_clausal_constraints(unsigned int formula_idx, LPClauseVector & v)
{
	// cout << "BEFORE " << v << endl;
	SOLVER_TRACE(ctrace << "  [cp] clausal, formula_idx = " << formula_idx << endl;);
	unsigned int n = v.get_size();
	unsigned int j = 0;
	for (unsigned int i = 0; i < n; i++) {
		unsigned int clause_idx = v.get(i);
		LPClause * clause = clauses.get_ptr(clause_idx);
		bool must_keep = true;
		SOLVER_TRACE(ctrace << "    [cp] ";
								 dump_clause(ctrace, *clause); ctrace << endl;);
		if (clause->was_deleted()) {
			// Do nothing...
			// Obs.: j is not updated here... so, we are removing the current clause from
			// the occurrences of formula_idx
			SOLVER_TRACE(ctrace << "       do nothing, clause was DELETED.\n";);
		}
		else if (clause->get_size() == 2) {
			// Solve binary clauses in a special way.
			LPFormulaId literal1 = clause->get_literal(0);
			LPFormulaId literal2 = clause->get_literal(1);
			if (absolute(literal1) == formula_idx) {
				ASSIGN_WATCH_LITERAL(literal2);
			}
			else {
				assert(absolute(literal2) == formula_idx);
				ASSIGN_WATCH_LITERAL(literal1);
			}
			KEEP_CLAUSE();
		}
		else {
			DBG_CODE(if (!(absolute(clause->get_watch1_literal()) == formula_idx ||
										 absolute(clause->get_watch2_literal()) == formula_idx)) {
				cerr << "Bug detected: formula_idx = " << formula_idx << ", clause: ";
				dump_clause(cerr, *clause);
				cerr << endl;
				assert(false);
			});
			bool is_watch1 = (absolute(clause->get_watch1_literal()) == formula_idx);
			assert(IMPLY(is_watch1, canonical_formula(clause->get_watch1_literal()) == -1));
			assert(IMPLY(!is_watch1, canonical_formula(clause->get_watch2_literal()) == -1));
			LPFormulaId * ini = clause->get_literals();
			LPFormulaId * watch1 = ini; 
			LPFormulaId * watch2 = is_watch1 ? clause->get_watch2() : clause->get_watch1();
			LPFormulaId * curr_watch1 = is_watch1 ? clause->get_watch1() : clause->get_watch2();
			assert(absolute(*curr_watch1) == formula_idx);
			unsigned int curr_watch1_decision_level = internal_formulas[absolute(*curr_watch1)].decision_level;
			LPFormulaId * higher_place = NULL;
			LPFormulaId * end = ini + clause->get_size();
			LPFormulaId curr = 0;
			bool already_satisfied = false;
			for (; watch1 != end; watch1++) {
				curr = fast_get_formula_value(*watch1);
				assert(curr >= -1 && curr <= 1);
				if (curr == 1) {
					already_satisfied = true;
					SOLVER_TRACE(ctrace << "       clause was already satisfied...\n";);
					if (watch1 != watch2) {
						// a new place was found
						SOLVER_TRACE(ctrace << "       a new place was found...\n";);
						break;
					}
				}
				else if (watch1 != watch2) {
					if (curr == 0) {
						// a new place was found
						SOLVER_TRACE(ctrace << "       a new place was found...\n";);
						break;
					}
					else if (curr == -1 && (internal_formulas[absolute(*watch1)].decision_level > curr_watch1_decision_level)) {
						// move the watch to the higher position...
						SOLVER_TRACE(ctrace << "       moving the watch to a higher position...\n";);
						higher_place = watch1;
						curr_watch1_decision_level = internal_formulas[absolute(*watch1)].decision_level;
						// the search must continue... :o)
					}
				}
			}
#ifndef NDEBUG
			int branch_taken = 0;
#endif
			if (watch1 != end || higher_place != NULL) {
				SOLVER_TRACE(ctrace << "     clause will be removed, k will not be updated, higher_place = " << higher_place << "\n";);
				LPFormulaId * to_move = (watch1 != end) ? watch1 : higher_place;
				assert(!(watch1 != end && is_clause_unsatisfiable(clause_idx)));
				assert(IMPLY((to_move == higher_place), (watch1 == end)));
				assert(IMPLY((watch1 == end), (higher_place != NULL && to_move == higher_place)));
				assert(IMPLY((watch1 != end), watch1 == to_move));
				if (is_watch1)
					clause->set_watch1(to_move);
				else
					clause->set_watch2(to_move);
				assert(clause->get_watch1() != clause->get_watch2());
				// add clause to the occurrences of the literal watch1
				assert(absolute(*to_move) != formula_idx);
				assert(absolute(*to_move) != absolute(*curr_watch1));
				assert(absolute(*to_move) != absolute(*watch2));
				LPInternalFormulaInfo & info = internal_formulas[absolute(*to_move)];
				DBG_CODE(LPClauseVector & to_check = *to_move < 0 ? info.clausal_negative_occurrences : info.clausal_positive_occurrences;
								 for (unsigned int w = 0; w < to_check.get_size(); w++) {
									 assert(clause_idx != to_check.get(w));
								 });
				if (*to_move < 0)
					info.clausal_negative_occurrences.push(clause_idx);
				else
					info.clausal_positive_occurrences.push(clause_idx);
				// Obs.: j is not updated here... so, we are removing the current clause from
				// the occurrences of formula_idx

				// New code: the following code is not necessary for completeness our soundness.
				// It is is used to avoid the miss of some contraints propagations...
				if (fast_get_formula_value(*watch2) == -1)
					to_process_queue_push(absolute(*watch2)); // we should find a new position for watch2...
				DBG_CODE(branch_taken = 1;);
				must_keep = false;
			} 
			else {
				assert(watch1 == end && higher_place == NULL);
				SOLVER_TRACE(ctrace << "     keeping the clause\n";);
				must_keep = true;
			}
			if (watch1 == end) {
				if (already_satisfied) {
					assert(get_formula_value(*watch2) == 1);
					DBG_CODE(branch_taken = 2;);
				}
				else {
					assert(!already_satisfied);
					assert(watch1 == end);
					// new place was not found...
					// we should apply the unit rule...
					RULE_TRACE(ctrace << " UNIT clausal constraint\n";);
#ifndef NDEBUG					
					unsigned int num_unknowns = get_clause_num_unknowns(clause_idx);
#endif					
					assert(num_unknowns <= 1);
					assert(!is_clause_satisfiable(clause_idx)); // the clause was not satisfied yet.
					ASSIGN_WATCH_LITERAL(*watch2);
					assert(num_unknowns == 1); // if the number of unknowns is zero, than a conflit was generated, and this control point is not reached.
					assert(is_clause_satisfiable(clause_idx));
					DBG_CODE(branch_taken = 3;);
				}
			}
			if (must_keep) {
				KEEP_CLAUSE();
			}
			assert(branch_taken != 0);
		}
	}
	v.resize(j); // adjust size of the occurrences vector
	// cout << "AFTER " << v << endl;
	
	return 0;
}
