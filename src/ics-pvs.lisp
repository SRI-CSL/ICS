(in-package 'pvs)

(defun ics-init (&optional full verbose)
  (ics_caml_startup (if full 1 0) #(0))
  (register_lisp_error_function
   (nth-value 1 (ff:register-function `ocaml_error)))
  (ics_set_verbose (if verbose 1 0)))

(defun ics-empty-state ()
  (let ((empty-state (list (ics_empty_state))))
    (excl:schedule-finalization empty-state 'ics-deregister-pointer)
    empty-state))

(defun ics-canon (state pvs-expr)
  (let* ((ics-expr (translate-to-ics pvs-expr))
	 (ics-term (ics_canon (car state) (car ics-expr))))
    (prog1 (translate-from-ics ics-term)
      (ics_deregister ics-term))))

(defun ics-d-consistent (ics_value)
  (let ((new-state (list (ics_d_consistent ics_value))))
    (excl:schedule-finalization new-state 'ics-deregister-pointer)
    new-state))

(defun ics-deregister-pointer (pointer)
  (ics_deregister (car pointer)))

(defun ics-process (state term)
  (ics_process (car state) (car term)))

(defvar *pvs-to-ics-hash* (make-hash-table :hash-function 'pvs-sxhash
					   :test 'tc-eq))
(defvar *ics-to-pvs-hash* (make-hash-table :test 'eql))

(defun translate-to-ics (expr)
  (translate-to-ics* expr))

(defmethod translate-to-ics* :around (ex)
  (let ((hv (gethash ex *pvs-to-ics-hash*)))
    (or hv
	(let ((ics-pointer (list (call-next-method))))
	  (unless (numberp ex)
	    (excl:schedule-finalization ics-pointer 'ics-deregister-pointer)
	    (unless (listp ex)
	      (setf (gethash (ics_tag (car ics-pointer)) *ics-to-pvs-hash*)
		    ex)))
	  (setf (gethash ex *pvs-to-ics-hash*) ics-pointer)))))

(defmethod translate-to-ics* ((ex expr))
  ;;(format t "***ics warning***: complex operator ignored: ~a" ex)
  (ics_fresh (ics_nil)))

(defmethod translate-to-ics* ((ex name-expr))
  (cond ((tc-eq ex *true*)
	 (ics_ptrue))
	((tc-eq ex *false*)
	 (ics_pfalse))
	((empty-set? ex)
	 (ics_empty_set (tag-of-set-expr ex)))
	((full-set? ex)
	 (ics_full_set (tag-of-set-expr ex)))
	((zero-bv? ex)
	 (ics_bv_zero (width-of-fixed-bv ex)))
	((one-bv? ex)
	 (ics_bv_one (width-of-fixed-bv ex)))
	(t (ics_var (string (id ex)))))) ;; not quite right

(defmethod translate-to-ics* ((ex number-expr))
  (ics_num (ics_num_of_string (format nil "~d" (number ex)))))

(defmethod translate-to-ics* ((num fixnum))
  num)

(defmethod translate-to-ics* ((ex equation))
  (ics_equal (car (translate-to-ics* (args1 ex)))
	     (car (translate-to-ics* (args2 ex)))))

(defmethod translate-to-ics* ((ex disequation))
  (ics_diseq (car (translate-to-ics* (args1 ex)))
	     (car (translate-to-ics* (args2 ex)))))

(defmethod translate-to-ics* ((ex branch))
  (ics_ite (car (translate-to-ics* (condition ex)))
	   (car (translate-to-ics* (then-part ex)))
	   (car (translate-to-ics* (else-part ex)))))

(defmethod translate-to-ics* ((ex negation))
  (cond ((equation? (args1 ex))
	 (ics_diseq (car (translate-to-ics* (args1 (args1 ex))))
		    (car (translate-to-ics* (args2 (args1 ex))))))
	((disequation? (args1 ex))
	 (ics_equal (car (translate-to-ics* (args1 (args1 ex))))
		    (car (translate-to-ics* (args2 (args1 ex))))))
	(t (call-next-method))))

(defmethod translate-ics* ((ex iff-or-boolean-equation))
  (let ((lhs (car (translate-to-ics* (args1 ex))))
	(rhs (car (translate-to-ics* (args2 ex)))))
    (ics_and (ics_implies lhs rhs)
	     (ics_implies rhs lhs))))

(defmethod translate-to-ics* ((ex implication))
  (ics_implies (car (translate-to-ics* (args1 ex)))
	       (car (translate-to-ics* (args2 ex)))))

(defmethod translate-to-ics* ((ex conjunction))
  (ics_and (car (translate-to-ics* (args1 ex)))
	   (car (translate-to-ics* (args2 ex)))))

(defmethod translate-to-ics* ((ex disjunction))
  (ics_or (car (translate-to-ics* (args1 ex)))
	  (car (translate-to-ics* (args2 ex)))))

(defmethod translate-to-ics* ((ex application))
  (let ((op (operator ex)))
    (cond ((tc-eq op (plus-operator))
	   (ics_plus (ics_cons (car (translate-to-ics* (args1 ex)))
			       (ics_cons (car (translate-to-ics* (args2 ex)))
					 (ics_nil)))))
	  ((tc-eq op (difference-operator))
	   (ics_minus (car (translate-to-ics* (args1 ex)))
		      (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (unary-minus-operator))
	   (ics_unary_minus (car (translate-to-ics* (args1 ex)))))
	  ((tc-eq op (times-operator))
	   (ics_times (ics_cons (car (translate-to-ics* (args1 ex)))
				(ics_cons (car (translate-to-ics* (args2 ex)))
					  (ics_nil)))))
	  ((tc-eq op (divides-operator))
	   (ics_divide (car (translate-to-ics* (args1 ex)))
		       (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (greatereq-operator))
	   (ics_ge (car (translate-to-ics* (args1 ex)))
		   (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (greater-operator))
	   (ics_gt (car (translate-to-ics* (args1 ex)))
		   (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (less-operator))
	   (ics_lt (car (translate-to-ics* (args1 ex)))
		   (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (lesseq-operator))
	   (ics_le (car (translate-to-ics* (args1 ex)))
		   (car (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (integer_pred))
	   (ics_integer_pred (car (translate-to-ics* (args1 ex)))))
	  ((union-set? op)
	   (ics_union (tag-of-set-expr ex)
		      (car (translate-to-ics* (args1 ex)))
		      (car (translate-to-ics* (args2 ex)))))
	  ((intersection-set? op)
	   (ics_inter (tag-of-set-expr ex)
		      (car (translate-to-ics* (args1 ex)))
		      (car (translate-to-ics* (args2 ex)))))
	  ((complement-set? op)
	   (ics_compl (tag-of-set-expr ex)
		      (car (translate-to-ics* (args1 ex)))))
	  ((symmetric-difference-set? op)
	   (ics_sym_diff (tag-of-set-expr ex)
			 (car (translate-to-ics* (args1 ex)))
			 (car (translate-to-ics* (args2 ex)))))
	  ((difference-set? op)
	   (ics_diff (tag-of-set-expr ex)
		     (car (translate-to-ics* (args1 ex)))
		     (car (translate-to-ics* (args2 ex)))))
	  ((and (conc-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv (args1 ex)))
		 (m (width-of-fixed-bv (args2 ex))))
	     (ics_bv_conc (car (translate-to-ics* n))
			  (car (translate-to-ics* (args1 ex)))
			  (car (translate-to-ics* m))
			  (car (translate-to-ics* (args2 ex))))))
	  ((and (extr-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv (args1 ex)))
		 (i (args1 (args2 ex)))
		 (j (args2 (args2 ex))))
	     (ics_bv_extr (car (translate-to-ics* n))
			  (car (translate-to-ics* (args1 ex)))
			  (car (translate-to-ics* i))
			  (car (translate-to-ics* j)))))
	  ((and (bitwise-and-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_bv_and (car (translate-to-ics* n))
			 (car (translate-to-ics* (args1 ex)))
			 (car (translate-to-ics* (args2 ex))))))
	  ((and (bitwise-or-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_bv_or (car (translate-to-ics* n))
			(car (translate-to-ics* (args1 ex)))
			(car (translate-to-ics* (args2 ex))))))
	  ((and (bitwise-xor-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_bv_xor (car (translate-to-ics* n))
			 (car (translate-to-ics* (args1 ex)))
			 (car (translate-to-ics* (args2 ex))))))
	  ((name-expr? op)
	   (let ((str (string (id op)))) ;; not quite right
	     (ics_app str (car (translate-to-ics* (arguments ex))))))
	  (t
	   (call-next-method)))))

(defun op? (op modulename name)
  (and (typep op 'name-expr)
       (eq (id (module-instance op)) modulename)
       (eq (id op) name)))

(defun empty-set? (op) (op? op '|sets| '|emptyset|))
(defun full-set? (op) (op? op '|sets| '|fullset|))
(defun union-set? (op) (op? op '|sets| '|union|))
(defun intersection-set? (op) (op? op '|sets| '|intersection|))
(defun complement-set? (op) (op? op '|sets| '|complement|))
(defun difference-set? (op) (op? op '|sets| '|difference|))
(defun symmetric-difference-set? (op) (op? op '|sets| '|symmetric_difference|))

(defun zero-bv? (op) (op? op '|bv| '|bvec0|))
(defun one-bv? (op)  (op? op '|bv| '|bvec1|))
(defun conc-bv? (op) (op? op '|bv_concat| '|o|))
(defun extr-bv? (op) (op? op '|bv_caret| '|^|))
(defun bitwise-and-bv? (op) (op? op '|bv_bitwise| '|AND|))
(defun bitwise-or-bv? (op) (op? op '|bv_bitwise| '|OR|))
(defun bitwise-xor-bv? (op) (op? op '|bv_bitwise| '|XOR|))
(defun bitwise-not-bv? (op) (op? op '|bv_bitwise| '|NOT|))

(defun fixed-bv? (ex)
  (let* ((type (find-supertype (type ex)))
	 (ptype (print-type type)))
    (and ptype
	 (eq (id ptype) '|bvec|)
	 (eq (id (module-instance ptype)) '|bv|)
	 (let ((actual (car (actuals (module-instance ptype)))))
	   (number-expr? (expr actual))))))
  
(defun width-of-fixed-bv (ex)
  (let* ((type (find-supertype (type ex)))
	 (ptype (print-type type)))
    (and ptype
	 (eq (id ptype) '|bvec|)
	 (eq (id (module-instance ptype)) '|bv|)
	 (let ((actual (car (actuals (module-instance ptype)))))
	   (and (number-expr? (expr actual))
	        (number (expr actual)))))))

(defvar *ics-types-to-tags-hash* (make-hash-table :hash-function 'pvs-sxhash
						  :test 'tc-eq))
(defvar *ics-tags-to-types-hash* (make-hash-table :test 'eql))
(defvar *ics-tags-to-types-counter* nil)

(defun tag-of-set-expr (ex)
  (let* ((type (find-supertype (type ex)))
	 (set-type (domain type)))
    (or (gethash set-type *ics-types-to-tags-hash*)
	(let ((tag (incf *ics-tags-to-types-counter*)))
	  (setf (gethash set-type *ics-types-to-tags-hash*) tag)
	  (setf (gethash tag *ics-tags-to-types-hash*) set-type)
	  tag))))

(defmethod translate-to-ics* ((ex cases-expr))
  (translate-to-ics* (translate-cases-to-if ex)))

(defmethod translate-to-ics* ((ex let-expr))
  (with-slots (operator argument) ex
    (let ((reduced-ex (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* ex)))))
      (translate-to-ics* reduced-ex))))

(defmethod translate-to-ics* ((ex tuple-expr))
  (ics_tup (car (translate-to-ics* (exprs ex)))))
	
(defmethod translate-to-ics* ((ex coercion))
  (with-slots (operator argument) ex
    (let ((reduced-ex (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* ex)))))
      (translate-to-ics* reduced-ex))))

(defmethod translate-to-ics* ((l null))
  (ics_nil))

(defmethod translate-to-ics* ((l cons))
  (ics_cons (car (translate-to-ics* (car l)))
	    (car (translate-to-ics* (cdr l)))))

(defmethod translate-to-ics* ((ex projection-application))
  (ics_proj (car (translate-to-ics* (1- (index ex))))
	    (car (translate-to-ics* (width-of (type (argument ex)))))
	    (car (translate-to-ics* (argument ex)))))

(defmethod translate-to-ics* ((ex field-application))
  (with-slots (id argument type) ex
    (let* ((fields (fields (find-supertype (type argument))))
	   (sfields (sort-fields fields))
	   (pos (position id sfields
			  :test #'(lambda (x y) (eq x (id y))))))
      (ics_proj (car (translate-to-ics* pos))
		(car (translate-to-ics* (length fields)))
		(car (translate-to-ics* argument))))))

(defmethod width-of ((type tupletype))
  (length (types type)))

(ff:defun-foreign-callable ocaml_error (fname msg)
  (error (format nil "~a: ~a" (excl:native-to-string fname)
		 (excl:native-to-string msg))))

(ff:def-foreign-call register_lisp_error_function (index))


;;; Update expressions
;;; Translate expressions of the form
;;; A WITH [ (0) := 1 ],
;;;    where A is an array of type int->int, into
;;; (APPLY int ARRAYSTORE A 0 1)
;;;
;;; f WITH [ (0,0) := 0],
;;;    where f is a function of type int,int->int into
;;; (APPLY int UPDATE f (0 0) 0)
;;;
;;; g WITH [ (0) := h, (1) (x,y) := 0, (1) (x,y)' := 1 ]
;;;    where g and h are functions of type
;;;    T = [function[int -> function[state[T0],state[T0] -> int]]
;;;
;;; This generates the form
;;;
;;; (APPLY function[state[T0],state[T0] -> int]
;;;        UPDATE
;;;        (APPLY function[state[T0],state[T0] -> int]
;;;               UPDATE
;;;               (APPLY function[state[T0],state[T0] -> int]
;;;                      UPDATE
;;;                      g (0) h)
;;;               (1) (APPLY int UPDATE g(1) (x y) 0))
;;;        (1) (APPLY int UPDATE g(1) (x' y') 1))

(defmethod translate-to-ics* ((expr update-expr))
  (translate-assignments-ics (assignments expr)
			     (car (translate-to-ics* (expression expr)))
			     (type expr)))

(defun translate-assignments-ics (assigns trbasis type)
  (if assigns
      (translate-assignments-ics
       (cdr assigns)
       (translate-assignment-ics (car assigns)
				 trbasis type)
       type)
      trbasis))

(defun translate-assignment-ics (assign trbasis type)
  (translate-assign-args-ics
   (arguments assign)
   (expression assign)
   trbasis
   (find-supertype type)))

(defun translate-assign-args-ics (args value trbasis type)
  (if (null args) (translate-to-ics* value)
      (let* ((sorted-fields (when (recordtype? type)
			      (sort-fields (fields type))))
	     (pos (typecase type
		    (recordtype
		     (car (translate-to-ics*
			   (position (id (caar args)) sorted-fields
				     :test #'eq :key #'id))))
		    (tupletype
		     (car (translate-to-ics* (1- (number (caar args))))))
		    (t (if (singleton? (car args))
			   (car (translate-to-ics* (caar args)))
			   (ics_tup (car (translate-to-ics* (car args))))))))
	     (val (let* ((ntrbasis-type
			  (find-supertype
			   (typecase type
			     (recordtype
			      (type (find (id (caar args)) (fields type)
					  :test #'eq :key #'id)))
			     (tupletype
			      (nth (1- (number (caar args)))
				   (types type)))
			     (t (range type)))))
			 (ntrbasis
			  (typecase type
			    (recordtype
			     (make-ics-field-application
			      (mk-funtype type ntrbasis-type)
			      (position (id (caar args)) sorted-fields
					:test #'eq :key #'id)
			      trbasis))
			    (tupletype
			     (make-ics-projection-application
			      ntrbasis-type (number (caar args)) trbasis))
			    (t (make-ics-assign-application
				type
				trbasis
				(if (singleton? (car args))
				    (car (translate-to-ics* (caar args)))
				    (ics_tup (car (translate-to-ics* (car args))))))))))
		    (translate-assign-args (cdr args)
					   value
					   ntrbasis
					   ntrbasis-type))))
	(ics_update trbasis pos val))))

(defun make-ics-field-application (field-accessor-type fieldnum dc-expr)
  (let ((pos (car (translate-to-ics* fieldnum)))
	(len (length (types field-accessor-type))))
    (ics_proj pos len dc-expr)))

(defun make-ics-projection-application (type number expr)
  (let ((pos (car (translate-to-ics* (1- number))))
	(len (length (types type))))
    (ics_proj pos len expr)))
	    
(defun make-ics-assign-application (fun-type expr args)
  (ics_app expr args)) ; ???


;; Translation back from ICS terms to PVS terms

(defun translate-list-from-ics (icslist)
  (if (plusp (ics_is_nil icslist))
      nil 
      (cons (translate-from-ics (ics_head icslist))
	    (translate-list-from-ics (ics_tail icslist)))))

(defun translate-plus-from-ics (icslist)
  (if (plusp (ics_is_nil (ics_tail icslist)))
      (translate-from-ics (ics_head icslist))
      (make!-plus (translate-from-ics (ics_head icslist))
		  (translate-plus-from-ics (ics_tail icslist)))))

(defun translate-times-from-ics (icslist)
  (if (plusp (ics_is_nil (ics_tail icslist)))
      (translate-from-ics (ics_head icslist))
      (make!-times (translate-from-ics (ics_head icslist))
		   (translate-times-from-ics (ics_tail icslist)))))

(def-pvs-term emptybv "empty_bv" "empty_bv")

(defun bv-const-from-ics (icsbv const)
  (let* ((size (ics_d_bv_size icsbv))
	 (ops (format nil "bv[~d].~a" size const)))
    (pc-typecheck (pc-parse ops 'expr))))

(defun bv-conc-from-ics (icsbv1 icsbv2)
  (let* ((size1 (ics_d_bv_size icsbv1))
	 (size2 (ics_d_bv_size icsbv2))
	 (ops (format nil "bv_concat_def[~d,~d].o" size1 size2))
	 (op (pc-typecheck (pc-parse ops 'expr))))
    (make!-application op (translate-from-ics icsbv1) 
		       (translate-from-ics icsbv2))))
		     
(defun bv-extr-from-ics (icsbv i j)
  (let* ((size (ics_d_bv_size icsbv))
	 (ops (format nil "bv_caret[~d].^" size))
	 (op (pc-typecheck (pc-parse ops 'expr))))
    (make!-application op (translate-from-ics icsbv) 
		       (make!-number-expr j) (make!-number-expr i))))

(defun bv-bitwise-from-ics (bwop icsbv1 icsbv2)
   (let* ((size (ics_d_bv_size icsbv1))
	 (ops (format nil "bv_biwise[~d].~a" size bwop))
	 (op (pc-typecheck (pc-parse ops 'expr))))
    (make!-application op (translate-from-ics icsbv1)
		       (translate-from-ics icsbv2))))

(defun set-operation-from-ics (expr op)
  (let* ((type (gethash (ics_d_set_type_tag expr) *ics-tags-to-types-hash*))
	 (ops (format nil "sets[~a].~a" type op))
	 (tops (pc-typecheck (pc-parse ops 'expr))))
    (setf (mod-id tops) nil)
    tops))

(defun translate-from-ics (expr-pointer)
  (let ((expr (if (consp expr-pointer)
		  (car expr-pointer)
		  expr-pointer)))
    (or
     ;; cached translations (in particular contains all names)
     (gethash (ics_tag expr) *ics-to-pvs-hash*)
     (cond 
      ;; uninterpreted function symbols
      ((plusp (ics_is_app expr))
       (let ((dexpr (ics_d_app expr)))
	 (make!-application (translate-from-ics (ics_fst dexpr))
			    (translate-list-from-ics (ics_snd dexpr)))))
      ;; arith terms
      ((plusp (ics_is_num expr))
       (make!-number-expr
	(parse-integer
	 (excl:native-to-string (ics_string_of_num (ics_d_num expr))))))
      ((plusp (ics_is_plus expr))
       (translate-plus-from-ics (ics_d_plus expr)))
      ((plusp (ics_is_minus expr))
       (let ((dexpr (ics_d_minus expr)))
	 (make!-difference (translate-from-ics (ics_fst dexpr))
			   (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_unary_minus expr))
       (make!-minus (translate-from-ics (ics_d_unary_minus expr))))
      ((plusp (ics_is_times expr))
       (translate-times-from-ics (ics_d_times expr)))
      ((plusp (ics_is_divide expr))
       (let ((dexpr (ics_d_divide expr)))
	 (make!-divides (translate-from-ics (ics_fst dexpr))
			(translate-from-ics (ics_snd dexpr)))))
      ;; tuples
      ((plusp (ics_is_tup expr))
       (make!-tuple-expr (translate-list-from-ics (ics_d_tup expr))))
      ((plusp (ics_is_proj expr))
       (let ((dexpr (ics_d_proj expr)))
	 (make!-projection-application (ics_snd (ics_fst dexpr))
				       (translate-from-ics (ics_snd dexpr)))))
      ;; arrays 
      ((plusp (ics_is_lookup expr))
       (let ((dexpr (ics_d_lookup expr)))
	 (make!-application (translate-from-ics (ics_fst dexpr))
			    (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_update expr))
       (let* ((dexpr (ics_d_update expr))
	      (ddexpr (ics_snd dexpr)))
	 (make!-update-expr 
	  (translate-from-ics (ics_fst dexpr))
	  (mk-assignment 'uni 
	    (list (list (translate-from-ics (ics_fst ddexpr))))
	    (translate-from-ics (ics_snd ddexpr))))))
      ;; atoms
      ((plusp (ics_is_ptrue expr)) *true*)
      ((plusp (ics_is_pfalse expr)) *false*)
      ((plusp (ics_is_equal expr))
       (let ((dexpr (ics_d_equal expr)))
	 (make!-equation (translate-from-ics (ics_fst dexpr))
			 (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_diseq expr))
       (let ((dexpr (ics_d_diseq expr)))
	 (make!-disequation (translate-from-ics (ics_fst dexpr))
			    (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_lt expr))
       (let ((dexpr (ics_d_lt expr)))
	 (make-less (translate-from-ics (ics_fst dexpr))
		    (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_le expr))
       (let ((dexpr (ics_d_le expr)))
	 (make-lesseq (translate-from-ics (ics_fst dexpr))
		      (translate-from-ics (ics_snd dexpr)))))
      ;; other props
      ;; TODO: reconstruct logical connectives from if-then-else props
      ((plusp (ics_is_ite expr))
       (let* ((dexpr (ics_d_ite expr))
	      (ddexpr (ics_snd dexpr)))
	 (make!-if-expr (translate-from-ics (ics_fst dexpr))
			(translate-from-ics (ics_fst ddexpr))
			(translate-from-ics (ics_snd ddexpr)))))
      ;; sets
      ((plusp (ics_is_empty_set expr))
       (set-operation-from-ics expr "emptyset"))
      ((plusp (ics_is_full_set expr))
       (set-operation-from-ics expr "fullset"))
      ((plusp (ics_is_compl expr))
       (make!-application (set-operation-from-ics expr "complement")
			  (translate-from-ics (ics_d_compl expr))))
      ((plusp (ics_is_inter expr))
       (let* ((dexpr (ics_d_inter expr)))
	 (make!-application (set-operation-from-ics expr "intersection")
			    (translate-from-ics (ics_fst dexpr))
			    (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_union expr))
       (let* ((dexpr (ics_d_union expr)))
	 (make!-application (set-operation-from-ics expr "union")
			    (translate-from-ics (ics_fst dexpr))
			    (translate-from-ics (ics_snd dexpr)))))
      ((plusp (ics_is_setite expr))
       (let* ((dexpr (ics_d_setite expr))
	      (a (ics_fst dexpr))
	      (ta (translate-from-ics a))
	      (ddexpr (ics_snd dexpr))
	      (b (ics_fst ddexpr))
	      (tb (translate-from-ics b))
	      (c (ics_snd ddexpr))
	      (tc (translate-from-ics c)))
	 (make!-application 
	  (set-operation-from-ics expr "union")
	  (make!-application (set-operation-from-ics expr "intersection")
			     ta tb)
	  (make!-application (set-operation-from-ics expr "intersection")
			     (make!-application 
			      (set-operation-from-ics expr "complement") ta) 
			     tc))))
      ;; bit vectors
      ((plusp (ics_is_bv_eps expr)) (emptybv))
      ((plusp (ics_is_bv_zero expr))
       (bv-conc-from-ics expr "bvec0"))
      ((plusp (ics_is_bv_one expr))
       (bv-conc-from-ics expr "bvec1"))
      ((plusp (ics_is_bv_conc expr))
       (let ((dexpr (ics_d_bv_conc expr)))
	 (bv-conc-from-ics (ics_fst dexpr) (ics_snd dexpr))))
      ((plusp (ics_is_bv_extr expr))
       (let* ((dexpr (ics_d_bv_extr expr))
	      (ddexpr (ics_snd dexpr)))
	 (bv-extr-from-ics (ics_fst dexpr) (ics_fst ddexpr) (ics_snd ddexpr))))
      ((plusp (ics_is_bv_and expr))
       (let ((dexpr (ics_d_bv_and expr)))
	 (bv-bitwise-from-ics "and" (ics_fst dexpr) (ics_snd dexpr))))
      ((plusp (ics_is_bv_or expr))
       (let ((dexpr (ics_d_bv_or expr)))
	 (bv-bitwise-from-ics "or" (ics_fst dexpr) (ics_snd dexpr))))
      ((plusp (ics_is_bv_xor expr))
       (let ((dexpr (ics_d_bv_xor expr)))
	 (bv-bitwise-from-ics "xor" (ics_fst dexpr) (ics_snd dexpr))))
      ;; else
      (t (error "ICS to PVS translation: shouldn't happen"))))))
