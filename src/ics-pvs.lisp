(in-package 'pvs)

;; Wrapping and unwrapping ICS values

(defmacro wrap (value)
  `(list ,value))

(defmacro unwrap (wrapper)
  `(car ,wrapper))


;; ICS-PVS error handling

(ff:defun-foreign-callable ocaml_error (fname msg)
  (error (format nil "~a: ~a" (excl:native-to-string fname)
		 (excl:native-to-string msg))))

(ff:def-foreign-call register_lisp_error_function (index))


;; PVS decision procedure interface

(defun ics-init (&optional full verbose)
  (ics_caml_startup (if full 1 0) #(0))
  (register_lisp_error_function
   (nth-value 1 (ff:register-function `ocaml_error)))
  (ics_set_verbose (if verbose 1 0)))

(defun ics-empty-state ()
  (let ((empty-state (wrap (ics_init))))
    (excl:schedule-finalization empty-state
				'ics-deregister-pointer)
    empty-state))

(defun ics-canon (state expr)
  (let ((term (ics_can (unwrap state) (unwrap (translate-to-ics expr)))))
    (prog1 (translate-from-ics term)
      (ics_deregister term))))

(defun ics-normalize (state expr)
  (declare (ignore state))
  (let ((term (unwrap (translate-to-ics expr))))
    (prog1
	(let ((*translate-fresh-vars* nil))
	  (declare (special *translate-fresh-vars))
	  (or (translate-from-ics term)
	      expr))           ;; result contained fresh variables (should not happen) 
      (ics_deregister term))))

(defun ics-d-consistent (ics_value)
  (let ((new-state (wrap (ics_d_consistent ics_value))))
    (excl:schedule-finalization new-state 'ics-deregister-pointer)
    new-state))

(defun ics-deregister-pointer (pointer)
  (ics_deregister (unwrap pointer)))

(defvar *verbose* nil)

(defmethod ics-process :around (state term)
  (declare (special *verbose*))
  (cond (*verbose*
	 (format t "~%Process" (ics_term_pp (unwrap term)))
	 (let ((status (call-next-method)))
	   (cond ((ics_is_consistent status)
		  (format t "~% --> Ok!"))
		 ((ics_is_inconsistent status)
		  (format t "~% --> Inconsistent!"))
		 ((ics_is_redundant status)
		  (format t "~% --> Redundant!")))
	   status))
	(t
	 (call-next-method))))

(defmethod ics-process (state term)
  (ics_process (unwrap state) (unwrap term)))

(defun ics-display-state (state)
  (ics_state_pp (unwrap state)))

(defun ics-solution (state expr)
  (assert (typep expr 'name-expr))
  (let* ((term (translate-to-ics expr))
	 (solution (ics_solution (unwrap state) (unwrap term))))
    (when (ics_is_some solution)
      (prog1
	  (let ((*translate-fresh-vars* nil))
	    (declare (special *translate-fresh-vars))
	    (or (translate-from-ics term)
		expr)          ;; result contained fresh variables  
	    (ics_deregister ics-term))))))

(defun ics-context (state)
  (let ((eqns (ics_ctxt_of (unwrap state))))
    (flet ((translate-eqns (lst)
			   (if (holds (ics_is_nil lst)) nil
			     (let* ((hd (ics_head lst))
				    (lhs (ics_fst hd))
				    (rhs (ics_snd hd)))
			       (acons (translate-from-ics lhs)
				      (translate-from-ics rhs)
				      (translate-eqns (ics_tail lst)))))))
      (translate-eqns eqns))))

(defun ics-constraint (state expr)
  (let* ((term (translate-to-ics expr))
	 (cnstrnt (ics_cnstrnt (unwrap expr) (unwrap term))))
    (prog1
	(translate-cnstrnt-app-from-ics cnstrnt cnstrnt (unwrap term))
      (ics_deregister term))))
    
	
    
;; Translating from PVS expressions to ICS terms

(defvar *pvs-to-ics-hash* (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
(defvar *ics-to-pvs-hash* (make-hash-table :test 'eql))

(defun translate-to-ics (expr)
  (translate-to-ics* expr))

(defmethod translate-to-ics* :around (expr)
  (let ((hv (gethash expr *pvs-to-ics-hash*)))
    (or hv
	(let ((ics-pointer (wrap (call-next-method))))
	  (unless (numberp expr)
	    (excl:schedule-finalization ics-pointer 'ics-deregister-pointer)
	 ;   (unless (listp expr)
	 ;     (setf (gethash (ics_tag (unwrap ics-pointer)) *ics-to-pvs-hash*)
	 ;          expr))
	    )
	  (setf (gethash expr *pvs-to-ics-hash*) ics-pointer)))))


(defmethod translate-to-ics* ((expr expr))
  (ics_mk_fresh "c"))

(defmethod translate-to-ics* ((expr name-expr))
  (declare (special *true*))
  (declare (special *false*))
  (cond ((tc-eq expr *true*)
	 (ics_mk_true))
	((tc-eq expr *false*)
	 (ics_mk_false))
	((empty-set? expr)
	 (ics_mk_empty (tag-of-set-expr expr)))
	((full-set? expr)
	 (ics_mk_full (tag-of-set-expr expr)))
	((zero-bv? expr)
	 (ics_mk_bv_zero (width-of-fixed-bv expr)))
	((one-bv? expr)
	 (ics_mk_bv_one (width-of-fixed-bv expr)))
	(t
	 (ics_mk_var (unique-name expr)))))

(defun unique-name (expr)
  (string (id expr)))           ;; not quite right


(defmethod translate-to-ics* ((expr number-expr))
  (ics_mk_num (ics_num_of_string (format nil "~d" (number expr)))))

(defmethod translate-to-ics* ((num fixnum))
  num)

(defmethod translate-to-ics* ((expr equation))
  (ics_mk_equal (unwrap (translate-to-ics* (args1 expr)))
		(unwrap (translate-to-ics* (args2 expr)))))

(defmethod translate-to-ics* ((expr disequation))
  (ics_mk_diseq (unwrap (translate-to-ics* (args1 expr)))
		(unwrap (translate-to-ics* (args2 expr)))))

(defmethod translate-to-ics* ((expr branch))
  (ics_mk_cond (unwrap (translate-to-ics* (condition expr)))
	       (unwrap (translate-to-ics* (then-part expr)))
	       (unwrap (translate-to-ics* (else-part expr)))))

(defmethod translate-to-ics* ((expr negation))
  (cond ((equation? (args1 expr))
	 (ics_mk_diseq (unwrap (translate-to-ics* (args1 (args1 expr))))
		    (unwrap (translate-to-ics* (args2 (args1 expr))))))
	((disequation? (args1 expr))
	 (ics_mk_equal (unwrap (translate-to-ics* (args1 (args1 expr))))
		    (unwrap (translate-to-ics* (args2 (args1 expr))))))
	(t
	 (ics_mk_not (unwrap (translate-to-ics* (args1 expr)))))))

(defmethod translate-ics* ((expr iff-or-boolean-equation))
  (let ((lhs (unwrap (translate-to-ics* (args1 expr))))
	(rhs (unwrap (translate-to-ics* (args2 expr)))))
    (ics_mk_iff lhs rhs)))

(defmethod translate-to-ics* ((expr implication))
  (ics_mk_imp (unwrap (translate-to-ics* (args1 expr)))
	      (unwrap (translate-to-ics* (args2 expr)))))

(defmethod translate-to-ics* ((expr conjunction))
  (ics_mk_and (unwrap (translate-to-ics* (args1 expr)))
	      (unwrap (translate-to-ics* (args2 expr)))))

(defmethod translate-to-ics* ((expr disjunction))
  (ics_mk_or (unwrap (translate-to-ics* (args1 expr)))
	     (unwrap (translate-to-ics* (args2 expr)))))

(defmethod translate-to-ics* ((ex application))
  (let ((op (operator ex)))
    (cond ((update-expr? op)
	   (translate-to-ics*
	    (make!-reduced-application
	     (translate-update-to-if op)
	     (args ex))))
	  ((tc-eq op (plus-operator))
	   (ics_mk_plus (ics_cons (unwrap (translate-to-ics* (args1 ex)))
				  (ics_cons (unwrap (translate-to-ics* (args2 ex)))
					    (ics_nil)))))
	  ((tc-eq op (difference-operator))
	   (ics_mk_minus (unwrap (translate-to-ics* (args1 ex)))
			 (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (unary-minus-operator))
	   (ics_mk_unary_minus (unwrap (translate-to-ics* (args1 ex)))))
	  ((tc-eq op (times-operator))
	   (ics_mk_times (ics_cons (unwrap (translate-to-ics* (args1 ex)))
				   (ics_cons (unwrap (translate-to-ics* (args2 ex)))
					     (ics_nil)))))
	  ((tc-eq op (divides-operator))
	   (ics_mk_div (unwrap (translate-to-ics* (args1 ex)))
		       (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (greatereq-operator))
	   (ics_mk_ge (unwrap (translate-to-ics* (args1 ex)))
		      (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (greater-operator))
	   (ics_mk_gt (unwrap (translate-to-ics* (args1 ex)))
		      (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (less-operator))
	   (ics_mk_lt (unwrap (translate-to-ics* (args1 ex)))
		      (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (lesseq-operator))
	   (ics_mk_le (unwrap (translate-to-ics* (args1 ex)))
		      (unwrap (translate-to-ics* (args2 ex)))))
	  ((tc-eq op (integer_pred))
	   (ics_mk_int (unwrap (translate-to-ics* (args1 ex)))))
	  ((or (tc-eq op (real_pred))
               (tc-eq op (rational_pred)))
	   (ics_mk_real (unwrap (translate-to-ics* (args1 ex)))))
	  ((union-set? op)
	   (ics_mk_union (tag-of-set-expr ex)
			 (unwrap (translate-to-ics* (args1 ex)))
			 (unwrap (translate-to-ics* (args2 ex)))))
	  ((intersection-set? op)
	   (ics_mk_inter (tag-of-set-expr ex)
			 (unwrap (translate-to-ics* (args1 ex)))
			 (unwrap (translate-to-ics* (args2 ex)))))
	  ((complement-set? op)
	   (ics_mk_compl (tag-of-set-expr ex)
			 (unwrap (translate-to-ics* (args1 ex)))))
	  ((symmetric-difference-set? op)
	   (ics_mk_sym_diff (tag-of-set-expr ex)
			    (unwrap (translate-to-ics* (args1 ex)))
			    (unwrap (translate-to-ics* (args2 ex)))))
	  ((difference-set? op)
	   (ics_mk_diff (tag-of-set-expr ex)
			(unwrap (translate-to-ics* (args1 ex)))
			(unwrap (translate-to-ics* (args2 ex)))))
	  ((and (conc-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv (args1 ex)))
		 (m (width-of-fixed-bv (args2 ex))))
	     (ics_mk_bv_conc
	      (ics_pair (unwrap (translate-to-ics* n))
			(unwrap (translate-to-ics* (args1 ex))))
	      (ics_pair (unwrap (translate-to-ics* m))
			(unwrap (translate-to-ics* (args2 ex)))))))
	  ((and (extr-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv (args1 ex)))
		 (i (args1 (args2 ex)))
		 (j (args2 (args2 ex))))
	     (ics_mk_bv_extr
	      (ics_pair (unwrap (translate-to-ics* n))
			(unwrap (translate-to-ics* (args1 ex))))
	      (unwrap (translate-to-ics* i))
	      (unwrap (translate-to-ics* j)))))
	  ((and (bitwise-and-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_mk_bv_and (unwrap (translate-to-ics* n))
			    (unwrap (translate-to-ics* (args1 ex)))
			    (unwrap (translate-to-ics* (args2 ex))))))
	  ((and (bitwise-or-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_mk_bv_or (unwrap (translate-to-ics* n))
			   (unwrap (translate-to-ics* (args1 ex)))
			   (unwrap (translate-to-ics* (args2 ex))))))
	  ((and (bitwise-xor-bv? op) (fixed-bv? ex))
	   (let ((n (width-of-fixed-bv ex)))
	     (ics_mk_bv_xor (unwrap (translate-to-ics* n))
			    (unwrap (translate-to-ics* (args1 ex)))
			    (unwrap (translate-to-ics* (args2 ex))))))
	  (t
	   (ics_mk_app
	    (unwrap (translate-to-ics* op))
	    (unwrap (translate-to-ics* (arguments ex))))))))

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
(defvar *ics-tags-to-types-counter* 0)

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

(defmethod translate-to-ics* ((expr record-expr))
  (let ((exprs (mapcar #'expression (sort-assignments (assignments expr)))))
    (ics_mk_tuple (unwrap (translate-to-ics* exprs)))))

(defun sort-assignments (assignments)
  (sort (copy-list assignments)
	#'string-lessp
	:key #'(lambda (assignment)
		 (id (caar (arguments assignment))))))

(defmethod translate-to-ics* ((ex tuple-expr))
  (ics_mk_tuple (unwrap (translate-to-ics* (exprs ex)))))
	
(defmethod translate-to-ics* ((ex coercion))
  (with-slots (operator argument) ex
    (let ((reduced-ex (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* ex)))))
      (translate-to-ics* reduced-ex))))

(defmethod translate-to-ics* ((l null))
  (ics_nil))

(defmethod translate-to-ics* ((l cons))
  (ics_cons (unwrap (translate-to-ics* (car l)))
	    (unwrap (translate-to-ics* (cdr l)))))

(defmethod translate-to-ics* ((ex projection-application))
  (ics_mk_proj (unwrap (translate-to-ics* (1- (index ex))))
	       (unwrap (translate-to-ics* (width-of (type (argument ex)))))
	       (unwrap (translate-to-ics* (argument ex)))))

(defmethod translate-to-ics* ((ex field-application))
  (with-slots (id argument type) ex
    (let* ((fields (fields (find-supertype (type argument))))
	   (sfields (sort-fields fields))
	   (pos (position id sfields
			  :test #'(lambda (x y) (eq x (id y))))))
      (ics_mk_proj (unwrap (translate-to-ics* pos))
		   (unwrap (translate-to-ics* (length fields)))
		   (unwrap (translate-to-ics* argument))))))

(defmethod width-of ((type tupletype))
  (length (types type)))


;; Translation back from ICS terms to PVS terms.
;; If *translate-fresh-vars* is set to [nil], this function
;; returns [nil] whenever the corresponding ics term contains fresh variables

(defvar *translate-fresh-vars* nil)

(defmacro holds (arg)
  `(plusp ,arg))

(defun translate-from-ics (ptr)
  (let ((trm (if (consp ptr) (unwrap ptr) ptr)))
    (or
  ;   (gethash (ics_tag trm) *ics-to-pvs-hash*)   ;; cached translations (in particular contains all names)
     (cond 
      ((holds (ics_is_app trm))                  ;; uninterpreted function symbols
       (translate-app-from-ics trm))
      ((holds (ics_is_update trm))              ;; function updates
       (translate-update-from-ics trm))
;      ((holds (ics_is_cond trm))
;       (translate-cond-from-ics trm))
      ((holds (ics_is_arith trm))               ;; arithmetic
       (translate-arith-from-ics trm))
      ((holds (ics_is_tuple trm))               ;; tuples
       (translate-tuple-from-ics trm))
      ((holds (ics_is_proj trm))
       (translate-proj-from-ics trm))
      ((holds (ics_is_bool trm))                 ;; boolean
       (translate-bool-from-ics trm))
      ((holds (ics_is_set trm))                  ;; sets
       (translate-empty-from-ics trm))
      ((holds (ics_is_bv trm))                   ;; bit vectors
       (translate-bv-from-ics trm))
      ((holds (ics_is_fresh trm))
       (when *translate-fresh-vars*
	 (translate-fresh-from-ics trm)))
      (t
       (error "ICS to PVS translation: shouldn't happen"))))))


(defun translate-list-from-ics (icslist)
  (if (holds (ics_is_nil icslist))
      nil 
      (cons (translate-from-ics (ics_head icslist))
	    (translate-list-from-ics (ics_tail icslist)))))


;; Arithmetic expressions

(defun translate-arith-from-ics (trm)
  (assert (ics_is_arith trm))
  (cond ((holds (ics_is_num trm))                  ;; arith terms
	 (translate-num-from-ics trm))
	((holds (ics_is_plus trm))
	 (translate-plus-from-ics trm))
	((holds (ics_is_minus trm))
	 (translate-minus-from-ics trm))
	((holds (ics_is_multq trm))
	 (translate-multq-from-ics trm))
	((holds (ics_is_mult trm))
	 (translate-mult-from-ics trm))
	((holds (ics_is_divide trm))
	 (translate-div-from-ics trm))))

(defun translate-q-from-ics (q)
  (let ((str (ics_string_of_num q)))
    (make!-number-expr
     (parse-integer (excl:native-to-string str)))))
 
(defun translate-num-from-ics (trm)
  (assert (ics_is_num trm))
  (translate-q-from-ics (ics_d_num trm)))
	 
(defun translate-plus-from-ics (trm)
  (assert (holds (ics_is_plus trm)))
  (let ((lst (ics_d_plus trm)))
    (if (holds (ics_is_nil (ics_tail lst)))
	(translate-from-ics (ics_head lst))
	(make!-plus (translate-from-ics (ics_head lst))
		    (translate-plus-from-ics (ics_tail lst))))))

(defun translate-minus-from-ics (trm)
  (assert (holds (ics_is_minus trm)))
  (let ((trm2 (ics_d_minus trm)))
    (make!-difference (translate-from-ics (ics_fst trm2))
		      (translate-from-ics (ics_snd trm2)))))

(defun translate-mult-from-ics (trm)
  (assert (holds (ics_is_mult trm)))
  (let ((lst (ics_d_mult trm)))
    (if (holds (ics_is_nil (ics_tail lst)))
	(translate-from-ics (ics_head lst))
	(make!-times (translate-from-ics (ics_head lst))
		     (translate-mult-from-ics (ics_tail lst))))))

(defun translate-multq-from-ics (trm)
  (assert (holds (ics_is_multq trm)))
  (let ((q_trm (ics_d_multq trm)))
    (make!-times (translate-q-from-ics (ics_fst q_trm))
		 (translate-from-ics (ics_snd q_trm)))))

(defun translate-div-from-ics (trm)
  (assert (holds (ics_is_div trm)))
  (let ((trm2 (ics_d_div trm)))
    (make!-divides (translate-q-from-ics (ics_fst trm2))
		   (translate-from-ics (ics_snd trm2)))))


;; Translating Boolean terms

(defun translate-bool-from-ics (trm)
  (declare (special *true*))
  (declare (special *false*))
  (assert (ics_is_bool trm))
  (cond ((holds (ics_is_true trm))        
	 *true*)      
	((holds (ics_is_false trm))
	 *false*)
;	((holds (ics_is_diseq trm))
;	 (translate-equal-from-ics trm))
	((holds (ics_is_equal trm))
	 (translate-equal-from-ics trm))
	((holds (ics_is_ite trm))
	 (translate-boolean-connective-from-ics trm))))

(defun translate-boolean-connective-from-ics (trm)
  (assert (holds (ics_is_ite trm)))
  (cond ((ics_is_not trm)
	 (make!-negation
	  (translate-from-ics (ics_d_not trm))))
	((ics_is_disj trm)
	 (let ((trm2 (ics_d_disj trm)))
	   (make!-disjunction
	    (translate-from-ics (ics_fst trm2))
	    (translate-from-ics (ics_snd trm2)))))
	((ics_is_conj trm)
	 (let ((trm2 (ics_d_conj trm)))
	   (make!-conjunction
	    (translate-from-ics (ics_fst trm2))
	    (translate-from-ics (ics_snd trm2)))))
	((ics_is_imp trm)
	 (let ((trm2 (ics_d_imp trm)))
	   (make!-implication
	    (translate-from-ics (ics_fst trm2))
	    (translate-from-ics (ics_snd trm2)))))
	((ics_is_iff trm)
	 (let ((trm2 (ics_d_iff trm)))
	   (make!-iff
	    (translate-from-ics (ics_fst trm2))
	    (translate-from-ics (ics_snd trm2)))))
	(t
	 (let ((trm3 (ics_d_ite trm)))
	   (make!-if-expr
	    (translate-from-ics (ics_fst_of_triple trm3))
	    (translate-from-ics (ics_snd_of_triple trm3))
	    (translate-from-ics	(ics_third_of_triple trm3)))))))

(defun translate-equal-from-ics (trm)
  (assert (holds (ics_is_equal trm)))
  (let ((trm2 (ics_d_equal trm)))
    (make!-equation (translate-from-ics (ics_fst trm1))
		    (translate-from-ics (ics_snd trm2)))))

(defun translate-diseq-from-ics (trm)
  (assert (holds (ics_is_diseq trm)))
  (let ((trm2 (ics_d_diseq trm)))
    (make!-disequation (translate-from-ics (ics_fst trm1))
		       (translate-from-ics (ics_snd trm2)))))


;; Translating function application and function update


(defun translate-app-from-ics (trm)
  (assert (holds (ics_is_app trm)))
  (let* ((trm2 (ics_d_app trm))
	 (op (ics_fst trm2))
	 (args (ics_snd trm2)))
    (if (and (holds (ics_is_cnstrnt op))
	     (not (holds (ics_is_nil args)))
	     (holds (ics_is_nil (ics_tail args))))
	(let ((arg (ics_head args)))
	  (translate-cnstrnt-app-from-ics op args))
	(make!-application (translate-from-ics op)
			   (translate-list-from-ics args)))))

(defun translate-cnstrnt-app-from-ics (cnstrnt trm)
  (assert (holds (ics_is_cnstrnt cnstrnt)))
  (let ((lst (ics_cnstrnt_to_list cnstrnt)))
    (make!-disjunction*
     (mapcar #'(lambda (interval)
		 (translate-interval-app-from-ics interval trm))
       lst))))

(defun translate-interval-app-from-ics (interval trm)
  (declare (special *true*))
  (let ((dinterval (ics_d_interval interval))
	(dom (ics_fst_of_triple dinterval))
	(low (ics_snd_of_triple dinterval))
	(high (ics_third_of_triple dinterval)))
    (cond ((and (holds (low_bound_is_neginf low))
		(holds (low_bound_is_posinf high)))
	   (translate-dom-from-ics trm))
	  ((holds (low_bound_is_neginf low))
	   (let ((num (translate-numberal-from-ics
		       (ics_high_bound_value high))))
	     (if (ics_high_bound_is_strict high)
		 (make-less (translate-from-ics trm) num)
		 (make-lesseq (translate-from-ics trm) num))))
	  ((holds (high_bound_is_posinf high))
	   (let ((num (translate-numberal-from-ics
		       (ics_low_bound_value low))))
	     (if (ics_low_bound_is_strict low)
		 (make-greater (translate-from-ics trm) num)
		 (make-greatereq (translate-from-ics trm) num))))
	  (t
	   (let* ((num1 (translate-numberal-from-ics
			 (ics_low_bound_value low)))
		  (num2 (translate-numberal-from-ics
			 (ics_high_bound_value high)))
		  (expr (translate-from-ics trm))
		  (ineq1 (if (ics_low_bound_is_strict low)
			     (make-less num1 expr)
			     (make-lesseq num1 expr)))
		  (ineq2 (if (ics_high_bound_is_strict high)
			     (make-greater num1 expr)
			     (make-greatereq num1 expr)))
		  (ineq (make!-conjunction ineq1 ineq2))
		  (pred (translate-dom-from-ics dom)))
	     (if (tc-eq pred *true*)
		 ineq
		 (make!-conjunction ineq pred)))))))

(defun translate-dom-from-ics (dom)
  (declare (special *true*))
  (cond ((holds (ics_interval_domain_is_int dom))
	 (make-integer_pred (translate-from-ics trm)))
	((holds (ics_interval_domain_is_nonintreal dom))
	 (make!-negation
	  (make-integer_pred (translate-from-ics trm))))
	(t
	 *true*))) 


;; Translating updates

(defun translate-update-from-ics (trm)
  (assert (holds (ics_is_update trm))) 
  (let ((trm3 (ics_d_update trm)))
    (make!-update-expr
     (translate-from-ics (ics_fst_of_triple trm3))
     (mk-assignment 'uni 
       (list (list (translate-from-ics (ics_snd_of_triple trm3))))
       (translate-from-ics (ics_third_of_triple trm3))))))


;; Translating non-boolean conditionals

(defun translate-cond-from-ics (trm)
  (assert (holds (ics_is_cond trm))) 
  (let ((trm3 (ics_d_cond trm)))
    (make!-if-expr
     (translate-from-ics (ics_fst_of_triple trm))
     (translate-from-ics (ics_snd_of_triple trm))
     (translate-from-ics (ics_third_of_triple trm)))))


;; Translating tuple expressions

(defun translate-tuple-from-ics (trm)
  (assert (holds (ics_is_tuple trm)))
  (make!-tuple-expr
   (translate-list-from-ics (ics_d_tuple trm))))

(defun translate-proj-from-ics (trm)
  (assert (holds (ics_is_proj trm)))
  (let* ((index_length_trm (ics_d_proj trm))
	 (index (ics_fst_of_triple index_length_trm))
	 (arg (ics_third_of_triple index_length_trm)))
    (make!-projection-application index (translate-from-ics arg))))


;; Translating sets

(defun translate-set-from-ics (trm)
  (assert (holds (ics_is_set trm)))
  (cond ((holds (ics_is_empty trm))
	 (translate-empty-from-ics trm))
	((holds (ics_is_full trm))
	 (translate-full-from-ics trm))
	((holds (ics_is_finite trm))
	 (translate-finite-from-ics trm))
	((holds (ics_is_cnstrnt trm))
         (translate-cnstrnt-from-ics trm))
	(t ; (holds (ics_is_setite trm))
	 (translate-setite-from-ics trm))))


(defun set-operation-from-ics (op tag)
  (let* ((type (gethash tag *ics-tags-to-types-hash*))
	 (ops (format nil "sets[~a].~a" type op))
	 (tops (pc-typecheck (pc-parse ops 'expr))))
    (setf (mod-id tops) nil)
    tops))

(defun translate-empty-from-ics (trm)
  (assert (holds (ics_is_empty trm)))
  (set-operation-from-ics "emptyset" (ics_d_empty trm)))

(defun translate-full-from-ics (trm)
  (assert (holds (ics_is_full_set trm)))
  (set-operation-from-ics "fullset" (ics_d_full trm)))

(defun translate-union-from-ics (trm)
  (assert (holds (ics_is_union trm)))
  (let* ((tag_trm_trm (ics_d_union trm))
	 (tag (ics_fst_of_triple tag_trm_trm))
	 (trm1 (ics_snd_of_triple tag_trm_trm))
	 (trm2 (ics_third_of_triple tag_trm_trm)))
    (make!-application (set-operation-from-ics "union" tag)     
		       (translate-from-ics trm1)
		       (translate-from-ics trm2))))

(defun translate-intersection-from-ics (trm)
  (assert (holds (ics_is_inter trm)))
  (let* ((tag_trm_trm (ics_d_inter trm))
	 (tag (ics_fst_of_triple tag_trm_trm))
	 (arg1 (ics_snd_of_triple tag_trm_trm))
	 (arg2 (ics_third_of_triple tag_trm_trm)))
    (make!-application (set-operation-from-ics "intersection" tag)     
		       (translate-from-ics arg1)
		       (translate-from-ics arg2))))

(defun translate-compl-from-ics (trm)
  (assert (holds (ics_is_compl trm)))
  (let* ((tag_trm (ics_d_inter trm))
	 (tag (ics_fst tag_trm))
	 (arg (ics_snd_of_triple tag_trm)))
    (make!-application (set-operation-from-ics "complement" tag)     
		       (translate-from-ics arg))))

(defun translate-setite-from-ics (trm)
  (assert (holds (ics_is_setite trm)))
  (let* ((tag_trm3 (ics_d_inter trm))
	 (tag (ics_fst_of_quadruple tag_trm3))
	 (union (set-operation-from-ics "union" tag))
	 (compl (set-operation-from-ics "complement" tag))
	 (inter (set-operation-from-ics "inter" tag))
	 (arg1 (translate-from-ics (ics_snd_of_quadruple tag_trm3)))
	 (arg2 (translate-from-ics (ics_third_of_quadruple tag_trm3)))
	 (arg3 (translate-from-ics (ics_fourth_of_quadruple tag_trm3))))
    (make!-application union
		       (make!-application inter arg1 arg2)
		       (make!-application inter
					  (make!-application compl arg1)
					  arg3))))

(defun translate-cnstrnt-from-ics (trm)
  (assert (holds (ics_is_cnstrnt trm)))
  (let* ((cnstrnt (ics_d_cnstrnt trm))
	 (lst (ics_cnstrnt_to_list cnstrnt)))
    (break)))
    

(defun translate-finite-from-ics (trm)
  (assert (holds (ics_is_finite trm)))
  (break))

;; Translating bitvector terms

(defun translate-bv-from-ics (trm)
  (cond ((holds (ics_is_bv_const trm))        ;; bit vectors
	 (translate-bitvector-const-from-ics trm))
	((holds (ics_is_bv_conc trm))
	 (translate-bitvector-conc-from-ics trm))
	((holds (ics_is_bv_extr trm))
	 (translate-bitvector-extr-from-ics trm))
	(; (holds (ics_is_bv_ite trm))
	 (translate-bitvector-ite-from-ics trm))))

(defun translate-bitvector-const-from-ics (trm)
  (assert (holds (ics_is_bv_const trm)))
  (let* ((args (ics_d_bv_const trm))
	 (size (ics_fst args))
	 (const (ics_snd args))
	 (ops (format nil "bv[~d].~a" size const)))
    (pc-typecheck (pc-parse ops 'expr))))

(defun translate-bitvector-conc-from-ics (trm)
  (assert (holds (ics_is_bv_conc trm)))
  (break))

(defun translate-bitvector-extr-from-ics (trm)
  (assert (holds (ics_is_bv_extr trm)))
  (break))

(defun translate-bitvector-bvite-from-ics (trm)
  (assert (holds (ics_is_bv_ite trm)))
  (break))

;; Translating fresh variables

(defun translate-fresh-from-ics (trm)
  (assert (holds (ics_is_fresh trm)))
  (let* ((str (ics_d_var trm))
	 (id (intern str))
	 (type (type-of-fresh trm))
	 (const (mk-new-const id type))
	 (ptr (wrap trm)))
    (excl:schedule-finalization ptr 'ics-deregister-pointer)
    (setf (gethash (ics_tag (unwrap ptr)) *ics-to-pvs-hash*) const)
    (setf (gethash const *pvs-to-ics-hash*) ptr)
    const))

(defun type-of-fresh (trm)   ;; for now...
  *integer*)

(defun mk-new-const (id type)
  "Generating a fresh constant and adding it to the current context"
  (let ((fresh-id (new-sko-symbol id *current-context*)))
    (makeskoconst fresh-id type *current-context*)))
