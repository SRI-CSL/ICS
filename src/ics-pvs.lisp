(in-package :pvs)

;; Wrapping and unwrapping ICS values in order to finalize 
;; objects of these types.

(defstruct (wrap
	    (:predicate wrap?)
	    (:constructor make-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
               (format t "<#wrap: ~a>" (wrap-address p)))))
  address)

(defun wrap (value)
  (assert (integerp value))
  (make-wrap value))

(defun unwrap (w)
  (assert (wrap? w))
  (wrap-address w))

(defun wrap= (w1 w2)
  (assert (wrap? w1))
  (assert (wrap? w2))
  (= (wrap-address w1) (wrap-address w2)))

(defun wrap-finalize! (w)
  (assert (wrap? w))
  (excl:schedule-finalization w 'ics-deregister-pointer))

(defun wrap-free! (w)
  (assert (wrap? w))
  (ics_deregister (unwrap w)))

(defun value-free! (v)
  (assert (integerp v))
  (ics_deregister v))

(defstruct (term-wrap
	    (:include wrap)
	    (:predicate term-wrap?)
	    (:constructor make-term-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#term: ~a>" (wrap-address p))))))

(defun term-wrap (value)
   (assert (integerp value))
   (make-term-wrap value))

(defun term-unwrap (w)
  (assert (term-wrap? w))
  (wrap-address w))

(defstruct (state-wrap
	    (:include wrap)
	    (:predicate state-wrap?)
	    (:constructor make-state-wrap (address))
	    (:print-function
	     (lambda (p s k)
	       (declare (ignore k))
	       (format s "<#state: ~a>" (wrap-address p))))))

(defun state-wrap (value)
   (assert (integerp value))
   (make-state-wrap value))

(defun state-unwrap (w)
  (assert (state-wrap? w))
  (wrap-address w))


;; check if ICS predicate holds

(defmacro holds (arg)
  `(plusp ,arg))

;; Macros for destructering ICS values

(defmacro with-ics-pair ((arg1 arg2) ics-pair &body body)
  (let ((resultsym (gensym)))
    `(let* ((,resultsym ,ics-pair)
	    (,arg1 (ics_fst ,resultsym))
	    (,arg2 (ics_snd ,resultsym)))
       (progn ,@body))))

(defmacro with-ics-triple ((arg1 arg2 arg3) ics-triple &body body)
  (let ((resultsym (gensym)))
    `(let* ((,resultsym ,ics-triple)
	    (,arg1 (ics_fst_of_triple ,resultsym))
	    (,arg2 (ics_snd_of_triple ,resultsym))
	    (,arg3 (ics_third_of_triple ,resultsym)))
       (progn ,@body))))

(defmacro with-ics-list ((head tail) ics-list &body body)
  (let ((resultsym (gensym)))
    `(let* ((,resultsym ,ics-list)
	    (,head (ics_head ,resultsym))
	    (,tail (ics_tail ,resultsym)))
       (progn ,@body))))

(defmacro with-ics-option (value ics-option &body body)
  (let ((resultsym (gensym)))
    `(let* ((,resultsym ,ics-option)
	    (,option (when (ics_is_some ,resultsym)
		       (ics_value_of ,resultsym))))
       (progn ,@body))))

(defmacro with-ics-interval ((domain lowkind lowval highval highkind) ics-interval &body body)
  (let ((resultsym (gensym))
	(domainsym (gensym))
	(lowvalsym (gensym))
	(highvalsym (gensym)))
    `(let* ((,resultsym ,ics-interval))
       (with-ics-triple (,domainsym ,lowvalsym ,highvalsym) ,ics-interval
	  (let ((,domain (cond ((holds (ics_interval_domain_is_int ,domainsym)) :integer)
			       ((holds (ics_interval_is_domain_is_real ,domainsym)) :real)
			       ((holds (ics_interval_is_domain_is_nonint ,domainsym)) :nonintreal))))
	    (multiple-value-bind (,lowkind ,lowval)
		(cond ((holds (ics_low_bound_is_neginf ,lowvalsym))
		       (values :open :neginf))
		      ((holds (ics_low_bound_is_open ,lowvalsym))
		       (values :open (ics_low_bound_value ,lowvalsym)))
		      ((holds (ics_low_bound_is_closed ,lowvalsym))
		       (values :closed (ics_low_bound_value ,lowvalsym))))
	      (multiple-value-bind (,highval ,highkind)
		  (cond ((holds (ics_high_bound_is_posinf ,highvalsym))
			 (values :posinf :open))
			((holds (ics_high_bound_is_open ,highvalsym))
			 (values (ics_high_bound_value ,highvalsym) :open))
			((holds (ics_high_bound_is_closed ,highvalsym))
			 (values (ics_high_bound_value ,highvalsym) :closed)))
		(progn ,@body))))))))

(defmacro ics_cons2 (trm1 trm2)
  `(ics_cons ,trm1 (ics_cons ,trm2 (ics_nil))))


;; Some PVS recognizers

(defun satisfies-predicate? (expr id modinst)
  (assert (typep expr 'expr))
  (flet ((predicate? (type)
	   (and (typep type 'subtype)
	        (predicate type)
	        (is? (predicate type) id modinst))))
    (or (predicate? (type expr))
	(some #'predicate? (judgement-types+ expr)))))

(defun associative? (expr)
  (satisfies-predicate? expr 'associative? 'operator_defs))

(defun commutative? (expr)
  (satisfies-predicate? expr 'commutative? 'operator_defs))

(defun attributes (expr)
  (let ((a (associative? expr))
	(c (commutative? expr)))
    (cond ((and a c) :ac)
	  (a :a)
	  (c :c))))
		

;; ICS-PVS error handling

(ff:defun-foreign-callable ics_error (fname msg)
  (error (format nil "~a: ~a" (excl:native-to-string fname)
		 (excl:native-to-string msg))))

(ff:def-foreign-call register_lisp_error_function (index))


;; PVS decision procedure interface

(defun ics-init (&optional full (verbose 0))
  (ics_caml_startup (if full 1 0) #(0))
  (register_lisp_error_function
   (nth-value 1 (ff:register-function `ics_error)))
  (ics_init verbose))

(defun ics-empty-state ()
  (let ((empty (state-wrap (ics_state_init))))
    (wrap-finalize! empty)
    empty))

(defun ics-d-consistent (value)
  (let ((state (state-wrap (ics_d_consistent value))))
    (wrap-finalize! state)
    state))

(defun ics-deregister-pointer (wrapper)
  (wrap-free! wrapper))

(defun ics-process (state term)
  (assert (state-wrap? state))
  (assert (term-wrap? term))
  (ics_process (state-unwrap state) (term-unwrap term)))


;; Additional ICS functionality

(defun ics-sigma (expr)
  (let ((term (term-unwrap (translate-to-ics expr))))
    (multiple-value-bind (expr bndngs)
	  (translate-from-ics term)
      (assert (null bndngs))
      (value-free! term)
      expr)))

(defun ics-canon (state expr)
  (let ((term (ics_can (state-unwrap state)
		       (term-unwrap (translate-to-ics expr)))))
    (multiple-value-bind (expr bndngs)
	(translate-from-ics term)
      (value-free! term)
      (if (null bndngs) expr
         (make!-forall-expr bndngs expr)))))

(defun ics-check (state expr &optional names)
  "Checks whether or not the expression EXPR holds in context STATE.
   Returns *TRUE* if expression EXPR is valid in context STATE,
   *FALSE* if it is unsatisfiable, and a list of solutions for VARS otherwise."
  (assert (state-wrap? state))
  (assert (typep expr 'expr))
  (let ((status (ics_check (state-unwrap state)
			   (term-unwrap (translate-to-ics expr)))))
    (cond ((holds (ics_is_check_valid status))
	   *true*)
	  ((holds (ics_is_check_inconsistent status))
	   *false*)
	  (t
	   (when names
	     (let ((states (translate-states-from-ics*
			    (ics_d_check_satisfiable status))))
	       (mapcar #'(lambda (state)
			   (ics-solution state names))
		       states)))))))

(defun translate-states-from-ics* (states)
  (if (holds (ics_is_nil states)) nil
    (with-ics-list (head tail) states
      (cons (state-wrap head) (translate-states-from-ics* tail)))))

(defun ics-solution (state names)
  "Given a STATE and PVS name expressions NAMES compute a list of
   solutions of the form (x . (e1 ... en)) for each x in NAMES; the
   ei do not contain any of NAMES as subterms. The second argument is
   the list of bindings for newly introduced variables."
  (assert (state-wrap? state))
  (assert (every #'(lambda (name) (typep name 'name-expr)) names))
  (let* ((vars (translate-list-as-set-to-ics* names))
	 (solution (ics_state_solutions (state-unwrap state) vars)))
    (translate-solution-from-ics solution)))
		

;; Translating from PVS expressions to ICS terms

(defvar *pvs-to-ics-hash* 
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))

(defvar *ics-to-pvs-hash* 
  (make-hash-table :test 'eql))

(defvar *translate-type-predicates* nil)

(defun translate-to-ics (expr)
  (translate-to-ics* expr))

(defmethod translate-to-ics* :around ((expr expr))
  (let ((hv (gethash expr *pvs-to-ics-hash*)))
    (or hv
	(let ((trm (call-next-method)))
	  (assert (integerp trm))
	  (let ((wrapper (term-wrap trm)))
	    (unless (numberp expr)
	      (wrap-finalize! wrapper))
	    (when (holds (ics_is_var trm))      ;; only hash names for back translation
	      (setf (gethash (ics_term_tag trm) *ics-to-pvs-hash*) expr))
	    (setf (gethash expr *pvs-to-ics-hash*) wrapper))))))

(defmethod translate-to-ics* ((expr expr))
  (let ((name (unique-name expr)))
    (cond ((integer? expr)
	   (ics_mk_intvar name))
	  ((real? expr)
	   (ics_mk_ratvar name))
	  ((tc-eq (type expr) *boolean*)
	   (ics_mk_boolvar name))
	  (t
	   (ics_mk_var name)))))

(defmethod translate-to-ics* ((expr name-expr))
  (declare (special *true*))
  (declare (special *false*))
  (cond ((tc-eq expr *true*)
	 (ics_mk_true))
	((tc-eq expr *false*)
	 (ics_mk_false))
	(t
	 (call-next-method))))

(defmethod unique-name ((expr name-expr))    ;; not quite right
  ; (symbol-name (intern (gensym (string (id expr))))))
  (symbol-name (id expr)))

(defmethod unique-name ((expr t))
  (symbol-name (intern (gensym))))

(defmethod translate-to-ics* ((expr number-expr))
  (let ((q (ics_num_of_string (format nil "~d" (number expr)))))
    (ics_mk_num q)))

(defmethod translate-to-ics* ((num fixnum))
  num)

(defun translate-unary-to-ics (ics-constructor expr)
  (assert (typep expr 'expr))
  (funcall ics-constructor (term-unwrap (translate-to-ics* expr))))

(defun translate-binary-to-ics (ics-constructor expr1 expr2)
  (assert (typep expr1 'expr))
  (assert (typep expr2 'expr))
  (funcall ics-constructor
	   (term-unwrap (translate-to-ics* expr1))
	   (term-unwrap (translate-to-ics* expr2))))

(defun translate-ternary-to-ics (ics-constructor expr1 expr2 expr3)
  (funcall ics-constructor
	   (term-unwrap (translate-to-ics* expr1))
	   (term-unwrap (translate-to-ics* expr2))
	   (term-unwrap (translate-to-ics* expr3))))

(defmethod translate-to-ics* ((expr equation))
  (translate-binary-to-ics #'ics_mk_equal (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr disequation))
  (translate-binary-to-ics #'ics_mk_diseq (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr branch))
  (translate-ternary-to-ics #'ics_mk_ite
			    (condition expr)
			    (then-part expr)
			    (else-part expr)))

(defmethod translate-to-ics* ((expr negation))
  (let ((arg (args1 expr)))
    (cond ((equation? arg)
	   (translate-binary-to-ics #'ics_mk_diseq (args1 arg) (args2 arg)))
	  ((disequation? arg)
	   (translate-binary-to-ics #'ics_mk_equal (args1 arg) (args2 arg)))
	  (t
	   (translate-unary-to-ics #'ics_mk_neg arg)))))

(defmethod translate-ics* ((expr iff-or-boolean-equation))
  (translate-binary-to-ics #'ics_mk_iff (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr implication))
  (translate-binary-to-ics #'ics_mk_imp (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr conjunction))
  (translate-binary-to-ics #'ics_mk_conj (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr disjunction))
  (translate-binary-to-ics #'ics_mk_disj (args1 expr) (args2 expr)))

(defmethod translate-to-ics* ((expr application))
  (declare (special *translate-type-predicates*))
  (let ((op (operator expr)))
    (cond ((update-expr? op)
	   (term-unwrap (translate-to-ics* (translate-update-to-if expr))))
	  ((tc-eq op (plus-operator))
	   (translate-binary-to-ics #'ics_mk_add2 (args1 expr) (args2 expr)))
	  ((tc-eq op (difference-operator))
	   (translate-binary-to-ics #'ics_mk_sub (args1 expr) (args2 expr)))
	  ((tc-eq op (unary-minus-operator))
	   (translate-unary-to-ics #'ics_mk_unary_minus (args1 expr)))
	  ((tc-eq op (times-operator))
	   (translate-binary-to-ics #'ics_mk_mult2 (args1 expr) (args2 expr)))
	  ((tc-eq op (divides-operator))
	   (translate-binary-to-ics #'ics_mk_div (args1 expr) (args2 expr)))
	  ((tc-eq op (greatereq-operator))
	   (translate-binary-to-ics #'ics_mk_ge (args1 expr) (args2 expr)))
	  ((tc-eq op (greater-operator))
	   (translate-binary-to-ics #'ics_mk_gt (args1 expr) (args2 expr)))
	  ((tc-eq op (less-operator))
	   (translate-binary-to-ics #'ics_mk_lt (args1 expr) (args2 expr)))
	  ((tc-eq op (lesseq-operator))
	   (translate-binary-to-ics #'ics_mk_le (args1 expr) (args2 expr)))
	  ((tc-eq op (integer_pred))
	   (if *translate-type-predicates*
	       (translate-unary-to-ics #'ics_mk_int (args1 expr))
	     (ics_mk_true)))
	  ((or (tc-eq op (real_pred)) ; ICS does not distinguish between rationals and reals
               (tc-eq op (rational_pred)))
	   (if *translate-type-predicates*
	       (translate-unary-to-ics #'ics_mk_real (args1 expr))
	     (ics_mk_true)))
	  (t
	   (translate-uninterp-to-ics* op (arguments expr))))))

(defun translate-uninterp-to-ics* (expr exprs)
  (let ((trm (term-unwrap (translate-to-ics* expr)))
	(trms (translate-list-to-ics* exprs)))
    (assert (not (null trm)))
    (let ((attrs (attributes expr)))
      (cond ((eql attrs :ac)
	     (ics_mk_uninterp_ac trm trms))
	    ((eql attrs :a)
	     (ics_mk_uninterp_a trm trms))
	    ((eql attrs :c)
	     (ics_mk_uninterp_c trm trms))
	    (t
	     (ics_mk_uninterp trm trms))))))

(defmethod translate-to-ics* ((expr cases-expr))
  (translate-to-ics* (translate-cases-to-if expr)))

(defmethod translate-to-ics* ((expr let-expr))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-ics* reduced-expr))))

(defmethod translate-to-ics* ((expr record-expr))
  (let* ((exprs (mapcar #'expression
			(sort-assignments (assignments expr))))
	 (trms (translate-list-to-ics* exprs)))
    (ics_mk_tuple trms)))

(defun sort-assignments (assignments)
  (sort (copy-list assignments)
	#'string-lessp
	:key #'(lambda (assignment)
		 (id (caar (arguments assignment))))))

(defmethod translate-to-ics* ((expr tuple-expr))
  (let ((trms (translate-list-to-ics* (exprs exprs))))
    (ics_mk_tuple trms)))
	
(defmethod translate-to-ics* ((expr coercion))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-ics* reduced-expr))))

(defun translate-list-to-ics* (exprs)
  (if (null exprs)
      (ics_nil)
    (let ((trm (translate-to-ics* (car exprs))))
      (ics_cons (term-unwrap trm) (translate-list-to-ics* (cdr exprs))))))

(defun translate-list-as-set-to-ics* (exprs)
  (if (null exprs)
      (ics_terms_empty)
    (let ((trm (translate-to-ics* (car exprs))))
      (ics_terms_add (term-unwrap trm) (translate-list-as-set-to-ics* (cdr exprs))))))


(defmethod translate-to-ics* ((expr projection-application))
  (let* ((arg (argument expr))
	 (width (width-of (type arg)))
	 (index (1- (index expr))))
    (translate-ternary-to-ics #'ics_mk_proj index width arg)))

(defmethod translate-to-ics* ((expr field-application))
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (pos (position id (sort-fields fields)
			  :test #'(lambda (x y) (eq x (id y)))))
	   (trm (unwrap (translate-to-ics* argument))))
      (ics_mk_proj pos (length fields) trm))))

(defmethod width-of ((type tupletype))
  (length (types type)))


;; Translation back from ICS terms to PVS terms.
;; If *translate-fresh-vars* is set to [nil], this function
;; returns [nil] whenever the corresponding ics term contains fresh variables

(defun translate-from-ics (arg)
  "Translate an ICS term to a PVS expression. This expression may
   contain fresh variables. The second result is a list of bindings
   for these newly introduced variables."
  (let ((trm (if (term-wrap? arg) (term-unwrap arg) arg))
	(*bndngs* nil))
    (declare (special *bndngs*))
    (let ((expr (translate-from-ics* trm)))
       (values expr *bndngs*))))

(defun translate-from-ics* (trm)
  (assert (integerp trm))
  (or
   (gethash (ics_term_tag trm) *ics-to-pvs-hash*)   ;; cached translations (contains all known names)
   (cond 
    ((holds (ics_is_uninterp trm))                  ;; uninterpreted function symbols
     (translate-app-from-ics* trm))
    ((holds (ics_is_update trm))               ;; function updates
     (translate-update-from-ics* trm))
    ((holds (ics_is_arith trm))                ;; arithmetic
     (translate-arith-from-ics* trm))
    ((holds (ics_is_tuple trm))                ;; tuples
     (translate-tuple-from-ics* trm))
    ((holds (ics_is_proj trm))
     (translate-proj-from-ics* trm))
    ((holds (ics_is_bool trm))                 ;; boolean
     (translate-bool-from-ics* trm))
    ((holds (ics_is_diseq trm))
     (translate-diseq-from-ics* trm))
    ((holds (ics_is_equal trm))
     (translate-equal-from-ics* trm))
    ((holds (ics_is_cnstrnt trm))
     (with-ics-pair (cnstrnt arg)
	  (ics_d_cnstrnt trm)
       (translate-cnstrnt-from-ics* cnstrnt arg)))
    ((holds (ics_is_fresh trm))
     (translate-fresh-from-ics* trm))
    ((holds (ics_is_rename_var trm))
     (with-ics-pair (name trm1)
	  (ics_d_rename_var trm)
       (translate-from-ics* trm1))) 
    (t
     (error "ICS to PVS translation: shouldn't happen"))))))

(defun translate-list-from-ics* (icslist)
  (if (holds (ics_is_nil icslist)) nil
    (with-ics-list (head tail) icslist
      (cons (translate-from-ics* head)
	    (translate-list-from-ics* tail)))))

(defun translate-terms-from-ics* (trms)       ;; translate as list
  (if (holds (ics_terms_is_empty trms)) '()
    (with-ics-pair (trm rest-trms)
          (ics_terms_choose trms)
       (cons (translate-from-ics* trm)
	     (translate-terms-from-ics* rest-trms)))))

;; Arithmetic expressions

(defun translate-arith-from-ics* (trm)
  (assert (ics_is_arith trm))
  (cond ((holds (ics_is_num trm))                  ;; arith terms
	 (translate-num-from-ics* trm))
	((holds (ics_is_add trm))
	 (translate-plus-from-ics* (ics_d_add trm)))
	((holds (ics_is_multq trm))
	 (translate-multq-from-ics* trm))
	((holds (ics_is_mult trm))
	 (translate-mult-from-ics* (ics_d_mult trm)))
	((holds (ics_is_div trm))
	 (translate-div-from-ics* trm))
	(t
	 (error "Unknown arithmetic term"))))

(defun translate-q-from-ics* (q)
  (let* ((str (ics_string_of_num q))
	 (val (parse-integer (excl:native-to-string str))))   ; how to parse a rational?
    (make!-number-expr val)))
 
(defun translate-num-from-ics* (trm)
  (assert (ics_is_num trm))
  (translate-q-from-ics* (ics_d_num trm)))
	 
(defun translate-plus-from-ics* (lst)
  (if (holds (ics_is_nil (ics_tail lst)))
      (translate-from-ics* (ics_head lst))
    (make!-plus (translate-from-ics* (ics_head lst))
		(translate-plus-from-ics* (ics_tail lst)))))

(defun translate-minus-from-ics* (trm)
  (assert (holds (ics_is_sub trm)))
  (with-ics-pair (arg1 arg2) (ics_d_sub trm)
     (make!-difference (translate-from-ics* arg1)
		       (translate-from-ics* arg2))))

(defun translate-mult-from-ics* (lst)
  (if (holds (ics_is_nil (ics_tail lst)))
      (translate-from-ics* (ics_head lst))
    (make!-times (translate-from-ics* (ics_head lst))
		 (translate-mult-from-ics* (ics_tail lst)))))

(defun translate-multq-from-ics* (trm)
  (assert (holds (ics_is_multq trm)))
  (with-ics-pair (q arg) (ics_d_multq trm)
    (make!-times (translate-q-from-ics* q)
		 (translate-from-ics* arg))))

(defun translate-div-from-ics* (trm)
  (assert (holds (ics_is_div trm)))
  (with-ics-pair (arg1 arg2) (ics_d_div trm)
    (make!-divides (translate-q-from-ics* arg1)
		   (translate-from-ics* arg2))))


;; Translating Boolean terms

(defun translate-bool-from-ics* (trm)
  (declare (special *true*))
  (declare (special *false*))
  (assert (ics_is_bool trm))
  (cond ((holds (ics_is_true trm))        
	 *true*)      
	((holds (ics_is_false trm))
	 *false*)
	((holds (ics_is_ite trm))
	 (translate-boolean-connective-from-ics* trm))))

(defun translate-boolean-connective-from-ics* (trm)
  (assert (holds (ics_is_ite trm)))
  (cond ((holds (ics_is_neg trm))
	 (make!-negation
	  (translate-from-ics* (ics_d_neg trm))))
	((holds (ics_is_disj trm))
	 (with-ics-pair (arg1 arg2) (ics_d_disj trm)
	   (make!-disjunction
	    (translate-from-ics* arg1)
	    (translate-from-ics* arg2))))
	((holds (ics_is_conj trm))
	 (with-ics-pair (arg1 arg2) (ics_d_conj trm)
	   (make!-conjunction
	    (translate-from-ics* arg1)
	    (translate-from-ics* arg2))))
	((holds (ics_is_imp trm))
	 (with-ics-pair (arg1 arg2) (ics_d_imp trm)
	   (make!-implication
	    (translate-from-ics* arg1)
	    (translate-from-ics* arg2))))
	((holds (ics_is_iff trm))
	 (with-ics-pair (arg1 arg2) (ics_d_iff trm)
	    (make!-iff
	     (translate-from-ics* arg1)
	     (translate-from-ics* arg2))))
	((holds (ics_is_ite trm))
	 (with-ics-triple (arg1 arg2 arg3) (ics_d_ite trm)
	   (make!-if-expr
	    (translate-from-ics* arg1)
	    (translate-from-ics* arg2)
	    (translate-from-ics* arg3))))
	(t
	 (error "ICS to PVS translation: unreachable"))))

(defun translate-equal-from-ics* (trm)
  (assert (holds (ics_is_equal trm)))
  (with-ics-pair (lhs rhs) (ics_d_equal trm)
    (make!-equation (translate-from-ics* lhs)
		    (translate-from-ics* rhs))))

(defun translate-diseq-from-ics* (trm)
  (assert (holds (ics_is_diseq trm)))
  (with-ics-pair (lhs rhs) (ics_d_diseq trm)
    (make!-disequation (translate-from-ics* lhs)
		       (translate-from-ics* rhs))))


;; Translating function application and function update


(defun translate-app-from-ics* (trm)
  (assert (holds (ics_is_uninterp trm)))
  (with-ics-pair (op args) (ics_d_uninterp trm)
      (make!-application
       (translate-from-ics* op)
       (translate-list-from-ics* args))))

;; Translating updates

(defun translate-update-from-ics* (trm)
  (assert (holds (ics_is_update trm))) 
  (with-ics-triple (arg1 arg2 arg3) (ics_d_update trm)
    (make!-update-expr
     (translate-from-ics* arg1)
     (mk-assignment 'uni 
       (list (list (translate-from-ics* arg2)))
       (translate-from-ics* arg3)))))

;; Translating tuple expressions

(defun translate-tuple-from-ics* (trm)
  (assert (holds (ics_is_tuple trm)))
  (make!-tuple-expr
   (translate-list-from-ics* (ics_d_tuple trm))))

(defun translate-proj-from-ics* (trm)
  (assert (holds (ics_is_proj trm)))
  (with-ics-triple (index length arg) (ics_d_proj trm)
    (make!-projection-application index (translate-from-ics* arg))))

;; Translating fresh variables

(defun translate-fresh-from-ics* (trm)
  (assert (holds (ics_is_fresh trm)))
  (with-ics-pair (str dom) (ics_d_fresh trm)
    (let* ((id (intern str))
	   (type (type-from-domain dom))
	   (bndng (mk-bind-decl id type type))
	   (expr (mk-name-expr id nil nil
			       (make-resolution bd nil type)))
	   (ptr (term-wrap trm)))
      (wrap-finalize! ptr)
      (setf (gethash (ics_term_tag trm) *ics-to-pvs-hash*) expr)
      (setf (gethash expr *pvs-to-ics-hash*) ptr)
      expr)))

(defun type-from-domain (dom)
  (cond ((holds (ics_is_intdom dom))
	 *integer*)
	((holds (ics_is_booldom dom))
	 *bool*)
	((holds (ics_is_ratdom dom))  ; ICS does not distinguish between rationals and reals
	 *real*)))


;; Translating constraints

(defun translate-cnstrnt-from-ics* (cnstrnt trm)
  (let ((expr (translate-from-ics* trm)))
    (assert (typep expr 'expr))
    (cond ((holds (ics_cnstrnt_is_bot cnstrnt))
	   *false*)
	  ((holds (ics_cnstrnt_is_top cnstrnt))
	   *true*)
	  ((holds (ics_cnstrnt_is_singleton cnstrnt))
	   (let ((q (ics_cnstrnt_d_singleton cnstrnt)))
	     (make!-equation expr (translate-q-from-ics* q))))
	  ((holds (ics_cnstrnt_is_boolean cnstrnt))
	   (if (subtype? (type expr) *boolean*) *true* *false*))
	  ((holds (ics_cnstrnt_is_tuple cnstrnt))
	   (break))
	  ((holds (ics_cnstrnt_is_arith cnstrnt))
	   (let ((intervals (lisp-list-of-ics-list (ics_cnstrnt_d_arith cnstrnt))))
	     (make!-disjunction*
	      (mapcar #'(lambda (interval)
			  (translate-interval-from-ics interval trm))
		      intervals)))))))

(defun lisp-list-of-ics-list (l)
  (if (ics_is_nil l) nil
    (cons (ics_hd l) (lisp-list-of-ics-list (ics_tl l)))))

(defun translate-interval-from-ics (interval trm)
  (declare (special *true*))
  (let ((expr (translate-from-ics* trm)))
    (with-ics-interval (domain lowkind lowval highval highkind) interval
       (let ((pred (make-domain-cnstrnt expr))
	     (ineq (cond ((and (eql lowval :neginf)
			       (eql highval :posinf))
			  *true*)
			 ((eql lowval :neginf)
			  (make-less-ineq expr highval highkind))
			 ((eql highval :posinf)
			  (make-greater-ineq expr lowval lowkind))
			 (t
			  (make!-conjunction
			     (make-less-ineq expr highval highkind)
			     (make-greater-ineq expr lowval lowkind))))))
	 (cond ((tc-eq pred *true*)
		ineq)
	       ((tc-eq pred *false*)
		*false*)
	       ((tc-eq ineq *true*)
		pred)
	       (t
		(make!-conjunction pred ineq)))))))

(defun make-less-ineq (expr highval kind)
  (let ((bound (translate-q-from-ics highval)))
    (cond ((eql highkind :closed)
	   (make-lesseq expr bound))
	  ((eql highkind :open)
	   (make-less expr bound)))))

(defun make-greater-ineq (expr lowval kind)
  (let ((bound (translate-q-from-ics lowval)))
    (cond ((eql lowkind :closed)
	   (make-greatereq expr bound))
	  ((eql lowkind :open)
	   (make-greater expr bound)))))

(defun make-domain-cnstrnt (dom expr)
  (declare (special *true*))
  (cond ((eql dom :integer)
	 (if (integer? expr) *true*
	   (make-integer_pred expr)))
	((eql dom :nonintreal)
	 (make!-conjunction
	  (make-real_pred expr)     
	  (make!-negation
	   (make-integer_pred expr))))
	((eql dom :real)
	 (if (real? expr) *true*
	   (make-real_pred expr)))))


;; Translating other ICS datatypes to PVS

(defun translate-term-list-from-ics (tl)
  "Translate a list of ICS terms to a corresponding list of
   PVS expression in the same order. Bindings for all newly
   introduced variables are returned as the second argument."
  (if (holds (ics_is_nil tl))
      (values nil nil)
    (with-ics-list (head tail) tl
       (multiple-value-bind (expr1 bndngs1)
            (translate-from-ics* head)
          (multiple-value-bind (exprs2 bndngs2)
                    (translate-term-list-from-ics tail)
            (let ((bndngs (union bndngs1 bndngs2 :test #'tc-eq))
		  (exprs (cons expr1 exprs2)))
	      (values exprs bndngs)))))))

(defun translate-terms-from-ics (trms)
  "Translate a set of ICS terms as a list of PVS expressions."
  (if (holds (ics_terms_is_empty trms))
      (values nil nil)
    (with-ics-pair (trm rest-trms)
          (ics_terms_choose trms)
       (multiple-value-bind (expr1 bndngs1)
	   (translate-from-ics* trm)
	 (multiple-value-bind (exprs2 bndngs2)
	     (translate-terms-from-ics rest-trms)
	   (let ((bndngs (union bndngs1 bndngs2 :test #'tc-eq))
		 (exprs (cons expr1 exprs2)))
	     (values exprs bndngs)))))))

(defun translate-subst-from-ics (subst)
  "Translate a set of ICS terms as a list of PVS expressions. Also
   returns bndngs for the newly introduced variables for fresh ICS terms."
  (translate-term-cross-term-list-from-ics*
   (ics_subst_to_list subst)))

(defun translate-term-cross-term-list-from-ics* (lst)
  (if (holds (ics_is_nil lst))
      (values nil nil)
    (with-ics-pair (var trm)
	(ics_head lst)
      (multiple-value-bind (expr-of-var bndngs-of-var)
	  (translate-from-ics var)
	(assert (null bndngs-of-var))
	(multiple-value-bind (expr-of-trm bndngs-of-trm)
	    (translate-from-ics trm)
	  (multiple-value-bind (exprs-of-rest bndngs-of-rest)
	      (translate-term-cross-term-list-from-ics* (ics_tail lst))
	    (let ((bndngs (union bndngs-of-trm bndngs-of-rest :test #'tc-eq))
		  (pairs (acons expr-of-var expr-of-trm exprs-of-rest)))
	      (values pairs bndngs))))))))

(defun translate-solution-from-ics (solutions)
  "Generate a list with bindings of the form (x . (e1 ... en))
   from an ICS solution; the order is unspecified. Eventually,
   all expressions ei should to be external."
  (if (holds (ics_is_nil solutions))
      (values nil nil)
    (with-ics-pair (var trms)
	(ics_head lst)
      (multiple-value-bind (expr-of-var bndngs-of-var)
	  (translate-from-ics var)
	(assert (null bndngs-of-var))
	(multiple-value-bind (exprs-of-trms bndngs-of-trms)
	    (translate-terms-from-ics trms)
	  (multiple-value-bind (exprs-of-rest bndngs-of-rest)
	      (translate-solution-from-ics (ics_tail lst))
	    (let ((bndngs (union bndngs-of-trms bndngs-of-rest :test #'tc-eq))
		  (pairs (acons expr-of-var expr-of-trms exprs-of-rest)))
	      (values pairs bndngs))))))))

(defun translate-map-term-to-terms-from-ics (map)
  "Generate an association list with entries of the form
   (TRM . (TRM1 ... TRMn)) from MAP; the order is unspecified."
  (translate-term-cross-terms-list-from-ics* (ics_map_to_list map)))

(defun translate-term-cross-terms-list-from-ics* (lst)
  (break "to do"))

;; Pretty-printing PVS equivalents of various ICS data structures

(defun ics-term-pp (trm)
  (multiple-value-bind (expr bndngs)
      (translate-from-ics trm)
    (declare (ignore bndngs))
    (format t "~a" expr)))

(defun ics-term-list-pp (lst)
  (multiple-value-bind (exprs bndngs)
      (translate-term-list-from-ics lst)
    (declare (ignore bndngs))
    (format t "\[~{~a~^,~}\]" exprs)))

(defun ics-term-set-pp (set)
  (multiple-value-bind (exprs bndngs)
      (translate-terms-from-ics set)
    (declare (ignore bndngs))
    (format t "\{~{~a~^,~}\}" exprs)))

(defun ics-subst-pp (subst)
  (multiple-value-bind (assocs bndngs)
      (translate-subst-from-ics subst)
    (declare (ignore bndngs))
    (loop for (x . y) in assocs
	  do (format t  "~a |-> ~a~%" x y))))

(defun ics-map-term-to-terms-pp (map)
  (multiple-value-bind (assocs bndngs)
      (translate-map-term-to-terms-from-ics map)
    (declare (ignore bndngs))
    (loop for (trm . trms) in assocs
	  do (ics-term-pp trm)
	     (format t " |-> ")
	     (ics-term-set-pp trms)
	     (format t "~%"))))

(defun ics-solution-pp (solution)
   (multiple-value-bind (assocs bndngs)
      (translate-solution-from-ics solution)
    (declare (ignore bndngs))
    (loop for (trm . trms) in assocs
	  do (format t  "~a |-> ~{~a~^,~}~%" trm trms))))
	
;; Querying ICS data base

(defun ics-query (state &key (context? t) uninterp arith bool tuple diseqs cnstrnts)
  (when context? (ics-query-context state))
  (ics-query-find arith (ics_theory_arith) state)
  (ics-query-find bool (ics_theory_boolean) state)
  (ics-query-find tuple (ics_theory_tuple) state)
  (ics-query-find uninterp (ics_theory_eq) state)
  (ics-query-diseqs diseqs state)
  (ics-query-cnstrnts cnstrnts state))

(defun ics-query-context (state)
  "Display the logical context of STATE."
  (assert (state-wrap? state))
  (let ((trms (ics_state_ctxt_of (state-unwrap state))))
    (multiple-value-bind (exprs bndngs)
	(translate-term-list-from-ics trms)
      (declare (ignore bndngs))
      (format t "~%Context:~%")
      (loop for expr in exprs do
	    (format t "~%~a" expr)))))

(defmethod ics-query-find ((arg null) theory state)
  (declare (ignore state))
  nil)

(defmethod ics-query-find ((arg string) theory state)
  (let ((expr (pc-typecheck (pc-parse arg 'expr))))
    (ics-query-find expr theory state)))

(defmethod ics-query-find ((arg expr) theory state)
  (assert (state-wrap? state))
  (let ((trm (term-unwrap (translate-to-ics arg)))
	(state (state-unwrap state)))
    (ics-term-pp (ics_state_find theory state trm))))

(defmethod ics-query-find (arg theory state)
  (assert (state-wrap? state))
  (let ((state (state-unwrap state)))			
    (ics-subst-pp (ics_state_find_of theory state))))

(defmethod ics-query-diseqs ((arg null) state)
  (declare (ignore state))
  nil)

(defmethod ics-query-diseqs ((arg string) state)
  (let ((expr (pc-typecheck (pc-parse arg 'expr))))
    (ics-query-diseqs expr)))

(defmethod ics-query-diseqs ((arg expr) state)
  (assert (state-wrap? state))
  (let ((term (term-unwrap (translate-to-ics arg)))
	(state (state-unwrap state)))
    (ics-term-set-pp (ics_state_diseqs state term))))

(defmethod ics-query-diseqs (arg state)
  (assert (state-wrap? state))
  (declare (ignore arg))
  (let ((state (state-unwrap state)))
    (ics-map-term-to-terms-pp (ics_state_diseqs_of state))))

(defmethod ics-query-cnstrnts ((cnstrnts null) state)
  (declare (ignore state))
  nil)

(defmethod ics-query-cnstrnts ((arg string) state)
  (let ((expr (pc-typecheck (pc-parse arg 'expr))))
    (ics-query-cnstrnts expr)))

(defmethod ics-query-cnstrnts ((arg expr) state)
  (assert (state-wrap? state))
  (let* ((term (term-unwrap (translate-to-ics arg)))
	 (state (state-unwrap state))
	 (cnstrnt (ics_cnstrnt state term))
	 (expr (translate-cnstrnt-from-ics* cnstrnt term)))
    (format t "~a" expr)))

(defmethod ics-query-cnstrnts (arg state)
  (assert (state-wrap? state))
  (declare (ignore arg))
  (let* ((state (state-unwrap state))
	 (cnstrnts (ics_state_cnstrnts_of state)))
    (break "to do")))
  
