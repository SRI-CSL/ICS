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

(ff:defun-foreign-callable ocaml_error (fname msg)
  (error (format nil "~a: ~a" (excl:native-to-string fname)
		 (excl:native-to-string msg))))

(ff:def-foreign-call register_lisp_error_function (index))


;; PVS decision procedure interface

(defun ics-init (&optional full (verbose 0))
  (ics_caml_startup (if full 1 0) #(0))
  (register_lisp_error_function
   (nth-value 1 (ff:register-function `ocaml_error)))
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
    (prog1
	(translate-from-ics term)
      (value-free! term))))

(defun ics-canon (state expr)
  (with-ics-pair (freshvars term)
             (ics_norm (state-unwrap state) 
		       (term-unwrap (translate-to-ics expr)))
    (prog1
	(values (translate-from-ics term) freshvars)
      (value-free! term))))

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

(defun translate-update-to-ics (op args)
  (translate-to-ics*
   (make!-reduced-application
    (translate-update-to-if op)
    args)))

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
  (let ((trm (if (term-wrap? arg) (term-unwrap arg) arg)))
    (let ((*bndngs* nil))
      (declare (special *bndngs*))
      (let ((expr (translate-from-ics* trm)))
	(assert (not (null expr)))
	(values expr *bndngs*)))))

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
	 (val (parse-integer (excl:native-to-string str))))
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


