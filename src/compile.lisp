;;; credits

;; Compiler is based on code from Peter Norvig's book,
;; Paradigms of Artificial Intelligence Programming.

(define position
  (lambda (item items)
    (let loop ((i 0)
	       (tail items))
	 (cond
	  ((nilp tail) -1)
	  ((startswith tail item) i)
	  (t (loop (+ i 1) (cdr tail)))))))

(define inenvp
  (lambda (name env)
    (let loop ((i 0)
	       (tail env))
	 (if (nilp tail)
	     nil
	   (let ((j (position name (car tail))))
	     (if (>= j 0)
		 (cons i j)
	       (loop (+ i 1) (cdr tail))))))))

(define gen
  (lambda x
    (list x)))

(define genvar
  (lambda (name env)
    (let ((p (inenvp name env)))
      (if p
	  (gen 'LVAR (car p) (cdr p) name)
	(gen 'GVAR name)))))

(define genset
  (lambda (name env)
    (let ((p (inenvp name env)))
      (if p
	  (gen 'LSET (car p) (cdr p) name)
	(gen 'GSET name)))))

(define genargs
  (lambda (args)
    (let loop ((nargs 0)
	       (tail args))
	 (cond
	  ((nilp tail)
	   (gen 'ARGS nargs))
	  ((symbolp tail)
	   (gen 'ARGSD nargs))
	  ((not (and (consp tail)
		     (symbolp (car tail))))
	   (throw "genargs: incorrect argument expression"))
	  (t (loop (+ nargs 1) (cdr tail)))))))

(define seq
  (lambda seqs
    (if (nilp seqs)
	nil
      (append (car seqs) (apply seq (cdr seqs))))))

(define comppop
  (lambda (context)
    (if (_ context valuep)
	nil
      (gen 'POP))))

(define compret
  (lambda (context)
    (if (_ context morep)
	nil
      (gen 'RETURN))))

(define compvar
  (lambda (name context)
    (if (_ context valuep)
	(seq (genvar name (_ context env))
	     (compret context))
      nil)))

(define compconst
  (lambda (value context)
    (if (_ context valuep)
	(seq (gen 'CONST value)
	     (compret context))
      nil)))

(define compif
  (lambda (pred tform fform context)
    (let ((pcode (comp pred
		       (clone context
			      (valuep t)
			      (morep t)))))
      (if (and (singlep pcode)
	       (startswith (car pcode) 'CONST))
	  (if (nilp (second (car pcode)))
	      (comp fform context)
	    (comp tform context))
	(let ((tcode (comp tform context))
	      (fcode (comp fform context))
	      (k1 (_ context (genlabel)))
	      (k2 (if (_ context morep)
		      (_ context (genlabel))
		    nil)))
	  (seq pcode
	       (gen 'FJUMP k1)
	       tcode
	       (if (_ context morep)
		   (gen 'JUMP k2)
		 nil)
	       (list k1)
	       fcode
	       (if (_ context morep)
		   (list k2)
		 nil)))))))

(define compargs
  (lambda (exps context)
    (if (nilp exps)
	nil
      (seq (comp (car exps)
		 (clone context
			(valuep t)
			(morep t)))
	   (compargs (cdr exps) context)))))

(define compcall
  (lambda (f args context)
    (if (and (consp f)
	     (= (car f) 'lambda)
	     (nilp (second f)))
	(if (nilp args)
	    (compbegin (cdr (cdr f)) context)
	  (throw "compcall: unexpected arguments"))
      (let ((tailcode (seq (compargs args context)
			   (comp f (clone context
					  (valuep t)
					  (morep t)))
			   (gen 'CALLJ (length args)))))
	(if (not (_ context morep))
	    tailcode
	  (let ((k (_ context (genlabel))))
	    (seq (gen 'SAVE k)
		 tailcode
		 (list k)
		 (comppop context))))))))

(define partition
  (lambda (x pred)
    (let loop ((yes nil)
	       (no nil)
	       (tail x))
	 (cond
	  ((nilp tail)
	   (list yes no))
	  ((pred (car tail))
	   (loop (cons (car tail) yes) no (cdr tail)))
	  (t
	   (loop yes (cons (car tail) no) (cdr tail)))))))

(define compreceive
  (lambda (clauses context)
    (let ((part (partition
		 clauses (lambda (c)
			   (= (caar c) 'after)))))
      (let ((after (if (nilp (car part))
		       nil
		     (caar part)))
	    (rs (second part)))
	(when after
	  (set after (list (second (car after))
			   (complambda nil
				       (cdr after)
				       context))))
	(set rs (map (lambda (c)
		       (list (caar c)
			     (complambda (cdar c)
					 (cdr c)
					 context)))
		     rs))
	(if (_ context morep)
	    (let ((k (_ context (genlabel))))
	      (seq (gen 'SAVE k)
		   (gen 'RECV rs after)
		   (list k)
		   (comppop context)))
	  (seq (gen 'RECV rs after)
	       (comppop context)))))))

(define compmacro
  (lambda (e context)
    (let ((expander (global (car e))))
      (comp (apply expander (cdr e)) context))))

(define comp
  (lambda (e context)
    (cond
     ((symbolp e)
      (compvar e context))
     ((atomp e)
      (compconst e context))
     (t (cond
	 ((startswith e 'quote)
	  (compconst (second e) context))
	 ((startswith e 'set)
	  (seq (comp (third e) (clone context
				      (valuep t)
				      (morep t)))
	       (genset (second e) (_ context env))
	       (comppop context)
	       (compret context)))
	 ((startswith e 'if)
	  (compif (second e) (third e) (fourth e) context))
	 ((startswith e 'begin)
	  (compbegin (cdr e) context))
	 ((startswith e 'lambda)
	  (if (not (_ context valuep))
	      nil
	    (seq (gen 'FN (complambda (second e)
				      (cdr (cdr e))
				      context))
		 (compret context))))
	 ((startswith e 'receive)
	  (compreceive (cdr e) context))
	 (t (if (macrop (car e))
		(compmacro e context)
	      (compcall (car e) (cdr e) context))))))))

(define compbegin
  (lambda (body context)
    (cond
     ((nilp body)
      (compconst nil context))
     ((singlep body)
      (comp (car body) context))
     (t (seq (comp (car body)
		   (clone context
			  (valuep nil)
			  (morep t)))
	     (compbegin (cdr body) context))))))

(define complambda
  (lambda (args body context)
    (let ((properargs (maketruelist args)))
      (let ((newcontext (clone context
			       (valuep t)
			       (morep nil)
			       (env (cons properargs
					  (_ context env))))))
	(assemble
	 (new (code (optimize
		     (seq (genargs args)
			  (compbegin body newcontext))))))))))

(define makelabel
  (lambda (n)
    (intern (strcat "L" (itoa n)))))

(define compile
  (lambda (e)
    (let ((labelnum 0))
      (let ((genlabel (lambda ()
			(let ((n labelnum))
			  (set labelnum (+ labelnum 1))
			  (makelabel n)))))
	(complambda nil (list e)
		    (new (genlabel genlabel)
			 (valuep t)
			 (morep nil)
			 (env nil)))))))

(define eval
  (lambda (e)
    ((boxfn (compile e)))))

(define labelp symbolp)

(defmac push
  (lambda (var value)
    `(set ,var (cons ,value ,var))))

(defmac pop
  (lambda (var)
    `(set ,var (cdr ,var))))

(defmac incr
  (lambda (var)
    `(set ,var (+ ,var 1))))

(define asm1st
  (lambda (code)
    (let ((labels nil)
	  (addr 0))
      (dolist (instr code)
	(if (labelp instr)
	    (push labels (list instr addr))
	  (incr addr)))
      labels)))

(define useslabelp
  (lambda (instr)
    (let ((op (car instr)))
      (or (= op 'JUMP)
	  (= op 'FJUMP)
	  (= op 'TJUMP)
	  (= op 'SAVE)))))

(define asm2nd
  (lambda (code labels)
    (let ((acc nil))
      (dolist (instr code)
	(cond
	 ((not (labelp instr))
	  (cond
	   ((useslabelp instr)
	    (let ((pc (lookup labels
			      (second instr))))
	      (setcdr (last instr) (list pc)))))
	  (push acc instr))))
      (reverse acc))))

(define assemble
  (lambda (f)
    (let ((code (_ f code)))
      (let ((labels (asm1st code)))
	(set_ f code (asm2nd code labels))
	f))))

(define instrset
  '((LSET i j name)	(GSET name)	(ARGS nargs)	(POP)
    (LVAR i j name)	(GVAR name)	(ARGSD nargs)	(CONST value)
    (FJUMP label)	(SAVE label)	(FN f)
    (TJUMP label)	(RETURN)	(PRIM name)
    (JUMP  label)	(CALLJ nargs)	(HALT)))

(define dis1st
  (lambda (code)
    (let ((labels nil))
      (dolist (instr code)
	(cond
	 ((useslabelp instr)
	  (push labels
		(list (third instr)
		      (second instr))))))
      (reverse labels))))

(define dis2nd
  (lambda (code labels)
    (let ((line "")
	  (i 0))
      (dolist (instr code)
	(let ((label (lookup labels i)))
	  (if label
	      (set line (write label))
	    (set line "")))
	(set line
	     (strcat line "\t" (itoa i)))
	(dolist (x instr)
	  (set line (strcat line "\t" (write x))))
	(log line)
	(incr i)))))

(define disassemble
  (lambda (f . nesting)
    (let ((code (_ (unboxfn f) code)))
      (dolist (n nesting)
	(let ((fn (nth n code)))
	  (if (not (startswith fn 'FN))
	      (throw "instruction op is not FN")
	    (set code
		 (_ (unboxfn (second fn)) code)))))
      (dis2nd code (dis1st code)))))

(define optimize
  (lambda (x) x))

(define compilefile
  (lambda (name)
    (http 'get (strcat deviceurl "/src/" name ".lisp"))
    (receive
     ((response code text)
      (let ((fasl ""))
	(when (= code 200)
	  (strextend fasl "[\n")
	  (let ((exps (readall text)))
	    (strextend fasl (tojson (compile (car exps))))
	    (dolist (exp (cdr exps))
	      (strextend fasl ",\n")
	      (strextend fasl (tojson (compile exp)))))
	  (strextend fasl "]\n")
	  (http 'put (strcat deviceurl "/obj/" name ".fasl") fasl)
	  (receive
	   ((response code)
	    (unless (= code 200)
	      (throw "compilefile: http put fail"))))))))))

(define refresh
  (lambda (name)
    (compilefile name)
    (load name)))
