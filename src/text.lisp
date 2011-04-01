(defmac strextend
  (lambda (var . s)
    `(set ,var (strcat ,var .,s))))

(define readall
  (lambda (s)
    (letrec
     ((i 0)
      (peek
       (lambda ()
	 (sref s i)))
      (advance
       (lambda ()
	 (set i (+ i 1))))
      (skipws
       (lambda ()
	 (let ((ws " \t\n;"))
	   (let loop ()
		(cond
		 ((donep) nil)
		 ((substringp (peek) ws)
		  (if (= (peek) ";")
		      (let loop ()
			   (cond
			    ((donep) nil)
			    ((!= (peek) "\n")
			     (begin (advance) (loop)))))
		    nil)
		  (unless (donep)
		    (advance))
		  (loop)))))))
      (donep
       (lambda ()
	 (= i (slength s))))
      (readlist
       (lambda ()
	 (let loop ((acc nil))
	      (skipws)
	      (cond
	       ((donep)
		(throw "unmatched \"(\""))
	       ((= (peek) ")")
		(advance)
		(reverse acc))
	       ((= (peek) ".")
		(advance)
		(set acc (reverse acc))
		(setcdr (last acc) (read))
		(skipws)
		(if (or (donep) (!= (peek) ")"))
		    (throw "ill-formed dotted list")
		  (begin (advance) acc)))
	       (t (loop (cons (read) acc)))))))
      (readatom
       (lambda ()
	 (let ((terminators "()'\" \t\n")
	       (mark i))
	   (let loop ()
		(if (or (donep)
			(substringp (peek) terminators))
		    nil
		  (begin (advance) (loop))))
	   (let ((substr (substring s mark i)))
	     (cond
	      ((= substr "nil") nil)
	      ((atoi substr)
	       (atoi substr))
	      (t (intern substr)))))))
      (readquasi
       (lambda ()
	 (list 'quasiquote (read))))
      (readcomma
       (lambda ()
	 (cond
	  ((donep) (throw "syntax error"))
	  ((= (peek) "@")
	   (advance)
	   (list 'unquotesplicing (read)))
	  (t (list 'unquote (read))))))
      (readstring
       (lambda ()
	 (let loop ((content ""))
	      (let ((pushjmp
		     (lambda ()
		       (let ((c (peek)))
			 (advance)
			 (loop (strcat content c)))))
		    (escjmp
		     (lambda (c)
		       (advance)
		       (loop (strcat content c)))))
		(cond
		 ((donep) (throw "unterminated string"))
		 ((= (peek) "\"")
		  (advance)
		  content)
		 ((= (peek) "\\")
		  (advance)
		  (cond
		   ((donep) (throw "unterminated string"))
		   ((= (peek) "t") (escjmp "\t"))
		   ((= (peek) "n") (escjmp "\n"))
		   ((= (peek) "\\") (escjmp "\\"))
		   ((= (peek) "\"") (escjmp "\""))
		   (t (throw (strcat "unknown escape: \\"
				     (peek))))))
		 (t (pushjmp)))))))
      (read
       (lambda ()
	 (skipws)
	 (if (donep)
	     (throw "read: syntax error")
	   nil)
	 (cond
	  ((= (peek) "`") (advance) (readquasi))
	  ((= (peek) ",") (advance) (readcomma))
	  ((= (peek) "(") (advance) (readlist))
	  ((= (peek) ")") (throw "unbalanced \")\""))
	  ((= (peek) "'") (advance) (list 'quote (read)))
	  ((= (peek) "\"") (advance) (readstring))
	  (t (readatom))))))
     (let loop ((acc nil))
	  (skipws)
	  (if (donep)
	      (reverse acc)
	    (loop (cons (read) acc)))))))

(define escape
  (lambda (s)
    (let ((n (slength s)))
      (let loop ((se "")
		 (i 0))
	   (let ((escjmp (lambda (c)
			   (loop (strcat se "\\" c)
				 (+ i 1)))))
	     (cond
	      ((= i n) se)
	      ((= (sref s i) "\\") (escjmp "\\"))
	      ((= (sref s i) "\"") (escjmp "\""))
	      ((= (sref s i) "\n") (escjmp "n"))
	      ((= (sref s i) "\t") (escjmp "t"))
	      (t (loop (strcat se (sref s i))
		       (+ i 1)))))))))

(define writecons
  (lambda (x)
    (strcat
     (let loop ((s "(")
		(tail x))
	  (let ((a (car tail))
		(tail (cdr tail)))
	    (cond
	     ((nilp tail)
	      (strcat s (write a)))
	     ((consp tail)
	      (loop (strcat s (write a) " ") tail))
	     (t (strcat s (write a) " . " (write tail))))))
     ")")))

(define write
  (lambda (x)
    (cond
     ((nilp x) "nil")
     ((symbolp x) (symbolname x))
     ((numberp x) (itoa x))
     ((stringp x) (strcat "\"" (escape x) "\""))
     ((consp x) (writecons x))
     ((templatep x) "<template>")
     ((functionp x) "<function>")
     ((processp x) "<process>")
     ((cellp x) "<cell>")
     ((arrayp x) "<array>")
     (t (throw "write: unknown type")))))

(define arrayfromlist
  (lambda (x)
    (if (nilp x)
	"[]"
      (let ((s "["))
	(set s (strcat s (tojson (car x))))
	(dolist (elt (cdr x))
	  (set s (strcat s "," (tojson elt))))
	(set s (strcat s "]"))
	s))))

(define tojson
  (lambda (x)
    (cond
     ((functionp x)
      (tojson (unboxfn x)))
     ((numberp x)
      (write x))
     ((symbolp x)
      (strcat "\"" (write x) "\""))
     ((stringp x)
      (strcat "{s:\"" (escape x) "\"}"))
     ((nilp x)
      "[]")
     ((consp x)
      (if (dottedp x)
	  (strcat "{d:" (arrayfromlist (maketruelist x)) "}")
	(arrayfromlist x)))
     (t (throw "tojson: unknown type")))))
