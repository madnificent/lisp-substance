(defmac define
  (lambda (var val)
    `(begin
      (def ',var)
      (set ,var ,val))))

(defmac defmac
  (lambda (var val)
    `(begin
      (define ,var ,val)
      (mac ',var))))

(define t 't)

(define nilp
  (lambda (x)
    (= x nil)))

(define caar
  (lambda (x)
    (car (car x))))

(define cadr
  (lambda (x)
    (car (cdr x))))

(define cdar
  (lambda (x)
    (cdr (car x))))

(define cddr
  (lambda (x)
    (cdr (cdr x))))

(defmac cond
  (lambda clauses
    (if (nilp clauses)
	nil
      `(if ,(caar clauses)
	   (begin .,(cdar clauses))
	 (cond .,(cdr clauses))))))

(define length
  (lambda (x)
    (if (nilp x)
	0
      (+ 1 (length (cdr x))))))

(define map
  (lambda (f x)
    (if (nilp x)
	nil
      (cons (f (car x))
	    (map f (cdr x))))))

(define not
  (lambda (x) (if x nil t)))

(defmac and
  (lambda x
    (if (nilp x)
	't
      `(if ,(car x)
	   (and .,(cdr x))
	 nil))))

(defmac or
  (lambda x
    (if (nilp x)
	'nil
      `(if ,(car x)
	   t
	 (or .,(cdr x))))))

(define list
  (lambda x x))

(define append
  (lambda (x y)
    (cond
     ((nilp x) y)
     ((nilp y) x)
     (t (cons (car x)
	      (append (cdr x) y))))))

(define startswith
  (lambda (x sym)
    (= (car x) sym)))

(define atomp
  (lambda (x)
    (or (nilp x) (numberp x) (symbolp x) (stringp x))))

(define qq
  (lambda (x)
    (cond
     ((atomp x)
      `(quote ,x))
     ((startswith x 'quote)
      `(list 'quote ,(qq (cadr x))))
     ((startswith x 'unquote)
      (cadr x))
     ((startswith x 'quasiquote)
      (qq (qq (cadr x))))
     ((and (consp (car x))
	   (startswith (car x) 'unquotesplicing))
      `(append ,(cadr (car x))
	       ,(qq (cdr x))))
     (t
      `(cons ,(qq (car x))
	     ,(qq (cdr x)))))))

(defmac quasiquote qq)

(define first car)
(define second cadr)

(define third
  (lambda (x)
    (second (cdr x))))

(define fourth
  (lambda (x)
    (third (cdr x))))

(define singlep
  (lambda (x)
    (and (consp x)
	 (= (cdr x) nil))))

(define maketruelist
  (lambda (x)
    (cond
     ((nilp x) nil)
     ((consp x) (cons (car x) (maketruelist (cdr x))))
     (t (list x)))))

(define dottedp
  (lambda (x)
    (cond
     ((nilp x)
      nil)
     ((consp x)
      (dottedp (cdr x)))
     (t t))))

(defmac letrec
  (lambda (bindings . body)
    `(let ,(map (lambda (v) (list (car v) nil)) bindings)
       ,@(map (lambda (v) `(set .,v)) bindings)
       .,body)))

(define namedlet
  (lambda (name init body)
    `(letrec ((,name (lambda ,(map car init)
		       .,body)))
	     (,name .,(map second init)))))

(define simplelet
  (lambda (bindings body)
    `((lambda ,(map car bindings) .,body)
      .,(map (lambda (x) (car (cdr x))) bindings))))

(defmac let
  (lambda (head . rest)
    (if (symbolp head)
	(namedlet head (car rest) (cdr rest))
      (simplelet head rest))))

(define reverse
  (lambda (x)
    (let loop ((acc nil)
	       (tail x))
	 (if (nilp tail)
	     acc
	   (loop (cons (car tail) acc)
		 (cdr tail))))))

(define last
  (lambda (x)
    (let loop ((tail x))
	 (cond
	  ((nilp tail) nil)
	  ((nilp (cdr tail)) tail)
	  ((consp (cdr tail))
	   (loop (cdr tail)))
	  (t tail)))))

(defmac when
  (lambda (p . body)
    `(if ,p (begin .,body) nil)))

(defmac unless
  (lambda (p . body)
    `(if ,p nil (begin .,body))))

(defmac dolist
  (lambda (binder . body)
    `(let ((f (lambda (,(car binder))
		.,body)))
       (let loop ((tail ,(second binder)))
	    (unless (nilp tail)
	      (f (car tail))
	      (loop (cdr tail)))))))

(defmac dotimes
  (lambda (binder . body)
    `(let ((f (lambda (,(car binder))
		.,body))
	   (n ,(second binder)))
       (let loop ((i 0))
	    (when (< i n)
	      (f i)
	      (loop (+ i 1)))))))

(define nth
  (lambda (n x)
    (cond
     ((nilp x) (throw "nth: index out of range"))
     ((= n 0) (car x))
     (t (nth (- n 1) (cdr x))))))

(define member
  (lambda (item items)
    (and items
	 (or (startswith items item)
	     (member item (cdr items))))))

;;; alists

(define slot
  (lambda (ob tag)
    (cond
     ((nilp ob)
      nil)
     ((startswith (car ob) tag)
      (car ob))
     (t
      (slot (cdr ob) tag)))))

(define get
  (lambda (ob tag)
    (let ((s (slot ob tag)))
      (if s (second s) nil))))

(define lookup
  (lambda (ob msg)
    (if (atomp msg)
	(get ob msg)
      (if (consp msg)
	  (apply (get ob (car msg))
		 (cdr msg))
	(throw (strcat "lookup: bad msg: "
		       (write msg)))))))

(define cascade
  (lambda (ob msgs)
    (if (nilp msgs)
	ob
      (cascade (lookup ob (car msgs))
	       (cdr msgs)))))

(defmac _
  (lambda (ob . msgs)
    `(cascade ,ob
	      (list .,(map (lambda (msg)
			     (if (symbolp msg)
				 `',msg
			       `(list ',(car msg)
				      .,(cdr msg))))
			   msgs)))))

(defmac new
  (lambda pairs
    `(list .,(map (lambda (pair)
		    `(list ',(car pair) ,(second pair)))
		  pairs))))

(defmac clone
  (lambda (proto . pairs)
    `(append (new .,pairs) ,proto)))

(defmac set_
  (lambda (obvar tag value)
    `(let ((value ,value)
	   (slotval (slot ,obvar ',tag)))
       (if slotval
	   (begin
	    (setcdr slotval (list value))
	    ,obvar)
	 (set ,obvar (cons (list ',tag value) ,obvar))))))

;;; messaging

(defmac send
  (lambda (pid msg)
    `(sendmsg ,pid (list ',(car msg) .,(cdr msg)))))

;;; startup

(define deviceurl "http://0.0.0.0:9000") ; listen to an inexisting address by default

(define makeimage
  (lambda fasls
    (let ((forms (cellnew nil))
	  (image (cellnew "var compiledcode = [\n")))
      (dolist (fasl fasls)
	(http 'get (strcat deviceurl "/obj/" fasl))
	(receive
	 ((response code text)
	  (unless (= code 200)
	    (throw (strcat "makeimage: http get fail for "
			   fasl)))
	  (cellput forms (append (cellget forms)
				 (fromjson text))))))
      (cellput image
	       (strcat (cellget image)
		       (tojson (first (cellget forms)))))
      (dolist (form (cdr (cellget forms)))
	(cellput image
		 (strcat (cellget image)
			 ",\n"
			 (tojson form))))
      (strcat (cellget image) "\n];\n"))))

(define writeimage
  (lambda (text)
    (http 'put (strcat deviceurl "/obj/bytecode.js") text)
    (receive
     ((response code)
      (unless (= code 200)
	(throw "writeimage: http put fail"))))))

(define refreshimage
  (lambda ()
    (writeimage
     (makeimage "compile.fasl" "debug.fasl" "repl.fasl" "text.fasl"
		"boot.fasl"))))

(define load
  (lambda (name)
    (http 'get (strcat deviceurl "/obj/" name ".fasl"))
    (receive
     ((response code text)
      (when (= code 200)
	(dolist (e (fromjson text))
	  ((boxfn e))))))))

(define log
  (lambda (text)
    (http 'post (strcat deviceurl "/log") (strcat text "\n"))
    (receive
     ((response code)
      (cond
       ((!= code 200)
	(throw "log: http put fail")))))))

(startrepl)
(startsubservience)

;; (begin
;;  (compilefile "boot")
;;  (compilefile "compile")
;;  (compilefile "debug")
;;  (compilefile "repl")
;;  (compilefile "text")
;;  )
;; (refreshimage)
