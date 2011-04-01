;;; REPL

(define replproc nil)

(define replio
  (lambda ()
    (http 'get (strcat deviceurl "/eval"))
    (receive
     ((response code text)
      (cond
       ((= code 200)
	(send replproc (eval text))))))
    (replio)))

(define repl
  (lambda ()
    (trapexits)
    (let loop ()
	 (receive
	  ((eval text)
	   (spawnlink
	    (lambda ()
	      (dolist (exp (map eval (readall text)))
		(log (strcat (write exp)))))))
	  ((exit snap reason)
	   (unless (= reason 'normal)
	     (log (strcat "eval process died: " (write reason))))))
	 (loop))))

(define startrepl
  (lambda ()
    (set replproc (spawn repl))
    (spawn replio)))

;;; subservience

(define subservience
  (lambda ()
    (trapexits)
    (giveup (self))
    (let loop ()
	 (receive
	  ((eval text callback)
	   (spawnlink
	    (lambda ()
	      (let ((exps (map eval (readall text))))
		(cond
		 ((consp exps)
		  (let ((output (write (car exps))))
		    (dolist (exp (cdr exps))
		      (strextend output "\n" (write exp)))
		    (callback output)))))))
	   (receive
	    ((exit snap reason)
	     (unless (= reason 'normal)
	       (log (strcat "eval process died: " (write reason))))))))
	 (loop))))

(define startsubservience
  (lambda ()
    (spawn subservience)))
