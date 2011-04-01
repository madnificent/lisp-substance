(define showenv
  (lambda (env)
    (let ((i 0) (j 0))
      (dolist (frame env)
	(dolist (arg frame)
	  (log (strcat (write i) "\t" (write j) ":\t" (write arg)))
	  (incr j))
	(incr i)))))

(define showsnapshot
  (lambda (s)
    (dolist (entry (reverse s))
      (cond
       ((startswith entry 'ret)
	(log (strcat "will return to instr "
		     (write (second entry))
		     " of:"))
	(disassemble (third entry))
	(log "with env:")
	(showenv (fourth entry)))))
    (log (strcat "about to execute instr "
		 (write (_ s pc))
		 " of:"))
    (disassemble (_ s f))
    (log "with env:")
    (showenv (_ s env))))

(define backtrace
  (lambda (name)
    (let ((s (snapshot name)))
      (showsnapshot s))))

(define break
  (lambda (s)
    (log (strcat "proc " (write (self)) " breakpoint: " s))
    (receive
     ((continue x) x))))

(define continue
  (lambda (pid x)
    (send pid (continue x))))
