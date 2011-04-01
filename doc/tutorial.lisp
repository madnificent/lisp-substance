;;; Tutorial for the lisp-addict language

;; The Lisp dialect used in lisp-addict is a goofy mix of ideas from
;; Scheme, Common Lisp, and Erlang.  This tutorial is being written to
;; help programmers with at least a passing familiarity with those
;; other languages to get a handle on how one might use this
;; lisp-addict language.

;; For brevity, and unspecified historical reasons, I'm going to refer
;; to this Lisp dialect as W in this tutorial.

;;; Outline

;; 1. Naming
;; 2. Data
;; 3. Sequential Control
;; 4. Concurrent Control
;; 5. Bootstrapping

;;; Naming

;; W is a Lisp-1, like Scheme.  There is a global environment which
;; you can use like this:

(define foo 1)
(set foo 2)

(define bar
  (lambda (x)
    (list foo x foo)))

;; The familiar "lambda" operator permits the introduction of local
;; names.  It is fashioned after Scheme's version of "lambda".  In
;; terms of binding values to names, the following forms are available
;; for collecting an argument list whose length is only known at
;; run time.

(define list
  (lambda x
    x))

(list 1 2 3)
; => (1 2 3)

(define foo
  (lambda (a b . z)
    (list a z b)))

(foo 1 2 3 4 5)
; => (1 (3 4 5) 2)

;; So in terms of naming, we have globals and locals.  Introduce
;; globals with "define" and locals with "lambda".  Local bindings are
;; lexical, which means that you can create closures.

;;; Data

;; Here's how you can describe some common literal types of data.

-1
3.14159265
"Aad Versteden"
'foo
'(a b c)
'(1 . 2)
nil

;; You can also use quasiquote.

`(a ,(+ 1 2))

;; The equal identity predicate is called "=" instead of the
;; traditional "eq".  There is also "!=" which is the negation of "=".
;; Strings with the same content are identical.

;; You can do arithmetic with numbers and you can compare them.  For
;; example:

(define factorial
  (lambda (n)
    (if (<= n 1)
	1
      (* n (factorial (- n 1))))))

;; Numbers are 64-bit floating point numbers, as inherited from
;; Javascript.

;; Some symbol primitives:

(intern "foo")
; => foo

(symbolname 'foo)
; => "foo"

;; Some string primitives:

(strcat "a" "b" "c")
; => "abc"

(sref "abc" 1)
; => "b"

(slength "abc")
; => 3

;; Of course we can process lists!

(map (lambda (x)
       (cons (cadr x) (list (car x))))
     '((a b) (1 2)))
; => ((b a) (2 1))

(length '(a b c))
; => 3

(append '(a b) '(c d))
; => (a b c d)

(nth 1 '(a b c))
; => b

;; The Common Lisp conventions for true and false are used.  So "nil"
;; is the only false value and, for convenience, the global "t" can be
;; used for a generic true result.  Trivial example:

(and (> 2 1) (or nil (= 'foo 'foo)))
; => t

;; There are arrays and cells.  Both permit destructive updates.  A
;; cell only contains one element.  Arrays contain an indexed sequence
;; of objects.

(let ((c (cellnew "a")))
  (log (cellget c))
  (cellput c "b")
  (log (cellget c)))
; a
; b
; => nil

;; Note that the "log" function is used to write strings to the
;; interaction server's standard output.

(let ((a (arraynew 4)))
  (arrayput a 2 "2")
  (arrayput a 0 "0")
  (dotimes (i 4)
    (when (arrayget a i)
      (log (strcat (arrayget a i))))))
; 0
; 2
; => nil

;; The above example uses "dotimes" and "when" for control.  These
;; behave as in Common Lisp.

;; Lastly, for this topic about data, a certain amount of reflection
;; on functions is possible using the following.  I won't describe
;; these in detail because this tutorial is to humble for that.

(unboxfn length)
; => ((code ((ARGS 1) (SAVE L1 5) (LVAR 0 0 x) (GVAR nilp) (CALLJ 1) (FJUMP L4 8) (CONST 0) (RETURN) (CONST 1) (SAVE L3 16) (SAVE L2 14) (LVAR 0 0 x) (GVAR cdr) (CALLJ 1) (GVAR length) (CALLJ 1) (GVAR +) (CALLJ 2))))

;; Note that the above expression represents the machine code
;; interpreted by the virtual machine in order to execute a call to
;; the length function.

((boxfn (unboxfn append))
 '(a b) '(c d))
; => (a b c d)

;; In the above, the append function is unboxed and then boxed back up
;; and finally called.  The interactive compiler works by generating
;; code and boxing it into executable functions via "boxfn".

;; One last example:

(unboxfn (lambda (x) (+ x x)))
; => ((code ((ARGS 1) (LVAR 0 0 x) (LVAR 0 0 x) (GVAR +) (CALLJ 2))))

;;; Sequential Control

;; Outline: if cond when unless dotimes dolist named-let

;; For sequential control, some simple constructs from Common Lisp are
;; provided.  Namely, these:

if cond when unless dotimes dolist

;; From Scheme, we get these:

letrec and "named let"

;; Named let looks like this:

(let counting ((i 3))
     (when (> i 0)
       (log (write i))
       (counting (- i 1))))
; 3
; 2
; 1
; => nil

;;; Concurrent Control

;; W has a scheduler that pre-emptively switches between a set of
;; runnable proceses.  Processes communicate using messages.  The
;; mechanisms were designed to roughly resemble Erlang mechanisms.

;; It's a little difficult to write short tutorial examples because
;; the concurrency starts to become more useful as more processes
;; become involved and failure recovery measures are put into place.

;; All code is run in some process.  The "self" procedure returns the
;; process object for the process which executes the call.

(self)
; => <process>

;; If you deliberately enter an expression that crashes, you can
;; observe that the REPL monitors crashes and reports them.

(1)
; eval process died: "exception: cannot call nonfunction"

;; The REPL reads and evaluates each incoming expression in an
;; independent process.  If you enter a number of long-running
;; computations in quick succession, they may be computed in an
;; interleaved manner.

;; The other way to get a number of process running concurrently is to
;; have some of them block.  For example:

(define aproc
  (spawn
   (lambda ()
     (receive
      ((foo x y)
       (log (write (list y x))))))))
; => <process>

(send aproc (foo 1 2))
; => nil
; (2 1)

;; The first expression spawned a process which immediately blocked
;; waiting for a message beginning with the tag "foo".  The second
;; expression send a matching message to the spawned process.  As a
;; result, the arguments of the message were logged in reverse order.

;; The pattern matching for messages is determined entirely by the
;; symbol at the head of the message.  Send is a macro which evaluates
;; only the arguments of the message and avoids treating the message
;; expression as a combination to be evaluated normally.

;; Receive expressions are shaped like cond expressions.  They may
;; have a number of clauses with each clause having a pattern and a
;; body.  When a message arrives, if there is a matching pattern, the
;; pattern variables are bound to the message arguments and the body
;; is evaluated.

;; Here's another goofy demo:

(define countdown
  (lambda (name proc)
    (receive
     ((odd n)
      (log (strcat name " sees " (write n)))
      (send proc (even (- n 1)))
      (countdown name proc))
     ((even n)
      (log (strcat name " sees " (write n)))
      (when (!= n 0)
	(send proc (odd (- n 1)))
	(countdown name proc))))))

(define proc1
  (spawn
   (lambda ()
     (receive
      ((start proc2)
       (countdown "Alice" proc2))))))

(define proc2
  (spawn
   (lambda ()
     (receive
      ((start proc2)
       (countdown "Bob" proc2))))))

(send proc1 (start proc2))
(send proc2 (start proc1))

(send proc1 (even 4))

;; The countdown function uses a blocking receive expression which
;; matches incoming messages against one of two patterns and recurs
;; iteratively until receiving (even 0).  Then two processes, "Alice"
;; and "Bob" are spawned and introduced to each other.  Finally, the
;; cascade is triggered by sending (even 4) to "Alice".  The output
;; is:

Alice sees 4
Bob sees 3
Alice sees 2
Bob sees 1
Alice sees 0

;; A key aspect of the whole concurrency system is the ability to link
;; processes and designate some as "system" processes.  A system
;; process receives a message when a linked process exits.  Here's a
;; sketch:

(spawn
 (lambda ()
   (trapexits)
   (spawnlink
    (lambda ()
      (log "i'm not dead yet")
      (log "i think i'm getting better")
      (throw "i'm dead buddy")))
   (receive
    ((exit snap reason)
     (log (strcat "noticed an exit with reason: "
		  (write reason)))))))

;; The other rule about linked processes is that if a process dies
;; abnormally, then non-system processes linked to it will also die.

;;; Bootstrapping

;; If one is paying attention while reading the Lisp code, one will
;; notice that many definitions are made in a circular fashion.  For
;; instance, "defmac" is defined in terms of itself and the function
;; for expanding quasiquote expressions uses quasiquotes itself.

;; This circularity can be very confusing at times, and it's hard to
;; describe how it works, but it also allows the Javascript sources to
;; be minimized since so much can be defined in W.

;; At any time, the system is defined by the Javascript VM, a boot
;; image file of compiled W code, and the W sources.  The VM can load
;; compiled W code directly and interpret it directly.  The compiled
;; boot image itself defines a compiler among other things.

;; Therefore, once the boot image has been loaded, its compiler can be
;; used to turn modified W sources into a modified boot image.  And
;; the cycle continues.  You almost have to try it a bit to appreciate
;; what's going on.

;; Sometimes it helps to think in terms of cross-compilation.  You
;; fork the system into two copies and use one to develop the other
;; until the other is capable of self-hosting again.

;; Good luck!
