(use apropos)
(use regex)
(use irregex)
(use srfi-18)
(use tcp)
(use posix)
(use chicken-doc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wraps output from geiser functions
;; Format is:
;; '((result "string representation of result") (output "string representation of output") (error "string representation of caught error"))
(define (call-with-result thunk)
  ;; TODO: Capture errors and dump them into '(error "I'm a string!")
  (let* ((result #f)
         (output
          (with-output-to-string
            (lambda ()
              (with-error-output-to-port 
               (current-output-port)
               (lambda () (set! result (thunk))))))))

    ; Hacks
    ; ->string doesn't escape strings, but with-output-to-string will
    (set! result (with-output-to-string (lambda () (write result))))

    (write `((result ,result)
             (output ,output)))
    (newline)))

;; This macro aids in the creation of toplevel definitions for the interpreter which are also available to code
;; toplevel passes parameters via the current-input-port, and so in order to make the definition behave nicely
;; in both usage contexts I defined a (get-arg) function which iteratively pulls arguments either from the 
;; input port or from the variable arguments, depending on context.
(define-syntax define-toplevel-for-geiser
  (lambda (f r c)
    (let* ((name (cadr f))
           (body (cddr f)))
      `(begin
         (,(r 'define) (,name . !!args)
          (,(r 'define) !!read-arg (null? !!args))
          (,(r 'define) (get-arg)
           (if !!read-arg
               (read)
               (let ((arg (car !!args)))
                 (set! !!args (cdr !!args))
                 arg)))
          (begin ,@body))
         (,(r 'toplevel-command) ',name ,name)))))

;; Locates all installed extensions
(define (installed-extensions)
  (let ((output (call-with-input-pipe "chicken-status" read-all)))
    (sort! (irregex-split "\\n" (irregex-replace/all " [^\\n]*" output))
           string<?)))

;; ;; Returns the value of a symbol if it is bound, false otherwise
;; (define (bound? sym)
;;   (if (string? sym)
;;       (bound? (string->symbol sym))
;;       (let ((value #f))
;;         (with-output-to-string 
;;           (lambda ()
;;             (condition-case 
;;              (set! value (eval sym)) 
;;              ((exn) #f))))
;;         value)))

;; Builds a signature list from an identifier
;; The format is:
;; ((,id (args ((required [signature]) (optional) (key))) (module [module path]) [(error "not found")]) ...)
(define (find-signatures id)
  (define (fmt node)
    (let ((id (car node))
          (type (cdr node)))
      (cond
       ((equal? 'macro type)
        `(,id (args ((required)
                     (optional)
                     (key)))
              (module)
              (docstring . ,(format "~s (macro)" id))))
       ((and (list? type) (equal? 'procedure (car type)))
        `(,id (args ((required ,@(cdr type))
                     (optional)
                     (key)))
              (module)
              (docstring . ,(format "~s (procedure) ~s" id (caddr node)))))
       (else
        `(,id (args ((required)
                     (optional)
                     (key)))
              (module)
              (docstring . ,(format "~s (unknown)"))
              (error "Unknown type"))))))

  (define (find id)
    (let ((id (cond 
               ((string? id) (string->symbol id)) 
               ((symbol? id) id)
               (else (error "Expected a symbol or string")))))
      (filter
       (lambda (s) (equal? (car s) id))
       (apropos-information-list id #:macros? #t))))

  (let* ((res (map fmt (find id))))
    (if (null? res)
        `((,id (args ((required) (optional) (key))) 
               (module "") 
               (error "not found")))
        res)))

;; Takes a list of signatures and prepares them for geiser
(define (export-signatures! sigs)
  (define (export! sig)
    (cond
     ((null? sig) sig)
     ((list? (car sig))
      (export! (car sig))
      (export! (cdr sig)))
     ((symbol? (car sig))
      (set! (car sig) (symbol->string (car sig)))
      (export! (cdr sig)))
     (else 
      (export! (cdr sig)))))
  (export! sigs)
  sigs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geiser core functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basically all non-core functions pass through geiser-eval
(define-toplevel-for-geiser geiser-eval
  (let* ((module (get-arg))
         (form (get-arg))
         (args (get-arg))
         (env (if module (module-environment module) #f))
         (proc (if env (eval form env) (eval form))))
    (call-with-result
     (lambda ()
       (if env
           (eval `(,proc ,@args) env)
           (eval `(,proc ,@args)))))))

;; The no-values identity
(define-toplevel-for-geiser geiser-no-values
  (values))

;; Invoke a newline
(define-toplevel-for-geiser geiser-newline
  (newline))

;; Spawn a server for remote repl access
(define-toplevel-for-geiser geiser-start-server
  (let* ((listener (tcp-listen 0))
         (port (tcp-listener-port listener)))
    (define (remote-repl)
      (receive (in out) (tcp-accept listener)
        (current-input-port in)
        (current-output-port out)
        (current-error-port out)
          
        (repl)))

    (thread-start! (make-thread remote-repl))

    (write `(port ,port))
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions, Autodoc and Signature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-toplevel-for-geiser geiser-completions 
  (let* ((prefix (get-arg))
         (re (regexp (string-append "^" (regexp-escape prefix)))))
    (sort! (map ->string
                (apropos-list re #:macros? #t))
           string<?)))

(define-toplevel-for-geiser geiser-module-completions
  (let* ((prefix (get-arg))
         (match (string-append "^" (regexp-escape prefix))))
    (filter (lambda (v) (string-search match v))
            (installed-extensions))))

(define-toplevel-for-geiser geiser-autodoc 
  (let ((ids (get-arg)))
    (define (generate-details id)
      (export-signatures! (find-signatures id)))

    (if (list? ids)
        (foldr append '()
               (map generate-details ids))
        '())))

(define-toplevel-for-geiser geiser-object-signature 
  (let* ((name (get-arg))
         (object (get-arg))
         (sig (geiser-autodoc `(,name))))
    (if (null? sig) '() (car sig))))

;; TODO: Divine some way to support this functionality
(define-toplevel-for-geiser geiser-symbol-location 
  (let ((symbol (get-arg)))
    '(("file") ("line"))))

(define-toplevel-for-geiser geiser-symbol-documentation
  (define (make-doc symbol #!optional (filter-for-procedures? #t))
    (with-output-to-string 
      (lambda () 
        (map (lambda (node)
               (display (string-append "= Document Node: " (node-signature node) " =\n"))
               (describe node)
               (display "\n\n")) 
             (filter 
              (lambda (n)
                (or (not filter-for-procedures?)
                    (string-search "\\(" (node-signature n))))
              (match-nodes symbol))))))

  (let* ((symbol (get-arg))
         (sig (find-signatures symbol))
         (macro? (not (find (lambda (v) (equal? symbol (symbol->string v))) (apropos-list symbol)))))
    (if (null? sig) 
        '() 
        `(("signature" ,@(export-signatures! (car sig))) 
          ("docstring" . ,(make-doc symbol (not macro?)))))))

;; TODO: Support generic methods
(define-toplevel-for-geiser geiser-generic-methods 
  (let ((symbol (get-arg)))
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and Buffer Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(define-toplevel-for-geiser geiser-load-file 
  (let ((file (get-arg)))
    (load file)))

;; TODO: Support compiling regions
(define-toplevel-for-geiser geiser-compile 
  (let ((form (get-arg))
        (module (get-arg)))
    (error "Chicken does not support compiling regions")))

(define-toplevel-for-geiser geiser-compile-file 
  (let ((path (get-arg)))
    (compile-file path)))

;; TODO: Search through available load paths
;; How to discover those?
(define-toplevel-for-geiser geiser-find-file 
  (let ((path (get-arg)))
    (if (file-exists? path)
        path
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-toplevel-for-geiser geiser-module-exports 
  (let ((module-name (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-symbol-module 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-module-name? 
  (let ((module-name (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-module-path 
  (let ((module-name (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-find-module 
  (let ((module-name (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-all-modules
  #f)

(define-toplevel-for-geiser geiser-submodules 
  (let ((module (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-module-location 
  (let ((name (get-arg)))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-toplevel-for-geiser geiser-set-warnings 
  (let ((level (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-add-to-load-path 
  (let ((directory (get-arg)))
    #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-toplevel-for-geiser geiser-callers 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-callees 
  (let ((symbol (get-arg)))
    #f))

