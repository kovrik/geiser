(use apropos)
(use regex)
(use irregex)
(use srfi-18)
(use tcp)
(use posix)
(use chicken-doc)
(use srfi-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define geiser-r5rs-symbols
  (make-parameter
   '(abs acos and angle append apply asin assoc assq assv atan begin 
     boolean? caar cadr call-with-current-continuation 
     call-with-input-file call-with-output-file call-with-values 
     car case cdddar cddddr cdr ceiling char->integer char-alphabetic?
     char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>? char-downcase
     char-lower-case? char-numeric? char-ready? char-upcase
     char-upper-case? char-whitespace? char<=? char<? char=? char>=?
     char>? char? close-input-port close-output-port complex? cond cons
     cos current-input-port current-output-port define define-syntax
     delay denominator display do dynamic-wind else eof-object? eq?
     equal? eqv? eval even? exact->inexact exact? exp expt floor
     for-each force gcd if imag-part inexact->exact inexact? input-port?
     integer->char integer? interaction-environment lambda lcm length
     let let* let-syntax letrec letrec-syntax list list->string 
     list->vector list-ref list-tail list? load log magnitude make-polar
     make-rectangular make-string make-vector map max member memq memv 
     min modulo negative? newline not null-environment null? 
     number->string number? numerator odd? open-input-file 
     open-output-file or output-port? pair? peek-char port? positive? 
     procedure? quasiquote quote quotient rational? rationalize read 
     read-char real-part real? remainder reverse round 
     scheme-report-environment set! set-car! set-cdr! setcar sin sqrt 
     string string->list string->number string->symbol string-append 
     string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>? 
     string-copy string-fill! string-length string-ref string-set! 
     string<=? string<? string=? string>=? string>? string? substring 
     symbol->string symbol? syntax-rules tan transcript-off transcript-on 
     truncate values vector vector->list vector-fill! vector-length 
     vector-ref vector-set! vector? with-input-from-file with-output-to-file 
     write write-char zero?)))

(define geiser-r7rs-small-symbols 
  (make-parameter
   '(* + - ... / < <= = => > >= abs and append apply assoc assq
     assv begin binary-port? boolean=? boolean? bytevector
     bytevector-append bytevector-copy bytevector-copy! bytevector-length
     bytevector-u8-ref bytevector-u8-set! bytevector? caar cadr
     call-with-current-continuation call-with-port call-with-values call/cc
     car case cdar cddr cdr ceiling char->integer char-ready? char<=?
     char<? char=? char>=? char>? char? close-input-port
     close-output-port close-port complex? cond cond-expand cons
     current-error-port current-input-port current-output-port
     define define-record-type define-syntax define-values denominator do
     dynamic-wind else eof-object? equal? error error-object-message
     even? exact-integer-sqrt exact? features floor floor-remainder
     flush-output-port gcd get-output-string if include-ci inexact?
     input-port? integer? lcm let let*-values let-values letrec* list
     list->vector list-ref list-tail make-bytevector make-parameter
     make-vector max memq min negative? not number->string numerator
     open-input-bytevector open-output-bytevector or output-port?
     parameterize peek-u8 positive? quasiquote quotient raise-continuable
     rationalize read-bytevector! read-error? read-string real? reverse
     set! set-cdr! string string->number string->utf8 string-append
     eof-object eq? eqv? error-object-irritants error-object? exact
     exact-integer? expt file-error? floor-quotient floor/ for-each
     get-output-bytevector guard include inexact input-port-open?
     integer->char lambda length let* let-syntax letrec letrec-syntax
     list->string list-copy list-set! list? make-list make-string map
     member memv modulo newline null? number? odd? open-input-string
     open-output-string output-port-open? pair? peek-char port?
     procedure? quote raise rational? read-bytevector read-char read-line
     read-u8 remainder round set-car! square string->list string->symbol
     string->vector string-copy string-copy! string-for-each string-map
     string-set! string<? string>=? string? symbol->string symbol?
     syntax-rules truncate truncate-remainder u8-ready? unquote
     utf8->string vector vector->string vector-copy vector-fill!
     vector-length vector-ref vector? with-exception-handler write-char
     write-u8 string-fill! string-length string-ref string<=?
     string=? string>? substring symbol=? syntax-error textual-port?
     truncate-quotient truncate/ unless unquote-splicing values
     vector->list vector-append vector-copy! vector-for-each vector-map
     vector-set! when write-bytevector write-string zero?)))

(define geiser-chicken-builtin-symbols 
  (make-parameter
   '(and-let* assume compiler-typecase cond-expand condition-case cut cute declare define-constant
     define-inline define-interface define-record define-record-type define-specialization
     define-syntax-rule define-type define-values dotimes ecase fluid-let foreign-lambda
     foreign-lambda* foreign-primitive foreign-safe-lambda foreign-safe-lambda* functor
     handle-exceptions import let*-values let-location let-optionals let-optionals*
     let-values letrec* letrec-values match-letrec module parameterize regex-case
     require-extension select set! unless use when with-input-from-pipe match
     match-lambda match-lambda* match-let match-let* receive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Wraps output from geiser functions
;; Format is:
;; '((result "string representation of result") (output "string representation of output") (error "string representation of caught error"))
(define (geiser-call-with-result thunk)
  ;; TODO: Capture errors and dump them into '(error "I'm a string!")
  (let* ((result #f)
         (output
          (with-output-to-string
            (lambda ()
              (with-error-output-to-port 
               (current-output-port)
               (lambda () (set! result (thunk))))))))

    ;; ->string doesn't escape strings, but with-output-to-string will
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
(define (geiser-installed-extensions)
  (let ((output (call-with-input-pipe "chicken-status" read-all)))
    (sort! (irregex-split "\\n" (irregex-replace/all " [^\\n]*" output))
           string<?)))

;; Locates any paths at which a particular symbol might be located
(define (geiser-find-library-paths id types)
  (let ((id (cond
             ((string? id) (string->symbol))
             ((symbol? id) id)
             (else (error "Expected either a symbol or string.")))))
    (append
     (if (any (lambda (sym) (eq? sym id)) (geiser-r5rs-symbols))
         '((r5rs))
         '())
     (if (any (lambda (sym) (eq? sym id)) (geiser-r7rs-small-symbols))
         '((r7rs))
         '())
     (if (any (lambda (sym) (eq? sym id)) (geiser-chicken-builtin-symbols))
         '((chicken))
         '())
     (map node-path 
          (filter 
           (lambda (n) 
             (let ((type (node-type n)))
               (any (lambda (t) (eq? type t)) types)))
           (match-nodes id))))))

;; Builds a signature list from an identifier
;; The format is:
;; ((,id (args ((required [signature]) (optional) (key))) (module [module path]) [(error "not found")]) ...)
(define (geiser-find-signatures id)
  (define (fmt node)
    (let ((id (car node))
          (type (cdr node)))
      (cond
       ((equal? 'macro type)
        `(,id (args ((required)
                     (optional)
                     (key)))
              (module ,@(geiser-find-library-paths id '(procedure syntax)))))
       ((and (list? type)
             (let ((type (car type)))
               (or (eq? 'procedure type)
                   (eq? 'record type)
                   (eq? 'setter type)
                   (eq? 'class type)
                   (eq? 'method type))))
        `(,id (args ((required ,@(cdr type))
                     (optional)
                     (key)))
              (module ,@(geiser-find-library-paths '(procedure)))))
       (else
        `(,id (args ((required)
                     (optional)
                     (key)))
              (module))))))

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
(define (geiser-export-signatures! sigs)
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

;; Builds the documentation from Chicken Doc for a specific ymbol
(define (geiser-make-doc symbol #!optional (filter-for-type #f))
  (with-output-to-string 
    (lambda () 
      (map (lambda (node)
             (display (string-append "= Node: " (->string (node-id node)) " " " =\n"))
             (describe node)
             (display "\n\n")) 
           (filter 
            (lambda (n)
              (or (not filter-for-type)
                  (eq? (node-type n) filter-for-type)))
            (match-nodes symbol))))))

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
    (geiser-call-with-result
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
         (prefix (if (symbol? prefix) (symbol->string prefix) prefix))
         (re (regexp (string-append "^" (regexp-escape prefix)))))
    (sort! (map (lambda (sym)
                  ;; Strip out the egg name
                  (string-substitute ".*#([^#]+)" "\\1" (->string sym)))
                (apropos-list re #:macros? #t))
           string<?)))

(define-toplevel-for-geiser geiser-module-completions
  (let* ((prefix (get-arg))
         (match (string-append "^" (regexp-escape prefix))))
    (filter (lambda (v) (string-search match v))
            (geiser-installed-extensions))))

(define-toplevel-for-geiser geiser-autodoc 
  (let ((ids (get-arg)))
    (define (generate-details id)
      (geiser-export-signatures! (geiser-find-signatures id)))

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
  (let* ((symbol (get-arg))
         (sig (geiser-find-signatures symbol)))
    (if (null? sig) 
        '() 
        `(("signature" ,@(geiser-export-signatures! (car sig))) 
          ("docstring" . ,(geiser-make-doc symbol))))))

;; TODO: Support generic methods
(define-toplevel-for-geiser geiser-generic-methods 
  (let ((symbol (get-arg)))
    '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and Buffer Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define geiser-load-paths (make-parameter '()))

(define-toplevel-for-geiser geiser-add-to-load-path 
  (let* ((directory (get-arg))
         (directory (if (symbol? directory) (symbol->string directory) directory)))
    (if (directory-exists? directory)
        (geiser-load-paths (cons directory (geiser-load-paths)))
        #f)))

(define-toplevel-for-geiser geiser-load-file 
  (let* ((file (get-arg))
         (file (if (symbol? file) (symbol->string file) file)))
    (define (try-load file paths)
      (cond
       ((null? paths) #f)
       ((file-exists? (string-append (car paths) "/" file))
        (load (string-append (car paths) "/" file)))
       (else (try-load file (cdr paths)))))
    (try-load file (cons "." (geiser-load-paths)))))

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

;; Returns true if every element of the list could be a module name
(define-toplevel-for-geiser geiser-module-name? 
  (let ((module-name (get-arg)))
    (and (list? module-name)
         (not (null? module-name))
         (every symbol? module-name))))

;; Should return:
;; '(("modules" . sub-modules) ("procs" . procedures) ("syntax" . macros) ("vars" . variables))
(define-toplevel-for-geiser geiser-module-exports 
  (let* ((module-name (get-arg))
         (nodes (match-nodes module-name)))
    (if (null? nodes)
        '()
        (let ((mod '())
              (proc '())
              (syn '())
              (var '()))
          (map
           (lambda (node)
             (let ((type (node-type node))
                   (name (node-id node))
                   (path (node-path node)))
               (cond
                ((or (eq? 'unit type) 
                     (eq? 'egg type))
                 (set! mod (cons name mod)))
                ((or (eq? 'procedure type) 
                     (eq? 'record type)
                     (eq? 'setter type)
                     (eq? 'class type)
                     (eq? 'method type))
                 (set! proc (cons name proc)))
                ((or (eq? 'read type)
                     (eq? 'syntax type))
                 (set! syn (cons name syn)))
                ((or (eq? 'parameter type)
                     (eq? 'constant type))
                 (set! var (cons name var))))))
           nodes)
          `(("modules" . ,mod)
            ("proces" . ,proc)
            ("syntax" . ,syn)
            ("vars" . ,var))))))

;; Returns the path for the file in which an egg or module was defined
(define-toplevel-for-geiser geiser-module-path 
  (let ((module-name (get-arg)))
    #f))

;; Returns:
;; `(("file" . ,(module-path name)) ("line"))
(define-toplevel-for-geiser geiser-module-location 
  (let ((name (get-arg)))
    #f))
