(module geiser

  ;; A bunch of these needn't be toplevel functions
  (geiser-eval
   geiser-no-values
   geiser-newline
   geiser-start-server
   geiser-completions
   geiser-autodoc
   geiser-object-signature
   geiser-symbol-location
   geiser-symbol-documentation
   geiser-find-file
   geiser-add-to-load-path
   geiser-load-file
   geiser-compile-file
   geiser-compile
   geiser-module-exports
   geiser-module-path
   geiser-module-location
   geiser-macroexpand
   make-geiser-toplevel-bindings
   without-current-module)
  
  (import chicken scheme extras data-structures ports csi)

  (use apropos)
  (use regex)
  (use irregex)
  (use srfi-18)
  (use tcp)
  (use posix)
  (use chicken-doc)
  (use srfi-1)
  (use utils)

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

  (define (make-apropos-regex toplevel-module prefix)
    (let* ((prefix (->string prefix))
           (prefix-module (if toplevel-module
                              (string-append toplevel-module "#" prefix)
                              #f)))
      (string-append 
       (if prefix-module
           (string-append "^" prefix-module "|")
           "")
       "^" prefix)))

  ;; ;; Returns true if every element of the list could be a module name
  ;; (define (module-name? module-name) 
  ;;   (and (list? module-name)
  ;;        (not (null? module-name))
  ;;        (every symbol? module-name)))

  ;; Wraps output from geiser functions
  (define (call-with-result thunk)
    ;; This really should be a chicken library function
    (define (write-exception exn)
      (define (write-call-entry call)
        (let ((type (vector-ref call 0))
              (line (vector-ref call 1)))
          (cond
           ((equal? type "<syntax>")
            (write type) (write " ") (write line) (newline))
           ((equal? type "<eval>")
            (write type) (write "   ") (write line) (newline)))))

      (display (format "Error: (~s) ~s: ~s"
                       ((condition-property-accessor 'exn 'location) exn)
                       ((condition-property-accessor 'exn 'message) exn)
                       ((condition-property-accessor 'exn 'arguments) exn)))
      (newline)
      (display "Call history: ") (newline)
      (map write-call-entry ((condition-property-accessor 'exn 'call-chain) exn))
      (newline))

    ;; And this should be a chicken library function as well
    (define (with-all-output-to-string thunk)
      (with-output-to-string
        (lambda ()
          (with-error-output-to-port 
           (current-output-port)
           thunk))))

    (let* ((result (if #f #f))
           (output (handle-exceptions exn 
                                      (with-all-output-to-string (lambda () (write-exception exn)))
                                      (with-all-output-to-string (lambda () (set! result (thunk)))))))

      (set! result (with-output-to-string (lambda () (write result))))

      (write `((result ,result)
               (output . ,output)))
      (newline)))

  (define geiser-toplevel-functions (make-parameter '()))

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
           (,(r 'geiser-toplevel-functions) (cons (cons ',name ,name) (geiser-toplevel-functions)))))))

  ;; Locates all installed extensions
  (define (installed-extensions)
    (let ((output (call-with-input-pipe "chicken-status" read-all)))
      (sort! (irregex-split "\\n" (irregex-replace/all " [^\\n]*" output))
             string<?)))

  ;; Locates any paths at which a particular symbol might be located
  (define (find-library-paths id types)
    ;; Removes the given id from the node path
    (define (remove-self-id id path)
      (cond
       ((not (list? path)) path)
       ((null? path) path)
       ((null? (cdr path))
        (if (eq? (car path) id)
            '()
            path))
       (else
        (cons (car path) (remove-self-id id (cdr path))))))

    (let ((id (->string id)))
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
       (map
        (lambda (node)
          (remove-self-id id (node-path node))) 
        (filter 
         (lambda (n) 
           (let ((type (node-type n)))
             (any (lambda (t) (eq? type t)) types)))
         (match-nodes id))))))

  ;; Builds a signature list from an identifier
  ;; The format is:
  ;; ((,id (args ((required [signature]) (optional) (key))) (module [module path]) [(error "not found")]) ...)
  (define (find-signatures toplevel-module id)
    (define (fmt node)
      (let ((id (car node))
            (module (cadr node))
            (type (cddr node)))
        (cond
         ((equal? 'macro type)
          `(,id (args ((required)
                       (optional)
                       (key)))
                (module ,@(if (not module) 
                              (find-library-paths id '(procedure syntax))
                              (list module)))))
         ((and (or (list? type) (pair? type))
               (let ((type (car type)))
                 (or (eq? 'procedure type)
                     (eq? 'record type)
                     (eq? 'setter type)
                     (eq? 'class type)
                     (eq? 'method type))))
          (let ((reqs '())
                (opts '()))
            (define (collect-args args)
              (when (not (null? args))
                (cond
                 ((or (pair? args) (list? args)) 
                  (set! reqs (append reqs (list (car args))))
                  (collect-args (cdr args)))
                 (else
                  (set! opts (list args '..))))))
            (collect-args (cdr type))

            `(,id (args ((required ,@reqs)
                         (optional ,@opts)
                         (key)))
                  (module ,@(if (not module) 
                                (find-library-paths id '(procedure record setter class method))
                                (list module))))))
         (else
          `(,id (args ((required)
                       (optional)
                       (key)))
                (module ,@(if (not module) '() (list module))))))))

    (define (find id)
      (filter
       (lambda (s)
         ;; Remove egg name and add module
         (let* ((str (symbol->string (car s)))
                (name (string-substitute ".*#([^#]+)" "\\1" str))
                (module (string-substitute "(.*)#[^#]+" "\\1" str))
                (module (if (equal? str module) #f module)))
           (set! (car s) name)
           (set! (cdr s) (cons module (cdr s)))
           (equal? name (->string id))))
       (apropos-information-list (regexp (make-apropos-regex toplevel-module id)) #:macros? #t)))

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

  ;; Builds the documentation from Chicken Doc for a specific ymbol
  (define (make-doc symbol #!optional (filter-for-type #f))
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

  (define-syntax without-current-module 
    (syntax-rules ()
      ((_ ..)
       (parameterize ((##sys#current-module #f)) ..))))

  (define (make-geiser-toplevel-bindings)
    (map
     (lambda (pair)
       (toplevel-command (car pair) (cdr pair)))
     (geiser-toplevel-functions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geiser toplevel functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Basically all non-core functions pass through geiser-eval

  (define-toplevel-for-geiser geiser-eval
    ;; We can't allow nested module definitions in Chicken
    (define (form-has-module? form)
      (let ((reg "\\( *module +|\\( *define-library"))
        (string-search reg form)))

    ;; Chicken doesn't support calling toplevel functions through eval,
    ;; So when we're in a module or calling into an environment we have
    ;; to first call from the toplevel environment and then switch
    ;; into the desired env.
    (define (form-has-geiser? form)
      (let ((reg "\\( *geiser-"))
        (string-search reg form)))

    ;; Rather than failing gracefully with #f, module-environment throws
    ;; an exception.
    (define (find-environment module)
      (handle-exceptions exn #f (module-environment module)))

    ;; All calls start at toplevel
    (without-current-module
     (let* ((module (get-arg))
            (form (get-arg))
            (str-form (format "~s" form))
            (is-module? (form-has-module? str-form))
            (is-geiser? (form-has-geiser? str-form))
            (env (and (not is-module?)
                      (not is-geiser?)
                      (find-environment module))))

       ;; Inject environment as the first parameter
       (when is-geiser?
         (let ((module 
                 (if module 
                     (->string module)
                     #f)))
           (set! form
                 `(begin
                    (import geiser)
                    ,(cons (car form) (cons module (cdr form)))))))

       (define (thunk)
         (if env
             (eval form env)
             (eval form)))

       (call-with-result thunk))))

  ;; Load a file

  (define-toplevel-for-geiser geiser-load-file 
    (let* ((file (get-arg))
           (file (if (symbol? file) (symbol->string file) file))
           (found-file (geiser-find-file #f file)))
      (call-with-result
       (lambda ()
         (when found-file
           (load found-file))))))

  ;; The no-values identity
  
  (define-toplevel-for-geiser geiser-no-values
    (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Invoke a newline

  (define (geiser-newline . rest)
    (newline))

  ;; Spawn a server for remote repl access

  (define (geiser-start-server . rest)
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

  (define (geiser-completions toplevel-module prefix . rest)
    ;; We search both toplevel definitions and module definitions
    (let* ((prefix (if (symbol? prefix) (symbol->string prefix) prefix))
           (re (regexp (make-apropos-regex toplevel-module prefix))))
      (sort! (map (lambda (sym)
                    ;; Strip out the egg name
                    (string-substitute ".*#([^#]+)" "\\1" (->string sym)))
                  (apropos-list re #:macros? #t))
             string<?)))

  (define (geiser-module-completions toplevel-module prefix . rest)
    (let* ((match (string-append "^" (regexp-escape prefix))))
      (filter (lambda (v) (string-search match v))
              (installed-extensions))))

  (define (geiser-autodoc toplevel-module ids . rest) 
    (define (generate-details id)
      (export-signatures! (find-signatures toplevel-module id)))

    (if (list? ids)
        (foldr append '()
               (map generate-details ids))
        '()))

  (define (geiser-object-signature toplevel-module name object . rest) 
    (let* ((sig (geiser-autodoc toplevel-module `(,name))))
      (if (null? sig) '() (car sig))))

    ;; TODO: Divine some way to support this functionality

  (define (geiser-symbol-location toplevel-module symbol . rest) 
    '(("file") ("line")))

  (define (geiser-symbol-documentation toplevel-module symbol . rest)
    (let* ((sig (find-signatures toplevel-module symbol)))
      (if (null? sig) 
          '() 
          `(("signature" ,@(export-signatures! (car sig))) 
            ("docstring" . ,(make-doc symbol))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File and Buffer Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define geiser-load-paths (make-parameter '()))

  (define (geiser-find-file toplevel-module file . rest)
    (let ((paths (append '("" ".") (geiser-load-paths))))
      (define (try-find file paths)
        (cond
         ((null? paths) #f)
         ((file-exists? (string-append (car paths) file))
          (string-append (car paths) file))
         (else (try-find file (cdr paths)))))
      (try-find file paths)))

  (define (geiser-add-to-load-path toplevel-module directory . rest) 
    (let* ((directory (if (symbol? directory) 
                          (symbol->string directory)
                          directory))
           (directory (if (not (equal? #\/ (string-ref directory (- (string-length directory 1))))) 
                          (string-append directory "/")
                          directory)))
      (call-with-result
       (lambda ()
         (when (directory-exists? directory)
           (geiser-load-paths (cons directory (geiser-load-paths))))))))

  (define (geiser-compile-file toplevel-module file . rest) 
    (let* ((file (if (symbol? file) (symbol->string file) file))
           (found-file (geiser-find-file toplevel-module file)))
      (call-with-result
       (lambda ()
         (when found-file
           (without-current-module
            (compile-file found-file)))))))

    ;; TODO: Support compiling regions

  (define (geiser-compile toplevel-module form module . rest) 
    (error "Chicken does not support compiling regions"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Should return:
  ;; '(("modules" . sub-modules) ("procs" . procedures) ("syntax" . macros) ("vars" . variables))
  (define (geiser-module-exports toplevel-module module-name . rest) 
    (let* ((nodes (match-nodes module-name)))
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

  (define (geiser-module-path toplevel-module module-name . rest) 
    #f)

  ;; Returns:
  ;; `(("file" . ,(module-path name)) ("line"))

  (define (geiser-module-location toplevel-module name . rest) 
    #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (geiser-macroexpand toplevel-module form . rest)
    (with-output-to-string
      (lambda ()
        (pretty-print (expand form)))))

;; End module    
  )

(import geiser)
(make-geiser-toplevel-bindings)
