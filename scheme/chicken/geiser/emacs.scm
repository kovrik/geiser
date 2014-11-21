(use apropos)
(use regex)

(define (call-with-result thunk)
  (let* ((result #f)
         (output
          (with-output-to-string
            (lambda ()
              (with-error-output-to-port 
               (current-output-port)
               (lambda () (set! result (thunk))))))))

    ; Hacks
    (set! result (with-output-to-string (lambda () (write result))))

    (write `((result ,result)
             (output ,output)))
    (newline)))

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

(define-toplevel-for-geiser geiser-no-values
  (values))

(define-toplevel-for-geiser geiser-newline
  (newline))

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

(define-toplevel-for-geiser geiser-load-file 
  (let ((file (get-arg)))
    (load file)))

(define-toplevel-for-geiser geiser-completions 
  (let* ((prefix (get-arg))
         (re (regexp (string-append "^" (regexp-escape prefix)))))
    (sort! (map ->string
                (apropos-list re #:macros? #t))
           string<?)))

(define-toplevel-for-geiser geiser-module-completions
  (let* ((prefix (get-arg))
         (re (regexp (string-append "^" (regexp-escape prefix)))))
    (sort! (map ->string
                (apropos-list re #:macros? #t))
           string<?)))

;; TODO: Make this work
(define-toplevel-for-geiser geiser-autodoc 
  (let ((ids (get-arg)))
    (define (helper id)
      `(,id ("args" (("required") ("optional") ("key"))) ("module" chicken)) )
    (if (list? ids)
        (map helper ids)
        '())))

(define-toplevel-for-geiser geiser-symbol-documentation 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-module-exports 
  (let ((module-name (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-object-signature 
  (let ((name (get-arg))
        (object (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-compile 
  (let ((form (get-arg))
        (module (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-compile-file 
  (let ((opts (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-set-warnings 
  (let ((level (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-add-to-load-path 
  (let ((directory (get-arg)))
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

(define-toplevel-for-geiser geiser-symbol-location 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-generic-methods 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-callers 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-callees 
  (let ((symbol (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-find-file 
  (let ((path (get-arg)))
    #f))

(define-toplevel-for-geiser geiser-start-server
  #f)

