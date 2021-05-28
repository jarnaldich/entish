#lang racket

(require (for-syntax syntax/stx
                     racket/list
                     racket/string
                     racket/port))
(provide forest)

;; Builtins are needed for the syntactic sugar of the root node...
(require "builtins.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DSL Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (parse-root-forms slug root-forms-stx)
  ; This list can contain syntax and non syntax elements...
  (cons #`(#,slug '())
        (for/list ([rf-stx (syntax->list root-forms-stx)])
          (cond
            [(stx-list? rf-stx) rf-stx]
            (else (list rf-stx 1))))))


(define-for-syntax (install-app-hook stx stack forms)
  (datum->syntax
   stx
   (for/list ([form (syntax->list forms)])
     (syntax-case form (let let* +esc+)
       [(let kv t ...)
        (begin
          (with-syntax ([ns (datum->syntax stx (cons 'list stack))]
                        [rec (install-app-hook stx stack #'(t ...))])
            #`(let kv . rec)))]
       [(+esc+ f ...)
        #`(begin f ...)]
       [(head f ...)
        (string-prefix? (symbol->string (syntax->datum #'head)) ".")
        #`(begin f ...)]
       [(head t ...)
        ;; En aquests casos no inserim
        (string-prefix? (symbol->string (syntax->datum #'head)) "+")
;        (free-identifier=? #'head (datum->syntax stx 'seq))
        (begin
          (with-syntax ([ns (datum->syntax stx (cons 'list stack))]
                        [trail (datum->syntax stx 'trail)]
                        [rec (install-app-hook stx stack #'(t ...))])
            #`(case-lambda
                [() (parameterize ([trail (remove (void) ns)])
                      (head . rec))]
                [(x) (cond [(eq? x 'name) 'head])])))]
       [(head path t ...)
        (let ([new-stack (cons #'path stack)])
          (with-syntax ([ns (datum->syntax stx (cons 'list new-stack))]
                        [trail (datum->syntax stx 'trail)]
                        [rec (install-app-hook stx new-stack #'(t ...))])
            ;(displayln #'rec)
            #`(case-lambda
                [() (parameterize ([trail (remove (void) ns)])
                      (head . rec))]
                [(x) (cond [(eq? x 'name) 'head])])))]

       ;; Quin cas?
       [(head t ...)
          ;(displayln #'rec)
          #`(case-lambda
              [() (head #,(cons list stack))]
              [(x) (cond [(eq? x 'name) 'head])])]

       [other #'other]))))

(define-for-syntax (parse-body slug ids-stx body-stx)
  (for/list ([body-form-stx (syntax->list body-stx)])
    (syntax-case body-form-stx ()
      [(head t ...)
       (cond
         [(member #'head ids-stx free-identifier=?)
          (with-syntax ([r (datum->syntax body-stx 'root)])
            #`(case-lambda
                [()  (parameterize ([trail (remove (void) (list head))])
                       (r #,@(install-app-hook #'head (list #'head) #'(t ...))))]
                [(x) (cond [(eq? x 'name) 'head])]))]
         [else
          (begin
                 #'(head  t ...))])]
      [val (begin  #'val)])))


(define-syntax (forest stx)
  (syntax-case stx (roots)
    [(forest (roots . root-forms) body ...)
     (let* ([slug (datum->syntax stx 'slug)]
            [root-forms-syntax-list (parse-root-forms slug #'root-forms)]
            [root-ids-stx (map stx-car root-forms-syntax-list)]
            [body-forms (parse-body slug root-ids-stx #'(body ...))])
       ; (displayln root-forms-syntax-list)
       ; (displayln body-forms-stx)
       (with-syntax ([decls (datum->syntax stx root-forms-syntax-list)]
                     ;[body-forms-stx (datum->syntax stx body-forms)]
                     [slug slug])
         ; Maybe in the future can be extended to a case-lambda if metadata is needed (grafts)
         #`(thunk (let decls  (map (lambda (thunk) (thunk))
                                   (list #,@body-forms))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility  macros for dynamic state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility  macros for node definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
