#lang racket

(require (for-syntax syntax/stx
                     racket/list
                     racket/port))

(provide forest)

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
     (syntax-case form ()
       [(head path t ...)
        (let ([new-stack (cons #'path stack)])
          (with-syntax ([ns (datum->syntax stx (cons 'list new-stack))]
                        [rec (install-app-hook stx new-stack #'(t ...))])
            ;(displayln #'rec)
            #`(case-lambda
                [() (head ns . rec)]
                [(x) (cond [(eq? x 'name) 'head])])))]
       [(head t ...)
          ;(displayln #'rec)
          #`(case-lambda
              [() (head #,(cons list stack))]
              [(x) (cond [(eq? x 'name) 'head])])]

       [other #'other]))))

(define-for-syntax (parse-body slug ids-stx body-stx)
  (for/list ([body-form-stx (syntax->list body-stx)])
    (syntax-case body-form-stx ()
      [(head  t ...)
       (cond
         [(member #'head ids-stx free-identifier=?)
          #`(map (lambda (x) (x)) (list #,@(install-app-hook #'head (list #'head) #'(t ...))))
          ]
         [else #'(head  t ...)])]
      [val #'val])))

(define-syntax (forest stx)
  (syntax-case stx (roots)
    [(forest (roots . root-forms) body ...)
     (let* ([slug (datum->syntax stx 'slug)]
            [root-forms-syntax-list (parse-root-forms slug #'root-forms)]
            [root-ids-stx (map stx-car root-forms-syntax-list)]
            [body-forms-stx (parse-body slug root-ids-stx #'(body ...))])
       ; (displayln root-forms-syntax-list)
       ; (displayln body-forms-stx)
       (with-syntax ([decls (datum->syntax stx root-forms-syntax-list)]
                     [slug slug])
         #`(let decls  #,@body-forms-stx)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility  macros for dynamic state
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility  macros for node definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
