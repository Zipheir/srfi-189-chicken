(module (srfi 189)
  (maybe? either? just nothing left right nothing? just? maybe= left?
   right? either= either-swap

   maybe-ref maybe-ref/default either-ref either-ref/default

   maybe-join maybe-bind maybe-compose either-join either-bind
   either-compose

   maybe-length maybe-filter maybe-remove either-length
   either-filter either-remove
   maybe-sequence either-sequence

   maybe->either either->maybe list->just list->right maybe->list
   either->list maybe->truth either->truth truth->maybe maybe->values
   maybe->two-values values->maybe either->values values values->either
   two-values->maybe maybe-for-each either-for-each maybe->generation
   generation->maybe list->left list->maybe list->either
   maybe->list-truth either->list-truth list-truth->maybe
   list-truth->either truth->either
   either->generation generation->either
   exception->either either-guard

   maybe-map maybe-fold maybe-unfold either-map either-fold
   either-unfold

   tri-not tri=? tri-and tri-or tri-merge

   (syntax: maybe-and unspecified make-type-condition)
   (syntax: maybe-or unspecified make-type-condition)
   (syntax: maybe-let* singleton? just-objs nothing-obj
                       make-type-condition payload-exception)
   (syntax: either-and unspecified make-type-condition)
   (syntax: either-or make-type-condition)
   (syntax: either-let* singleton? right-objs make-type-condition payload-exception)
   (syntax: maybe-let*-values make-type-condition)
   (syntax: either-let*-values make-type-condition)
   (syntax: maybe-if make-type-condition)
   )

  (import (scheme)
          (chicken base)
          (chicken condition)
          (chicken platform)
          (chicken syntax)
          (chicken type)
          (only (srfi 1) list-copy find every list=)
          typed-records)

  (register-feature! 'srfi-189)

  ;; R7RS shim
  (define (eof-object) #!eof)

  (define (assertion-violation loc msg . args)
    (abort
     (make-composite-condition
      (make-property-condition 'exn
       'location loc
       'message msg
       'arguments args)
      (make-property-condition 'assertion))))

  (define (make-type-condition loc msg . args)
    (make-composite-condition
     (make-property-condition 'exn
      'location loc
      'message msg
      'arguments args)
     (make-property-condition 'type)
     (make-property-condition 'assertion)))

  (define (make-arity-condition loc msg . args)
    (make-composite-condition
     (make-property-condition 'exn
      'location loc
      'message msg
      'arguments args)
     (make-property-condition 'arity)
     (make-property-condition 'assertion)))

  (define-syntax assert-type
    (syntax-rules ()
      ((assert-type loc expr . args)
       (unless expr
         (abort
          (make-type-condition loc
                               "type check failed"
                               'expr
                               . args))))))

  ;; Called by a variadic procedure when it is passed the wrong
  ;; number of arguments.
  (define (arity-exception loc . args)
    (abort
     (make-composite-condition
      (make-property-condition 'exn
       'location loc
       'message "invalid number of arguments"
       'arguments args)
      (make-property-condition 'arity)
      (make-property-condition 'assertion))))

  ;; Called by procedures like maybe-join which take a Maybe/Either
  ;; with a specific number or kind of payload values.
  (define (payload-exception loc msg . args)
    (abort
     (make-composite-condition
      (make-property-condition 'exn
       'location loc
       'message msg
       'arguments args)
      (make-property-condition 'type)
      (make-property-condition 'payload))))

  (include "189.scm"))
