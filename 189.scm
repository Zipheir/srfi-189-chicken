;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-record-type <just>
  (raw-just objs)
  just?
  (objs just-objs : list))

(define-record-type <nothing>
  (make-nothing)
  nothing?)

(define-type just-t (struct <just>))
(define-type maybe-t (or (struct <nothing>) (struct <just>)))

(define-record-type <left>
  (raw-left objs)
  left?
  (objs left-objs : list))

(define-record-type <right>
  (raw-right objs)
  right?
  (objs right-objs : list))

(define-type right-t (struct <right>))
(define-type left-t (struct <left>))
(define-type either-t (or (struct <left>) (struct <right>)))

(: nothing-obj (struct <nothing>))
(define nothing-obj (make-nothing))

(: nothing (--> (struct <nothing>)))
(define (nothing)
  nothing-obj)

;;;; Utility

(define-syntax const
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

(define (singleton? lis)
  (and (pair? lis) (null? (cdr lis))))

(define-syntax fast-apply
  (syntax-rules ()
    ((_ proc args)
     (if (singleton? args) (proc (car args)) (apply proc args)))))

(define-syntax fast-list->values
  (syntax-rules ()
    ((_ vals)
     (if (singleton? vals) (car vals) (apply values vals)))))

(define unspecified (if #f #f))

;;;; Constructors

(: just (#!rest --> just-t))
(define (just . objs)
  (raw-just objs))

(: left (#!rest --> left-t))
(define (left . objs)
  (raw-left objs))

(: right (#!rest --> right-t))
(define (right . objs)
  (raw-right objs))

;;; List -> <container> equivalents of the basic constructors.

;;; Here and elsewhere, we copy caller-provided list arguments when
;;; passing them to raw-just/left/right, so that later mutation of the
;;; list argument doesn't affect the payloads of any Maybes/Eithers.

(: list->just (list --> just-t))
(define (list->just lis)
  (assert-type 'list->just (or (null? lis) (pair? lis)))
  (raw-just (list-copy lis)))

(: list->right (list --> right-t))
(define (list->right lis)
  (assert-type 'list->right (or (null? lis) (pair? lis)))
  (raw-right (list-copy lis)))

(: list->left (list --> left-t))
(define (list->left lis)
  (assert-type 'list->left (or (null? lis) (pair? lis)))
  (raw-left (list-copy lis)))

(: maybe->either (maybe-t #!rest --> either-t))
(define (maybe->either maybe . default-objs)
  (assert-type 'maybe->either (maybe? maybe))
  (if (nothing? maybe)
      (raw-left default-objs)
      (raw-right (just-objs maybe))))

(: either->maybe (either-t --> maybe-t))
(define (either->maybe either)
  (assert-type 'either->maybe (either? either))
  (if (left? either)
      nothing-obj
      (raw-just (right-objs either))))

(: either-swap (either-t --> either-t))
(define (either-swap either)
  (assert-type 'either-swap (either? either))
  (if (right? either)
      (raw-left (right-objs either))
      (raw-right (left-objs either))))

;;;; Predicates

(: maybe? (* --> boolean))
(define (maybe? obj)
  (or (just? obj) (nothing? obj)))

(: nothing? (* --> boolean))
(define (nothing? obj)
  (eqv? obj nothing-obj))

;; True if all the maybes are Nothing, or if all are Justs containing
;; the same number of values which are element-wise equal in the sense
;; of `equal'.
(: maybe= ((* * -> *) #!rest maybe-t -> boolean))
(define (maybe= equal . maybes)
  (assert-type 'maybe= (procedure? equal))
  (unless (pair? maybes) (arity-exception 'maybe=))
  (let ((maybe1 (car maybes)))
    (every (lambda (maybe2) (%maybe=2 equal maybe1 maybe2))
           (cdr maybes))))

;; Compare two Maybes.
(: %maybe=2 ((* * -> *) maybe-t maybe-t -> boolean))
(define (%maybe=2 equal maybe1 maybe2)
  (or (eqv? maybe1 maybe2)  ; Also handles the Nothing = Nothing case.
      (and (just? maybe1)
           (just? maybe2)
           (list= equal (just-objs maybe1) (just-objs maybe2)))))

(: either? (* --> boolean))
(define (either? obj)
  (or (left? obj) (right? obj)))

;; True if the eithers are all Lefts/all Rights containing the same
;; number of values which are element-wise equal in the sense of `equal'.
(: either= ((* * -> *) #!rest either-t -> boolean))
(define (either= equal . eithers)
  (assert-type 'either= (procedure? equal))
  (unless (pair? eithers) (arity-exception 'either=))
  (let ((either1 (car eithers)))
    (every (lambda (either2) (%either=2 equal either1 either2))
           (cdr eithers))))

;; Compare two Eithers.
(: %either=2 ((* * -> *) either-t either-t -> boolean))
(define (%either=2 equal either1 either2)
  (let ((e= (lambda (acc) (list= equal (acc either1) (acc either2)))))
    (or (eqv? either1 either2)
        (and (left? either1) (left? either2) (e= left-objs))
        (and (right? either1) (right? either2) (e= right-objs)))))

;;;; Accessors

;; Unwrap a Maybe.  If it's a Just, call the optional success
;; continuation (default: values) on its payload; otherwise,
;; call failure.
(: maybe-ref (or (maybe-t procedure -> *)
                 (maybe-t procedure procedure -> *)))
(define (maybe-ref maybe failure . %opt-args)
  (assert-type 'maybe-ref (maybe? maybe))
  (assert-type 'maybe-ref (procedure? failure))
  (if (just? maybe)
      (let ((objs (just-objs maybe))
            (success (if (pair? %opt-args) (car %opt-args) values)))
        (fast-apply success objs))
      (failure)))

;; Unwrap a Maybe.  If it's a Nothing, return the default objects.
(: maybe-ref/default (maybe-t #!rest --> *))
(define (maybe-ref/default maybe . default-objs)
  (assert-type 'maybe-ref/default (maybe? maybe))
  (if (just? maybe)
      (let ((objs (just-objs maybe)))
        (fast-list->values objs))
      (fast-list->values default-objs)))

(define (%either-ref-single either accessor cont)
  (let ((objs (accessor either)))
    (fast-apply cont objs)))

;; Unwrap an Either, calling failure on the payload of a Left and the
;; optional success continuation (default: values) on that of a Right.
(: either-ref (or (either-t procedure -> *)
                  (either-t procedure procedure -> *)))
(define (either-ref either failure . %opt-args)
  (assert-type 'either-ref (either? either))
  (assert-type 'either-ref (procedure? failure))
  (if (right? either)
      (%either-ref-single either right-objs (if (pair? %opt-args)
                                                (car %opt-args)
                                                values))
      (%either-ref-single either left-objs failure)))

;; Unwrap an Either.  If it's a Left, return the default objects.
(: either-ref/default (either-t #!rest --> *))
(define (either-ref/default either . default-objs)
  (assert-type 'either-ref/default (either? either))
  (if (right? either)
      (let ((objs (right-objs either)))
        (fast-list->values objs))
      (fast-list->values default-objs)))

;;;; Join and bind

(define-type mp-maybe (#!rest -> maybe-t))
(define-type mp-either (#!rest -> either-t))

;; If maybe is a Just containing a single Maybe, return that Maybe.
(: maybe-join (maybe-t --> maybe-t))
(define (maybe-join maybe)
  (assert-type 'maybe-join (maybe? maybe))
  (if (nothing? maybe)
      nothing-obj
      (let ((objs (just-objs maybe)))
        (unless (and (singleton? objs) (maybe? (car objs)))
          (payload-exception
           'maybe-join
           "invalid payload: not a single Maybe value"
           objs))
        (car objs))))

;; Call the first mproc on the payload of a Maybe, producing a Maybe.
;; Repeat the operation with this Maybe and the remaining mprocs.
;; Return a Nothing immediately.
(: maybe-bind (maybe-t #!rest mp-maybe -> maybe-t))
(define (maybe-bind maybe mproc . mprocs)
  (assert-type 'maybe-bind (maybe? maybe))
  (if (null? mprocs)
      (maybe-ref maybe nothing mproc)  ; fast path
      (let lp ((m maybe) (mp mproc) (mprocs mprocs))
        (maybe-ref m
                   nothing
                   (lambda objs
                     (if (null? mprocs)
                         (fast-apply mp objs) ; tail-call last
                         (lp (fast-apply mp objs)
                             (car mprocs)
                             (cdr mprocs))))))))

;; Compose the argument mprocs and return the resulting monadic procedure.
(: maybe-compose (mp-maybe #!rest mp-maybe -> mp-maybe))
(define (maybe-compose . mprocs)
  (unless (pair? mprocs) (arity-exception 'maybe-compose))
  (lambda args
    (let lp ((args args) (mproc (car mprocs)) (rest (cdr mprocs)))
      (if (null? rest)
          (fast-apply mproc args)             ; tail-call last
          (maybe-ref (fast-apply mproc args)
                     nothing
                     (lambda objs
                       (lp objs (car rest) (cdr rest))))))))

;; If either is a Right containing a single Either, return that Either.
(: either-join (either-t --> either-t))
(define (either-join either)
  (assert-type 'either-join (either? either))
  (if (left? either)
      either
      (let ((objs (right-objs either)))
        (unless (and (singleton? objs) (either? (car objs)))
          (payload-exception
           'either-join
           "invalid payload: not a single Either value"
           objs))
        (car objs))))

;; Call the first mproc on the payload of a Either, producing a Either.
;; Repeat the operation with this Either and the remaining mprocs.
;; Return a Left immediately.
(: either-bind (either-t #!rest mp-either -> either-t))
(define (either-bind either mproc . mprocs)
  (assert-type 'either-bind (either? either))
  (if (null? mprocs)
      (either-ref either (const either) mproc)  ; fast path
      (let lp ((e either) (mp mproc) (mprocs mprocs))
        (either-ref e
                    (const e)
                    (lambda objs
                      (if (null? mprocs)
                          (fast-apply mp objs)  ; tail-call last
                          (lp (fast-apply mp objs)
                              (car mprocs)
                              (cdr mprocs))))))))

;; Compose the argument mprocs and return the resulting monadic procedure.
(: either-compose (mp-either #!rest mp-either -> mp-either))
(define (either-compose . mprocs)
  (unless (pair? mprocs) (arity-exception 'either-compose))
  (lambda args
    (let lp ((args args) (mproc (car mprocs)) (rest (cdr mprocs)))
      (if (null? rest)
          (fast-apply mproc args)              ; tail-call last
          (either-ref (fast-apply mproc args)
                      left
                      (lambda objs
                        (lp objs (car rest) (cdr rest))))))))

;;;; Sequence operations

(: maybe-length (maybe-t --> fixnum))
(define (maybe-length maybe)
  (assert-type 'maybe-length (maybe? maybe))
  (if (just? maybe) 1 0))

;; Return maybe if its payload satisfies pred; otherwise, return Nothing.
(: maybe-filter (procedure maybe-t -> maybe-t))
(define (maybe-filter pred maybe)
  (assert-type 'maybe-filter (procedure? pred))
  (assert-type 'maybe-filter (maybe? maybe))
  (if (and (just? maybe) (fast-apply pred (just-objs maybe)))
      maybe
      nothing-obj))

;; Return maybe if its payload doesn't satisfy pred; otherwise, return
;; Nothing.
(: maybe-remove (procedure maybe-t -> maybe-t))
(define (maybe-remove pred maybe)
  (assert-type 'maybe-remove (procedure? pred))
  (assert-type 'maybe-remove (maybe? maybe))
  (if (and (just? maybe)
           (not (fast-apply pred (just-objs maybe))))
      maybe
      nothing-obj))

;; Traverse a container of Maybes with cmap, collect the payload
;; objects with aggregator, and wrap the new collection in a Just.
;; If a Nothing is encountered while traversing, return it
;; immediately.
(: maybe-sequence (or (* (* * -> *) -> maybe-t)
                      (* (* * -> *) (* -> *) -> maybe-t)))
(define maybe-sequence
  (case-lambda
   ((container cmap) (maybe-sequence container cmap list))
   ((container cmap aggregator)
    (assert-type 'maybe-sequence (procedure? cmap))
    (assert-type 'maybe-sequence (procedure? aggregator))
    (call-with-current-continuation
     (lambda (return)
       (just (cmap (lambda (m)
                     (maybe-ref m (lambda () (return m)) aggregator))
                   container)))))))

(: either-length (either-t --> fixnum))
(define (either-length either)
  (assert-type 'either-length (either? either))
  (if (right? either) 1 0))

;; Return either if its payload satisfies pred; otherwise, return
;; a Left of the default objects.
(: either-filter (procedure either-t #!rest -> either-t))
(define (either-filter pred either . default-objs)
  (assert-type 'either-filter (procedure? pred))
  (assert-type 'either-filter (either? either))
  (if (and (right? either) (fast-apply pred (right-objs either)))
      either
      (raw-left default-objs)))

(: either-remove (procedure either-t #!rest -> either-t))
(define (either-remove pred either . default-objs)
  (assert-type 'either-remove (procedure? pred))
  (assert-type 'either-remove (either? either))
  (if (and (right? either)
           (not (fast-apply pred (right-objs either))))
      either
      (raw-left default-objs)))

;; Traverse a container of Eithers with cmap, collect the payload
;; objects with aggregator, and wrap the new collection in a Right.
;; If a Left is encountered while traversing, return it immediately.
(: either-sequence (or (* (* * -> *) -> either-t)
                       (* (* * -> *) (* -> *) -> either-t)))
(define either-sequence
  (case-lambda
   ((container cmap) (either-sequence container cmap list))
   ((container cmap aggregator)
    (assert-type 'either-sequence (procedure? cmap))
    (assert-type 'either-sequence (procedure? aggregator))
    (call-with-current-continuation
     (lambda (return)
       (right (cmap (lambda (e)
                      (either-ref e (const (return e)) aggregator))
                    container)))))))

;;;; Protocol conversion

(: maybe->list (maybe-t --> list))
(define (maybe->list maybe)
  (assert-type 'maybe->list (maybe? maybe))
  (if (nothing? maybe) '() (just-objs maybe)))

(: either->list (either-t --> list))
(define (either->list either)
  (assert-type 'either->list (either? either))
  ((if (right? either) right-objs left-objs) either))

(: list->maybe (list --> maybe-t))
(define (list->maybe lis)
  (assert-type 'list->maybe (or (null? lis) (pair? lis)))
  (if (null? lis) nothing-obj (raw-just (list-copy lis))))

(: list->either (list #!rest --> either-t))
(define (list->either lis . default-objs)
  (assert-type 'list->either (or (null? lis) (pair? lis)))
  (if (null? lis)
      (raw-left default-objs)
      (raw-right (list-copy lis))))

;; If maybe is a Just, return its payload; otherwise, return false.
(: maybe->truth (maybe-t --> *))
(define (maybe->truth maybe)
  (assert-type 'maybe->truth (maybe? maybe))
  (if (nothing? maybe)
      #f
      (let ((objs (just-objs maybe)))
        (unless (singleton? objs)
          (payload-exception 'maybe->truth
                             "invalid payload: not a single value"
                             objs))
        (car objs))))

;; If either is a Right, return its payload; otherwise, return false.
(: either->truth (either-t --> *))
(define (either->truth either)
  (assert-type 'either->truth (either? either))
  (if (left? either)
      #f
      (let ((objs (right-objs either)))
        (unless (singleton? objs)
          (payload-exception 'either->truth
                             "invalid payload: not a single value"
                             objs))
        (car objs))))

(: truth->maybe (* --> maybe-t))
(define (truth->maybe obj)
  (if obj (just obj) nothing-obj))

(: truth->either (* #!rest --> either-t))
(define (truth->either obj . default-objs)
  (if obj (right obj) (raw-left default-objs)))

;;; These procedures interface between the Maybe protocol and the
;;; list-truth protocol, which uses #f to represent failure and a
;;; list to represent success.

(: maybe->list-truth (maybe-t --> (or list false)))
(define (maybe->list-truth maybe)
  (assert-type 'maybe->list-truth (maybe? maybe))
  (if (just? maybe) (just-objs maybe) #f))

(: either->list-truth (either-t --> (or list false)))
(define (either->list-truth either)
  (assert-type 'either->list-truth (either? either))
  (if (right? either) (right-objs either) #f))

;; If list-or-false is #f, return Nothing.  If it's a list, return a
;; Just containing its elements.
(: list-truth->maybe ((or list false) --> maybe-t))
(define (list-truth->maybe list-or-false)
  (if list-or-false
      (begin
       (assert-type 'list-truth->maybe
                    (or (null? list-or-false) (pair? list-or-false)))
       (raw-just (list-copy list-or-false)))
      nothing-obj))

;; If list-or-false is #f, return a Left of the default objects.
;; If it's a list, return a Right containing its elements.
(: list-truth->either ((or list false) #!rest --> either-t))
(define (list-truth->either list-or-false . default-objs)
  (if list-or-false
      (begin
       (assert-type 'list-truth->either
                    (or (null? list-or-false) (pair? list-or-false)))
       (raw-right (list-copy list-or-false)))
      (raw-left default-objs)))

;;; The following procedures interface between the Maybe protocol and
;;; the generation protocol, which uses an EOF object to represent
;;; failure and any other value to represent success.

;; If maybe is a Just whose payload is a single value, return that
;; value.  If it's a Nothing, return an eof-object.
(: maybe->generation (maybe-t --> *))
(define (maybe->generation maybe)
  (assert-type 'maybe->generation (maybe? maybe))
  (if (nothing? maybe)
      (eof-object)
      (let ((objs (just-objs maybe)))
        (unless (singleton? objs)
          (payload-exception 'maybe->generation
                             "invalid payload: must be a single value"
                             objs))
        (car objs))))

;; If either is a Right whose payload is a single value, return that
;; value.  If it's a Left, return an eof-object.
(: either->generation (either-t --> *))
(define (either->generation either)
  (assert-type 'either->generation (either? either))
  (if (left? either)
      (eof-object)
      (let ((objs (right-objs either)))
        (unless (singleton? objs)
          (payload-exception 'either->generation
                             "invalid payload: must be a single value"
                             objs))
        (car objs))))

(: generation->maybe (* --> maybe-t))
(define (generation->maybe obj)
  (if (eof-object? obj) nothing-obj (just obj)))

(: generation->either (* --> either-t))
(define (generation->either obj . default-objs)
  (if (eof-object? obj) (raw-left default-objs) (right obj)))

;;; These procedures interface between the Maybe/Either protocols and
;;; the values protocol, which returns one or more values to represent
;;; success and zero values to represent failure.

(: maybe->values (maybe-t --> *))
(define (maybe->values maybe)
  (assert-type 'maybe->values (maybe? maybe))
  (maybe-ref maybe values values))

(: values->maybe (procedure -> maybe-t))
(define (values->maybe producer)
  (assert-type 'values->maybe (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) nothing-obj (raw-just objs)))))

;;; The following procedures interface between the Maybe protocol and
;;; the two-values protocol, which returns |#f, #f| to represent
;;; failure and |<any object>, #t| to represent success.

;; If maybe is Just containing a single value, return that value and #t.
;; If it's a Nothing, return |#f, #f|.
(: maybe->two-values (maybe-t --> * boolean))
(define (maybe->two-values maybe)
  (assert-type 'maybe->two-values (maybe? maybe))
  (if (nothing? maybe)
      (values #f #f)
      (begin
       (assert-type 'maybe->two-values (singleton? (just-objs maybe))
               "maybe->two-values: invalid payload")
       (values (car (just-objs maybe)) #t))))

(: two-values->maybe (procedure -> maybe-t))
(define (two-values->maybe producer)
  (assert-type 'two-values->maybe (procedure? producer))
  (call-with-values
   producer
   (lambda (obj success)
     (if success (just obj) nothing-obj))))

(: either->values (either-t --> *))
(define (either->values either)
  (assert-type 'either->value (either? either))
  (either-ref/default either))

(: values->either (procedure #!rest -> either-t))
(define (values->either producer . default-objs)
  (assert-type 'values->either (procedure? producer))
  (call-with-values
   producer
   (lambda objs
     (if (null? objs) (raw-left default-objs) (raw-right objs)))))

(: exception->either (procedure procedure -> either-t))
(define (exception->either pred thunk)
  (assert-type 'exception->either (procedure? pred))
  (assert-type 'exception->either (procedure? thunk))
  (guard (obj ((pred obj) (left obj)))
    (call-with-values thunk right)))

;;;; Map, fold, and unfold

;; If maybe is a Just, apply proc to its payload and wrap the result
;; in a Just.  Otherwise, return Nothing.
(: maybe-map (procedure maybe-t -> maybe-t))
(define (maybe-map proc maybe)
  (assert-type 'maybe-map (procedure? proc))
  (assert-type 'maybe-map (maybe? maybe))
  (if (nothing? maybe)
      nothing-obj
      (call-with-values (lambda () (fast-apply proc (just-objs maybe)))
                        just)))

(: maybe-for-each (procedure maybe-t -> undefined))
(define (maybe-for-each proc maybe)
  (assert-type 'maybe-for-each (procedure? proc))
  (maybe-ref maybe (const #f) proc)
  unspecified)

;; If maybe is a Just, apply kons to its payload values and nil.
;; Otherwise, return nil.
(: maybe-fold (procedure * maybe-t -> *))
(define (maybe-fold kons nil maybe)
  (assert-type 'maybe-fold (procedure? kons))
  (assert-type 'maybe-fold (maybe? maybe))
  (if (nothing? maybe)
      nil
      (let ((objs (just-objs maybe)))
        (if (singleton? objs)
            (kons (car objs) nil)
            (apply kons (append objs (list nil)))))))

;; If the seeds satisfy stop?, return Nothing.  Otherwise, call
;; successor on seeds and apply stop? to the results; if stop? returns
;; true, apply mapper to seeds and return the results wrapped in a Just.
(: maybe-unfold (procedure procedure procedure #!rest -> maybe-t))
(define (maybe-unfold stop? mapper successor . seeds)
  (assert-type 'maybe-unfold (procedure? stop?))
  (assert-type 'maybe-unfold (procedure? mapper))
  (if (singleton? seeds)
      (let ((seed (car seeds)))  ; fast path
        (if (stop? seed)
            nothing-obj
            (begin
             ;; successor might return multiple seeds.
             (assert-type 'maybe-unfold (call-with-values (lambda () (successor seed)) stop?))
             (call-with-values (lambda () (mapper (car seeds))) just))))
      (if (apply stop? seeds)
          nothing-obj
          (begin
           (assert-type 'maybe-unfold (call-with-values (lambda () (apply successor seeds))
                                     stop?))
           (call-with-values (lambda () (apply mapper seeds)) just)))))

;; If either is a Right, apply proc to its payload and wrap the result
;; in a Right.  Otherwise, return either.
(: either-map (procedure either-t -> either-t))
(define (either-map proc either)
  (assert-type 'either-map (procedure? proc))
  (assert-type 'either-map (either? either))
  (if (left? either)
      either
      (call-with-values (lambda () (fast-apply proc (right-objs either)))
                        right)))

(: either-for-each (procedure either-t -> undefined))
(define (either-for-each proc either)
  (assert-type 'either-for-each (procedure? proc))
  (either-ref either (const #f) proc)
  unspecified)

;; If either is a Right, apply kons to its payload values and nil.
;; Otherwise, return nil.
(: either-fold (procedure * either-t -> *))
(define (either-fold kons nil either)
  (assert-type 'either-fold (procedure? kons))
  (assert-type 'either-fold (either? either))
  (if (left? either)
      nil
      (let ((objs (right-objs either)))
        (if (singleton? objs)
            (kons (car objs) nil)
            (apply kons (append objs (list nil)))))))

;; If the seeds satisfy stop?, return a Left of seeds.  Otherwise, call
;; successor on seeds and apply stop? to the results; if stop? returns
;; true, apply mapper to seeds and return the results wrapped in a Right.
(: either-unfold (procedure procedure procedure #!rest -> either-t))
(define (either-unfold stop? mapper successor . seeds)
  (assert-type 'either-unfold (procedure? stop?))
  (assert-type 'either-unfold (procedure? mapper))
  (if (singleton? seeds)
      (let ((seed (car seeds)))  ; fast path
        (if (stop? seed)
            (raw-left seeds)
            (begin
             ;; successor might return multiple values.
             (assert-type 'either-unfold (call-with-values (lambda () (successor seed)) stop?))
             (call-with-values (lambda () (apply mapper seeds)) right))))
      (if (apply stop? seeds)
          (raw-left seeds)
          (begin
           (assert-type 'either-unfold (call-with-values (lambda () (apply successor seeds))
                                     stop?))
           (call-with-values (lambda () (apply mapper seeds)) right)))))

;;;; Syntax

(define-syntax maybe-if
  (syntax-rules ()
    ((_ maybe-expr just-expr nothing-expr)
     (let ((mval maybe-expr))
       (unless (maybe? mval)
         (error "maybe-if: ill-typed value"))
       (if (just? mval) just-expr nothing-expr)))))

;; Return the value of expr if it satisfies pred.
(define-syntax %guard-value
  (syntax-rules ()
    ((_ pred expr)
     (let ((val expr))
       (unless (pred val)
         (error "ill-typed value" pred val))
       val))))

;; Maybe analog of and.  Evaluate the argument expressions in order.
;; If any expression evaluates to Nothing, return it.  Otherwise,
;; return the last Just.
(define-syntax maybe-and
  (syntax-rules ()
    ((_) (just unspecified))
    ((_ maybe-expr) (%guard-value maybe? maybe-expr))
    ((_ maybe-expr maybe-expr* ...)
     (let ((maybe maybe-expr))
       (cond ((just? maybe) (maybe-and maybe-expr* ...))
             ((nothing? maybe) nothing-obj)
             (else
              (error "maybe-and: ill-typed value" maybe? maybe)))))))

;; Maybe analog of or.  Evaluate the argument expressions in order.
;; If any expression evaluates to a Just, return it immediately.
;; Otherwise, return Nothing.
(define-syntax maybe-or
  (syntax-rules ()
    ((_) (nothing))
    ((_ maybe-expr) (%guard-value maybe? maybe-expr))
    ((_ maybe-expr maybe-expr* ...)
     (let ((maybe maybe-expr))
       (cond ((just? maybe) maybe)
             ((nothing? maybe) (maybe-or maybe-expr* ...))
             (else
              (error "maybe-or: ill-typed value" maybe? maybe)))))))

;; Maybe analog of SRFI 2's and-let*.  Each claw evaluates an expression
;; or bound variable to a Maybe, or binds the payload of the value of a
;; Maybe expression to a name in later claws and the body.  If any claw
;; gives a Nothing, the whole expression evaluates to Nothing.
(define-syntax maybe-let*
  (syntax-rules ()
    ((_ () expr1 expr2 ...)
     (call-with-values (lambda () expr1 expr2 ...) just))
    ((_ ((id maybe-expr) . claws) . body)
     (let ((maybe maybe-expr))
       (cond ((and (just? maybe) (singleton? (just-objs maybe)))
              (let ((id (car (just-objs maybe))))
                (maybe-let* claws . body)))
             ((nothing? maybe) nothing-obj)
             (else (error "ill-typed value" maybe? maybe)))))
    ((_ ((maybe-expr) . claws) . body)
     (let ((maybe maybe-expr))
       (cond ((just? maybe) (maybe-let* claws . body))
             ((nothing? maybe) nothing-obj)
             (else (error "ill-typed value" maybe? maybe)))))
    ((_ (id . claws) . body)
     (cond ((just? id) (maybe-let* claws . body))
           ((nothing? id) nothing-obj)
           (else (error "ill-typed value" maybe? id))))
    ((_ . _)
     (syntax-error "ill-formed maybe-let* form"))))

;; Like maybe-let*, but a claw of the form (<formals> <maybe-expr>)
;; binds the payload of the value of <maybe-expr> to <formals> in the
;; manner of let-values.
(define-syntax maybe-let*-values
  (syntax-rules ()
    ((_ () expr1 expr2 ...)
     (call-with-values (lambda () expr1 expr2 ...) just))
    ((_ (((id id* ...) maybe-expr) . claws) . body)
     (maybe-bind (%guard-value maybe? maybe-expr)
                 (lambda (id id* ...)
                   (maybe-let*-values claws . body))))
    ((_ ((ids maybe-expr) . claws) . body)
     (maybe-bind (%guard-value maybe? maybe-expr)
                 (lambda ids
                   (maybe-let*-values claws . body))))
    ((_ ((maybe-expr) . claws) . body)
     (let ((maybe maybe-expr))
       (cond ((just? maybe) (maybe-let*-values claws . body))
             ((nothing? maybe) nothing-obj)
             (else (error "ill-typed value" maybe? maybe)))))
    ((_ (id . claws) . body)
     (cond ((just? id) (maybe-let*-values claws . body))
           ((nothing? id) nothing-obj)
           (else (error "ill-typed value" maybe? id))))
    ((_ . _)
     (syntax-error "ill-formed maybe-let*-values form"))))

;; Either analog of and.  Evaluate the argument expressions in order.
;; If any expression evaluates to a Left, return it immediately.
;; Otherwise, return the last Right.
(define-syntax either-and
  (syntax-rules ()
    ((_) (right unspecified))
    ((_ either-expr) (%guard-value either? either-expr))
    ((_ either-expr either-expr* ...)
     (let ((either either-expr))
       (cond ((right? either) (either-and either-expr* ...))
             ((left? either) either)
             (else (error "ill-typed value" either? either)))))))

;; Either analog of or.  Evaluate the argument expressions in order.
;; If any expression evaluates to a Right, return it immediately.
;; Otherwise, return the last Left.
(define-syntax either-or
  (syntax-rules ()
    ((_) (left unspecified))
    ((_ either-expr) (%guard-value either? either-expr))
    ((_ either-expr either-expr* ...)
     (let ((either either-expr))
       (cond ((right? either) either)
             ((left? either) (either-or either-expr* ...))
             (else (error "ill-typed value" either? either)))))))

;; Either analog of SRFI 2's and-let*.  Each claw evaluates an expression
;; or bound variable to a Maybe, or binds the payload of the value of an
;; Either expression to a name in later claws and the body.  If any claw
;; gives a Left, then the whole expression evaluates to that Left.
(define-syntax either-let*
  (syntax-rules ()
    ((_ () expr1 expr2 ...)
     (call-with-values (lambda () expr1 expr2 ...) right))
    ((_ ((id either-expr) . claws) . body)
     (let ((either either-expr))
       (cond ((and (right? either) (singleton? (right-objs either)))
              (let ((id (car (right-objs either))))
                (either-let* claws . body)))
             ((left? either) either)
             (else (error "ill-typed value" either? either)))))
    ((_ ((either-expr) . claws) . body)
     (let ((either either-expr))
       (cond ((right? either) (either-let* claws . body))
             ((left? either) either)
             (else (error "ill-typed value" either? either)))))
    ((_ (id . claws) . body)
     (cond ((right? id) (either-let* claws . body))
           ((left? id) id)
           (else (error "ill-typed value" either? either))))
    ((_ . _)
     (syntax-error "ill-formed either-let* form"))))

;; Like either-let*, but a claw of the form (<formals> <either-expr>)
;; binds the payload of the value of <either-expr> to <formals> in the
;; manner of let-values.
(define-syntax either-let*-values
  (syntax-rules ()
    ((_ () expr1 expr2 ...)
     (call-with-values (lambda () expr1 expr2 ...) right))
    ((_ (((id id* ...) either-expr) . claws) . body)
     (either-bind (%guard-value either? either-expr)
                  (lambda (id id* ...)
                    (either-let*-values claws . body))))
    ((_ ((ids either-expr) . claws) . body)
     (either-bind (%guard-value either? either-expr)
                  (lambda ids
                    (either-let*-values claws . body))))
    ((_ ((either-expr) . claws) . body)
     (let ((either either-expr))
       (cond ((right? either) (either-let*-values claws . body))
             ((left? either) either)
             (else (error "ill-typed value" either? either)))))
    ((_ (id . claws) . body)
     (cond ((right? id) (either-let*-values claws . body))
           ((left? id) id)
           (else (error "ill-typed value" either? either))))
    ((_ . _)
     (syntax-error "ill-formed either-let*-values form"))))

(define-syntax either-guard
  (syntax-rules ()
    ((_ pred-expr expr1 expr2 ...)
     (exception->either pred-expr (lambda () expr1 expr2 ...)))))

;;;; Trivalent logic

;;; In the following procedures, (just #f) is considered to be false.
;;; All other Just values are taken to be true.

(: just->boolean (maybe-t --> boolean))
(define (just->boolean maybe)
  (not (equal? (just-objs maybe) '(#f))))

(: tri-not (maybe-t --> maybe-t))
(define (tri-not maybe)
  (maybe-bind maybe (lambda (x) (just (not x)))))

;; Returns #t if all arguments are true or all false.  If any argument
;; is Nothing or if any two arguments have different (tri-)truth values,
;; #f is returned.
(: tri=? (maybe-t #!rest maybe-t --> maybe-t))
(define (tri=? maybe . ms)
  (let ((make-pred
         (lambda (b)
           (lambda (m)
             (assert-type 'tri=? (maybe? m))
             (and (just? m) (eqv? (just->boolean m) b))))))
    (if (nothing? maybe)
        (just #f)
        (let ((tri-same? (make-pred (just->boolean maybe))))
          (if (every tri-same? ms) (just #t) (just #f))))))

;; Returns #t if all arguments are true.  If any argument is false or
;; Nothing, return the first such object.
(: tri-and (#!rest maybe-t --> maybe-t))
(define (tri-and . maybes)
  (or (find (lambda (m)
              (assert-type 'tri-and (maybe? m))
              (or (nothing? m) (not (just->boolean m))))
            maybes)
      (just #t)))

;; Returns #f if all arguments are false.  If any argument is true or
;; Nothing, return the first such object.
(: tri-or (#!rest maybe-t --> maybe-t))
(define (tri-or . maybes)
  (or (find (lambda (m)
              (assert-type 'tri-or (maybe? m))
              (or (nothing? m) (just->boolean m)))
            maybes)
      (just #f)))

;; If all arguments are Nothing, then return Nothing.  Otherwise,
;; return the first Just value.
(: tri-merge (#!rest maybe-t --> maybe-t))
(define (tri-merge . maybes)
  (or (find just? maybes) nothing-obj))
