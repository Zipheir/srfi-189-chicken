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

   (syntax: maybe-and unspecified)
   (syntax: maybe-or unspecified)
   (syntax: maybe-let* singleton? just-objs nothing-obj)
   (syntax: either-and unspecified)
   either-or
   (syntax: either-let* singleton? right-objs)
   maybe-let*-values either-let*-values
   maybe-if)

  (import (only (r7rs) guard eof-object))

  (import (scheme)
          (chicken base)
          (chicken syntax)
          (only (srfi 1) list-copy find every list=)
          (srfi 145))

  (include "189.scm"))
