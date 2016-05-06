#lang racket

(provide
 (prefix-out
  rc/
  (combine-out
   (except-out (all-defined-out)
               struct-lit)
   (rename-out [struct struct-lit]))))

;; TODO: add contracts/types

(struct struct-lit   (id vars cdtors funcs))
(struct ctor     (id ret stmts))
(struct dtor     (id stmts))
(struct func     (id ret ref params stmts))
(struct extern-func (id ret params))
(struct extern-var (id type array))


;; provide function is-initialized?
(struct var (id static type array value))
(struct val (id static type value))

(struct base-t ())
(struct struct-t ())
;(struct rc/var      (id static type array))
