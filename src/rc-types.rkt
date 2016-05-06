#lang rackrt
(provide-all-out)

(struct rc/struct   (id vars cdtors funcs))
(struct rc/ctor     (id ret stmts))
(struct rc/dtor     (id stmts))
(struct rc/func     (id ret ref params stmts))
(struct rc/func-sig (id ret params))
(struct rc/var      (id type array))
