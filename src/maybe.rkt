# lang typed/racket

(struct None ())
(struct (a) Just ([v : a]))
(define-type (Maybe a) (U None (Just a)))
