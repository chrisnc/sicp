(require "dd-common.rkt")
(require "dd-coerce.rkt")

(put 'add '(a b c) +)

(put-coercion 'a 'c (lambda (x) x))
(put-coercion 'c 'a (lambda (x) x))
(put-coercion 'a 'b (lambda (x) x))
(put-coercion 'b 'c (lambda (x) x))
