#lang racket
(require "./macros.rkt")

(provide derive-sol-traits
         write-string-to-file
         read-file)

;; A template of the usage of the sol-macro
(def-template (sol-macro solidity) "
sol! {
  {{solidity}}
}
")

;; Derives the needed traits on the solidity types, for use in the sol-macro in rust
;; Returns the usage of the rust macro
(define/contract (derive-sol-traits source)
  (-> string? string?)
  (define derive "#[derive(Serialize, Deserialize, Debug)]")
  (define solidity
    (thread-replace source "contract" derive "interface" derive "struct" derive "enum" derive))
  (sol-macro solidity))

(define (write-string-to-file string filename)
  (with-output-to-file filename (lambda () (pretty-display string)) #:exists 'replace))

(define (read-file path)
  (port->string (open-input-file path)))
