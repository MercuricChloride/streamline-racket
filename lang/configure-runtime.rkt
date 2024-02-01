#lang racket

(require (prefix-in parser: streamline/lang/parser)
         streamline/lang/classes
         syntax/strip-context
         megaparsack
         readline/readline)

(provide configure-runtime!
         streamline:read-interaction)

; In DrRacket, multi-line input is completely possible, so #<eof>s are inserted between each
; interaction within the port. Therefore, we should just lex/parse the whole thing. We need
; to actually return #<eof> in order for the REPL to advance to the next prompt, though (for
; whatever reason), so we’ll also pass lone #<eof>s through here.
(define (drracket-input? in)
  (and (char-ready? in) (not (eof-object? (peek-char in)))))
(define (handle-drracket-input in)
  (streamline:read-interaction in))

; At the terminal, input is delimited by newlines. Therefore, we should read a line at a time
; before handing things off to the lexer and parser. If we ever get #<eof>, we should pass it
; through. That way, the user can exit the REPL by sending ^D.
(define (terminal-input? in)
  (terminal-port? in))
(define (handle-terminal-input in)
  (let ([line (read-line in 'any)]) (if (eof-object? line) eof (streamline:read-interaction in))))

(define (readline-input? in)
  (eq? (object-name in) 'readline-input))
; The REPL works pretty differently when used at the terminal and when used from within DrRacket.
; Therefore, it’s necessary to branch on the result of terminal-port? so we can check what
; behavior to expect.
; Additionally, when readline is loaded, it installs itself in place of the terminal port with
; the name 'readline-input. Therefore, we should handle that case the same way.
(define (read-multiline-input prompt continuation-prompt)
  (define (read-more lines)
    (let* ([line (read-line)]
           [new-lines (append lines (list line))]
           [input (string-join new-lines "\n")])
      (when (eof-object? line)
        eof)
      (if (string=? "" line)
          (streamline:read-interaction (open-input-string input))
          (with-handlers ([exn:fail:read:megaparsack? (lambda (exn)
                                                        (begin
                                                          (display continuation-prompt)
                                                          (read-more new-lines)))])
            (streamline:read-interaction (open-input-string input))))))
  (read-more '()))

(define (configure-runtime!)
  (current-read-interaction (λ (_src in)
                              (read-multiline-input ">    " "..    ")
                              ;; (print in)
                              ;; (cond
                              ;;   [(readline-input? in) (handle-readline-input in)]
                              ;;   [(drracket-input? in) (handle-drracket-input in)]
                              ;;   [(terminal-input? in) (handle-terminal-input in)]
                              ;;   [else eof])
                              )))

(define (streamline:read-interaction input)
  (define interaction (parser:parse-streamline-interaction! input))
  (with-syntax ([(node ...) (map transform-interaction (syntax->list interaction))])
    (strip-context #'(begin
                       node ...))))
