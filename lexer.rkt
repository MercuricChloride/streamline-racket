#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
 (whitespace? whitespace)
 (keyword? (:or "map" "filter" "reduce" "mfn" "sfn" "fn" "true" "false" "import"))
 (identifier? (:seq (:or alphabetic (char-set "$_")) (:* (:or alphabetic numeric (char-set "$_")))))
 ;TODO Fix operator precedence
 (operator? (:or "+" "-" "*" "/" "==" "!=" "<" ">" "<=" ">=" "&&" "||" "!" "|>" "="))
 (punct? (:or (char-set "(){}[];:\"'.,#") "=>"))
 ;; literal abbreviations
 (address? (:seq "0x" (:= 40 (:or (:/ #\a #\f) (:/ #\A #\F) numeric))))
 (num? (:+ numeric))
 (str? (:or (:seq "\"" (:* (:~ "\"")) "\"") (:seq "'" (:* (:~ "'")) "'")))
 (boolean? (:or "true" "false")))

(define-tokens common-tokens (IDENTIFIER OPERATOR NUMBER ADDRESS STRING))
(define-empty-tokens keyword-tokens (MFN SFN MAP FILTER REDUCE TRUE FALSE SOURCE))
(define-tokens operator-tokens
               (PLUS MINUS MUL DIV EQ NOT-EQ LT GT LTE GTE AND OR NOT PIPE ASSIGNMENT))
(define-empty-tokens punct-tokens
                     (LPAREN RPAREN
                             LCURLY
                             RCURLY
                             LBRACKET
                             RBRACKET
                             SEMI
                             DOUBLE-QUOTE
                             SINGLE-QUOTE
                             COLON
                             FAT-ARROW
                             DOT
                             HASH
                             COMMA))

(define (string->punct str)
  (match str
    ["(" token-LPAREN]
    [")" token-RPAREN]
    ["{" token-LCURLY]
    ["}" token-RCURLY]
    ["[" token-LBRACKET]
    ["]" token-RBRACKET]
    [";" token-SEMI]
    [":" token-COLON]
    ["'" token-SINGLE-QUOTE]
    ["\"" token-DOUBLE-QUOTE]
    ["=>" token-FAT-ARROW]
    ["." token-DOT]
    ["," token-COMMA]
    ["#" token-HASH]))

(define (string->operator str)
  (match str
    ["+" (token-PLUS str)]
    ["-" (token-MINUS str)]
    ["*" (token-MUL str)]
    ["/" (token-DIV str)]
    ["==" (token-EQ str)]
    ["!=" (token-NOT-EQ str)]
    ["<" (token-LT str)]
    [">" (token-GT str)]
    ["<=" (token-LTE str)]
    [">=" (token-GTE str)]
    ["&&" (token-AND str)]
    ["||" (token-OR str)]
    ["!" (token-NOT str)]
    ["|>" (token-PIPE str)]
    ["=" (token-ASSIGNMENT str)]))

(define (string->keyword-token str)
  (match str
    ["mfn" token-MFN]
    ["sfn" token-SFN]
    ["map" token-MAP]
    ["filter" token-FILTER]
    ["reduce" token-REDUCE]
    ["true" token-TRUE]
    ["false" token-FALSE]
    ["import" token-SOURCE]))

(define (string->string-token str)
  (let ([replacement (if (string-prefix? str "\"") "\"" "'")]) (string-replace str replacement "")))

(define streamline-lexer
  (lexer-src-pos [(eof) (return-without-pos 'EOF)]
                 [whitespace? (return-without-pos (streamline-lexer input-port))] ;remove whitespace
                 [keyword? ((string->keyword-token lexeme))]
                 [identifier? (token-IDENTIFIER lexeme)]
                 [operator? (string->operator lexeme)]
                 ;; literals
                 [address? (token-ADDRESS lexeme)]
                 [num? (token-NUMBER lexeme)]
                 [str? (token-STRING (string->string-token lexeme))]
                 ;; punctuation
                 [punct? ((string->punct lexeme))]))

(define (tokenize port)
  (port-count-lines! port)
  (let ([token (streamline-lexer port)]) (if (equal? 'EOF token) '() (cons token (tokenize port)))))

;tokenized-input
(provide tokenize)
