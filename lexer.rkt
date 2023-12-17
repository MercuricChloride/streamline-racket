#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(define-lex-abbrevs
  (whitespace? whitespace)
  (keyword? (:or "map" "filter" "reduce" "mfn" "sfn" "fn"))
  (identifier? (:seq alphabetic (:* (:or alphabetic numeric))))
  (num? (:+ numeric))
  ;TODO Fix operator precedence
  (operator? (:or "+" "-" "*" "/" "==" "!=" "<" ">" "<=" ">=" "&&" "||" "!" "|>" "="))

  (punct? (:or (char-set "(){}[];:\"'.,") "=>"))
  ;; literal abbreviations
  (address? (:seq "0x" (:= 40 (:or (:/ #\a #\f) (:/ #\A #\F) numeric))))
   )
  
(define-tokens common-tokens (IDENTIFIER OPERATOR NUMBER))
(define-empty-tokens keyword-tokens (MFN SFN MAP FILTER REDUCE))
(define-empty-tokens operator-tokens (PLUS MINUS MUL DIV EQ NOT-EQ LT GT LTE GTE AND OR NOT PIPE ASSIGNMENT))
(define-empty-tokens punct-tokens (LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET SEMI DOUBLE-QUOTE SINGLE-QUOTE COLON FAT-ARROW DOT COMMA))

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
    ["," token-COMMA]))

(define (string->operator str)
  (match str
    ["+" token-PLUS]
    ["-" token-MINUS]
    ["*" token-MUL]
    ["/" token-DIV]
    ["==" token-EQ]
    ["!=" token-NOT-EQ]
    ["<" token-LT]
    [">" token-GT]
    ["<=" token-LTE]
    [">=" token-GTE]
    ["&&" token-AND]
    ["||" token-OR]
    ["!" token-NOT]
    ["|>" token-PIPE]
    ["=" token-ASSIGNMENT]
    ))

(define (string->keyword-token str)
  (match str
    ["mfn" token-MFN]
    ["sfn" token-SFN]
    ["map" token-MAP]
    ["filter" token-FILTER]
    ["reduce" token-REDUCE]))

(define streamline-lexer
  (lexer-src-pos
   [(eof) (return-without-pos 'EOF)]
   [whitespace? (return-without-pos (streamline-lexer input-port))] ;remove whitespace
   [keyword? ((string->keyword-token lexeme))]
   [identifier? (token-IDENTIFIER lexeme)]
   [operator? (token-OPERATOR ((string->operator lexeme)))]
   [address? (token-NUMBER lexeme)]
   [num? (token-NUMBER lexeme)]
   [punct? ((string->punct lexeme))]))

(define (tokenize port)
  (let ([token (streamline-lexer port)])
    (unless (equal? 'EOF token)
        (cons token (tokenize port)))))

(tokenize (open-input-string "

mfn burns = erc721Transfers
    |> filter (transfer) => transfer.address == asdfasdfasdf;
    |> map (transfer) => { burner: transfer.from, token: transfer.tokenId };


"))

(provide tokenize)
