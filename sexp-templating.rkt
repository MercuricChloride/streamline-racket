#lang racket

(require web-server/templates)

(define-syntax-rule (call path args) (let ([rpc (rpc-call path args)]) (rpc-call/t rpc)))

(struct rpc-call (path args))

(define rpc-call/test-input (rpc-call (list 'ERC721 'ownerOf) (list (cons 'uint 'tokenId))))

(define (rpc-call/t input)
  (match-let ([(rpc-call path args) input])
    (include-template "templates/rpc_call.rs")))

(call `(ERC721 ownerOf) `((uint tokenId)))

(struct mfn-def (name inputs module-body))

(define mfn/test-input (mfn-def 'map_epic_events `(map_events) "todo!();"))

(define (mfn/t input)
  (match-let* ([(mfn-def name inputs module-body) input]
               [inputs (string-join (map (lambda (part) (format "~a: prost_wkt_types::Struct" part))
                                         inputs)
                                    ",")])
    (include-template "templates/mfn.rs")))

(define-syntax-rule (mfn name inputs module-body)
  (let ([mfn-def (mfn-def name inputs module-body)]) (mfn/t mfn-def)))

(mfn 'submitted_bids '(map_events) "something!")
