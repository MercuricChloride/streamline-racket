#lang racket

(require "./templating.rkt")

(define puke
  #<<EOF
        \\//////      _______________
        \\    |      /                \
        \(_  x|     /  Bloooeeahhh!!! |
        \    __)  _/__________________/
      __/   _\ ,. .
     /     /  .%:. * .
    |  |   |   . ;>, $  '
    |  |   |    .=3D.~@ .  &
    UUUU---|   :?. ;. .^#  .   ^
    |      |    < * '   .   ,  *
       \     |      .  " .
EOF
  )

(define hearts
  #<<EOF

  ,o8o, ,o8o,
,888888,888888,
888888888888888
888888888888888
`8888888888888'
  `888888888'
    `88888'
      `8' mh




                      ,ae,
                     ,88888e
              ,a888b.9888888i
              888888888888888
              88888888888888Y
              '8888888888888'
                "S888888888"
                  "7888888Y
                     "e88j
                       "Y
EOF
  )

(define pirate
  #<<EOF



         _,-._
        ; ___ :           ,------------------------------.
    ,--' (. .) '--.__    |         Yarrgh Matey,          |
  _;      |||        \   |                                |
 '._,-----''';=.____,"   |  Substreams arrgh so easy now, |
   /// < o>   |##|       | even me parrot can index data! |
   (o        \`--'       |                                |
  ///\ >>>>  _\ <<<<     |                                |
 --._>>>>>>>><<<<<<<<    |   Don't settle for bad tools,  |
 ___() >>>[||||]<<<<     |    Or yee'll walk the plank!   |
 `--'>>>>>>>><<<<<<<     //`-----------------------------'
      >>>>>>><<<<<<     //
        >>>>><<<<<     /
         >>ctr<<

    ----------------
   | Captain Gusrein |
    ----------------
EOF
  )

(define path (make-parameter #f))

(command-line
 #:program "Streamline CLI"
 #:usage-help "Streamline CLI"
 "The best way to build substreams. Made with <3 by Spyglass Labs!\n"
 "This software is in Beta, so please reach out to us if anything breaks and we will help you as quick as we can!\n"
 "Contact links available at spygpc.com! :)"
 ;#:once-each [("-o" "--output")
 ; MY-PARAMETER-NAME
 ; "write a short description of what setting MY-PARAMETER-NAME does"
 ; (my-parameter (string->number MY-PARAMETER-NAME))]
 ;[("-a" "--another-parameter")
 ; ANOTHER-PARAM
 ; "a little description of ANOTHER-PARAM"
 ; (another-parameter (string=? "true" ANOTHER-PARAM))]
 ;[("-s" "--string") A-STRING "what is A-STRING?" (a-string A-STRING)]
 #:args [cmd]
 (match cmd
   ["build"
    (command-line #:program "Streamline Build"
                  #:argv (rest (vector->list (current-command-line-arguments)))
                  #:args (path)
                  (generate-streamline-file path))]
   ["run"
    (command-line #:program "Streamline Run"
                  #:argv (rest (vector->list (current-command-line-arguments)))
                  #:args (path)
                  (generate-streamline-file path))]
   ["wisdom" (pretty-display pirate)]
   ["dune" (pretty-display puke)]
   ["pinax"
    (begin
      (pretty-display hearts)
      (pretty-display "Spyglass Labs <3 Pinax!"))]
   ["streamingfast"
    (begin
      (pretty-display hearts)
      (pretty-display "Spyglass Labs <3 Streamingfast!"))]
   ["the-graph"
    (begin
      (pretty-display hearts)
      (pretty-display "Spyglass Labs <3 The Graph!"))]
   [_ "UNKNOWN COMMAND"]))
