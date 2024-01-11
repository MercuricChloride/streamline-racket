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

(define skull
  #<<EOF
                       uuuuuuuuuuuuuuuuuuuuu.
                   .u$$$$$$$$$$$$$$$$$$$$$$$$$$W.
                 u$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$Wu.
               $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$i
              $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         `    $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
           .i$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$i
           $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$W
          .$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$W
         .$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$i
         #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$.
         W$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$u       #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$~
$#      `"$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$i        $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$        #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
$$         $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$.        $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#
 $$      $iW$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$!
 $$i      $$$$$$$#"" `"""#$$$$$$$$$$$$$$$$$#""""""#$$$$$$$$$$$$$$$W
 #$$W    `$$$#"            "       !$$$$$`           `"#$$$$$$$$$$#
  $$$     ``                 ! !iuW$$$$$                 #$$$$$$$#
  #$$    $u                  $   $$$$$$$                  $$$$$$$~
   "#    #$$i.               #   $$$$$$$.                 `$$$$$$
          $$$$$i.                """#$$$$i.               .$$$$#
          $$$$$$$$!         .   `    $$$$$$$$$i           $$$$$
          `$$$$$  $iWW   .uW`        #$$$$$$$$$W.       .$$$$$$#
            "#$$$$$$$$$$$$#`          $$$$$$$$$$$iWiuuuW$$$$$$$$W
               !#""    ""             `$$$$$$$##$$$$$$$$$$$$$$$$
          i$$$$    .                   !$$$$$$ .$$$$$$$$$$$$$$$#
         $$$$$$$$$$`                    $$$$$$$$$Wi$$$$$$#"#$$`
         #$$$$$$$$$W.                   $$$$$$$$$$$#   ``
          `$$$$##$$$$!       i$u.  $. .i$$$$$$$$$#""
             "     `#W       $$$$$$$$$$$$$$$$$$$`      u$#
                            W$$$$$$$$$$$$$$$$$$      $$$$W
                            $$`!$$$##$$$$``$$$$      $$$$!
                           i$" $$$$  $$#"`  """     W$$$$
                                                   W$$$$!
                      uW$$  uu  uu.  $$$  $$$Wu#   $$$$$$
                     ~$$$$iu$$iu$$$uW$$! $$$$$$i .W$$$$$$
             ..  !   "#$$$$$$$$$$##$$$$$$$$$$$$$$$$$$$$#"
             $$W  $     "#$$$$$$$iW$$$$$$$$$$$$$$$$$$$$$W
             $#`   `       ""#$$$$$$$$$$$$$$$$$$$$$$$$$$$
                              !$$$$$$$$$$$$$$$$$$$$$#`
                              $$$$$$$$$$$$$$$$$$$$$$!
                            $$$$$$$$$$$$$$$$$$$$$$$`
                             $$$$$$$$$$$$$$$$$$$$"

   _____ _______ _____  ______          __  __ _      _____ _   _ ______
  / ____|__   __|  __ \|  ____|   /\   |  \/  | |    |_   _| \ | |  ____|
 | (___    | |  | |__) | |__     /  \  | \  / | |      | | |  \| | |__
  \___ \   | |  |  _  /|  __|   / /\ \ | |\/| | |      | | | . ` |  __|
  ____) |  | |  | | \ \| |____ / ____ \| |  | | |____ _| |_| |\  | |____
 |_____/ _ |_|_ |_|__\_\______/_/__  \_\_| _|_|______|_____|_| \_|______|__     __
  / ____| |  | |/ ____/ ____|  ____|/ ____/ ____|  ____| |  | | |    | | \ \   / /
 | (___ | |  | | |   | |    | |__  | (___| (___ | |__  | |  | | |    | |  \ \_/ /
  \___ \| |  | | |   | |    |  __|  \___ \\___ \|  __| | |  | | |    | |   \   /
  ____) | |__| | |___| |____| |____ ____) |___) | |    | |__| | |____| |____| |
 |_____/_\____/_\_____\_____|______|_____/_____/|_|____ \____/|______|______|_|
 |_   _| \ | |/ ____|__   __|/\   | |    | |    |  ____|  __ \
   | | |  \| | (___    | |  /  \  | |    | |    | |__  | |  | |
   | | | . ` |\___ \   | | / /\ \ | |    | |    |  __| | |  | |
  _| |_| |\  |____) |  | |/ ____ \| |____| |____| |____| |__| |
 |_____|_| \_|_____/   |_/_/    \_\______|______|______|_____/


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

(define (check-dependency-met command test-command)
  (if (not (= 0 (system/exit-code test-command)))
      (error
       (format
        "\n\nERROR WHILE INSTALLING STREAMLINE!!!\n\n~a not found! Please make sure ~a is installed and on your $PATH!\n\n"
        command
        command))
      (pretty-display (format "~a is already installed!" command))))

(define (streamline-install)
  ; Check depencendies are met on system
  (check-dependency-met "git" "git --version")
  (check-dependency-met "rust" "cargo --version")
  (check-dependency-met "substreams" "substreams --version")
  (check-dependency-met "protobuf / protoc" "protoc --version")

  ; Check if $HOME./streamline/ exists
  ; And clone the repo
  (if (directory-exists? streamline-path)
      (pretty-display "~/.streamline/ already exists")
      (begin
        (pretty-display "Creating ~/.streamline/")
        (check-dependency-met
         "Cloned Repo"
         (format "git clone https://github.com/MercuricChloride/streamline-template-repository ~a"
                 streamline-path))
        (pretty-display "Created ~/.streamline/!")))
  (pretty-display skull))

(define (streamline-run mod-name)
  (define cmd (format "cd ~a && substreams run ~a" streamline-path mod-name))
  (if (directory-exists? streamline-path) (system cmd) (error "Streamline path not found!")))

(define (streamline-gui mod-name)
  (define cmd (format "cd ~a && substreams gui ~a" streamline-path mod-name))
  (if (directory-exists? streamline-path) (system cmd) (error "Streamline path not found!")))

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
 #:args [cmd . remaining]
 (match cmd
   ["build"
    (command-line #:program "Streamline Build"
                  #:argv remaining
                  #:args (path)
                  (generate-streamline-file path))]
   ["run"
    (command-line #:program "Streamline Run"
                  #:argv remaining
                  #:args (mod-name)
                  (streamline-run mod-name))]
   ["gui"
    (command-line #:program "Streamline gui"
                  #:argv remaining
                  #:args (mod-name)
                  (streamline-gui mod-name))]
   ["install" (streamline-install)]
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
   [_ (format "UNKNOWN COMMAND ~a" cmd)]))
