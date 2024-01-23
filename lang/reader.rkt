#lang s-exp syntax/module-reader
streamline/lang/expander

#:whole-body-readers?
#t

#:read
$-streamline:read
#:read-syntax
$-streamline:read-syntax

(require (prefix-in $- streamline/lang/expander))
