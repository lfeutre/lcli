#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script simply shows the default argument/option handling from
;;;; Erlang that is most useful for LFE scripts.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/init-only.lfe --greeting "Hello" --greetee "World"
;;;;
(let ((args (init:get_plain_arguments)))
  (lfe_io:format "Args: ~p~n" `(,args)))
