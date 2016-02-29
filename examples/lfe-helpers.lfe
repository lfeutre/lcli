#!/usr/bin/env lfe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This script shows the default argument/option handling from Erlang
;;;; that is most useful for LFE scripts as well as the helper variables
;;;; provided by LFE for getting arguments and script name. It shows nothing
;;;; of the functionality provided by lcli.
;;;;
;;;; To run the script, ensure that lfe is in your $PATH, then:
;;;;
;;;;   $ ./examples/lfe-helpers.lfe --greeting "Hello" --greetee "World"
;;;;
(let ((args (init:get_plain_arguments)))
  (lfe_io:format "Args: ~p~n" `(,args))
  (lfe_io:format "Script name: ~p~n" `(,script-name))
  (lfe_io:format "Script args: ~p~n" `(,script-args)))
