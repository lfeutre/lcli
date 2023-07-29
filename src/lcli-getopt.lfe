;;;; This module encapsulates all access to the getopt library, converting to
;;;; expected inputs and converting from the library's outputs to the lcli
;;;; internal data format.
(defmodule lcli-getopt
  ;; public funs
  (export
   (->speclist 1)
   (parse 2)
   (usage-cmd-line 2) (usage-cmd-line 3)
   (usage-options 1) (usage-options 2))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

(defun ->speclist (opts)
  (lists:map #'->spec/1 opts))

(defun parse
  "While this does return a record representing parsed results, the actual
  parsed command line opts (associated with the 'options' field of the record)
  are put into a map -- these are defined by the user, and while we could ask
  users to define records for their results, it's much easier to just use a
  map."
  ((opts (match-plain-args script s args as))
   (case (getopt:parse (->speclist opts) as)
     (`#(ok #(,opts ,as))
      (make-parsed options (parsed-opts->map opts)
                   args as))
     (`#(error ,err)
      (let ((err (io_lib:format "Failed to parse args with given opts, ~p: ~p"
                                (list as err))))
        (make-parsed error (lists:flatten err)))))))

(defun usage-options (opts)
  (usage-options opts '()))

(defun usage-options (opts args)
  (getopt:usage_options (->speclist opts) args))

(defun usage-cmd-line (cmd opts)
  (getopt:usage_cmd_line cmd (->speclist opts)))

(defun usage-cmd-line (cmd opts indent)
  (let ((usage (usage-cmd-line cmd opts)))
    (log-debug "~p" (list usage))
    (clean-cli-usage usage indent)))

;;; Private functions

(defun usage-prefix () "Usage: ")
(defun usage-prefix-len () (+ (length (usage-prefix)) 1))

(defun ->spec
  ((opt) (when (is_map opt))
   (->spec
    (lcli-type:map->record opt)))
  (((match-option name n short s long l help h val-type vt default d))
   `#(,(spec-name n l) ,s ,l ,(arg-spec vt d) ,h)))

(defun arg-spec
  (('undefined 'undefined)
   'undefined)
  ((val-type 'undefined)
   val-type)
  ((val-type default)
  `#(,val-type ,default)))

(defun spec-name
  (("" long)
   (list_to_atom long))
  (('undefined long)
   (list_to_atom long))
  ((name _)
   name))

(defun parsed-opts->map (opts)
  "Convert parsed CLI opts to a map."
  (maps:from_list
   (lists:map #'parsed-opt->tuple/1 opts)))

(defun parsed-opt->tuple
  "Ensure parsed opt is a tuple."
  ((opt) (when (is_tuple opt))
   opt)
  ((opt) (when (is_atom opt))
   `#(,opt true)))

(defun clean-cli-usage (usage indent)
  (let ((`(,head . ,tail) (string:split (lists:flatten usage)
                                        "\n"
                                        'all)))
    (string:join
     (lists:append (list (clean-cli-head head))
                   (clean-cli-tail tail indent))
     "\n")))

(defun clean-cli-head (head)
  (string:substr head (usage-prefix-len)))

(defun clean-cli-tail (tail indent)
  (lists:map
   (lambda (x)
     (++ (lists:duplicate indent " ")
         (string:substr x (usage-prefix-len))))
   tail))
