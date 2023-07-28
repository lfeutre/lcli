;;;; This module encapsulates all access to the getopt library, converting to
;;;; expected inputs and converting from the library's outputs to the lcli
;;;; internal data format.
(defmodule lcli-getopt
  ;; public funs
  (export
   (->speclist 1)
   (parse 2)
   (usage-cmd-line 2)
   (usage-options 1) (usage-options 2))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

(defun ->speclist (opts)
  (lists:map #'->spec/1 opts))

(defun parse
  ((opts (match-plain-args script s args as))
   (case (getopt:parse (->speclist opts) as)
     (`#(ok #(,opts ,as))
      (make-parsed options opts
                   args as))
     (err
      (log-error "Failed to parse args with given opts: ~p" (list as))
      (log-error err)))))

(defun usage-options (opts)
  (usage-options opts '()))

(defun usage-options (opts args)
  (getopt:usage_options (->speclist opts) args))

(defun usage-cmd-line (cmd opts)
  (getopt:usage_cmd_line cmd (->speclist opts)))

;; Private functions

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
