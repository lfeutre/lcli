(defmodule lcli-parse
  (export
   (app 2)
   (command 2)
   (recordlist 2))
  ;; Just to make xref shut up about an include
  (export
   (--loaded-lcli-records-- 0)))

(include-lib "logjam/include/logjam.hrl")
(include-lib "lcli/include/records.lfe")

(defun app
  (((match-app name n options os commands cs) args)
   ;; XXX There's lots more to do here with commands, etc.
   (let ((result (lcli-getopt:parse os args)))
     ;;     (update-parsed result app-name n)
     result
     )))

(defun app2
  (((match-app name n options os commands cs) args)
   ;; XXX There's lots more to do here with commands, etc.
   (let* (((match-command options cos) cs)
          (app-result (lcli-getopt:parse os args))
          (cmd-result (lcli-getopt:parse cos args)))
     (update-parsed app-result
                    app-name n
                    commands (parsed-options cmd-result)
                    args (parsed-args cmd-result)))))

(defun command (data args)
  (io:format "Command-parsing TBD~n")
  )

(defun recordlist
  ((data (= (match-plain-args script name) args))
   (let ((result (lcli-getopt:parse data args)))
     ;;(update-parsed-app-name result name)
     result)))

;;; Private functions
