(defmodule lcli-util
  (export all))

(defun get-version ()
  "Get just the version of lcli."
  (lutil:get-app-version 'lcli))

(defun get-versions ()
  "Get the version of lcli and other major components."
  (++ (lutil:get-versions)
      `(#(lcli ,(get-version)))))
