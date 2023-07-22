(defmodule lcli-record-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

(deftest app-record
  (let ((r (make-app name "git")))
    (is-equal "git"
              (app-name r))))

(deftest group-record
  (let ((r (make-group name "start a working area")))
    (is-equal "start a working area"
              (group-name r))))

(deftest command-record
  (let ((r (make-command name "clone")))
    (is-equal "clone"
              (command-name r))))

(deftest option-record
  (let ((r (make-option name "origin"
                        short #\o
                        help "Use this instead of using the remote name")))
    (is-equal "origin"
              (option-name r))
    (is-equal #\o
              (option-short r))
    (is-equal "Use this instead of using the remote name"
              (option-help r))))

(deftest help-record
  (let ((r (make-help name "git-clone - Clone a repository into a new directory")))
    (is-equal "git-clone - Clone a repository into a new directory"
              (help-name r))))
