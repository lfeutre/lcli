(defmodule lcli-rcrd-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

(deftest app-rcrd->map
  (let* ((r (make-app name "my-app"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'app (mref m 'type))
    (is-equal "my-app" (mref m 'name))))

(deftest group-rcrd->map
  (let* ((r (make-group summary "the X group"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'group (mref m 'type))
    (is-equal "the X group" (mref m 'summary))))

(deftest command-rcrd->map
  (let* ((r (make-command title "the X command"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'command (mref m 'type))
    (is-equal "the X command" (mref m 'title))))

(deftest option-rcrd->map
  (let* ((r (make-option long "ex" short #\x help "the X option"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'option (mref m 'type))
    (is-equal 'undefined (mref m 'val-type))
    (is-equal "the X option" (mref m 'help))))

(deftest arg-rcrd->map
  (let* ((r (make-arg name "anarg"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'arg (mref m 'type))
    (is-equal "anarg" (mref m 'name))))

(deftest help-rcrd->map
  (let* ((r (make-help title "ex"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'help (mref m 'type))
    (is-equal "NAME" (mref m 'title-heading))
    (is-equal "ex" (mref m 'title))))

(deftest parsed-rcrd->map
  (let* ((r (make-parsed app "./script.lfe"))
         (m (lcli-rcrd:->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'parsed (mref m 'type))
    (is-equal "./script.lfe" (mref m 'app))))
