(defmodule lcli-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

;;; Predicates

(deftest maplist?
  (is-equal 'false (lcli-util:maplist? '()))
  (is-equal 'true (lcli-util:maplist? '(#m())))
  (is-equal 'true (lcli-util:maplist? '(#m(a 1) #m(b 2))))
  (is-equal 'false (lcli-util:maplist? '(#m(a 1) #m(b 2) 3)))
  (is-equal 'false (lcli-util:maplist? '(#())))
  (is-equal 'false (lcli-util:maplist? '(#(a 1))))
  (is-equal 'false (lcli-util:maplist? '(#(a 1) #(b 2)))))

(deftest speclist?
  (is-equal 'false (lcli-util:speclist? '()))
  (is-equal 'true (lcli-util:speclist? '(#())))
  (is-equal 'true (lcli-util:speclist? '(#(a 1))))
  (is-equal 'true (lcli-util:speclist? '(#(a 1) b)))
  (is-equal 'true (lcli-util:speclist? '(#(a b c))))
  (is-equal 'false (lcli-util:speclist? '(#m(a b) #m(c d)))))

(deftest tuplelist?
  (is-equal 'false (lcli-util:tuplelist? '()))
  (is-equal 'true (lcli-util:tuplelist? '(#())))
  (is-equal 'true (lcli-util:tuplelist? '(#(a 1))))
  (is-equal 'false (lcli-util:tuplelist? '(#(a 1) b)))
  (is-equal 'true (lcli-util:tuplelist? '(#(a b c))))
  (is-equal 'false (lcli-util:tuplelist? '(#m(a b) #m(c d)))))

;;; Records to maps

(deftest app-rcrd->map
  (let* ((r (make-app name "my-app"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'app (mref m 'type))
    (is-equal "my-app" (mref m 'name))))

(deftest group-rcrd->map
  (let* ((r (make-group summary "the X group"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'group (mref m 'type))
    (is-equal "the X group" (mref m 'summary))))

(deftest command-rcrd->map
  (let* ((r (make-command title "the X command"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'command (mref m 'type))
    (is-equal "the X command" (mref m 'title))))

(deftest option-rcrd->map
  (let* ((r (make-option long "ex" short #\x help "the X option"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'option (mref m 'type))
    (is-equal 'undefined (mref m 'val-type))
    (is-equal "the X option" (mref m 'help))))

(deftest arg-rcrd->map
  (let* ((r (make-arg name "anarg"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'arg (mref m 'type))
    (is-equal "anarg" (mref m 'name))))

(deftest help-rcrd->map
  (let* ((r (make-help title "ex"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'help (mref m 'type))
    (is-equal "NAME" (mref m 'title-heading))
    (is-equal "ex" (mref m 'title))))

(deftest parsed-rcrd->map
  (let* ((r (make-parsed app "./script.lfe"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'parsed (mref m 'type))
    (is-equal "./script.lfe" (mref m 'app))))

;;; Maps to records

(deftest app-map->record
  (let* ((m #m(type app name "my-app"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'app))
    (is-equal "my-app" (app-name r))))

(deftest group-map->record
  (let* ((m #m(type group summary "the X group"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'group))
    (is-equal "the X group" (group-summary r))))

(deftest command-map->record
  (let* ((m #m(type command title "the X command"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'command))
    (is-equal "the X command" (command-title r))))

(deftest option-map->record
  (let* ((m #m(type option long "ex" short #\x help "the X option"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'option))
    (is-equal "the X option" (option-help r))))

(deftest arg-map->record
  (let* ((m #m(type arg name "anarg"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'arg))
    (is-equal "anarg" (arg-name r))))

(deftest help-map->record
  (let* ((m #m(type help title "ex"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'help))
    (is-equal "NAME" (help-title-heading r))
    (is-equal "ex" (help-title r))))

(deftest parsed-map->record
  (let* ((m #m(type parsed app "./script.lfe"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'parsed))
    (is-equal "./script.lfe" (parsed-app r))))
