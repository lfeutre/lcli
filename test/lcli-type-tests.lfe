(defmodule lcli-type-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "lcli/include/records.lfe")

;;; Predicates

(deftest maplist?
  (is-equal 'false (lcli-type:maplist? '()))
  (is-equal 'true (lcli-type:maplist? '(#m())))
  (is-equal 'true (lcli-type:maplist? '(#m(a 1) #m(b 2))))
  (is-equal 'false (lcli-type:maplist? '(#m(a 1) #m(b 2) 3)))
  (is-equal 'false (lcli-type:maplist? '(#())))
  (is-equal 'false (lcli-type:maplist? '(#(a 1))))
  (is-equal 'false (lcli-type:maplist? '(#(a 1) #(b 2)))))

(deftest speclist?
  (is-equal 'false (lcli-type:speclist? '()))
  (is-equal 'true (lcli-type:speclist? '(#())))
  (is-equal 'true (lcli-type:speclist? '(#(a 1))))
  (is-equal 'true (lcli-type:speclist? '(#(a 1) b)))
  (is-equal 'true (lcli-type:speclist? '(#(a b c))))
  (is-equal 'false (lcli-type:speclist? '(#m(a b) #m(c d)))))

(deftest tuplelist?
  (is-equal 'false (lcli-type:tuplelist? '()))
  (is-equal 'true (lcli-type:tuplelist? '(#())))
  (is-equal 'true (lcli-type:tuplelist? '(#(a 1))))
  (is-equal 'false (lcli-type:tuplelist? '(#(a 1) b)))
  (is-equal 'true (lcli-type:tuplelist? '(#(a b c))))
  (is-equal 'false (lcli-type:tuplelist? '(#m(a b) #m(c d)))))

;;; Records to maps

(deftest app-record->map
  (let* ((r (make-app name "my-app"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'app (mref m 'type))
    (is-equal "my-app" (mref m 'name))))

(deftest group-record->map
  (let* ((r (make-group summary "the X group"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'group (mref m 'type))
    (is-equal "the X group" (mref m 'summary))))

(deftest command-record->map
  (let* ((r (make-command title "the X command"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'command (mref m 'type))
    (is-equal "the X command" (mref m 'title))))

(deftest option-record->map
  (let* ((r (make-option long "ex" short #\x help "the X option"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'option (mref m 'type))
    (is-equal 'undefined (mref m 'val-type))
    (is-equal "the X option" (mref m 'help))))

(deftest arg-record->map
  (let* ((r (make-arg name "anarg"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'arg (mref m 'type))
    (is-equal "anarg" (mref m 'name))))

(deftest help-record->map
  (let* ((r (make-help title "ex"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'help (mref m 'type))
    (is-equal "NAME" (mref m 'title-heading))
    (is-equal "ex" (mref m 'title))))

(deftest parsed-record->map
  (let* ((r (make-parsed script "./script.lfe"))
         (m (lcli-type:record->map r)))    
    (is-equal 'true (is_map m))
    (is-equal 'parsed (mref m 'type))
    (is-equal "./script.lfe" (mref m 'script))))

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
  (let* ((m #m(type parsed script "./script.lfe"))
         (r (lcli-type:map->record m)))
    (is-equal 'true (is_record r 'parsed))
    (is-equal "./script.lfe" (parsed-script r))))
