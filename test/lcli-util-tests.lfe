(defmodule lcli-util-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest maplist
  (is-equal 'false (lcli-util:maplist? '()))
  (is-equal 'true (lcli-util:maplist? '(#m())))
  (is-equal 'true (lcli-util:maplist? '(#m(a 1) #m(b 2))))
  (is-equal 'false (lcli-util:maplist? '(#m(a 1) #m(b 2) 3)))
  (is-equal 'false (lcli-util:maplist? '(#())))
  (is-equal 'false (lcli-util:maplist? '(#(a 1))))
  (is-equal 'false (lcli-util:maplist? '(#(a 1) #(b 2)))))

(deftest speclist
  (is-equal 'false (lcli-util:speclist? '()))
  (is-equal 'true (lcli-util:speclist? '(#())))
  (is-equal 'true (lcli-util:speclist? '(#(a 1))))
  (is-equal 'true (lcli-util:speclist? '(#(a 1) b)))
  (is-equal 'true (lcli-util:speclist? '(#(a b c))))
  (is-equal 'false (lcli-util:speclist? '(#m(a b) #m(c d)))))

(deftest tuplelist
  (is-equal 'false (lcli-util:tuplelist? '()))
  (is-equal 'true (lcli-util:tuplelist? '(#())))
  (is-equal 'true (lcli-util:tuplelist? '(#(a 1))))
  (is-equal 'false (lcli-util:tuplelist? '(#(a 1) b)))
  (is-equal 'true (lcli-util:tuplelist? '(#(a b c))))
  (is-equal 'false (lcli-util:tuplelist? '(#m(a b) #m(c d)))))
