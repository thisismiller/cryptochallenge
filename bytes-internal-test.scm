(declare (uses bytes-internal))
(import bytes-internal)
(use test)

(test-group "hex->int"
  (test "0" 0 (hex->int #\0))
  (test "5" 5 (hex->int #\5))
  (test "A" 10 (hex->int #\A))
  (test "a" 10 (hex->int #\a))
  (test "F" 15 (hex->int #\f))
)

(test-group "int->hex"
  (test "0" #\0 (int->hex 0))
  (test "5" #\5 (int->hex 5))
  (test "a" #\a (int->hex 10))
  (test "f" #\f (int->hex 15))
)

(test-group "int->base64"
  (test " 0=A" #\A (int->base64 0))
  (test "26=a" #\a (int->base64 26))
  (test "52=0" #\0 (int->base64 52))
  (test "63=/" #\/ (int->base64 63))
)

(test-group "int->base64"
  (test "A=0"  0  (base64->int #\A))
  (test "a=26" 26 (base64->int #\a))
  (test "0=52" 52 (base64->int #\0))
  (test "/=63" 63 (base64->int #\/))
)

(test-group "explode-number"
  (test "255/4" '(3 3 3 3) (explode-number 255 8 2))
  (test "0/4" '(0 0 0 0) (explode-number 0 8 2))
  (test "(2^32-1)/8" '(255 255 255 255)
        (explode-number (- (arithmetic-shift 1 32) 1) 32 8))
)

(test-group "implode-number"
  (test "0101" 5 (implode-number '(1 1) 2))
)
