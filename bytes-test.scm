(declare (uses bytes))
(use test)

(import bytes)

(test-group "base16->bytes"
  (test "01" (list 1) (base16->bytes "01"))
  (test "01af" (list 1 175) (base16->bytes "01af"))
)

(test-group "bytes->base16"
  (test "01" "01" (bytes->base16 '(1)))
  (test "01af" "01af" (bytes->base16 '(1 175)))
)

(test-group "base64->bytes"
  (test "AAAA" '(0 0 0) (base64->bytes "AAAA"))
  (test "BBBB" '(4 16 65) (base64->bytes "BBBB"))
)

(test-group "bytes->base64"
  (test "0 0 0" "AAAA" (bytes->base64 '(0 0 0)))
  (test "4 16 65" "BBBB" (bytes->base64 '(4 16 65)))
)

(test-group "base16->bytes->base16"
  (test "83f7de9c01d8" "83f7de9c01d8"
        (bytes->base16 (base16->bytes "83F7DE9C01D8")))
)

(test-group "base64->bytes->base64"
  (test "sJeyVz37+/Js2fAW" "sJeyVz37+/Js2fAW"
        (bytes->base64 (base64->bytes "sJeyVz37+/Js2fAW")))
)
