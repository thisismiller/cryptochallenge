(declare (uses bytes))
(import bytes)
(use test)

(test "Challenge 1 - hex to base64" "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" (bytes->base64 (base16->bytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")))
(test "Challenge 1 - base64 to hex" "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" (bytes->base16 (base64->bytes "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")))
(test "Challenge 2 - Fixed XOR" "746865206b696420646f6e277420706c6179" (bytes->base16 (bytes-xor (base16->bytes "1c0111001f010100061a024b53535009181c") (base16->bytes "686974207468652062756c6c277320657965"))))
