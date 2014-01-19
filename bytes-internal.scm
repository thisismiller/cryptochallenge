(declare (unit bytes-internal)
         (emit-import-library bytes-internal)
         (uses srfi-1))


(module bytes-internal (hex->int int->hex
                        base64->int int->base64
                        explode-number implode-number)
  (import scheme chicken srfi-1)

  (define (hex->int h)
    (cond ((char-alphabetic? h) (+ 10 (- (char->integer (char-upcase h))
                                         (char->integer #\A))))
          ((char-numeric? h) (- (char->integer h) (char->integer #\0)))
          (else (assert #f))))

  (define (int->hex n)
    (cond ((and (<= 0 n) (< n 10)) (integer->char (+ n (char->integer #\0))))
          ((and (<= 10 n) (< n 16)) (integer->char (+ (- n 10)
                                                      (char->integer #\a))))
          (else (assert #f))))

  ; It unnerves me to no end that 0-15 of base64 do not match 0-15 of hex.
  (define (base64->int c)
    (cond ((char-upper-case? c) (- (char->integer c)
                                   (char->integer #\A)))
          ((char-lower-case? c) (+ 26 (- (char->integer c)
                                         (char->integer #\a))))
          ((char-numeric? c) (+ 52 (- (char->integer c)
                                      (char->integer #\0))))
          ((equal? c #\+) 62)
          ((equal? c #\/) 63)
          (else (assert #f))))

  (define (int->base64 n)
    (cond ((and (<=  0 n) (< n 26)) (integer->char (+ n (char->integer #\A))))
          ((and (<= 26 n) (< n 52)) (integer->char (+ (- n 26)
                                                      (char->integer #\a))))
          ((and (<= 52 n) (< n 62)) (integer->char (+ (- n 52)
                                                      (char->integer #\0))))
          ((= n 62) #\+)
          ((= n 63) #\/)
          (else (assert #f))))

  ; Think `chop` for the bits of a number
  (define (explode-number n width bits)
    (assert (= 0 (modulo width bits)))
    (let ((m (arithmetic-shift 1 bits)))
      (unfold-right (lambda (x) (= (cdr x) 0))
                    (lambda (x) (modulo (car x) m))
                    (lambda (x) (cons (arithmetic-shift (car x) (- 0 bits))
                                      (- (cdr x) bits)))
                    (cons n width))))

  (define (implode-number lst width)
    (fold (lambda (x acc) (bitwise-ior (arithmetic-shift acc width) x)) 0 lst))

)
