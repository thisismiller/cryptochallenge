(declare (unit bytes)
         (emit-import-library bytes)
         (uses bytes-internal srfi-1))

(module bytes (base16->bytes bytes->base16
               base64->bytes bytes->base64
               string->bytes bytes->string
               bytes-xor)
  (import scheme chicken data-structures srfi-1)

  ; Very sadly, (chicken?) scheme doesn't allow nested modules, so for the sake
  ; of keeping a clean export list and being able to test all of the internal
  ; functions to make sure they work right, they all got moved to a seperate
  ; module. :/
  (import bytes-internal)


  ; With hex, things are pretty convenient since 4 bits + 4 bits = 1 byte.
  ; We could cheat and directly form the byte rather than the
  ; explode->chop->implode chain, but doing this makes it consistent with the
  ; code below, and that's generally a good thing.
  ; And hey, what's more lisp-y than representing data as a list of bytes?
  (define (base16->bytes str)
    (let* ((nibbles (map hex->int (string->list str)))
           (dnibbles (chop nibbles 2)))
      (map (lambda (lst) (implode-number lst 4)) dnibbles)))

  (define (bytes->base16 bytes)
    (let ((nibbles (flatten (map (lambda (x) (explode-number x 8 4)) bytes))))
      (list->string (map int->hex nibbles))))


  ; Now these get a bit more complicated.  We need to split apart the bits in
  ; each byte (explode-number), re-chop them (chop), and then turn them back
  ; into a single number (implode-number) before we map them to the correct
  ; representation (bytes or base64).
  ;
  ; Somewhat graphically, this goes:
  ;  0xAAAA -> 01 01 01 01 01 01 01 01 ->
  ;  (01 01 01) (01 01 01) (01 01 01) -> (base64) VVV
  ; And padding is so annoying -___-;;
  ;
  ; It seems the more common way to do this is to convert chunks of 4 base64
  ; characters into 3 bytes.  But, that's not how I did it.
  (define (base64->bytes str) 
    (let* ((b64i (map base64->int (string->list str)))
           (bits (flatten (map (lambda (x) (explode-number x 6 2)) b64i)))
           (quads (chop bits 4))
           (bytes (map (lambda (lst) (implode-number lst 2)) quads))
           (padding (count (lambda (x) (= x 64)) b64i)))
      (drop-right bytes padding)))

  (define (bytes->base64 bytes)
    (let* ((padding (modulo (- 3 (modulo (length bytes) 3)) 3))
           (padded (append bytes (make-list padding 0)))
           (dbits (flatten (map (lambda (x) (explode-number x 8 2)) padded)))
           (sbits (map (lambda (lst) (implode-number lst 2)) (chop dbits 3))))
      (list->string (append (drop-right (map int->base64 sbits) padding)
                            (make-list padding #\=)))))


  ; And these are just convenient to have
  (define (string->bytes str)
    (map char->integer (string->list str)))

  (define (bytes->string lst)
    (list->string (map integer->char lst)))


  ; Challenge two definitely easy...
  (define (bytes-xor lst1 lst2)
    (map bitwise-xor lst1 lst2))

)
