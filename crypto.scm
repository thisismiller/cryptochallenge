(declare (unit crypto)
         (emit-import-library crypto)
         (uses bytes))

(module crypto (break-char-xor break-repeating-xor
                find-breakable encrypt-xor letter-frequency
                letter-frequency-scorer most-spaces-scorer
                minf maxf zip-longer)  ; These should move to a utils module
  (import scheme chicken data-structures srfi-1 bytes)

  ; From http://en.wikipedia.org/wiki/Letter_frequency
  ; Because who doesn't trust information they find on Wikipedia?
;          '(#\e . 0.12702) '(#\t . 0.09056) '(#\a . 0.08167) '(#\o . 0.07507)
;          '(#\i . 0.06966) '(#\n . 0.06749) '(#\s . 0.06327) '(#\h . 0.06094)
;          '(#\r . 0.05987) '(#\d . 0.04253) '(#\l . 0.04025) '(#\c . 0.02782)
;          '(#\u . 0.02758) '(#\m . 0.02406) '(#\w . 0.02360) '(#\f . 0.02228)
;          '(#\g . 0.02015) '(#\y . 0.01974) '(#\p . 0.01929) '(#\b . 0.01492)
;          '(#\v . 0.00978) '(#\k . 0.00772) '(#\j . 0.00153) '(#\x . 0.00150)
;          '(#\q . 0.00095) '(#\z . 0.00074)
; Is the table from wikipedia.  I've adjusted this table to include the guess
; that about 15% of the characters should be spaces, so I need the letter
; frequencies to sum to 85%.  (Including spaces made a significant improvement
; in english string identification ability.)
  (define letter-frequency
    (list
      '(#\e . 0.107967)  '(#\t . 0.076976)  '(#\a . 0.0694195) '(#\o . 0.0638095)
      '(#\i . 0.059211)  '(#\n . 0.0573665) '(#\s . 0.0537795) '(#\h . 0.051799)
      '(#\r . 0.0508895) '(#\d . 0.0361505) '(#\l . 0.0342125) '(#\c . 0.023647)
      '(#\u . 0.023443)  '(#\m . 0.020451)  '(#\w . 0.02006)   '(#\f . 0.018938)
      '(#\g . 0.0171275) '(#\y . 0.016779)  '(#\p . 0.0163965) '(#\b . 0.012682)
      '(#\v . 0.008313)  '(#\k . 0.006562)  '(#\j . 0.0013005) '(#\x . 0.001275)
      '(#\q . 0.0008075) '(#\z . 0.000629)
      ; About 15% are spaces?  Very rough guess...
      '(#\  . 0.15000)
      ; Here we try to filter out punctuation by preferring results with
      ; mainly alphabetic content.  I can't seem to find any data on the
      ; actual frequency of these characters in real text.  Note that I've
      ; left out some common punctuation characters where 0.0 might not be
      ; a good approximation.
      '(#\~ . 0.00000) '(#\` . 0.00000) '(#\! . 0.00000) '(#\| . 0.00000)
      '(#\@ . 0.00000) '(#\# . 0.00000) '(#\$ . 0.00000) '(#\% . 0.00000)
      '(#\^ . 0.00000) '(#\& . 0.00000) '(#\* . 0.00000) '(#\_ . 0.00000)
      '(#\+ . 0.00000) '(#\\ . 0.00000) '(#\< . 0.00000) '(#\> . 0.00000)
      '(#\{ . 0.00000) '(#\} . 0.00000) '(#\[ . 0.00000) '(#\] . 0.00000)
      '(#\0 . 0.00000) '(#\1 . 0.00000) '(#\2 . 0.00000) '(#\3 . 0.00000)
      '(#\4 . 0.00000) '(#\5 . 0.00000) '(#\6 . 0.00000) '(#\7 . 0.00000)
      '(#\8 . 0.00000) '(#\9 . 0.00000) '(#\0 . 0.00000) '(#\= . 0.00000)))

  ; From http://www.evanmiller.org/how-not-to-sort-by-average-rating.html
  ; We have how to calculate a confidence interval:
  ; def ci_lower_bound(pos, n, confidence)
  ;     if n == 0
  ;         return 0
  ;     end
  ;     z = Statistics2.pnormaldist(1-(1-confidence)/2)
  ;     phat = 1.0*pos/n
  ;     (phat+z*z/(2*n) - 
  ;      z * Math.sqrt( (phat*(1-phat) + z*z/(4*n)) / n )
  ;     ) / (1+z*z/n)
  ; end
  ; This lets us see if, given that we have a limited sample, the known
  ; character frequency could have resulted in the score that we have.

  (define (confidence-interval n total)
    (let* ((z 1.96)
           (n (* 4 n))
           (total (* 4 total))
           (phat (/ n total))
           (left (+ phat (/ (* z z) (* 2 total))))
           (right (* z (sqrt (/ (+ (* phat (- 1 phat))
                                   (/ (* z z) (* 4 total))) total))))
           (divisor (+ 1 (/ (* z z) n))))
      (cons (if (= n 0) 0 (/ (- left right) divisor))
            (/ (+ left right) divisor))))
  ; Translating infix to prefix by hand is surprisingly hard...

  ; We can weight infractions against the range by calculating how far outside
  ; of the acceptable range that they fell.  If we just subtract the two values,
  ; we under-weight distant outliers, since we're on a e^(-x^2) curve.  Thus we
  ; calculate the inverse of the bell curve that we're on, and then subtract our
  ; value and boundary to get errors back on a linear scale.
  (define (inverse-bell mean stddev value)
    (let ((pi 3.1415926535897))
      ; x = mean + sqrt( - stddev^2 * (log(2*pi) - 2*log(1/stddev/y)))
      (+ mean (sqrt (* (- 0 (* stddev stddev))
                       (- (log (* 2 pi) (* 2 (log (/ 1 stddev value))))))))))
  (define (distance-from-range value range)
    (let* ((lbound (car range))
           (rbound (cdr range))
           (mean (/ (+ lbound rbound) 2))
           (stddev (/ (- rbound lbound) 4))) ; 95% CI = 2 stddev from mean
    (cond ((< value (car range)) (- (inverse-bell mean stddev value)
                                    (inverse-bell mean stddev lbound)))
          ((> value (cdr range)) (- (inverse-bell mean stddev value)
                                    (inverse-bell mean stddev rbound)))
          (else 0))))

  ; We score a piece of text by first building a mapping of how many times each
  ; character occurred.  We then build the confidence intervals for these
  ; counts.  We then take each character we have a frequency for, and see if it
  ; falls outside of our confidence interval.  If it does, we record by how
  ; much.  We then sum up these distances as our final score, with lower score
  ; being better.
  ; Note that although we might not have each character in our string,
  ; confidence intervals still allow us to test if that counts as a violation
  ; of the frequency or not. \o/
  (define (letter-frequency-scorer charlst)
    (let* ((alphas (map char-downcase
                        (filter (lambda (x) (assoc x letter-frequency))
                                charlst)))
           (scores (fold (lambda (x acc)
                           (alist-update! x (+ (alist-ref x acc eqv? 0) 1) acc))
                         '()
                         alphas))
           (total (apply + (map cdr scores)))
           (ci-scores (map (lambda (x)
                             (cons (car x)
                                   (confidence-interval (cdr x) total)))
                           scores))
           (marks (map (lambda (x)
                         (distance-from-range
                           (expt (- 1 (cdr x)) total)
                           (alist-ref (car x)
                                      ci-scores
                                      eqv?
                                      '(0 . 0))))
                       letter-frequency)))
      ; I really like how scheme works like this.  (sum)?  No, I'm just going
      ; to apply + to a ton of arguments.
      (apply + marks)))

  ; Here ends the most math I've ever used since college.


  ; It's amazing how good this is as a scorer given that it's 1 line of code.
  ; With the filtering below, it even solves challenge 4...
  (define (most-spaces-scorer charlst)
    ; We need #t = bad, and #f = good, so negate that equivalence.
    (count (lambda (x) (not (eqv? #\  x))) charlst))

  ; One interesting characteristic of strings that have been xor'd with one
  ; repeating byte is that, to remain printable, all bytes must have the same
  ; high bit, since there exists no ASCII character with the highest order bit
  ; set.  This also constrains the keyspace.
  (define (high-bit byte) (bitwise-and byte 128))

  (define (possible-key-len? len bytes)
    (let ((keybits (apply circular-list (map high-bit (take bytes len)))))
      (every (lambda (x y) (= x (high-bit y))) keybits bytes)))

  (define (char-printable? c)
    (let ((i (char->integer c)))
          ; Most printable characters fall in this range.
      (or (and (<= 32 i) (<= i  126))
          ; Except newline and a couple others.  WHY MATASANO.  WHY WOULD YOU
          ; INCLUDE A NEWLINE AT THE END.  LIFE COULD HAVE BEEN SO PERFECT.
          ; I HAD TO TALLY CHARACTER COUNTS TO BREAK IT BY HAND.
          ; ARGGGGGGGGGHHHHHHHHHHHHHHHHHHHHHHHHHH.
          ; And then I went and tweaked the letter frequency scoring algorithm
          ; to weight characters that are drastically incorrect, and now I don't
          ; even need this filter.  So much sadness.  Alas.
          (and (<= 9 i)
               (<= i 13)))))

  ; Is this seriously not in a library somewhere?!
  (define (minf f lst)
    (assert (< 0 (length lst)))
    (cdr (fold (lambda (x acc) (if (> (car acc) (f x))
                                (cons (f x) x)
                                acc))
              (cons (f (car lst)) (car lst))
              (cdr lst))))
  (define (maxf f lst)
    (assert (< 0 (length lst)))
    (cdr (fold (lambda (x acc) (if (< (car acc) (f x))
                                (cons (f x) x)
                                acc))
              (cons (f (car lst)) (car lst))
              (cdr lst))))

  (define (encrypt-xor lst key)
    (bytes-xor lst (apply circular-list key)))

  (define (char-xor-candidates scorer n bytes)
    (let* ((possibilities (iota 256))
           (candidates (map (lambda (x) (encrypt-xor bytes (list x)))
                            possibilities))
           (charlsts (map (lambda (x) (map integer->char x))
                         candidates))
           (filtered (filter (lambda (lst) (every char-printable? lst))
                             charlsts))
           (scored (map (lambda (x) (cons (scorer x) x)) filtered))
           (sorted (sort scored
                         (lambda (x y) (< (car x) (car y))))))
      (take sorted (min n (length sorted)))))

  (define (break-char-xor scorer bytes)
    (car (char-xor-candidates scorer 1 bytes)))

  ; Oh where, oh where did my standard library go
  ; Oh where, oh where could it be?
  ; If only I cared enough
  ; to tail call thee
  ; I forsake efficiency
  (define (zip-longer . lsts)
    (if (pair? lsts)
      (append (list (map car lsts))
              (apply zip-longer (filter pair? (map cdr lsts))))
      '()))

  ; http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan
  (define (count-bits x)
    ; I feel sad that I can't find some standard library thing to remove
    ; the tail call boilerplate from this code.
    ; I found map-accum now, but that's in `traversal`, which feels weird.
    (define (count-bits-tail x acc)
      (if (= x 0)
        acc
        (count-bits-tail
          (bitwise-and x (- x 1))
          (+ acc 1))))
    (count-bits-tail x 0))

  (define (hamming-distance x y)
    (count-bits (bitwise-xor x y)))

  (define (bytes-hamming-distance x y)
    (let ((distances (map hamming-distance x y)))
      (/ (apply + distances)
         (length distances))))

  (define (key-length-score len bytes)
    (let* ((rotated (drop (apply circular-list bytes) len))
           (scored (map hamming-distance bytes rotated))
           (average (/ (apply + scored) (length scored))))
      average))

  (define (key-lengths-of n bytes)
    (let* ((keylens (iota 10 2))
           (distances (map (lambda (len)
                             (cons len
                                   (key-length-score len bytes)))
                           keylens))
           (ordered (sort distances
                          (lambda (x y) (< (cdr x) (cdr y))))))
      (map car (take ordered n))))

  (define (combinations lst . lsts)
    (if (pair? lsts)
      (let ((combs (apply combinations lsts)))
        (apply append
               (map (lambda (x) (map (lambda (y) (cons x y))
                                     combs))
                    lst)))
      (map list lst)))

  (define BLARGDEFAULT (cons 640000 (list #\#)))

  (define (break-repeating-xor-keylen scorer keylen bytes)
    (let* ((pieceslst (chop bytes keylen))
           (pieceskeyd (apply zip-longer pieceslst))
           (candidparts (map (lambda (lst) (char-xor-candidates scorer 2 lst))
                             pieceskeyd))
           (unscored (map (lambda (lst) (map cdr lst)) candidparts))
           (combos (apply combinations unscored))
           (strings (map (lambda (lst) (flatten (apply zip-longer lst)))
                         combos))
           (rescored (map (lambda (x) (cons (scorer x) x)) strings))
;           (_ (map (lambda (x) (display (car x)) (display #\ ) (write (list->string (cdr x))) (newline)) rescored))
           (best (minf car (cons BLARGDEFAULT rescored))))
      best))

  (define (break-repeating-xor scorer bytes)
    ; I started trying to actually just try every length hoping that my letter
    ; frequency scoring was good enough, but there's just too many false
    ; positives, and I'd have to implement using second/third/etc. choices
    (let* ((key-lengths (key-lengths-of 2 bytes))
           (best-choices (map (lambda (x)
                                (break-repeating-xor-keylen scorer x bytes))
                              key-lengths))
           (scorecomb (minf car best-choices)))
      scorecomb))

  (define (find-breakable breaker scorer byteslst)
    (let* ((possibilities (map (lambda (x) (breaker scorer x)) byteslst))
           (best (minf (lambda (x) (if (pair? x) (car x) 640000))
                       possibilities)))
      (list->string (cdr best))))

)
