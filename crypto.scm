(declare (unit crypto)
         (emit-import-library crypto)
         (uses bytes))

(module crypto (break-char-xor find-breakable-xor
                encrypt-xor
                letter-frequency-scorer most-spaces-scorer
                minf)
  (import scheme chicken data-structures srfi-1 bytes)

  ; From http://en.wikipedia.org/wiki/Letter_frequency
  ; Because who doesn't trust information they find on Wikipedia?
  (define letter-frequency
    (list '(#\e . 0.12702) '(#\t . 0.09056) '(#\a . 0.08167) '(#\o . 0.07507)
          '(#\i . 0.06966) '(#\n . 0.06749) '(#\s . 0.06327) '(#\h . 0.06094)
          '(#\r . 0.05987) '(#\d . 0.04253) '(#\l . 0.04025) '(#\c . 0.02782)
          '(#\u . 0.02758) '(#\m . 0.02406) '(#\w . 0.02360) '(#\f . 0.02228)
          '(#\g . 0.02015) '(#\y . 0.01974) '(#\p . 0.01929) '(#\b . 0.01492)
          '(#\v . 0.00978) '(#\k . 0.00772) '(#\j . 0.00153) '(#\x . 0.00150)
          '(#\q . 0.00095) '(#\z . 0.00074) '(#\  . 0.15000)))
          ; Here we try to filter out punctuation by preferring results with
          ; mainly alphabetic content.  I can't seem to find any data on the
          ; actual frequency of these characters in real text.  Note that I've
          ; left out some common punctuation characters where 0.0 might not be
          ; a good approximation.
          ;'(#\~ . 0.00000) '(#\` . 0.00000) '(#\! . 0.00000) '(#\| . 0.00000)
          ;'(#\@ . 0.00000) '(#\# . 0.00000) '(#\$ . 0.00000) '(#\% . 0.00000)
          ;'(#\^ . 0.00000) '(#\& . 0.00000) '(#\* . 0.00000) '(#\_ . 0.00000)
          ;'(#\+ . 0.00000) '(#\\ . 0.00000) '(#\< . 0.00000) '(#\> . 0.00000)
          ;'(#\{ . 0.00000) '(#\} . 0.00000) '(#\[ . 0.00000) '(#\] . 0.00000)
          ;'(#\0 . 0.00000) '(#\1 . 0.00000) '(#\2 . 0.00000) '(#\3 . 0.00000)
          ;'(#\4 . 0.00000) '(#\5 . 0.00000) '(#\6 . 0.00000) '(#\7 . 0.00000)
          ;'(#\8 . 0.00000) '(#\9 . 0.00000) '(#\0 . 0.00000) '(#\= . 0.00000)))
          ; It was a decent idea, but ends up crowding out the rest.  I'd have
          ; to add weighting into this to make it work.

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
           (phat (/ n total))
           (left (+ phat (/ (* z z) (* 2 total))))
           (right (* z (sqrt (/ (+ (* phat (- 1 phat))
                                   (/ (* z z) (* 4 total))) total))))
           (divisor (+ 1 (/ (* z z) n))))
      (cons (if (= n 0) 0 (/ (- left right) divisor))
            (/ (+ left right) divisor))))
  ; Translating infix to prefix by hand is surprisingly hard...

  ; We can weight infractions against the range by calculating how far outside
  ; of the acceptable range that they fell.  Since the distribution isn't
  ; linear, but we're treating it as such here, we over-weight distant outliars,
  ; but it works out okay for now.
  (define (distance-from-range value range)
    (cond ((< value (car range)) (- (car range) value))
          ((> value (cdr range)) (- value (cdr range)))
          (else 0)))

  ; We score a piece of text by first building a mapping of how many times each
  ; character occurred.  We then build the confidence intervals for these
  ; counts.  Each time that a frequency falls within our confidence interval,
  ; we count that as a mark.  The score is the total number of marks, with
  ; higher score being better.
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
                         (distance-from-range (cdr x)
                                    (alist-ref (car x)
                                               ci-scores
                                               eqv?
                                               '(0 . 0))))
                       letter-frequency)))
      ; I really like how scheme works like this.  (sum)?  No, I'm just going
      ; to apply + to a ton of arguments.
      (apply + marks)))


  ; It's amazing how good this is as a scorer given that it's 1 line of code.
  ; With the filtering below, it even solves challenge 4...
  (define (most-spaces-scorer charlst)
    ; We need #t = bad, and #f = good, so negate that equivalence.
    (count (lambda (x) (not (eqv? #\  x))) charlst))


  (define (char-printable? c)
    (let ((i (char->integer c)))
          ; Most printable characters fall in this range.
      (or (and (<= 32 i)
               (<= i  126))
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

  (define (encrypt-xor lst key)
    (bytes-xor lst (apply circular-list key)))

  (define (break-char-xor scorer bytes)
    (let* ((possibilities (list-tabulate 256 identity))
           (candidates (map (lambda (x) (encrypt-xor bytes (list x)))
                            possibilities))
           (charlsts (map (lambda (x) (map integer->char x))
                         candidates))
           (filtered (filter (lambda (lst) (every char-printable? lst))
                             charlsts))
           (scored (map (lambda (x) (cons (scorer x) x)) filtered))
;           (_ (map (lambda (x) (display x) (newline)) scored))
                                                     ; 640K is enough for anyone
           (best (if (pair? scored) (minf car scored) (cons 640000 '()))))
      (cons (car best) (list->string (cdr best)))))

  (define (find-breakable-xor scorer hexlst)
    (let* ((possibilities (map (lambda (x)
                                 (break-char-xor scorer
                                                 (base16->bytes x)))
                               hexlst))
           (best (minf car possibilities)))
      (cdr best)))


)
