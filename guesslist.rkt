#lang racket
(#%require (lib "27.ss" "srfi"))  ; random integer function

(define players-guesses '()) ; storing player's name and number of guesses

(define (get-guess prompt)
  (display prompt) 
  (read))

(define (play-guess random-num num-guesses)
  (display "Guess a number from 1 to 10:\n")
  (let loop ((guess (get-guess "Enter guess ")) ; getting the initial guess
             (count num-guesses))
    (cond ((equal? guess 'q) ; user can press 'q' to quit
           (begin
             (display "Quitting the game.\n")
             (show-summary)
             (return)))
          ((not (number? guess)) ; to check wheter input is valid
           (display "Invalid input, please enter a number.\n")
           (loop (get-guess "Enter guess ") count))
          ((< guess random-num)
           (loop (get-guess "Higher...Enter guess ") (+ count 1))) 
          ((> guess random-num)
           (loop (get-guess "Lower...Enter guess ") (+ count 1))) 
          (else ; correct guess
           (display "Correct! ")
           (display count)
           (display " guesses... Please enter your name: ")
           (let ((name (read)))
             (set! players-guesses (cons (cons name count) players-guesses)) 
             (display "Good game, ")
             (write name)
             (newline)
             (post-game)))))) ; proceeds to the post game options

(define (post-game)
  (display "Enter q to quit or any other key to continue:\n")
  (let ((decision (read)))
    (if (equal? decision 'q)
        (begin (display "Quitting the game.\n") (show-summary) (return))
        (play-guess (+ (random-integer 9) 1) 1)))) ; to start a new game

(define (show-summary)
  (for-each (lambda (player-guess)
              (display (car player-guess))
              (display " ")
              (display (cdr player-guess))
              (newline)
              (display "-----------------------\n"))
            (reverse players-guesses)))

(define (return)
  (display "Thank you for playing!\n"))

; to start the game
(play-guess (+ (random-integer 9) 1) 1)
