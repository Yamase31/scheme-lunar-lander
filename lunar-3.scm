#lang racket

;; this is the code for problem set -- Lunar Lander

; terminal i/o primitives supported in MIT Scheme but not DrScheme -----------

(define write-line
  (lambda (line)
    (begin
      (display line)
      (newline))))

(define prompt-for-command-char 
  (lambda (prompt)
    (begin
        (display prompt)
        (read))))

; Lunar Landar functions -----------------------------------------------------
; Problem 1 --------------------------------
(define make-ship-state
  (lambda (height velocity fuel)
    (list height velocity fuel)))

(define height
  (lambda (ship-state)
    (list-ref ship-state 0)))

(define velocity
  (lambda (ship-state)
    (list-ref ship-state 1)))

(define fuel
  (lambda (ship-state)
    (list-ref ship-state 2)))

;Problem 2 ---------------------------------- 
;(define update 
;  (lambda (ship-state fuel-burn-rate)
;
;(define update
;  (lambda (ship-state fuel-burn-rate)
;      (let((new-burn-rate (if (> (fuel ship-state) 0 )fuel-burn-rate 0)))
;           (make-ship-state
;            (+ (height ship-state) (* (velocity ship-state) dt)) ; height
;            (+ (velocity ship-state)
;               (* (- (*  engine-strength new-burn-rate) gravity)
;                  dt))                                           ; velocity
;            (- (fuel ship-state) (* new-burn-rate dt))))))       ; fuel

;Problem 3 ----------------------------------
(define full-burn (lambda (ship-state) 1))
(define no-burn (lambda (ship-state) 0))
(define ask-user (lambda (ship-state) (get-burn-rate)))

;Problem 4 ----------------------------------
;(define random-choice
;  (lambda (strategy-1 strategy-2)
;    (lambda (ship-state)
;      (if (= (random 2) 0)
;          (strategy-1 ship-state)
;          (strategy-2 ship-state)))))

;Problem 5 ----------------------------------
;(define height-choice
;  (lambda (strategy-1 strategy-2 critical-height)
;    (lambda (ship-state)
;    (if (> (height ship-state) critical-height)
;        (strategy-1 ship-state)
;        (strategy-2 ship-state)))))

;Problem 6 ----------------------------------
(define random-choice
     (lambda (strategy-1 strategy-2)
       (choice strategy-1
               strategy-2
               (lambda (ship-state) (= (random 2) 0)))))

(define choice
        (lambda (strategy-1 strategy-2 condition)
        (lambda  (ship-state)
          (if (eq? (condition ship-state) #t)
              (strategy-1 ship-state)
              (strategy-2 ship-state)))))


(define height-choice
  (lambda (strategy-1 strategy-2 critial-height)
    (choice strategy-1
            strategy-2
            (lambda (ship-state) (cond ((>= (height ship-state) critial-height) #t)
                                       (else #f))))))            

;Problem 7 ----------------------------------
(define square (lambda (x) (* x x)))
(define get-acc (lambda (ship-state) (/(square (velocity ship-state))(* 2 (height ship-state)))))

(define constant-acc 
  (lambda (ship-state)
    (/ 
     (+ (get-acc ship-state) 
        gravity) 
     engine-strength)))

;Problem 8 ----------------------------------
(define update
  (lambda (ship-state fuel-burn-rate)
    (let ((new-burn-rate ; keep it between 0 and 1
           (if (< (fuel ship-state) 1)
               (fuel ship-state)
               (min fuel-burn-rate 1))))
      (display (list fuel-burn-rate new-burn-rate))
      (make-ship-state
       (+ (height ship-state) (* (velocity ship-state) dt)) ; height
       (+ (velocity ship-state)
          (* (- (*  engine-strength new-burn-rate) gravity)
             dt))                                           ; velocity
       (- (fuel ship-state) (* new-burn-rate dt))))))       ; fuel




;Fin ----------------------------------

(define lander-loop
  (lambda (ship-state strategy)
    (show-ship-state ship-state)
    (if (landed? ship-state)
        (end-game ship-state)
        (lander-loop (update ship-state (strategy ship-state)) strategy))))

(define show-ship-state 
  (lambda (ship-state)
    (write-line 
      (list 'height (height ship-state)
            'velocity (velocity ship-state)
            'fuel (fuel ship-state)))))

(define landed? 
  (lambda (ship-state) 
    (<= (height ship-state) 0)))

(define end-game 
  (lambda (ship-state)
    (let ((final-velocity (velocity ship-state)))
         (write-line final-velocity)
         (cond ((>= final-velocity safe-velocity)
                 (write-line "good landing")
                 'game-over)
               (else
                 (write-line "you crashed!")
                 'game-over)))))

(define get-burn-rate
  (lambda ()
    (if (eq? (player-input) burn-key)
        1
        0)))

 
(define play 
  (lambda (strategy)
    (lander-loop (initial-ship-state) strategy)))


; making initial-ship-state a function allows make-ship-state to remain
; unwritten without causing a load-time error
(define initial-ship-state 
  (lambda ()
    (make-ship-state 50       ; 50 km high
                     0        ; not moving (0 km/sec)
                     20)))    ; 20 kg of fuel left

(define dt 1)               ; 1 second interval of simulation
  
(define gravity 0.5)        ; 0.5 km/sec/sec
  
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash

(define engine-strength 1)  ; 1 kilonewton-second

(define player-input
  (lambda ()
    (prompt-for-command-char " action: ")))

(define burn-key 'b)

