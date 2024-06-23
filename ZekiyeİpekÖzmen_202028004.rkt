#lang scheme

; Function to build a maze from a list of strings
(define (buildMaze rows)
  (map string->list rows))

; Task 1: Define getHeight and getWidth functions
(define (getHeight maze)
  (length maze))

(define (getWidth maze)
  (if (null? maze)
      0
      (length (car maze))))

; Task 2: Define getLetter function
(define (getLetter maze row col)
  (if (and (>= row 0) (< row (getHeight maze))
           (>= col 0) (< col (getWidth maze)))
      (string (list-ref (list-ref maze row) col)) ; Convert to string
      "out-of-bounds"))

; Task 3: Define solveMaze function
(define (solveMaze maze)
  (let loop ((row 0) (col 0) (path '()))
    (cond
      ((= row (sub1 (getHeight maze))) path) ; reached the bottom, return path
      ((string=? (getLetter maze row col) "F") path) ; reached the exit, return path
      (else
       (let ((down (getLetter maze (+ row 1) col))
             (right (getLetter maze row (+ col 1))))
         (cond
           ((string=? down "E") (loop (+ row 1) col (cons "D" path)))
           ((string=? right "E") (loop row (+ col 1) (cons "R" path)))
           (else path)))))))

; Example usage:
(define sampleMaze
  (buildMaze
   '("S----"
     "E-----"
     "EEEEE-"
     "----E-"
     "----EF")))

(display "Height: ")
(display (getHeight sampleMaze))
(newline)

(display "Width: ")
(display (getWidth sampleMaze))
(newline)

(display "Letter at (1, 0): ")
(display (getLetter sampleMaze 1 0))
(newline)

(display "Solution: ")
(display (solveMaze sampleMaze))
(newline)