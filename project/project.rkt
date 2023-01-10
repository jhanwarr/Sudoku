#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")

(require typed/test-engine/racket-tests)

(define-type (Optional A) (U 'None (Some A)))

(define-struct (Some A)
  ([value : A]))

(define-type Sudoku (Listof Integer))

(define-struct SudokuWorld
  ([cell-size : Integer]
   [font-size : Byte]
   [grid-color : Image-Color]
   [bg-color : Image-Color]
   [clue-color : Image-Color]
   [user-color : Image-Color]
   [to-change-color : Image-Color]
   [puzzle : Sudoku]
   [user-answer : Sudoku]
   [waiting-input : (Optional Integer)]))

(define-struct Click
  ([x : Integer]
   [y : Integer]))

;; a sample sudoku puzzle
;; you can use this for testing
(: sudoku1 : Sudoku)
(define sudoku1
  (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 0))

;; a sample SudokuWorld
(: sw1 : SudokuWorld)
(define sw1 (SudokuWorld 36
                         24
                         'black
                         'white
                         'black
                         'blue
                         'gray
                         sudoku1
                         (make-list 81 0)
                         (Some 7)))

; you may need to set some image's color to transparent
; and here is its definition
(: transparent : Image-Color)
(define transparent (color 0 0 0 0))

; ======= Task 0: general helper functions

(: check : Integer Integer Integer -> Boolean)
; validating the inputs of the get-sublist function
(define (check len start end)
  (cond
    [(or (< start 0) (< end 0)) #f]
    [(or (> start len) (> end len)) #f]
    [(> start end) #f]
    [else #t]))


(: sublist : All (A) (Listof A) Integer Integer -> (Listof A))
; fetching the required sublist of the inputted list
(define (sublist l start end)
  (cond
    [(> start 0) (get-sublist (rest l) (- start 1) (- end 1))]
    [else (cond
            [(= end 0) '()]
            [else (append (list (first l))
                          (get-sublist (rest l) 0 (- end 1)))])]))


(: get-sublist : All (A) (Listof A) Integer Integer -> (Listof A))
;; returning a sublist of the given list from starting index to the ending one
(define (get-sublist l start end)
  (if (check (length l) start end)
      (sublist l start end)
      (error "Invalid Input Entered!!!")))

(check-expect (get-sublist '(1 2 3 4 5) 0 5) (list 1 2 3 4 5))
(check-expect (get-sublist '(1 2 3 4 5) 0 4) (list 1 2 3 4))
(check-expect (get-sublist '(1 2 3 4 5) 0 3) (list 1 2 3))
(check-expect (get-sublist '(1 2 3 4 5) 2 4) (list 3 4))
(check-expect (get-sublist '(1 2 3 4 5) 4 5) (list 5))
(check-expect (get-sublist '(1 2 3 4 5) 0 0) '())
(check-error (get-sublist '(1 2 3 4 5) -1 5) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 2 -1) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) -1 -1) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 1 6) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 6 1) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 6 6) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 3 2) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 5 0) "Invalid Input Entered!!!")
(check-error (get-sublist '(1 2 3 4 5) 4 1) "Invalid Input Entered!!!")



; ======= Task 1: draw a grid based on the given Sudoku, sizes, and colors

(: draw-grid : Sudoku Integer Byte Image-Color Image-Color Image-Color -> Image)
; drawing the Sudoku Grids with hints
(define (draw-grid s cell font grid bg col)
  (underlay (square (* cell 9) "solid" bg)
            (draw-coloumns s cell font grid col 0)))


(: draw-coloumns : Sudoku Integer Byte Image-Color Image-Color Integer -> Image)
; drawing the coloumns of the sudoku grid
(define (draw-coloumns s cell font grid col check)
  (above (draw-rows s cell font grid col (* 9 check) 0)
         (if (< check 8)
             (draw-coloumns s cell font grid col (+ check 1))
             (square 1 "outline" transparent))))
 

(: draw-rows : Sudoku Integer Byte Image-Color Image-Color Integer Integer
   -> Image)
; drawing the rows of the Sudoku Grid
(define (draw-rows sudo cell font grid col index check)
  (beside (underlay (square cell "outline" grid)
                   (text (number->string (element-at-pos sudo index)) font
                         (if (= 0 (element-at-pos sudo index))
                             transparent
                             col)))
          (if (< check 8)
              (draw-rows sudo cell font grid col (+ index 1) (+ check 1))
              (square 1 "outline" transparent))))

; ======= Task 2: more general helper functions

;; You are welcome to define more helper functions for all tasks.

(: element-at-pos : Sudoku Integer -> Integer)
; finding the element at index i
(define (element-at-pos s i)
  (match s
    ['() (error "Missing Grids")]
    [(cons head tail) (if (= 0 i) head (element-at-pos tail (- i 1)))]))

(: editable? : SudokuWorld Integer -> Boolean)
; checking whether a grid is editable or not
(define (editable? world pos)
  (= (element-at-pos (SudokuWorld-puzzle world) pos) 0))

(check-expect (editable? sw1 79) #f)
(check-expect (editable? sw1 80) #t)
(check-expect (editable? sw1 72) #f)
(check-expect (editable? sw1 0) #t)
(check-expect (editable? sw1 2) #f)


(: clicked-within : Click SudokuWorld -> (Optional Integer))
; finding the grid where the user has clicked and whether it is editable or not
(define (clicked-within click world)
  (local
    {(: index Integer)
     (define index (+ (* (quotient (Click-y click)
                                   (SudokuWorld-cell-size world)) 9)
                      (quotient (Click-x click)
                                (SudokuWorld-cell-size world))))}
    (if (editable? world index) (Some index) 'None)))

(check-expect (clicked-within (Click 26 38) (SudokuWorld 7
                         5
                         'black
                         'white
                         'black
                         'blue
                         'gray
                         sudoku1
                         (make-list 81 0)
                         'None)) 'None)

(check-expect (clicked-within (Click 19 38) (SudokuWorld 7
                         5
                         'black
                         'white
                         'black
                         'blue
                         'gray
                         sudoku1
                         (make-list 81 0)
                         'None)) (Some 47))

(check-expect (clicked-within (Click 12 38) (SudokuWorld 7
                         5
                         'black
                         'white
                         'black
                         'blue
                         'gray
                         sudoku1
                         (make-list 81 0)
                         'None)) (Some 46))

(check-expect (clicked-within (Click 5 38) (SudokuWorld 7
                         5
                         'black
                         'white
                         'black
                         'blue
                         'gray
                         sudoku1
                         (make-list 81 0)
                         'None)) 'None)

(: replace-at : All (A) Integer A (Listof A) -> (Listof A))
; replacing an element of the list at the given index with the given value 
(define (replace-at i x xs)
  (match xs
    ['() '()]
    [(cons head tail) (if (= 0 i)
                          (cons x (replace-at (- i 1) x tail))
                          (cons head (replace-at (- i 1) x tail)))]))


(: update-cell-by-index : Sudoku Integer Integer -> Sudoku)
;; replace the item at the given position after validation
(define (update-cell-by-index xs i x)
  (if (or (< i 0) (> i 80))
      xs
      (match xs
        ['() '()]
        [(cons head tail) (if (= 0 i)
                              (cons x tail)
                              (cons head (replace-at (- i 1) x tail)))])))

(check-expect (update-cell-by-index sudoku1 0 9) (list 9 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 0))

(check-expect (update-cell-by-index sudoku1 80 3) (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 3))

(check-expect (update-cell-by-index sudoku1 15 5) (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 5 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 0))

(check-expect (update-cell-by-index sudoku1 68 9) (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 9 0 0 0
        3 0 0 1 0 0 0 6 0))

(check-expect (update-cell-by-index sudoku1 -3 9) (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 0))

(check-expect (update-cell-by-index sudoku1 81 9) (list 0 0 6 0 0 8 5 0 0
        0 0 0 0 7 0 6 1 3
        0 0 0 0 0 0 0 0 9
        0 0 0 0 9 0 0 0 1
        0 0 1 0 0 0 8 0 0
        4 0 0 5 3 0 0 0 0
        1 0 7 0 5 3 0 0 0
        0 5 0 0 6 4 0 0 0
        3 0 0 1 0 0 0 6 0))

;(: point-to-index : )

(: go-col : Integer Integer Image-Color -> Image)
;drawing the gray box
(define (go-col cell index to-col)
  (local
    {(: x Integer)
     (define x (+ 1 (* (modulo index 9) cell)))
     (: y Integer)
     (define y (+ (* (quotient index 9) cell) cell 1))}
    (beside/align "bottom"
                  (rectangle x y 'outline transparent)
                  (above (square (- cell 2) 'solid to-col)
                         (square 2 'outline transparent)))))


; ======= Task 3: universe

(: draw : SudokuWorld -> Image)
; draw a 9*9 grid, the index of each cell is from 0-80, starting from top-left
; corner. The numbering of index goes from left to right first, then from top
; to down.
; user's inputs and clicking effect should be correctly rendered as well
(define (draw world)
  (match world
    [(SudokuWorld cell font grid-col bg-col c-col u-col to-col p user-ans wait)
     (overlay/align "left" "top"
                    (draw-grid user-ans cell font transparent transparent u-col)
                    (match wait
                      ['None (triangle 1 'outline transparent)]
                      [(Some index) (go-col cell index to-col)])
                    (draw-grid p cell font grid-col bg-col c-col))]))

(: react-to-mouse : SudokuWorld Integer Integer Mouse-Event -> SudokuWorld)
; handle user input
(define (react-to-mouse world cx cy e)
  (match e
    ["button-down" (match (clicked-within (Click cx cy) world)
                     ['None world]
                     [(Some index) (match world
                                     [(SudokuWorld c f g-c b-c c-c
                                                   u-c t-c p u _)
                                      (SudokuWorld c f g-c b-c c-c u-c t-c p u
                                                   (Some index))])])]
    [_ world]))

(: react-to-keyboard : SudokuWorld String -> SudokuWorld)
; depending on the pressed key, put a new number into the grid or not
(define (react-to-keyboard world key)
  (match (SudokuWorld-waiting-input world)
    ['None world]
    [(Some index) (match key
                    ["0" (SudokuWorld (SudokuWorld-cell-size world)
                                      (SudokuWorld-font-size world)
                                      (SudokuWorld-grid-color world)
                                      (SudokuWorld-bg-color world)
                                      (SudokuWorld-clue-color world)
                                      (SudokuWorld-user-color world)
                                      (SudokuWorld-to-change-color world)
                                      (SudokuWorld-puzzle world)
                                      (update-cell-by-index
                                       (SudokuWorld-user-answer world) index 0)
                                      'None)]
                    ["9" (SudokuWorld (SudokuWorld-cell-size world)
                                      (SudokuWorld-font-size world)
                                      (SudokuWorld-grid-color world)
                                      (SudokuWorld-bg-color world)
                                      (SudokuWorld-clue-color world)
                                      (SudokuWorld-user-color world)
                                      (SudokuWorld-to-change-color world)
                                      (SudokuWorld-puzzle world)
                                      (update-cell-by-index
                                       (SudokuWorld-user-answer world) index 9)
                                      'None)]
                    [k (if (and (string>? k "0") (string<? k "9"))
                             (SudokuWorld (SudokuWorld-cell-size world)
                                      (SudokuWorld-font-size world)
                                      (SudokuWorld-grid-color world)
                                      (SudokuWorld-bg-color world)
                                      (SudokuWorld-clue-color world)
                                      (SudokuWorld-user-color world)
                                      (SudokuWorld-to-change-color world)
                                      (SudokuWorld-puzzle world)
                                      (update-cell-by-index
                                       (SudokuWorld-user-answer world)
                                       index
                                       (cast (string->number k) Integer))
                                      'None)
                             world)])]))

(: run : Integer Byte
   Image-Color Image-Color Image-Color Image-Color Image-Color
   Sudoku -> SudokuWorld)
; create a new world
(define (run cell-size font-size
             grid-c bg-c clue-c user-c to-change-c
             sudoku)
  (big-bang (SudokuWorld cell-size font-size
                         grid-c bg-c clue-c user-c to-change-c
                         sudoku (make-list 81 0) 'None) : SudokuWorld
    [to-draw draw]
    [on-mouse react-to-mouse]
    [on-key react-to-keyboard]))

;; Sample input -- (run 36 24 'black 'white 'black 'blue 'gray sudoku1)

(test)
