;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pipe-fantasy) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; pipe dream pt 1

(define-struct pipe [top bot left right])
;; A Pipe is a (make-pipe Boolean Boolean Boolean Boolean)
;; Interpretation: a pipe with openings in the given directions. A  #true for 
;; one of top, bot, left, right indicates an opening in that direction.
(define PIPE-TL (make-pipe #true #false #true #false))
(define PIPE-TR (make-pipe #true #false #false #true))
(define PIPE-BL (make-pipe #false #true #true #false))
(define PIPE-BR (make-pipe #false #true #false #true))
(define PIPE-TB (make-pipe #true #true #false #false))
(define PIPE-LR (make-pipe #false #false #true #true))
(define PIPE-TBLR (make-pipe #true #true #true #true))
(define PIPE-STARTING-L (make-pipe #false #false #true #false))
(define PIPE-STARTING-R (make-pipe #false #false #false #true))
(define PIPE-STARTING-T (make-pipe #true #false #false #false))
(define PIPE-STARTING-B (make-pipe #false #true #false #false))

(define (pipe-temp p)
  (... (pipe-top p) ...
       (pipe-bottom p) ...
       (pipe-left p) ...
       (pipe-right p) ...))

(define ALL-PIPES (list PIPE-TL
                        PIPE-TR
                        PIPE-BL
                        PIPE-BR
                        PIPE-TB
                        PIPE-LR
                        PIPE-TBLR
                        PIPE-STARTING-L
                        PIPE-STARTING-R
                        PIPE-STARTING-T
                        PIPE-STARTING-B))


(define TILE-LENGTH 50)
(define PIPE-WIDTH 15)

(define (TOP-PIPE PW TSL) (design-pipe PW (+ (* TSL .5) (* PW .5))) )
(define (BOT-PIPE PW TSL) (design-pipe PW (+ (* TSL .5) (* PW .5))) )
(define (LEFT-PIPE PW TSL) (design-pipe (+ (* TSL .5) (* PW .5)) PW))
(define (RIGHT-PIPE PW TSL) (design-pipe (+ (* TSL .5) (* PW .5)) PW))

(define (TOP-POSN PW TSL) (make-posn (* TSL .5) (* (+ (* TSL .5) (* PW .5)) .5)))
(define (BOT-POSN PW TSL) (make-posn (* TSL .5) (- TSL (* (+ (* TSL .5) (* PW .5)) .5))))
(define (LEFT-POSN PW TSL) (make-posn (* (+ (* TSL .5) (* PW .5)) .5) (* TSL .5)))
(define (RIGHT-POSN PW TSL) (make-posn (- TSL (* (+ (* TSL .5) (* PW .5)) .5)) (* TSL .5)))


;; pipe->image: Pipe Integer Integer Boolean -> Image
;; Draws the given pipe on a square tile with length tile-side-length. The width
;; of the pipe is pipe-width. Pipe-width should be less than tile-side-length
;; If filled? then draw pipe with goo
(define (pipe->image pipe pipe-width tile-side-length filled?)
  (cond
    [(and (pipe-top pipe) (pipe-bot pipe) (pipe-left pipe) (pipe-right pipe))
     (place-images
      (cons (TOP-PIPE pipe-width tile-side-length)  ;top
            (cons(BOT-PIPE pipe-width tile-side-length) ;bottom
            (cons (LEFT-PIPE pipe-width tile-side-length) ;left
            (cons (RIGHT-PIPE pipe-width tile-side-length) '())))) ;right
      (cons (TOP-POSN pipe-width tile-side-length)
            (cons (BOT-POSN pipe-width tile-side-length)
            (cons (LEFT-POSN pipe-width tile-side-length)
            (cons (RIGHT-POSN pipe-width tile-side-length) '()))))
      (tile tile-side-length))]
    [(and (pipe-top pipe) (pipe-left pipe))
     (place-images
      (cons (TOP-PIPE pipe-width tile-side-length)
            (cons (LEFT-PIPE pipe-width tile-side-length) '()))
      (cons (TOP-POSN pipe-width tile-side-length)
            (cons(LEFT-POSN pipe-width tile-side-length) '()))
      (tile tile-side-length))]
    [(and (pipe-top pipe) (pipe-right pipe))
     (place-images
      (cons (TOP-PIPE pipe-width tile-side-length)
            (cons (RIGHT-PIPE pipe-width tile-side-length) '()))
      (cons (TOP-POSN pipe-width tile-side-length)
            (cons (RIGHT-POSN pipe-width tile-side-length) '()))
      (tile tile-side-length))]
    [(and (pipe-bot pipe) (pipe-left pipe))
     (place-images
      (cons (BOT-PIPE pipe-width tile-side-length)
            (cons (LEFT-PIPE pipe-width tile-side-length) '()))
      (cons (BOT-POSN pipe-width tile-side-length)
            (cons (LEFT-POSN pipe-width tile-side-length) '()))
      (tile tile-side-length))]
    [(and (pipe-bot pipe) (pipe-right pipe))
    (place-images
     (cons (BOT-PIPE pipe-width tile-side-length)
           (cons (RIGHT-PIPE pipe-width tile-side-length) '()))
     (cons (BOT-POSN pipe-width tile-side-length)
            (cons (RIGHT-POSN pipe-width tile-side-length) '()))
     (tile tile-side-length))]
    [(and (pipe-top pipe) (pipe-bot pipe))
     (place-images
      (cons (TOP-PIPE pipe-width tile-side-length) 
            (cons (BOT-PIPE pipe-width tile-side-length) '()))
      (cons (TOP-POSN pipe-width tile-side-length)
            (cons (BOT-POSN pipe-width tile-side-length) '()))
      (tile tile-side-length))]
    [(and (pipe-left pipe) (pipe-right pipe))
     (place-images
      (cons (LEFT-PIPE pipe-width tile-side-length) 
            (cons (RIGHT-PIPE pipe-width tile-side-length) '()))
      (cons (LEFT-POSN pipe-width tile-side-length)
            (cons (RIGHT-POSN pipe-width tile-side-length) '()))
      (tile tile-side-length))]))

; tile : Number -> Image
; draws a tile based on the given length.
(check-expect (tile 50) (overlay (square 50 "outline" "black") (square 50 "solid" "grey")))
(check-expect (tile 5) (overlay (square 5 "outline" "black") (square 5 "solid" "grey")))
(check-expect (tile 100) (overlay (square 100 "outline" "black") (square 100 "solid" "grey")))
(define (tile l)
  (overlay (square l "outline" "black") (square l "solid" "grey")))

; design-pipe : Number Number -> Image
; draws the pipe based on the given width and length.
(check-expect (design-pipe 15 50) (rectangle 15 50 "solid" "black"))
(check-expect (design-pipe 20 90) (rectangle 20 90 "solid" "black"))
(check-expect (design-pipe 10 10) (rectangle 10 10 "solid" "black"))
(define (design-pipe w l)
  (rectangle w l "solid" "black"))

(pipe->image PIPE-TL PIPE-WIDTH TILE-LENGTH #f)
(pipe->image PIPE-TR PIPE-WIDTH TILE-LENGTH #f)
(pipe->image PIPE-BL PIPE-WIDTH TILE-LENGTH #f)
(pipe->image PIPE-BR PIPE-WIDTH TILE-LENGTH #f)   
(pipe->image PIPE-TB PIPE-WIDTH TILE-LENGTH #f)
(pipe->image PIPE-LR PIPE-WIDTH TILE-LENGTH #f)
(pipe->image PIPE-TBLR PIPE-WIDTH TILE-LENGTH #f)

(define-struct pipe-coord [pipe r c])
; a Pipe-Coord is a (make-pipe-coord Pipe Number Number)
; pipe is the pipe being placed
; r is the row of the pipe
; c is the column of the pipe
(define PC-1 (make-pipe-coord PIPE-TL 2 3))
(define PC-2 (make-pipe-coord PIPE-BR 3 3))
(define PC-3 (make-pipe-coord PIPE-TR 1 1))
(define (pipe-coord-temp pc)
  (... (pipe-temp (pipe-coord-pipe pc)) ...
       (pipe-coord-r pc) ...
       (pipe-coord-c pc) ...))

; a [List-of PC] is one of:
; '()
; (cons Pipe-Coord [List-of PC])
; Interpretation: The empty list or a list of Pipe-Coords.
(define LPC-1 (cons PC-1 '()))
(define LPC-2 (cons PC-2 LPC-1))
(define LPC-3 (cons PC-3 LPC-2))
(define (list-pc-temp l)
  (... (cond [(empty? l) ...]
             [(cons? l) ... (first l)
                        ... (list-pc-temp (rest l)) ...]) ...))

(define-struct grid [n list-pc tile-length pipe-width])
; a Grid is a (make-grid Number [List-of PC] Number Number)
; n is the length of the grid
; list-pc is the list of pipe coordinates
; tile-length is the length of the tiles
; pipe-width it the width of the pipes
(define GRID1 (make-grid 4 LPC-1 50 15))
(define GRID2 (make-grid 8 LPC-2 50 15))
(define GRID3 (make-grid 5 LPC-3 50 15))
(define STARTING-GRID (make-grid 7 '() 50 15))
(define (grid-temp g)
  (... (grid-n g) ...
       (list-pc-temp (grid-list-pc g)) ...
       (grid-tile-length g) ...
       (grid-pipe-width g) ...))

;; place-pipe: Grid Pipe Integer Integer -> Grid
;; Places the pipe on the grid at the given row and column. We assume that the
;; row and column are valid positions on the grid.
(check-expect (place-pipe GRID1 PIPE-TL 4 4) (make-grid 4 (list (make-pipe-coord PIPE-TL 4 4) PC-1) 50 15))
(check-expect (place-pipe GRID2 PIPE-BL 1 8) (make-grid 8 (list (make-pipe-coord PIPE-BL 1 8) PC-2 PC-1) 50 15))
(check-expect (place-pipe GRID3 PIPE-TL 5 2) (make-grid 5 (list (make-pipe-coord PIPE-TL 5 2) PC-3 PC-2 PC-1) 50 15))
(check-expect (place-pipe GRID1 PIPE-BL 2 3) (make-grid 4 (list (make-pipe-coord PIPE-BL 2 3)) 50 15))
(define (place-pipe grid pipe row col)
  (make-grid (grid-n grid)
             (if (and (= row (pipe-coord-r (first (grid-list-pc grid))))
                      (= col (pipe-coord-c (first (grid-list-pc grid)))))
                 (cons (make-pipe-coord pipe row col)
                       (rest (grid-list-pc grid)))
                 (cons (make-pipe-coord pipe row col)
                       (grid-list-pc grid)))
             (grid-tile-length grid)
             (grid-pipe-width grid)))

;; pipe-at: Grid Integer Integer -> [Optional Pipe]
;; Produces the pipe at the given row and column, or #false if that position is
;; is blank. We assume that the row and column are valid positions on the grid.
(check-expect (pipe-at GRID1 2 3) PIPE-TL)
(check-expect (pipe-at GRID1 1 3) #false)
(check-expect (pipe-at GRID2 3 3) PIPE-BR)
(define (pipe-at grid row col)
  (cond [(empty? (grid-list-pc grid)) #false]
        [(cons? (grid-list-pc grid)) (if (and (= row (pipe-coord-r (first (grid-list-pc grid))))
                                              (= col (pipe-coord-c (first (grid-list-pc grid)))))
                                         (pipe-coord-pipe (first (grid-list-pc grid)))
                                         (pipe-at (make-grid (grid-n grid)
                                                             (rest (grid-list-pc grid))
                                                             (grid-tile-length grid)
                                                             (grid-pipe-width grid))
                                                  row col))]))

;; grid->image: Grid Integer Integer -> Image
;; Draws the grid of pipes with each tile being tile-side-length long
;; and every pipe being the width of pipe-width
(define (grid->image grid tile-side-length pipe-width)
   (place-images
    (all-tiles (grid-list-pc grid) tile-side-length pipe-width)
    (position (grid-list-pc grid) tile-side-length)
    (draw-box (grid-n grid) (grid-n grid) tile-side-length)))

; draw-row: Number Number -> Image
; draws the rows of a grid based on the given length
(check-expect (draw-row 3 50) (beside (tile 50) (beside (tile 50) (tile 50))))

(check-expect (draw-row 5 20) (beside (tile 20)
                              (beside (tile 20)
                              (beside (tile 20)
                              (beside (tile 20) (tile 20))))))

(check-expect (draw-row 2 100) (beside (tile 100) (tile 100)))

(define (draw-row length tile-side-length)
  (cond
    [(= 1 length) (tile tile-side-length)]
    [else (beside (tile tile-side-length) (draw-row (sub1 length) tile-side-length))]))

; draw-box : Number Number Number -> Image
; draws the entire grid based on the given length and number of columns
(check-expect (draw-box 2 2 50) (above (beside (tile 50) (tile 50)) (beside (tile 50) (tile 50))))

(check-expect (draw-box 3 3 100)
              (above (beside (tile 100) (beside (tile 100) (tile 100)))
                     (above (beside (tile 100) (beside (tile 100) (tile 100)))
                            (beside (tile 100) (beside (tile 100) (tile 100))))))

(check-expect (draw-box 1 1 10) (tile 10))


(define (draw-box col length tile-side-length)
  (cond
    [(= 1 col) (draw-row length tile-side-length)]
    [else (above (draw-row length tile-side-length) (draw-box (sub1 col) length tile-side-length))]))

; all-tiles : [List-of PC] Number Number -> [List-of PC]
; draws all the pipes in a list with the pipes being the width pipe-width
; and the length of the tile being tile-side-length
(check-expect (all-tiles LPC-1 20 30) (list (pipe->image PIPE-TR 30 20 #f)))
(check-expect (all-tiles LPC-2 10 20) (list (pipe->image PIPE-BR 20 10 #f)(pipe->image PIPE-TR 20 10 #f)))
(check-expect (all-tiles LPC-3 50 100) (list (pipe->image PIPE-TR 100 50 #f) (pipe->image PIPE-BR 100 50 #f)(pipe->image PIPE-TR 100 50 #f)))

(define (all-tiles lopc tile-side-length pipe-width)
  (local [
          (define (tile-image lopc)
            (pipe->image (pipe-coord-pipe lopc) pipe-width tile-side-length #f) )]
    (map tile-image lopc)))

; position : [List-of PC] Number -> [List-of Posn]
; calculates the position of each tile
(check-expect (position LPC-1 20) (list (make-posn 70 50)))
(check-expect (position LPC-2 50) (list (make-posn 175 175) (make-posn 175 125)))
(check-expect (position LPC-3 100) (list (make-posn 150 150) (make-posn 350 350) (make-posn 350 250)))

(define (position LOPC tile-side-length)
  (local [
          (define (tile-posn tile)
            (make-posn
             (+ (* (pipe-coord-c tile) tile-side-length) (/ tile-side-length 2))
             (+ (* (pipe-coord-r tile) tile-side-length) (/ tile-side-length 2))))]
    (map tile-posn LOPC)))

; a [List-of Pipe] is one of:
; '()
; (cons Pipe [List-of Pipe])
; Interpretation: the empty list or a list of pipes
(define PIPES-1 (list PIPE-TL
                      PIPE-BL
                      PIPE-TB))
(define PIPES-2 (list PIPE-BL
                      PIPE-BL
                      PIPE-TL
                      PIPE-TBLR
                      PIPE-BR))
(define PIPES-3 '())
(define (list-pipe-temp lop)
  (... (cond [(empty? lop) ...]
             [(cons? lop) ... (first lop)
                          ... (list-pipe-temp (rest lop))])...))

(define-struct gamestate [grid pipes])
; a GameState is a (make-gamestate Grid [List-of Pipe])
; grid is the n x n grid for the game
; pipes is the list of incoming pipes
(define GS-1 (make-gamestate GRID1 PIPES-1))
(define GS-2 (make-gamestate GRID2 PIPES-2))
(define GS-3 (make-gamestate GRID3 PIPES-3))
(define (gamestate-temp gs)
  (... (grid-temp (gamestate-grid gs)) ... (list-pipe-temp (gamestate-pipes gs)) ...))

;; place-pipe-on-click : GameState Integer Integer MouseEvent -> GameState`
;; If the user clicks on a tile and there are incoming pipes available, places
;; the next incoming pipe on that tile. If no pipes are available, does nothing.
(check-expect (place-pipe-on-click GS-1 160 110 "button-down")
              (make-gamestate
               (make-grid 4 (list (make-pipe-coord PIPE-TL 2 3)) 50 15)
               (list PIPE-BL PIPE-TB)))
(check-expect (place-pipe-on-click GS-2 52 25 "button-down")
              (make-gamestate
               (make-grid 8 (list (make-pipe-coord PIPE-BL 0 1) PC-2 PC-1) 50 15)
               (list PIPE-BL PIPE-TL PIPE-TBLR PIPE-BR)))
(check-expect (place-pipe-on-click GS-3 10 10 "button-down") GS-3)
(check-expect (place-pipe-on-click (make-gamestate GRID1 PIPES-2) 160 110 "button-down")
              (make-gamestate (make-grid 4 (list (make-pipe-coord PIPE-BL 2 3)) 50 15)
                              (list PIPE-BL PIPE-TL PIPE-TBLR PIPE-BR)))

(define (place-pipe-on-click gs x y m)
  (cond [(string=? m "button-down")
         (cond [(empty? (gamestate-pipes gs)) gs]
               [(cons? (gamestate-pipes gs))
                (make-gamestate (place-pipe (gamestate-grid gs)
                                            (first (gamestate-pipes gs))
                                            (pipe-pos y
                                                      (grid-n (gamestate-grid gs))
                                                      (grid-tile-length (gamestate-grid gs)))
                                            (pipe-pos x
                                                      (grid-n (gamestate-grid gs))
                                                      (grid-tile-length (gamestate-grid gs))))
                                (rest (gamestate-pipes gs)))])]
        [else gs]))

; pipe-pos : Number Number Number -> Number
; determines the row/column value of the pipe based on its position,
; the n value of the grid, and the tile length
(check-expect (pipe-pos 60 4 50) 1)
(check-expect (pipe-pos 49 4 50) 0)
(check-expect (pipe-pos 312 8 50) 6)
(check-expect (pipe-pos 380 8 50) 7)
(define (pipe-pos p n l)
  (if (> p (* n l)) n (pipe-pos p (sub1 n) l)))

; draw-game : GameState -> Image
; passes the correct parameters to grid->image to draw the current GameState
(check-expect (draw-game GS-1)
              (place-images
               (all-tiles (grid-list-pc (gamestate-grid GS-1)) 50 15)
               (position (grid-list-pc (gamestate-grid GS-1)) 50)
               (draw-box (grid-n (gamestate-grid GS-1)) (grid-n (gamestate-grid GS-1)) 50)))
(check-expect (draw-game GS-2)
              (place-images
               (all-tiles (grid-list-pc (gamestate-grid GS-2)) 50 15)
               (position (grid-list-pc (gamestate-grid GS-2)) 50)
               (draw-box (grid-n (gamestate-grid GS-2)) (grid-n (gamestate-grid GS-2)) 50)))
(check-expect (draw-game GS-3)
              (place-images
               (all-tiles (grid-list-pc (gamestate-grid GS-3)) 50 15)
               (position (grid-list-pc (gamestate-grid GS-3)) 50)
               (draw-box (grid-n (gamestate-grid GS-3)) (grid-n (gamestate-grid GS-3)) 50)))
(define (draw-game gs)
  (grid->image (gamestate-grid gs)
               (grid-tile-length (gamestate-grid gs))
               (grid-pipe-width (gamestate-grid gs))))

(define (pipe-fantasy initial-game-state)
  (big-bang initial-game-state
    [to-draw draw-game]
    [on-mouse place-pipe-on-click]))

;(pipe-fantasy GS-1)
             
