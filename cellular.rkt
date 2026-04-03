#lang racket
(require racket/gui racket/draw)

;; ── Configuration ──────────────────────────────────────────
(define CELL-SIZE 6)          ; pixels per cell side
(define GRID-WIDTH 200)       ; number of cells per row
(define current-rule 5)       ; Wolfram rule (0–255), mutable
(define current-color-mode 0) ; 0 = black/white, 1 = state colors
(define NUM-COLOR-MODES 2)

;; ── Color palettes ─────────────────────────────────────────
;; Each palette maps neighbourhood index 0–7 to a color for "alive" cells.
;; Dead cells (rule says off) use the background color.

;; Palette 0: classic black & white (alive = black regardless of state)
(define palette-bw
  (vector (make-color   0   0   0)    ; 0 → black
          (make-color   0   0   0)    ; 1 → black
          (make-color   0   0   0)    ; 2 → black
          (make-color   0   0   0)    ; 3 → black
          (make-color   0   0   0)    ; 4 → black
          (make-color   0   0   0)    ; 5 → black
          (make-color   0   0   0)    ; 6 → black
          (make-color   0   0   0)))  ; 7 → black

;; Palette 1: vivid state colours — each of the 8 neighbourhood
;; configurations gets its own colour.
(define palette-state
  (vector (make-color  30  30  30)    ; 0 (000) → near-black
          (make-color  29 145 192)    ; 1 (001) → teal
          (make-color 127  72 163)    ; 2 (010) → purple
          (make-color  44 162  95)    ; 3 (011) → green
          (make-color 227  74  51)    ; 4 (100) → red-orange
          (make-color 253 187  45)    ; 5 (101) → gold
          (make-color  70 130 180)    ; 6 (110) → steel blue
          (make-color 231  41 138)))  ; 7 (111) → hot pink

(define palettes (vector palette-bw palette-state))

(define (current-palette)
  (vector-ref palettes current-color-mode))

(define color-mode-names
  (vector "Black & White" "State Colors"))

;; ── Rule logic ─────────────────────────────────────────────
;; Convert rule number to an 8-bit vector of outcomes.
;; Index = 3-bit neighbourhood (left, center, right) as a number 0–7.
(define (make-rule-table n)
  (for/vector ([i (in-range 8)])
    (bitwise-bit-set? n i)))

(define rule-table (make-rule-table current-rule))

;; Each cell stores its neighbourhood index (0–7) if alive, or #f if dead.
;; This lets us colour by the pattern that produced the cell.

;; cell-alive? : check if a cell value counts as alive
(define (cell-alive? v) (and v #t))

;; Given a row of cell-values (#f or 0–7), produce the next row.
(define (next-generation row)
  (define len (vector-length row))
  (for/vector ([i (in-range len)])
    (let* ([l (cell-alive? (if (> i 0)         (vector-ref row (- i 1)) #f))]
           [c (cell-alive? (vector-ref row i))]
           [r (cell-alive? (if (< i (- len 1)) (vector-ref row (+ i 1)) #f))]
           [idx (+ (if l 4 0) (if c 2 0) (if r 1 0))])
      (if (vector-ref rule-table idx)
          idx    ; alive — store which neighbourhood produced it
          #f)))) ; dead

;; ── Build all generations ──────────────────────────────────
;; Start with a single live cell in the centre (state 2 = "010").
(define (make-initial-row width)
  (define row (make-vector width #f))
  (vector-set! row (quotient width 2) 2)  ; centre cell alive, state 2
  row)

(define (build-generations width num-rows)
  (define gens (make-vector num-rows))
  (vector-set! gens 0 (make-initial-row width))
  (for ([g (in-range 1 num-rows)])
    (vector-set! gens g (next-generation (vector-ref gens (- g 1)))))
  gens)

;; ── GUI ────────────────────────────────────────────────────
(define frame
  (new frame%
       [label (format "Rule ~a  |  ~a" current-rule
                      (vector-ref color-mode-names current-color-mode))]
       [width  (* CELL-SIZE GRID-WIDTH)]
       [height (* CELL-SIZE GRID-WIDTH)]
       [style '(fullscreen-button)]))   ; adds the OS fullscreen button

(define generations #f)  ; computed once we know the canvas size

(define (update-title!)
  (send frame set-label
        (format "Rule ~a  |  ~a"
                current-rule
                (vector-ref color-mode-names current-color-mode))))

(define (set-rule! n)
  (set! current-rule (modulo n 256))
  (set! rule-table (make-rule-table current-rule))
  (update-title!))

(define (cycle-color-mode! dir)
  (set! current-color-mode
        (modulo (+ current-color-mode dir) NUM-COLOR-MODES))
  (update-title!))

;; Custom canvas that handles keyboard input
(define automaton-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char event)
      (define key (send event get-key-code))
      (cond
        ;; Right arrow → next rule
        [(eq? key 'right)
         (set-rule! (+ current-rule 1))
         (send this refresh)]
        ;; Left arrow → previous rule
        [(eq? key 'left)
         (set-rule! (- current-rule 1))
         (send this refresh)]
        ;; Up arrow → next color mode
        [(eq? key 'up)
         (cycle-color-mode! 1)
         (send this refresh)]
        ;; Down arrow → previous color mode
        [(eq? key 'down)
         (cycle-color-mode! -1)
         (send this refresh)]
        ;; Page Up → jump forward 10 rules
        [(eq? key 'prior)
         (set-rule! (+ current-rule 10))
         (send this refresh)]
        ;; Page Down → jump back 10 rules
        [(eq? key 'next)
         (set-rule! (- current-rule 10))
         (send this refresh)]
        [else (void)]))))

(define automaton-canvas
  (new automaton-canvas%
       [parent frame]
       [style '(no-autoclear)]
       [paint-callback
        (λ (canvas dc)
          ;; Compute generations to fill the visible area
          (define-values (cw ch) (send canvas get-client-size))
          (define cols (quotient cw CELL-SIZE))
          (define rows (quotient ch CELL-SIZE))
          (set! generations (build-generations cols rows))

          ;; Clear background
          (send dc set-background (make-color 255 255 255))
          (send dc clear)

          ;; Draw each cell
          (define pal (current-palette))
          (send dc set-pen "black" 0 'transparent)
          (for* ([r (in-range rows)]
                 [c (in-range cols)])
            (let ([v (vector-ref (vector-ref generations r) c)])
              (when v
                (send dc set-brush (vector-ref pal v) 'solid)
                (send dc draw-rectangle
                      (* c CELL-SIZE) (* r CELL-SIZE)
                      CELL-SIZE CELL-SIZE)))))]))

;; Make sure canvas gets keyboard focus
(send automaton-canvas focus)

;; Show the window and enter the event loop
(send frame maximize #t)   ; start maximised
(send frame show #t)
