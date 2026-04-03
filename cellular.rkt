#lang racket
(require racket/gui racket/draw)

;; ── Configuration ──────────────────────────────────────────
(define CELL-SIZE 6)          ; pixels per cell side
(define current-rule 30)      ; Wolfram rule (0–255), mutable
(define current-color-mode 0) ; cycles through palettes
(define SCROLL-INTERVAL 50)   ; ms between scroll steps (~20 fps)

;; ── Color palettes ─────────────────────────────────────────
;; Each palette maps neighbourhood index 0–7 to a color for alive cells.

(define palette-bw          ; 0: Classic black & white
  (vector (make-color   0   0   0) (make-color   0   0   0)
          (make-color   0   0   0) (make-color   0   0   0)
          (make-color   0   0   0) (make-color   0   0   0)
          (make-color   0   0   0) (make-color   0   0   0)))

(define palette-vivid       ; 1: Vivid state colors
  (vector (make-color  30  30  30) (make-color  29 145 192)
          (make-color 127  72 163) (make-color  44 162  95)
          (make-color 227  74  51) (make-color 253 187  45)
          (make-color  70 130 180) (make-color 231  41 138)))

(define palette-ocean       ; 2: Ocean blues & greens
  (vector (make-color   8  29  88) (make-color  37  52 148)
          (make-color  34  94 168) (make-color  29 145 192)
          (make-color  65 182 196) (make-color 127 205 187)
          (make-color 199 233 180) (make-color 237 248 177)))

(define palette-fire        ; 3: Fire – dark red to bright yellow
  (vector (make-color  60   0   0) (make-color 128   0  10)
          (make-color 180  18   0) (make-color 215  48   5)
          (make-color 245 100   0) (make-color 253 170  20)
          (make-color 254 217  80) (make-color 255 255 178)))

(define palette-neon        ; 4: Neon on dark
  (vector (make-color  20  20  20) (make-color   0 255 136)
          (make-color 255  20 147) (make-color   0 191 255)
          (make-color 255 255   0) (make-color 138  43 226)
          (make-color   0 255 255) (make-color 255 105  80)))

(define palette-earth       ; 5: Earth tones
  (vector (make-color  60  40  20) (make-color 120  72  36)
          (make-color 168 120  60) (make-color 192 156  96)
          (make-color 120 144  72) (make-color  72 132  84)
          (make-color 192 180 144) (make-color 228 216 180)))

(define palette-synthwave   ; 6: Synthwave / retrowave
  (vector (make-color  10   3  40) (make-color  72  15 115)
          (make-color 162  30 132) (make-color 228  60  92)
          (make-color 255 130  80) (make-color 255 200 100)
          (make-color  30 180 230) (make-color 100 220 255)))

(define palette-pastel      ; 7: Soft pastels
  (vector (make-color 180 180 180) (make-color 255 179 186)
          (make-color 255 223 186) (make-color 255 255 186)
          (make-color 186 255 201) (make-color 186 225 255)
          (make-color 218 186 255) (make-color 255 186 243)))

(define palettes
  (vector palette-bw palette-vivid palette-ocean palette-fire
          palette-neon palette-earth palette-synthwave palette-pastel))

(define color-mode-names
  (vector "Black & White" "Vivid" "Ocean" "Fire"
          "Neon" "Earth" "Synthwave" "Pastel"))

(define NUM-COLOR-MODES (vector-length palettes))

(define (current-palette)
  (vector-ref palettes current-color-mode))

;; Pre-built brushes for each palette (avoids creating objects every frame)
(define brush-sets
  (for/vector ([pal (in-vector palettes)])
    (for/vector ([c (in-vector pal)])
      (new brush% [color c] [style 'solid]))))

(define (current-brushes)
  (vector-ref brush-sets current-color-mode))

(define bg-brush (new brush% [color (make-color 255 255 255)] [style 'solid]))
(define no-pen (new pen% [style 'transparent]))

;; ── Rule logic ─────────────────────────────────────────────
(define (make-rule-table n)
  (for/vector ([i (in-range 8)])
    (bitwise-bit-set? n i)))

(define rule-table (make-rule-table current-rule))

(define (cell-alive? v) (and v #t))

(define (next-generation row)
  (define len (vector-length row))
  (for/vector ([i (in-range len)])
    (let* ([l (cell-alive? (if (> i 0)         (vector-ref row (- i 1)) #f))]
           [c (cell-alive? (vector-ref row i))]
           [r (cell-alive? (if (< i (- len 1)) (vector-ref row (+ i 1)) #f))]
           [idx (+ (if l 4 0) (if c 2 0) (if r 1 0))])
      (if (vector-ref rule-table idx) idx #f))))

(define (make-initial-row width)
  (define row (make-vector width #f))
  (vector-set! row (quotient width 2) 2)
  row)

;; ── Scrolling state & offscreen bitmap ─────────────────────
(define row-buffer #f)   ; vector of row-vectors
(define buf-cols 0)
(define buf-rows 0)
(define scrolling? #t)
(define offscreen #f)    ; bitmap for double-buffering
(define offscreen-dc #f) ; drawing context for the bitmap
(define needs-full-redraw? #t)

;; Draw a single row onto the offscreen bitmap at pixel-row y
(define (draw-row-to-bitmap! row-data screen-y)
  (define brushes (current-brushes))
  (define cols (vector-length row-data))
  ;; clear this row to white
  (send offscreen-dc set-brush bg-brush)
  (send offscreen-dc draw-rectangle 0 screen-y (* cols CELL-SIZE) CELL-SIZE)
  ;; draw alive cells
  (for ([c (in-range cols)])
    (define v (vector-ref row-data c))
    (when v
      (send offscreen-dc set-brush (vector-ref brushes v))
      (send offscreen-dc draw-rectangle
            (* c CELL-SIZE) screen-y CELL-SIZE CELL-SIZE))))

;; Fill buffer from scratch with seed + generations, redraw bitmap fully
(define (init-buffer! cols rows)
  (set! buf-cols cols)
  (set! buf-rows rows)
  (define buf (make-vector rows #f))
  (vector-set! buf 0 (make-initial-row cols))
  (for ([g (in-range 1 rows)])
    (vector-set! buf g (next-generation (vector-ref buf (- g 1)))))
  (set! row-buffer buf)
  ;; Create/resize offscreen bitmap
  (define pw (* cols CELL-SIZE))
  (define ph (* rows CELL-SIZE))
  (set! offscreen (make-bitmap pw ph))
  (set! offscreen-dc (new bitmap-dc% [bitmap offscreen]))
  (send offscreen-dc set-pen no-pen)
  (send offscreen-dc set-smoothing 'unsmoothed)
  ;; Draw all rows
  (send offscreen-dc set-background (make-color 255 255 255))
  (send offscreen-dc clear)
  (for ([r (in-range rows)])
    (draw-row-to-bitmap! (vector-ref buf r) (* r CELL-SIZE)))
  (set! needs-full-redraw? #f))

;; Scroll: shift bitmap up by CELL-SIZE pixels, compute + draw only the new row
(define (scroll-step!)
  (when (and row-buffer offscreen-dc (> buf-rows 1))
    ;; Shift the row-buffer
    (for ([i (in-range (- buf-rows 1))])
      (vector-set! row-buffer i (vector-ref row-buffer (+ i 1))))
    (define new-row (next-generation (vector-ref row-buffer (- buf-rows 2))))
    (vector-set! row-buffer (- buf-rows 1) new-row)
    ;; Shift bitmap pixels up by CELL-SIZE
    (define pw (send offscreen get-width))
    (define ph (send offscreen get-height))
    (send offscreen-dc draw-bitmap-section
          offscreen 0 0   ; dest x,y
          0 CELL-SIZE      ; src x,y
          pw (- ph CELL-SIZE))  ; src w,h
    ;; Clear and draw the new bottom row
    (draw-row-to-bitmap! new-row (* (- buf-rows 1) CELL-SIZE))))

;; ── GUI ────────────────────────────────────────────────────
;; Custom frame that actually exits the process on close
(define app-frame%
  (class frame%
    (super-new)
    (define/augment (on-close)
      (exit 0))))

(define frame
  (new app-frame%
       [label "Cellular Automata"]
       [width 1200]
       [height 800]))

(define (update-title!)
  (send frame set-label
        (format "Rule ~a  |  ~a~a"
                current-rule
                (vector-ref color-mode-names current-color-mode)
                (if scrolling? "" "  [PAUSED]"))))

(define (set-rule! n)
  (set! current-rule (modulo n 256))
  (set! rule-table (make-rule-table current-rule))
  (when (and (> buf-cols 0) (> buf-rows 0))
    (init-buffer! buf-cols buf-rows))
  (update-title!))

(define (cycle-color-mode! dir)
  (set! current-color-mode
        (modulo (+ current-color-mode dir) NUM-COLOR-MODES))
  ;; Need to redraw bitmap with new colors
  (set! needs-full-redraw? #t)
  (when (and (> buf-cols 0) (> buf-rows 0) offscreen-dc)
    (send offscreen-dc set-background (make-color 255 255 255))
    (send offscreen-dc clear)
    (for ([r (in-range buf-rows)])
      (draw-row-to-bitmap! (vector-ref row-buffer r) (* r CELL-SIZE)))
    (set! needs-full-redraw? #f))
  (update-title!))

;; Custom canvas with keyboard handling
(define automaton-canvas%
  (class canvas%
    (super-new)
    (define/override (on-char event)
      (define key (send event get-key-code))
      (cond
        [(eq? key 'right)
         (set-rule! (+ current-rule 1))
         (send this refresh)]
        [(eq? key 'left)
         (set-rule! (- current-rule 1))
         (send this refresh)]
        [(eq? key 'up)
         (cycle-color-mode! 1)
         (send this refresh)]
        [(eq? key 'down)
         (cycle-color-mode! -1)
         (send this refresh)]
        [(eq? key 'prior)
         (set-rule! (+ current-rule 10))
         (send this refresh)]
        [(eq? key 'next)
         (set-rule! (- current-rule 10))
         (send this refresh)]
        [(eq? key #\space)
         (set! scrolling? (not scrolling?))
         (update-title!)]
        [else (void)]))))

(define (paint-canvas canvas dc)
  (define-values (cw ch) (send canvas get-client-size))
  (define cols (max 1 (quotient cw CELL-SIZE)))
  (define rows (max 1 (quotient ch CELL-SIZE)))

  ;; Init buffer on first paint or resize
  (when (or (not row-buffer)
            (not (= cols buf-cols))
            (not (= rows buf-rows)))
    (init-buffer! cols rows))

  ;; Just blit the offscreen bitmap — fast!
  (when offscreen
    (send dc draw-bitmap offscreen 0 0)))

(define automaton-canvas
  (new automaton-canvas%
       [parent frame]
       [style '(no-autoclear)]
       [paint-callback paint-canvas]))

;; ── Scroll timer ───────────────────────────────────────────
(define scroll-timer
  (new timer%
       [notify-callback
        (lambda ()
          (when scrolling?
            (scroll-step!)
            (send automaton-canvas refresh)))]
       [interval SCROLL-INTERVAL]))

;; ── Launch ─────────────────────────────────────────────────
(send frame show #t)
(send frame maximize #t)
(send automaton-canvas focus)
(update-title!)
