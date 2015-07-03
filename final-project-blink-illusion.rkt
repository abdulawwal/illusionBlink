(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; Design a World Program with Compound Data. You can be as creative as you like,
; but keep it simple. Above all, follow the recipes!
; You must also stay within the scope of the first part of the course.
; Do not use language features we have not seen in the videos.


; Blinking illusion fools our brain that two small circles are not blinking
; (or changing color) at the same time.
; Design a World program that shows this illusion.


;; =================
;; Domain analysis:
;; Constants:
;; - WIDTH    - scene width
;; - HEIGHT   - scene height
;; - TEXT     - background text img
;; - MTS-GRAY - gray background img
;; - C2-BLACK - black bigger circle img
;; - C2-WHITE - white bigger circle img
;; - R        - small circles radius
;; - C-BLACK  - black smaller circle img
;; - C-WHITE  - white smaller circle img
;; - BW       - black and white background img
;; - W-CIRCLES - two white circles
;; - B-CIRCLES - two black circles
;; Changing:
;; - two circles color (black and white)
;; - backgrounds (black-white and gray)
;; BIG-BANG:
;; - on-tick
;; - to-draw
;; - on-key
;; =================
;; Constants:
(define WIDTH 400)
(define HEIGHT 400)
(define TEXT (text "PRESS SPACE" 20 "light gray"))
(define MTS-GRAY (place-image TEXT
                              (/ WIDTH 2)
                              (image-height TEXT)
                              (empty-scene WIDTH HEIGHT "gray")))
(define C2-BLACK (circle (/ WIDTH 4) "solid" "black"))
(define C2-WHITE (circle (/ WIDTH 4) "solid" "white"))
(define R (/ (image-width C2-BLACK) 4))
(define C-BLACK (circle R "solid" "black"))
(define C-WHITE (circle R "solid" "white"))
(define BW (overlay
            (overlay/offset C2-BLACK (image-height C2-BLACK) 0 C2-WHITE) 
            MTS-GRAY))
(define W-CIRCLES (overlay/offset C-WHITE (/ WIDTH 2) 0 C-WHITE))
(define B-CIRCLES (overlay/offset C-BLACK (/ WIDTH 2) 0 C-BLACK))
;; =================
;; Data definitions:
(define-struct illusion-state (circles background))
;; IllusionState is (make-illusion-state Image String)
;; interp. (make-illusion-state circles background)
;;          circles means image of two circles with colors black or white
;;          (W-CIRCLES or B-CIRCLES)
;;          background means gray or black-white background
;;          ("gray" or "black-white")
(define IS1 (make-illusion-state B-CIRCLES "black-white"))
(define IS2 (make-illusion-state W-CIRCLES "gray"))
(define IS3 (make-illusion-state B-CIRCLES "gray"))
(define IS4 (make-illusion-state W-CIRCLES "black-white")) 
#;
(define (fn-for-illusion-state is)
  (... (illusion-state-circles is)
       (illusion-state-background is)))      
;; Template rules used:
;;  - compound: 2 fields
;; =================
;; World - Illusion animation
;; Functions:

;; IllusionState -> IllusionState
;; start the world with (main (IS1))

(define (main is)
  (big-bang is                      ; IllusionState
            (on-tick   tock 0.1)    ; IllusionState -> IllusionState
            (to-draw   render)      ; IllusionState KeyEvent -> Image
            (on-key    key-event))) ; IllusionState KeyEvent -> IllusionState

;; IllusionState -> IllusionState
;; produce the next IllusionState informations for render func.
(check-expect (tock IS1) (make-illusion-state W-CIRCLES "black-white"))
(check-expect (tock IS2) (make-illusion-state B-CIRCLES "gray")) 
(check-expect (tock IS3) (make-illusion-state W-CIRCLES "gray"))
(check-expect (tock IS4) (make-illusion-state B-CIRCLES "black-white")) 

;(define (tock is) (make-illusion-state C-WHITE "black-white"))  ; stub

;; <template used from IllusionState>
(define (tock is)
  (if (image=? (illusion-state-circles is) B-CIRCLES)
      (make-illusion-state W-CIRCLES (illusion-state-background is))
      (make-illusion-state B-CIRCLES (illusion-state-background is)))) 

;; IllusionState -> Image 
;; render images for animation 
(check-expect (render IS1) (overlay B-CIRCLES BW))
(check-expect (render IS2) (overlay W-CIRCLES MTS-GRAY))
(check-expect (render IS3) (overlay B-CIRCLES MTS-GRAY))
(check-expect (render IS4) (overlay W-CIRCLES BW))

;(define (render is) (circle 25 "solid" "red"))  ;stub

;; <template used from IllusionState>
(define (render is)
  (if (string=? (illusion-state-background is) "black-white") 
      (overlay (illusion-state-circles is) BW)
      (overlay (illusion-state-circles is) MTS-GRAY)))  

;; IllusionState KeyEvent -> IllusionState
;; when space key is pressed switch backgrounds
(check-expect (key-event IS1 " ") (make-illusion-state B-CIRCLES "gray"))
(check-expect (key-event IS2 " ") (make-illusion-state W-CIRCLES "black-white")) 
(check-expect (key-event IS3 " ") (make-illusion-state B-CIRCLES "black-white"))
(check-expect (key-event IS4 " ") (make-illusion-state W-CIRCLES "gray"))

;(define (key-event is ke) IS1) ;stub

;; <template used from IllusionState>
(define (key-event is ke)
  (cond
    [(and (key=? " " ke)
          (string=?
           (illusion-state-background is)
           "black-white")) 
     (make-illusion-state (illusion-state-circles is)
                          "gray")]
    [(and (key=? " " ke)
          (string=?
           (illusion-state-background is)
           "gray")) 
     (make-illusion-state (illusion-state-circles is)
                          "black-white")]
    [else
     (make-illusion-state (illusion-state-circles is)
                          (illusion-state-background is))]))
; ------------------------------------------------------------------------------
(main IS1)
