#lang typed/racket/base

;; ams: edited Feb 27 2020 to add selectors
;; color-red, color-blue, color-green, color-alpha

(require (for-syntax racket/base
                     syntax/parse
                     racket/class
                     racket/draw)
         (only-in typed/racket/gui
                  Bitmap%
                  Font-Family Font-Style Font-Weight)
         typed/lang/posn)

; exposes the color names from racket/draw as a type
(define-syntax (define-color-database-types stx)
  (syntax-parse stx
    [(_ type-name:id)
     (define color-names (send the-color-database get-names))
     (with-syntax ([(color-name ...) (datum->syntax stx color-names)]
                   [(color-symbol ...) (datum->syntax stx (map string->symbol color-names))])
       #'(define-type type-name (U color-name ... 'color-symbol ...)))]))
(define-color-database-types ColorDatabase-Color)

;; opaque types
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/image
 ; 2.3.8 Image Predicates
 [#:opaque htdp:image image?]
 [#:opaque Color color?]
 [#:opaque Pen pen?])

; provide capitalized predicates as well
(provide
 (rename-out [color? Color?]
             [pen? Pen?]))

;; simple exports
;; ---------------------------------------------------------------------------------------------------

(require/typed/provide
 2htdp/image
 ; 2.3.1 Basic Images
;; [circle (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;; [ellipse (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[line (Real Real (U Pen Image-Color) -> Image)]
;; [add-line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
;; [add-curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
;; [add-solid-curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
;; [text (String Positive-Byte Image-Color -> Image)]
;; [text/font (String Positive-Byte Image-Color (Option String) Font-Family Font-Style Font-Weight Any -> Image)]
 [empty-image Image]
 ; 2.3.2 Polygons
;; [triangle (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;; [right-triangle (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[isosceles-triangle (Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
;;[triangle/sss (Nonnegative-Real Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;; [triangle/ass (Angle Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[triangle/sas (Nonnegative-Real Angle Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[triangle/ssa (Nonnegative-Real Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
;;[triangle/aas (Angle Angle Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[triangle/asa (Angle Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
;;[triangle/saa (Nonnegative-Real Angle Angle Mode (U Pen Image-Color) -> Image)]
;;[square (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[rectangle (Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;; [rhombus (Nonnegative-Real Angle Mode (U Pen Image-Color) -> Image)]
;;[star (Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[star-polygon (Nonnegative-Real Positive-Integer Positive-Integer Mode (U Pen Image-Color) -> Image)]
;;[radial-star (Positive-Integer Nonnegative-Real Nonnegative-Real Mode (U Pen Image-Color) -> Image)]
;;[regular-polygon (Nonnegative-Real Positive-Integer Mode (U Pen Image-Color) -> Image)]
;;[polygon ((Listof Posn) Mode (U Pen Image-Color) -> Image)]
;; [add-polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
;;[scene+polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
 ; 2.3.3 Overlaying Images
 [overlay (Image Image Image * -> Image)]
 [overlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [overlay/offset (Image Real Real Image -> Image)]
 [overlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [overlay/xy (Image Real Real Image -> Image)]
 [underlay (Image Image Image * -> Image)]
 [underlay/align (X-Place Y-Place Image Image Image * -> Image)]
 [underlay/offset (Image Real Real Image -> Image)]
 [underlay/align/offset (X-Place Y-Place Image Real Real Image -> Image)]
 [underlay/xy (Image Real Real Image -> Image)]
 [beside (Image Image Image * -> Image)]
 [beside/align (Y-Place Image Image Image * -> Image)]
 [above (Image Image Image * -> Image)]
 [above/align (X-Place Image Image Image * -> Image)]
 ; 2.3.4 Placing Images & Scenes
 [empty-scene ((Nonnegative-Real Nonnegative-Real) (Image-Color) . ->* . Image)]
 [place-image (Image Real Real Image -> Image)]
 [place-image/align (Image Real Real X-Place Y-Place Image -> Image)]
 [place-images ((Listof Image) (Listof Posn) Image -> Image)]
 [place-images/align ((Listof Image) (Listof Posn) X-Place Y-Place Image -> Image)]
 [scene+line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
 [scene+curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 ; 2.3.5 Rotating, Scaling, Flipping, Cropping, and Framing Images
 [rotate (Angle Image -> Image)]
;; [scale (Positive-Real Image -> Image)]
 [scale (Real Image -> Image)] ;; uc151
;;[scale/xy (Positive-Real Positive-Real Image -> Image)]
 [scale/xy (Real Real Image -> Image)] ;; uc151
 [flip-horizontal (Image -> Image)]
 [flip-vertical (Image -> Image)]
 [crop (Real Real Nonnegative-Real Nonnegative-Real Image -> Image)]
 [crop/align (X-Place Y-Place Nonnegative-Real Nonnegative-Real Image -> Image)]
 [frame (Image -> Image)]
 [color-frame ((U Pen Image-Color) Image -> Image)]
 ; 2.3.6 Bitmaps
 ; TODO: missing `bitmap` macro
 [bitmap/url (String -> Image)]
 [bitmap/file (Path-String -> Image)]
 [image->color-list (Image -> (Listof Color))]
 [color-list->bitmap ((Listof Color) Nonnegative-Real Nonnegative-Real -> Image)]
 [freeze (case->
          (Image -> Image)
          (Nonnegative-Real Nonnegative-Real Image -> Image)
          (Real Real Nonnegative-Real Nonnegative-Real Image -> Image))]
 ; 2.3.7 Image Properties
 [image-width (Image -> Nonnegative-Integer)]
 [image-height (Image -> Nonnegative-Integer)]
 [image-baseline (Image -> Nonnegative-Integer)]
 ; 2.3.8 Image Predicates
 [mode? (Any -> Boolean)]
 [image-color? (Any -> Boolean)]
 [color ((Byte Byte Byte) (Byte) . ->* . Color)]
 [make-color ((Byte Byte Byte) (Byte) . ->* . Color)]
 [y-place? (Any -> Boolean)]
 [x-place? (Any -> Boolean)]
 [angle? (Any -> Boolean)]
 [side-count? (Any -> Boolean)]
 [step-count? (Any -> Boolean)]
 [real-valued-posn? (Any -> Boolean)]
 [pen (Image-Color Nonnegative-Real Pen-Style Pen-Cap Pen-Join -> Pen)]
 [make-pen (Image-Color Nonnegative-Real Pen-Style Pen-Cap Pen-Join -> Pen)]
 ; 2.3.10 Pinholes
 [center-pinhole (Image -> Image)]
 [put-pinhole (Integer Integer Image -> Image)]
 [pinhole-x (Image -> (Option Integer))]
 [pinhole-y (Image -> (Option Integer))]
 [clear-pinhole (Image -> Image)]
 [overlay/pinhole (Image Image Image * -> Image)]
 [underlay/pinhole (Image Image Image * -> Image)]
 ; 2.3.11 Exporting Images to Disk
 [save-image ((Image Path-String) (Nonnegative-Real Nonnegative-Real) . ->* . Boolean)]
 [save-svg-image ((Image Path-String) (Nonnegative-Real Nonnegative-Real) . ->* . Boolean)])

;; derived / reimplemented exports
;; ---------------------------------------------------------------------------------------------------

(provide
 Image image?
 Image-Color Image-Color? image-color?
 Mode Mode? mode?
 Angle Angle?
 X-Place Y-Place X-Place? Y-Place?
 Pen-Style Pen-Cap Pen-Join Pen-Style? Pen-Cap? Pen-Join?
 Font-Family Font-Style Font-Weight Font-Family? Font-Style? Font-Weight?)

;; derived type declarations
;; ---------------------------------------------------------------------------------------------------

(define-type Image (U htdp:image (Instance Bitmap%))) ; TODO: missing Image-Snip% instance possibility
(define-type Image-Color (U Color ColorDatabase-Color))
(define-type Mode (U 'solid "solid" 'outline "outline" Byte))
(define-type Angle Real) ; more specifically, Finite-NonNaN-Real, which isn't a type
(define-type X-Place (U 'left 'right 'middle 'center 'pinhole
                        "left" "right" "middle" "center" "pinhole"))
(define-type Y-Place (U 'top 'bottom 'middle 'center 'baseline 'pinhole
                        "top" "bottom" "middle" "center" "baseline" "pinhole"))
(define-type Pen-Style (U 'solid 'dot 'long-dash 'short-dash 'dot-dash
                          "solid" "dot" "long-dash" "short-dash" "dot-dash"))
(define-type Pen-Cap (U 'round 'projecting 'butt "round" "projecting" "butt"))
(define-type Pen-Join (U 'round 'bevel 'miter "round" "bevel" "miter"))

(define-predicate Image-Color? Image-Color)
(define-predicate Mode? Mode)
(define-predicate Angle? Angle)
(define-predicate X-Place? X-Place)
(define-predicate Y-Place? Y-Place)
(define-predicate Pen-Style? Pen-Style)
(define-predicate Pen-Cap? Pen-Cap)
(define-predicate Pen-Join? Pen-Join)

(define-predicate Font-Family? Font-Family)
(define-predicate Font-Style? Font-Style)
(define-predicate Font-Weight? Font-Weight)

;; UChicago CS 151 modifications
;; provide dynamic errors rather than static type errors
;; for potential negative dimensions or invalid color names

(: cs151-image-version : String)
(define cs151-image-version "1.0")
(provide cs151-image-version)

(: cs151-image-date : String)
(define cs151-image-date "September 26, 2017")
(provide cs151-image-date)

(require/typed 2htdp/image
 [circle (Real Mode (U Pen Image-Color) -> Image)]
 [triangle (Real Mode (U Pen Image-Color) -> Image)]
 [square (Real Mode (U Pen Image-Color) -> Image)]
 [rectangle (Real Real Mode (U Pen Image-Color) -> Image)]
 [ellipse (Real Real Mode (U Pen Image-Color) -> Image)]
 [line (Real Real (U Pen Image-Color) -> Image)]
 [add-line (Image Real Real Real Real (U Pen Image-Color) -> Image)]
 [add-curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 [add-solid-curve (Image Real Real Angle Real Real Real Angle Real (U Pen Image-Color) -> Image)]
 [text (String Byte Image-Color -> Image)]
 [text/font (String Byte Image-Color (Option String) Font-Family Font-Style Font-Weight Any -> Image)]
 [right-triangle (Real Real Mode (U Pen Image-Color) -> Image)]
 [isosceles-triangle (Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/sss (Real Real Real Mode (U Pen Image-Color) -> Image)]
 [triangle/ass (Angle Real Real Mode (U Pen Image-Color) -> Image)]
 [triangle/sas (Real Angle Real Mode (U Pen Image-Color) -> Image)]
 [triangle/ssa (Real Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/aas (Angle Angle Real Mode (U Pen Image-Color) -> Image)]
 [triangle/asa (Angle Real Angle Mode (U Pen Image-Color) -> Image)]
 [triangle/saa (Real Angle Angle Mode (U Pen Image-Color) -> Image)]
 [rhombus (Real Angle Mode (U Pen Image-Color) -> Image)]
 [star (Real Mode (U Pen Image-Color) -> Image)]
 [star-polygon (Real Integer Integer Mode (U Pen Image-Color) -> Image)]
 [radial-star (Integer Real Real Mode (U Pen Image-Color) -> Image)]
 [regular-polygon (Real Integer Mode (U Pen Image-Color) -> Image)]
 [polygon ((Listof Posn) Mode (U Pen Image-Color) -> Image)]
 [add-polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
 [scene+polygon (Image (Listof Posn) Mode (U Pen Image-Color) -> Image)]
 [color-red   (Image-Color -> Integer)]
 [color-green (Image-Color -> Integer)]
 [color-blue  (Image-Color -> Integer)]
 [color-alpha (Image-Color -> Integer)]
 )

(: uchicago151-circle : Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-circle radius mode color)
  (circle radius (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-circle circle]))

(: uchicago151-triangle : Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle side mode color)
  (triangle side (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle triangle]))

(: uchicago151-square : Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-square side mode color)
  (square side (cast mode Mode) (cast (cast color Image-Color) (U Pen Image-Color))))
(provide (rename-out [uchicago151-square square]))

(: uchicago151-rectangle : Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-rectangle width height mode color)
  (rectangle width height (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-rectangle rectangle]))

(: uchicago151-ellipse : Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-ellipse a b mode color)
  (ellipse a b (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-ellipse ellipse]))

(: uchicago151-line : Real Real (U String Pen Image-Color) -> Image)
(define (uchicago151-line a b color)
  (line a b (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-line line]))

(: uchicago151-add-line : Image Real Real Real Real (U String Pen Image-Color) -> Image)
(define (uchicago151-add-line a b c d e color)
  (add-line a b c d e (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-add-line add-line]))

(: uchicago151-add-curve : Image Real Real Angle Real Real Real Angle Real (U String Pen Image-Color) -> Image)
(define (uchicago151-add-curve a b c d e f g h i color)
  (add-curve a b c d e f g h i (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-add-curve add-curve]))

(: uchicago151-add-solid-curve : Image Real Real Angle Real Real Real Angle Real (U String Pen Image-Color) -> Image)
(define (uchicago151-add-solid-curve a b c d e f g h i color)
  (add-solid-curve a b c d e f g h i (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-add-solid-curve add-solid-curve]))

(: uchicago151-text : String Byte (U String Image-Color) -> Image)
(define (uchicago151-text str size color)
  (text str size (cast color Image-Color)))
(provide (rename-out [uchicago151-text text]))

(: uchicago151-text/font : String Byte (U String Image-Color) (Option String) Font-Family Font-Style Font-Weight Any -> Image)
(define (uchicago151-text/font str size color opt family style weight any)
  (text/font str size (cast color Image-Color) opt family style weight any))
(provide (rename-out [uchicago151-text/font text/font]))

(: uchicago151-right-triangle : Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-right-triangle a b mode color)
  (right-triangle a b (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-right-triangle right-triangle]))

(: uchicago151-isosceles-triangle : Real Angle (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-isosceles-triangle s a mode color)
  (isosceles-triangle s a (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-isosceles-triangle isosceles-triangle]))

(: uchicago151-triangle/sss : Real Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/sss s1 s2 s3 mode color)
  (triangle/sss s1 s2 s3 (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/sss triangle/sss]))

(: uchicago151-triangle/ass : Angle Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/ass a s1 s2 mode color)
  (triangle/ass a s1 s2 (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/ass triangle/ass]))

(: uchicago151-triangle/sas : Real Angle Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/sas s1 a s2 mode color)
  (triangle/sas s1 a s2 (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/sas triangle/sas]))

(: uchicago151-triangle/ssa : Real Real Angle (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/ssa s1 s2 a mode color)
  (triangle/ssa s1 s2 a (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/ssa triangle/ssa]))

(: uchicago151-triangle/aas : Angle Angle Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/aas a1 a2 s mode color)
  (triangle/aas a1 a2 s (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/aas triangle/aas]))

(: uchicago151-triangle/asa : Angle Real Angle (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/asa a1 s a2 mode color)
  (triangle/asa a1 s a2 (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/asa triangle/asa]))

(: uchicago151-triangle/saa : Real Angle Angle (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-triangle/saa s a1 a2 mode color)
  (triangle/saa s a1 a2 (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-triangle/saa triangle/saa]))

(: uchicago151-rhombus : Real Angle (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-rhombus s a mode color)
  (rhombus s a (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-rhombus rhombus]))

(: uchicago151-star : Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-star s mode color)
  (star s (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-star star]))

(: uchicago151-star-polygon : Real Integer Integer (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-star-polygon a b c mode color)
(star-polygon a b c (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-star-polygon star-polygon]))

(: uchicago151-radial-star : Integer Real Real (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-radial-star a b c mode color)
  (radial-star a b c (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-radial-star radial-star]))

(: uchicago151-regular-polygon : Real Integer (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-regular-polygon s n mode color)
  (regular-polygon s n (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-regular-polygon regular-polygon]))

(: uchicago151-polygon : (Listof Posn) (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-polygon vs mode color)
  (polygon vs (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-polygon polygon]))

(: uchicago151-add-polygon : Image (Listof Posn) (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-add-polygon n vs mode color)
  (add-polygon n vs (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-add-polygon add-polygon]))

(: uchicago151-scene+polygon : Image (Listof Posn) (U String Mode) (U String Pen Image-Color) -> Image)
(define (uchicago151-scene+polygon s vs mode color)
  (scene+polygon s vs (cast mode Mode) (cast color (U Pen Image-Color))))
(provide (rename-out [uchicago151-scene+polygon scene+polygon]))

(: pixel : Image-Color -> Image)
(define (pixel c)
  (square 1 'solid c))

(: color-component : (Color -> Integer) -> (Image-Color -> Integer))
(define ((color-component selector) color)
  (if (or (string? color) (symbol? color))
      (selector (car (image->color-list (pixel color))))
      (selector color)))

(: uchicago151-color-red : Image-Color -> Integer)
(define uchicago151-color-red
  (color-component color-red))
(provide (rename-out [uchicago151-color-red color-red]))

(: uchicago151-color-green : Image-Color -> Integer)
(define uchicago151-color-green
  (color-component color-green))
(provide (rename-out [uchicago151-color-green color-green]))

(: uchicago151-color-blue : Image-Color -> Integer)
(define uchicago151-color-blue
  (color-component color-blue))
(provide (rename-out [uchicago151-color-blue color-blue]))

(: uchicago151-color-alpha : Image-Color -> Integer)
(define uchicago151-color-alpha
  (color-component color-alpha))
(provide (rename-out [uchicago151-color-alpha color-alpha]))
