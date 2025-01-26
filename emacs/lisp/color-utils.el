(require 'base16-colors)
(require 'color)

(defun base16-get (id)
  (if-let ((res (plist-get base16-colors id)))
          res
          (error (concat "no color with the name " (symbol-name id)))))

(defun rgb-mix (color1 color2 weight1)
  (defun mix (x y)
    (+ (* weight1 x)
       (* (- 1 weight1) y)))
  (pcase-let ((`(,r1 ,g1 ,b1) (color-name-to-rgb color1))
              (`(,r2 ,g2 ,b2) (color-name-to-rgb color2)))
             (color-rgb-to-hex (mix r1 r2) (mix g1 g2) (mix b1 b2))))

(defun color-of-region (&optional darken)
  (color-darken-name
   (base16-get :base02)
   (or darken 0)))

(defun colored-region (id weight &optional darken)
  (color-darken-name
    (rgb-mix
      (base16-get id)
      (base16-get :base02)
      weight)
    (or darken 0)))

(provide 'color-utils)
