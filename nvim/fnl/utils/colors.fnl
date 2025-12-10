(local math (require :math))

(fn clamp [val lower upper]
  (-> val
      (math.max lower)
      (math.min upper)))

(fn clamp-encoded [val]
  (clamp val 0 255))

(fn round [val]
  (math.floor (+ val 0.5)))

(fn gamma-decode [c]
  "sRGB (0-255) → linear RGB (0-1) (one channel)"
  (let [c (/ c 255.0)]
    (if (<= c 0.04045)
        (/ c 12.92)
        (math.pow (/ (+ c 0.055) 1.055) 2.4))))

(fn gamma-encode [c]
  "linear RGB (0-1) → sRGB (0-255) (one channel)"
  (let [c (if (<= c 0.0031308)
              (* c 12.92)
              (- (* 1.055 (math.pow c (/ 1.0 2.4))) 0.055))
        c (+ (* c 255) 0.5)]
    (-> c
        (+ 0.5)
        (math.floor)
        (clamp-encoded))))

(fn hex->rgb [hex]
  (let [parse-slice (fn [start end]
                      (let [slice (hex:sub start end)]
                        (tonumber slice 16)))]
    (let [r (parse-slice 2 3)
          g (parse-slice 4 5)
          b (parse-slice 6 7)]
      [r g b])))

(fn rgb->hex [[r g b]]
  (string.format "#%02x%02x%02x" r g b))

(fn rgb->linear [rgb]
  (vim.tbl_map gamma-decode rgb))

(fn linear->rgb [lrgb]
  (vim.tbl_map gamma-encode lrgb))

;; RGB (0-255) → HSL (H: 0-360, S: 0-1, L: 0-1)
(fn rgb->hsl [rgb]
  (let [rgb (vim.tbl_map #(/ $1 255.0) rgb)
        max-c (-> rgb _G.unpack math.max)
        min-c (-> rgb _G.unpack math.min)
        [r g b] rgb
        delta (- max-c min-c)]
    (if (= delta 0)
        ;; Greyscale, hue and saturation are 0
        [0 0 max-c]
        ;; Chromatic
        (let [l (/ (+ max-c min-c) 2.0)
              s (/ delta (- 1 (math.abs (- (* 2 l) 1))))
              h (-> (match max-c
                      r (+ (/ (- g b) delta) 0)
                      g (+ (/ (- b r) delta) 2)
                      b (+ (/ (- r g) delta) 4))
                    (* 60)
                    (% 360))]
          [h s l]))))

;; HSL (H: 0-360, S: 0-1, L: 0-1) → RGB (0-255)
(fn hsl->rgb [[h s l]]
  (if (= s 0)
      ;; Greyscale
      (let [c (math.floor (+ (* l 255) 0.5))]
        [c c c])
      ;; Chromatic
      (let [c (* (- 1 (math.abs (- (* 2 l) 1))) s)
            x (* c (- 1 (math.abs (- (% (/ h 60) 2) 1))))
            m (- l (/ c 2)) ;; Still normalized
            rgb (if (< h 60) [c x 0]
                    (< h 120) [x c 0]
                    (< h 180) [0 c x]
                    (< h 240) [0 x c]
                    (< h 300) [x 0 c]
                    [c 0 x])
            denormalize #(-> $1
                             (+ m)
                             (* 255)
                             (round))]
        (vim.tbl_map denormalize rgb))))

(fn scale [factor arr]
  (vim.tbl_map #(* $1 factor) arr))

(fn lighten [color amount]
  (->> color
       (hex->rgb)
       (rgb->linear)
       (scale (+ 1 amount))
       (linear->rgb)
       (rgb->hex)))

;; (fn lighten [color amount]
;;   (let [[h s l] (-> color (hex->rgb) (rgb->hsl))
;;         ;; new-l (-> l
;;         ;;           (+ amount)
;;         ;;           (clamp 0 1))
;;         new-l (-> l
;;                   (* (+ 1 amount))
;;                   (clamp 0 1))
;;         ]
;;     (-> [h s new-l]
;;         (hsl->rgb)
;;         (rgb->hex))))
;;
(fn darken [color amount]
  (lighten color (- amount)))

(fn add [arr1 arr2]
  (icollect [i v1 (ipairs arr1)]
    (+ v1 (. arr2 i))))

(fn mix [color1 color2 weight1]
  (let [decode #(-> $1 (hex->rgb) (rgb->linear))
        encode #(-> $1 (linear->rgb) (rgb->hex))
        weight2 (- 1 weight1)]
    (encode (add (scale weight1 (decode color1))
                 (scale weight2 (decode color2))))))

(fn mk-named []
  (let [{: colors} (require :base16-colors)
        ext {:error colors.base08
             :hint colors.base0D
             :info colors.base0B
             :warn colors.base09
             :transparent colors.terminal_bg
             ;; FIXME: mid -> border?
             :mid (mix colors.fg0 colors.bg0 0.5)
             :border (mix colors.fg0 colors.bg0 0.5)
             :statusline colors.base02}
        colors (vim.tbl_extend :error colors ext)
        ;; darkened (collect [name color (pairs colors)]
        ;;            (values (.. "dark_" name) (darken color 0.2)))
        ;; lightened (collect [name color (pairs colors)]
        ;;             (values (.. "light_" name) (lighten color 0.2)))
        ]
    ;; FIXME: remove duplication
    ;; (vim.tbl_extend :force colors darkened lightened)
    colors
    ))

(local named-cache (mk-named))

(fn get-named []
  ;; (when (not named-cache)
  ;;   (set named-cache (mk-named)))
  named-cache)

{: lighten
 : darken
 ;; : scale
 : mix
 :dump (fn []
         (vim.api.nvim_put (vim.split (vim.inspect (get-named)) "\n") :l true
                           true))
 : get-named}
