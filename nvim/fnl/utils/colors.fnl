(local {: starts-with} (require :utils))

(fn starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

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

(fn clamp [val]
  (-> val
      (math.max 0)
      (math.min 255)))

(fn add-rgb [[r1 g1 b1] [r2 g2 b2]]
  (let [add-channel #(clamp (+ $1 $2))]
    [(add-channel r1 r2) (add-channel g1 g2) (add-channel b1 b2)]))

(fn scale-rgb [[r g b] weight]
  (let [scale-channel #(clamp (* weight $1))]
    [(scale-channel r) (scale-channel g) (scale-channel b)]))

(fn scale [color weight]
  (let [rgb (hex->rgb color)]
    (rgb->hex (scale-rgb rgb weight))))

(fn mix [color1 color2 weight]
  (let [rgb1 (hex->rgb color1)
        rgb2 (hex->rgb color2)]
    (rgb->hex (add-rgb (scale-rgb rgb1 weight) (scale-rgb rgb2 (- 1 weight))))))

(fn lighten [color weight]
  (scale color (+ 1 weight)))

(fn darken [color weight]
  (scale color (- 1 weight)))

(fn get-named []
  (let [{: colors} (require :base16-colors)
        ext {:error colors.base08
             :hint colors.base0D
             :info colors.base0B
             :warn colors.base09
             ;; FIXME: mid -> border?
             :mid (mix colors.fg0 colors.bg0 0.5)
             :border (mix colors.fg0 colors.bg0 0.5)
             :statusline colors.base02
             }]
    (vim.tbl_extend :error colors ext)))

{: lighten : darken : scale : mix : get-named}
