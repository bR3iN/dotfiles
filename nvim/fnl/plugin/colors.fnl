(local M {})

(var colors nil)

(fn starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn hexcolor? [str]
  (and (= (length str) 7)
       (starts-with str :#)))

(fn parse-color [color]
  (match (type color)
    :string (if (not (hexcolor? color))
              (error (.. color " is not a valid hex color code"))
              color)
    :number (. colors color)))

(fn hex->rgb [hex]
  (let [parse-slice (fn [start end]
                      (let [slice (hex:sub start end)]
                        (tonumber slice 16)))]
    (let [r (parse-slice 2 3)
          g (parse-slice 4 5)
          b (parse-slice 6 7)]
      [r g b])))

(fn rgb->hex [rgb]
  (string.format "#%02x%02x%02x" (table.unpack rgb)))

(fn into-bounds [val]
  (-> val
      (math.max 0)
      (math.min 255)))

(fn add-rgb [[r1 g1 b1] [r2 g2 b2]]
  (let [add-channel #(into-bounds (+ $1 $2))]
    [(add-channel r1 r2)
     (add-channel g1 g2)
     (add-channel b1 b2)]))

(fn scale-rgb [rgb weight]
  (let [scale-channel #(into-bounds (* weight $1))]
    (vim.tbl_map scale-channel rgb)))

(fn M.scale [color weight]
  (let [rgb (hex->rgb (parse-color color))]
    (rgb->hex (scale-rgb rgb weight))))

(fn M.mix [color1 color2 weight]
  (let [rgb1 (hex->rgb (parse-color color1))
        rgb2 (hex->rgb (parse-color color2))]
    (rgb->hex
      (add-rgb
        (scale-rgb rgb1 weight)
        (scale-rgb rgb2 (- 1 weight))))))

(fn M.lighten [color weight]
  (M.scale color (+ 1 weight)))

(fn M.darken [color weight]
  (M.scale color (- 1 weight)))

(fn M.highlight [name ?fg ?bg ?val]
 (let [fg (-?> ?fg
               (parse-color))
       bg (-?> ?bg
               (parse-color))]
   (let [extend (partial vim.tbl_extend :error)
         val (extend (or ?val {})
                     {: fg : bg})]
     (vim.api.nvim_set_hl 0 name val))))

{:init (fn [new-colors]
         (set colors new-colors)
         M)}
