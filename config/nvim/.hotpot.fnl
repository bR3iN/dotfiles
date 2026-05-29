{:schema :hotpot/2
 :target :cache
 :ignore [:lua/**/*.lua]
 :compiler {:modules {:correlate true}
            ;; Restores default fennel behavior
            ;; :allowedGlobals false
            :extra-compiler-env {: vim}}}
