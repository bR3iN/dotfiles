{:schema :hotpot/2
 :target :cache
 :ignore [:lua/**/*.lua]
 :compiler {:modules {:correlate true} :extra-compiler-env {: vim}}}
