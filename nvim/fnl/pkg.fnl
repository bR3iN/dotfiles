(local {: nmap!} (require :utils.nvim))
(local uv vim.loop)
(local {: starts-with} (require :utils))
(local {: spawn-with-callback : scan-dir} (require :utils.async))

; Package directory
(local pkg-dir (.. vim.env.HOME "/.local/share/nvim/site/pack/pkgs/start"))

; Global package list
(var pkgs {})

(fn reponame->dirname [reponame]
  (-> reponame
      (string.gsub :/ :_)
      (string.gsub "%." :_)))

(fn reponame->url [reponame]
  (.. "https://github.com/" reponame))

(fn dirname->path [dirname]
  (.. pkg-dir :/ dirname))

(fn reponame->path [reponame]
  (-> reponame
      (reponame->dirname)
      (dirname->path)))

(fn rm-and-report [dirname]
  (let [path (dirname->path dirname)]
    (spawn-with-callback
      [:rm :-r path]
      (fn [code]
        (let [msg (if (= code 0)
                    (.. "Removed " path)
                    (.. "Failed to remove " path))]
          (print msg))))))

(fn dir? [path]
  (-> path
       vim.fn.isdirectory
       (not= 0)))

(fn git-repo? [path]
  (-> path
       (.. :/.git)
       dir?))

(fn clean! []
  (let [pkg-dirnames (vim.tbl_map reponame->dirname pkgs)
        remove? (fn [dirname]
                  (not (vim.tbl_contains pkg-dirnames dirname)))]
    (scan-dir
      pkg-dir
      (fn [filename filetype]
        (if (and (= filetype :directory)
                 (remove? filename))
          (rm-and-report filename))))))

(fn gen-helptags [path]
  (let [doc-path (.. path :/doc)]
    (if (dir? doc-path)
      (vim.cmd (.. ":helptags " path :/doc)))))

(fn packloadall! []
  (vim.cmd :packloadall!))

(fn unrequire [mod]
  (tset _G.package.loaded mod nil))

(fn action->callback [action reponame]
  (let [dirname (reponame->dirname reponame)
        config-module (.. :configs. dirname)]
    (match action
      nil nil
      :load-config (fn []
                     (unrequire config-module)
                     (require config-module))
      :setup (fn [?arg]
               (unrequire config-module)
               (let [config (require config-module)]
                 (config.setup ?arg)))
      other (error (.. "Unrecognized action '" other ";")))))

(fn add! [reponame action ?arg]
  (table.insert pkgs reponame)
  (let [?cb (action->callback action reponame)
        path (reponame->path reponame)]
    (if (dir? path)
      (if ?cb (?cb ?arg))
      (let [url (reponame->url reponame)]
        (spawn-with-callback
          [:git :clone url path]
          (fn [code]
            (print (.. "Installed " reponame))
            (gen-helptags path)
            (packloadall!)
            (if ?cb (?cb ?arg))))))))

(fn list! []
  (table.sort pkgs)
  (print "Plugins:")
  (let [indent (partial .. "  ")]
    (vim.tbl_map #(print (indent $1)) pkgs)))

(fn update! []
  (scan-dir
    pkg-dir
    (fn [fname ftype]
      (let [path (dirname->path fname)]
        (if (and (= ftype :directory)
                 (git-repo? path))
          (spawn-with-callback
            [:git :pull]
            (fn [code]
              (if (= code 0)
                (gen-helptags path)
                (packloadall!)))
            {:cwd path}))))))

(fn init []
  ; Reset internal package list
  (set pkgs {})
  ; Create package directory if necessary
  (vim.fn.mkdir pkg-dir :p))

(nmap! "<Plug>PkgUpdate" #(update!))
(nmap! "<Plug>PkgList"   #(list!))
(nmap! "<Plug>PkgClean"  #(clean!))

{
 : add!
 : init
 }
