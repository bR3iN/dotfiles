(let [sumneko-root (.. vim.env.HOME :/Github/sumneko-lua)
      sumneko-bin (.. sumneko_root :/bin/Linux/lua-language-server)]
  {:cmd [sumneko-bin :-E (.. sumneko-root :/main.lua)]
   :settings {:Lua {:runtime :LuaJIT
                    :path (vim.split package.path ";")
                    :diagnostics {:globals [:vim]}
                    :workspace {:library
                                {(vim.fn.expand :$VIMRUNTIME/lua) true
                                 (vim.fn.expand :$VIMRUNTIME/lua/vim/lsp) true}}
                    :telemetry {:enable false}}}})
