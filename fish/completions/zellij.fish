# Output of `zellij setup --generate-completion fish`
complete -c zellij -n "__fish_use_subcommand" -l max-panes -d 'Maximum panes on screen, caution: opening more panes will close old ones'
complete -c zellij -n "__fish_use_subcommand" -l data-dir -d 'Change where zellij looks for layouts and plugins'
complete -c zellij -n "__fish_use_subcommand" -l server -d 'Run server listening at the specified socket path'
complete -c zellij -n "__fish_use_subcommand" -s s -l session -d 'Specify name of a new session'
complete -c zellij -n "__fish_use_subcommand" -s l -l layout -d 'Name of a layout file in the layout directory'
complete -c zellij -n "__fish_use_subcommand" -l layout-path -d 'Path to a layout yaml file'
complete -c zellij -n "__fish_use_subcommand" -s c -l config -d 'Change where zellij looks for the configuration file'
complete -c zellij -n "__fish_use_subcommand" -l config-dir -d 'Change where zellij looks for the configuration directory'
complete -c zellij -n "__fish_use_subcommand" -s d -l debug
complete -c zellij -n "__fish_use_subcommand" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_use_subcommand" -s V -l version -d 'Prints version information'
complete -c zellij -n "__fish_use_subcommand" -f -a "options" -d 'Change the behaviour of zellij'
complete -c zellij -n "__fish_use_subcommand" -f -a "setup" -d 'Setup zellij and check its configuration'
complete -c zellij -n "__fish_use_subcommand" -f -a "list-sessions" -d 'List active sessions'
complete -c zellij -n "__fish_use_subcommand" -f -a "attach" -d 'Attach to session'
complete -c zellij -n "__fish_use_subcommand" -f -a "help" -d 'Prints this message or the help of the given subcommand(s)'
complete -c zellij -n "__fish_seen_subcommand_from options" -l theme -d 'Set the default theme'
complete -c zellij -n "__fish_seen_subcommand_from options" -l default-mode -d 'Set the default mode'
complete -c zellij -n "__fish_seen_subcommand_from options" -l default-shell -d 'Set the default shell'
complete -c zellij -n "__fish_seen_subcommand_from options" -l layout-dir -d 'Set the layout_dir, defaults to subdirectory of config dir'
complete -c zellij -n "__fish_seen_subcommand_from options" -l simplified-ui -d 'Allow plugins to use a more simplified layout that is compatible with more fonts'
complete -c zellij -n "__fish_seen_subcommand_from options" -l disable-mouse-mode
complete -c zellij -n "__fish_seen_subcommand_from options" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_seen_subcommand_from options" -s V -l version -d 'Prints version information'
complete -c zellij -n "__fish_seen_subcommand_from setup" -l generate-completion -d 'Generates completion for the specified shell'
complete -c zellij -n "__fish_seen_subcommand_from setup" -l dump-config -d 'Dump the default configuration file to stdout'
complete -c zellij -n "__fish_seen_subcommand_from setup" -l clean -d 'Disables loading of configuration file at default location, loads the defaults that zellij ships with'
complete -c zellij -n "__fish_seen_subcommand_from setup" -l check -d 'Checks the configuration of zellij and displays currently used directories'
complete -c zellij -n "__fish_seen_subcommand_from setup" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_seen_subcommand_from setup" -s V -l version -d 'Prints version information'
complete -c zellij -n "__fish_seen_subcommand_from list-sessions" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_seen_subcommand_from list-sessions" -s V -l version -d 'Prints version information'
complete -c zellij -n "__fish_seen_subcommand_from attach" -s f -l force -d 'Force attach- session will detach from the other zellij client (if any) and attach to this'
complete -c zellij -n "__fish_seen_subcommand_from attach" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_seen_subcommand_from attach" -s V -l version -d 'Prints version information'
complete -c zellij -n "__fish_seen_subcommand_from help" -s h -l help -d 'Prints help information'
complete -c zellij -n "__fish_seen_subcommand_from help" -s V -l version -d 'Prints version information'
