c.url.start_pages = ["about:blank"]

c.downloads.position = 'bottom'
c.downloads.remove_finished = 60000

c.messages.timeout = 3000

## Keybindings
config.bind('gm', 'bookmark-add')
config.bind('<F1>', 'open -t https://raw.githubusercontent.com/qutebrowser/qutebrowser/master/doc/img/cheatsheet-big.png')
config.bind('<Space>F', 'spawn firefox -p Hardened {url}')
config.bind('<Space>f', 'spawn firefox -p Normal {url}')
config.bind('<Space>y', 'spawn mpv {url}')
# config.bind('<Space>c', 'config-cycle -p content.cookies.accept no-3rdparty never')
# config.bind('<Space>j', 'config-cycle -p content.javascript.enabled')
# config.bind('<Space>m', 'config-cycle -p messages.timeout 0 3000')

config.bind(',m', 'spawn mpv {url}')
config.bind(',M', 'hint links spawn mpv {hint-url}')

## Appearance
c.fonts.default_size = '11pt'
c.fonts.default_family = "Noto Sans"
c.tabs.title.format = '{current_title}'

# Load colors and change some
config.source('base16.py')

# Load autoconfig to allow for local overrides
config.load_autoconfig()

# vim: foldmethod=marker
