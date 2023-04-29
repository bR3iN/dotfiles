c.url.start_pages = ["about:blank"]

c.content.canvas_reading = False
c.content.cookies.store = False
c.content.cookies.store = False
c.content.geolocation = False
c.content.javascript.enabled = False
config.set('content.javascript.enabled', True, '*://*.duckduckgo.com/*')
c.content.webgl = False
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'

c.downloads.position = 'bottom'
c.downloads.remove_finished = 60000

c.messages.timeout = 3000

c.editor.command = ['kitty', 'bash', '-c', 'nvim "{file}" "+normal {line}G{column0}l"']

## Keybindings
config.bind('<F1>', 'open -t https://raw.githubusercontent.com/qutebrowser/qutebrowser/master/doc/img/cheatsheet-big.png')
config.bind('<Space>F', 'spawn firefox -p Hardened {url}')
config.bind('<Space>f', 'spawn firefox -p Normal {url}')
config.bind('<Space>y', 'spawn mpv {url}')
# config.bind('<Space>c', 'config-cycle -p content.cookies.accept no-3rdparty never')
config.bind('<Space>j', 'config-cycle -p content.javascript.enabled')
config.bind('<Space>m', 'config-cycle -p messages.timeout 0 3000')


## Appearance
c.fonts.default_size = '11pt'
c.fonts.default_family = "Noto Sans"
c.tabs.title.format = '{current_title}'

# Load colors and change some
config.source('base16.py')

# Load autoconfig to allow for local overrides
config.load_autoconfig()

# vim: foldmethod=marker
