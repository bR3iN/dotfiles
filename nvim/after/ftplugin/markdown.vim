setlocal nobackup
setlocal nowritebackup
setlocal iskeyword+=\
abbreviate <buffer> \bf \mathbf
abbreviate <buffer> \rm \mathrm
abbreviate <buffer> \cal \mathcal
abbreviate <buffer> \bb \mathbb
abbreviate <buffer> iff if and only if

let b:surround_66="**\r**"
