#!/usr/bin/bash
FORMAT="(%W) %N"

main()
{
    if [ -z "$*" ]; then
        swaymsg -t get_tree \
            | jq -r "$PARSE_ROOT | $FORMAT_LEAVE" \
            | sed 's/%NULL%/\x00/
                   s/%SEPARATOR%/\x1f/g'
    else
        swaymsg [con_id=$ROFI_INFO] focus &> /dev/null
    fi
}

# `jq` filter, see `man jq` for details
GET_NODES="
.nodes[], .floating_nodes[]
"

IS_LEAVE="
.nodes == [] and .floating_nodes == []
"

GET_LEAVES="
recurse($GET_NODES) | select($IS_LEAVE)
"

PARSE_LEAVE="
{name, urgent, id, app_id}
"

NOT_WORKSPACE="
.type == \"workspace\" | not
"

PARSE_WORKSPACE="
{workspace: .name} 
    + ($GET_LEAVES | select($NOT_WORKSPACE) | $PARSE_LEAVE)
"

PARSE_OUTPUT="
{output: .name, scratchpad: false}
    + ($GET_NODES | $PARSE_WORKSPACE)
"

PARSE_SCRATCHPAD="
{scratchpad: true, workspace: \"SP\"}
    + ($GET_NODES | $GET_LEAVES | $PARSE_LEAVE)
"

PARSE_ROOT="
$GET_NODES
    | if .name == \"__i3\" then
        $GET_NODES
            | select(.name == \"__i3_scratch\")
            | $PARSE_SCRATCHPAD
    else
        $PARSE_OUTPUT
    end
"

ROW_OPTIONS="
\"%NULL%icon%SEPARATOR%\" + (.app_id | tostring)
    + \"%SEPARATOR%info%SEPARATOR%\" + (.id | tostring)
    + \"\n\"
"

parse_format()
{
    echo "$1" | sed 's/%W/" + .workspace + "/g
                     s/%O/" + .output    + "/g
                     s/%I/" + .id        + "/g
                     s/%A/" + .app_id    + "/g
                     s/%N/" + .name      + "/g
                     '
}

FORMAT_LEAVE="
\"$(parse_format "$FORMAT")\"
    + $ROW_OPTIONS
"

main "$@"
