#!/bin/sh
# Emits one conky line: per CPU core an index, a small vertical bar (block
# glyph, height ~ temperature) and the temperature. Text is drawn first at the
# base size, then all bars in a single font-size switch, so nothing drifts.

sen=$(sensors 2>/dev/null)
temps=""
bars=""
xidx=8
xbar=20
xtmp=34
for core in 0 1 2 3; do
    t=$(printf '%s\n' "$sen" | grep "^Core $core" | tr -s ' ' | cut -d' ' -f3 | sed 's/[+°C]//')
    [ -z "$t" ] && t=0
    ti=${t%.*}
    lvl=$(( (ti - 30) / 8 ))
    [ "$lvl" -lt 0 ] && lvl=0
    [ "$lvl" -gt 7 ] && lvl=7
    case $lvl in
        0) g='▁' ;;
        1) g='▂' ;;
        2) g='▃' ;;
        3) g='▄' ;;
        4) g='▅' ;;
        5) g='▆' ;;
        6) g='▇' ;;
        7) g='█' ;;
    esac
    if [ "$lvl" -le 2 ]; then hc='${color8}'
    elif [ "$lvl" -le 5 ]; then hc='${color2}'
    else hc='${color9}'
    fi
    temps="$temps\${goto $xidx}\${color3}${core}\${color}\${goto $xtmp}$hc${ti}°C\${color}"
    bars="$bars\${goto $xbar}$hc$g\${color}"
    xidx=$(( xidx + 62 ))
    xbar=$(( xbar + 62 ))
    xtmp=$(( xtmp + 62 ))
done
printf '%s' "$temps\${voffset -3}\${font Noto Sans Mono:bold:size=14}$bars\${font}\${voffset 0}"
