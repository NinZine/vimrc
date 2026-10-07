#!/bin/sh
# Emits one conky line per real (block-device) mounted filesystem,
# so every drive shows up automatically regardless of where it is mounted.

findmnt -rn -o TARGET,SOURCE,FSTYPE 2>/dev/null | while read -r target source fstype; do
    case "$source" in
        *"["*) continue ;;
        /dev/loop*) continue ;;
        /dev/sr*) continue ;;
        /dev/*) ;;
        *) continue ;;
    esac
    case "$fstype" in
        swap|squashfs|overlay|tmpfs|devtmpfs|autofs|ramfs|efivarfs|iso9660) continue ;;
    esac
    case "$target" in
        /proc|/proc/*|/sys|/sys/*|/dev|/dev/*|/run/*|/snap/*|/boot|/boot/*|/usr|/usr/*|/etc|/etc/*|/var|/var/*|/tmp|/tmp/*) continue ;;
    esac
    label=$(lsblk -no LABEL "$source" 2>/dev/null | head -1)
    [ -z "$label" ] && label=$(basename "$source")
    label=$(printf '%.9s' "$label")
    printf '%s\n' "\${goto 8}\${color3}${label}\${color}\${goto 78}\${color}\${fs_bar 10,60 ${target}}\${color}\${goto 146}\${color2}\${fs_used ${target}}/\${fs_size ${target}}\${alignr 8}\${fs_used_perc ${target}}%\${color}"
done
