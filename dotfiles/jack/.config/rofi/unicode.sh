#!/bin/sh

script='{
    desc = $11 == "" ? $2 : $11
    if (desc !~ "^<") {
        if ($3 !~ "^[MZC]")
	    printf "%c %s %s", strtonum("0x" $1), desc, $1
	else
	    printf "%s %s", desc, $1
	printf "\x00info\x1f%s\n", $1
    }
}'

if [ "$ROFI_RETV" = 0 ]; then
    awk -v FS=';' "$script" "$UNICODE_DATA_TXT"
elif [ "$ROFI_RETV" = 1 ]; then
    printf "\U$ROFI_INFO" | clip-copy > /dev/null 2>&1
fi
