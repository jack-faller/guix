#!/bin/sh

if [ "$ROFI_RETV" = 0 ]; then
	ps -Ao pid,command --no-headers |
		sed 's/ *\([0-9]*\) \(.*\)/\2\x00info\x1f\1/'
elif [ "$ROFI_RETV" = 1 ]; then
	kill "$ROFI_INFO"
fi
