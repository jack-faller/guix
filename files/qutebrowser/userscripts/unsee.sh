#!/bin/sh
if MATCH=$(echo "$1" | grep -m 1 -o 'album#.\{12\}'); then
   echo "$MATCH" | sed 's/.*album#\(.\{12\}\).*/open '"$2"' unsee.cc\/album#\1/' >> "$QUTE_FIFO"
else
	echo "message-error 'Could not determine unsee format.'" >> "$QUTE_FIFO"
fi
