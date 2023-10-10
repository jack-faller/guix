#!/bin/sh
if MATCH=$(echo "$1" | grep -m 1 -o 'unsee.*cc/album#.\{12\}'); then
   echo "$MATCH" | sed 's/.*\(unsee\).*\(cc\/album#.\{12\}\).*/open '"$2"' \1.\2/' >> "$QUTE_FIFO"
else
	echo "message-error 'Could not determine unsee format.'" >> "$QUTE_FIFO"
fi
