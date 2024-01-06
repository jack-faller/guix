#!/bin/sh
if MATCH=$(echo "$1" | tr -d '[:space:]' | grep -m 1 -o '#.\{12\}'); then
   echo "$MATCH" | sed 's/.*/open '"$2"' unsee.cc\/album&/' >> "$QUTE_FIFO"
else
	echo "message-error 'Could not determine unsee format.'" >> "$QUTE_FIFO"
fi
