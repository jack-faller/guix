#!/bin/sh
if echo "$1" | grep 'unsee.*cc/album#.\{12\}'; then
   echo "$1" | sed 's/\(unsee\).*\(cc\/album#.\{12\}\)/open '"$2"' \1.\2/' >> "$QUTE_FIFO"
else
	echo "message-error 'Could not determine unsee format.'" >> "$QUTE_FIFO"
fi
