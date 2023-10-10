#! /bin/sh
echo ":set-cmd-text :open -t $(curl "$QUTE_URL" | zbarimg - | sed 's/QR-Code://g')" > $QUTE_FIFO
