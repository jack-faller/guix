#! /bin/sh

echo "$QUTE_TITLE $QUTE_URL" >> "$QUTE_CONFIG_DIR/stowed"
echo "tab-close $1" >> "$QUTE_FIFO"
