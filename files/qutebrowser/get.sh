#! /bin/sh

. "$HOME/.cache/wal/colors.sh"
line=$(cat "$QUTE_CONFIG_DIR/stowed" | sort | dmenu -i -b -l 20 -nb "$color0" -nf "$color15" -sb "$color1" -sf "$color15")
url=$(echo "$line" | sed 's/.* //g') #strip tab title

if [ $(grep -x -v "$line" "$QUTE_CONFIG_DIR/stowed") = "" ]; then #don't open tabs not in file
    exit
fi

if [ "$url" != "" ]; then
    echo "open -t $url" >> "$QUTE_FIFO"
    grep -x -v --fixed-strings "$line" "$QUTE_CONFIG_DIR/stowed" > "$QUTE_CONFIG_DIR/tmp_stowed" #remove url
    mv "$QUTE_CONFIG_DIR/tmp_stowed" "$QUTE_CONFIG_DIR/stowed"
fi
