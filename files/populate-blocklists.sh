#!/bin/sh
set -euo pipefail
for i in 'www.youtube.com'; do
	IPs="$(dig "$i" a +noall +answer | cut -f4 | sed 1d | tr '\n' ', ')"
	nft add element inet filter ip4blocklist \{ "$IPs" \}
done
