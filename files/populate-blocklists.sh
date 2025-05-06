#!/bin/sh
set -euo pipefail
# Wait for network to come up.
while ! ping -c 1 -W 10 google.com ; do sleep 5; done
for i in 'www.youtube.com'; do
	IPs="$(dig "$i" a -4 +noall +answer | cut -f4 | sed 1d | tr '\n' ', ')"
	nft add element inet filter ip4blocklist \{ "$IPs" \}
done
