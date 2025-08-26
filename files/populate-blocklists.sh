#!/bin/sh
set -euo pipefail
# Wait for network to come up.
while ! ping -c 1 -W 10 google.com ; do sleep 5; done
nft flush set inet filter ip4blocklist
grep -v '^[\t ]*#' | while read i; do
	IPs="$(dig "$i" a -4 +noall +answer | grep -Eo '[0-9]+.[0-9]+.[0-9]+.[0-9]+$' | tr '\n' ', ')"
	nft add element inet filter ip4blocklist \{ "$IPs" \}
done < /blocked-ips.txt
