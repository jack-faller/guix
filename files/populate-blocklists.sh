#!/bin/sh
set -eo pipefail
for i in 'www.youtube.com'; do
	 nft add element inet filter ip4blocklist \{ "$(dig "$i" a +noall +answer | cut -f4 | sed 1d | tr '\n' ', ')" \}
done
