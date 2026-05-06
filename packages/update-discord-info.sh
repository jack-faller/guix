#!/bin/sh
set -ex

# Install `jq` dependency by re-running the script in a Guix shell.
if ! which jq; then
    guix shell jq -- "$0" "$@"
    exit $?
fi

# Fetch the latest version info from Discord.
# See https://docs.discord.food/topics/client-distribution#get-latest-distributed-application-manifest.
MANIFEST="$(curl 'https://updates.discord.com/distributions/app/manifests/latest?channel=stable&platform=linux&arch=x64')"
# Run a `jq` query on the manifest.
query () {
    echo "$MANIFEST" | jq "$1" --raw-output
}
# Print the hash of all files passed as arguments.
gethash () {
    for url in "$@"; do
        guix download "$url" | sed -n 2p
    done
}

MODULE_NAMES="$(query '.required_modules | join("\" \"") | "(\"\(.)\")"')"
MODULE_URIS=""
MODULE_HASHES=""
for url in $(query '. as $x | .required_modules | .[] | $x.modules[.].full.url'); do
    MODULE_URIS="${MODULE_URIS:+$MODULE_URIS }\"$url\""
    MODULE_HASHES="${MODULE_HASHES:+$MODULE_HASHES }\"$(gethash $url)\""
done
MODULE_URIS="($MODULE_URIS)"
MODULE_HASHES="($MODULE_HASHES)"
URI="$(query .full.url)"
VERSION="$(query '.full.host_version | "\(.[0]).\(.[1]).\(.[2])"')"
STUB_URI="https://cdn.discordapp.com/apps/linux/$VERSION/discord-$VERSION.tar.gz"

VARIABLES=";; begin generated content
(define discord-version \"$VERSION\")
(define discord-hash \"$(gethash "$URI")\")
(define discord-uri \"$URI\")
(define stub-uri \"$STUB_URI\")
(define stub-hash \"$(gethash "$STUB_URI")\")
(define module-uris '$MODULE_URIS)
(define module-hashes '$MODULE_HASHES)
(define module-names '$MODULE_NAMES)
;; end generated content"

# Substitute the lines between `begin/end generated content` inclusive with
# the updated definitions.
echo "$VARIABLES" | sed -e '/;; begin generated/,/;; end generated/!b' -e '/;; end generated/!d;r /dev/stdin' -e 'd' -i "$(dirname "$0")"/discord.scm
