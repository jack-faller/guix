#!/bin/sh
set -ex

if ! which jq; then
    guix shell jq -- "$0" "$@"
    exit $?
fi

MANIFEST="$(curl 'https://updates.discord.com/distributions/app/manifests/latest?channel=stable&platform=linux&arch=x64')"
query () {
    echo "$MANIFEST" | jq "$1" --raw-output
}
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
echo "
(define discord-version \"$(query '.full.host_version | "\(.[0]).\(.[1]).\(.[2])"')\")
(define discord-hash \"$(gethash "$URI")\")
(define discord-uri \"$URI\")
(define module-uris '$MODULE_URIS)
(define module-hashes '$MODULE_HASHES)
(define module-names '$MODULE_NAMES)
" > "$(dirname "$0")"/discord-info.scm
