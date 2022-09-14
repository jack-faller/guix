#NOTE: in the location the os is to be installed and edit the install script script to more files into the existing one
# this script should be run with .

# should install nscd and wget

# this is only needed when the system lacks space

TARGET=/mnt
refresh () {
	mkdir "$1" || (rm -rf "$1"; mkdir "$2")
}
binddir () {
	mkdir -p "$2"
	refresh "${TARGET:?}/$1"
	cp "$2"/* "$TARGET/$1"
	mount --bind "$TARGET/$1" "$2"
}
refresh "${TARGET:?}/install-tmp"
binddir install-cache ~/.cache
binddir install-gnu-store /gnu
binddir install-var-guix /var/guix

cd "${TARGET:?}/install-tmp"
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
sed -i 's#\[\[ -e "/var/guix" || -e "/gnu" \]\]#false#' guix-install.sh
sed -i 's#mv "\(.*\)/gnu" /#mv "\1/gnu"/* /gnu#' guix-install.sh
sed -i 's#mv "\(.*\)/var/guix" /var/#mv "\1/var/guix"/* /var/guix#' guix-install.sh
sed -i 's#tmp_path=.*#tmp_path='"'${TARGET:?}/install-tmp'#" guix-install.sh
chmod +x guix-install.sh
./guix-install.sh

cat <<EOF > ~/.config/guix/channels.scm
(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
EOF
sed -i "s#ExecStart.*guix-daemon#& --substitute-urls='https://ci.guix.gnu.org https://substitutes.nonguix.org'#" /etc/systemd/system/guix-daemon.service
curl https://substitutes.nonguix.org/signing-key.pub | guix archive --authorize
guix archive --authorize < "$(find / -name ci.guix.gnu.org.pub | sed 1q)"

guix pull

export GUIX_PROFILE="$HOME/.config/guix/current"
. "$GUIX_PROFILE/etc/profile"
systemctl daemon-reload
systemctl restart guix-daemon.service

set_perms () { chown 30000 "$1"; chmod 1775 "$1"; }
# first try to unmount all the shit, then pass the mount directory, the gnu/store directory and the location for the instal dir on the guix iso
cow_service () {
	INSTALLDIR="$1"
	STORE="$2"
	OUTPUT="$2"
	mkdir -p "$INSTALLDIR/tmp"
	mount --bind "$INSTALLDIR/tmp" "/tmp"
	RWDIR="$INSTALLDIR/tmp/guix-inst"
	WORKDIR="$INSTALLDIR/tmp/.overlays-workdir"
	mkdir -p "$WORKDIR"
	mkdir -p "$RWDIR"
	mkdir -p /.rw-store
	set_perms "$RWDIR"
	set_perms /.rw-store

	mount -t overlay overlay -o lowerdir="$STORE",upperdir="$RWDIR",workdir="$WORKDIR" /.rw-store
	mount --move /.rw-store "$OUTPUT"
	rmdir /.rw-store
}
# to install:
# cd this_dir
# mkdir $TARGET/etc
# cp channels.scm $TARGET/etc
# giux system init test-system.scm  $TARGET
