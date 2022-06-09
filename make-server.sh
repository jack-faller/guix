ehco enter host name
read -r NAME
echo "$NAME" > /etc/hostname 
echo "127.0.0.1    localhost
::1    localhost
127.0.1.1    $NAME.localdomain    $NAME" >> /etc/hosts
mkdir .ssh
echo 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJNcDBG1O8GhsxaNwI3n76ECitK5Yt+Dqwd046Xgp6o jack.t.faller@gmail.com' >> .ssh/authorized_keys

# pacman -S base-devel git
useradd -m jack
passwd jack
echo 'jack   ALL=(ALL:ALL) ALL' >> /etc/sudoers
su jack
cd ~
mkdir .ssh
echo 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJNcDBG1O8GhsxaNwI3n76ECitK5Yt+Dqwd046Xgp6o jack.t.faller@gmail.com' > .ssh/authorized_keys
# git clone https://aur.archlinux.org/guix-installer.git
# cd guix-installer
# makepkg -sri
# cd ~
# rm -rf guix-installer

cd ~
mkdir .config
git clone https://github.com/jack-faller/guix .config/guix

cd /tmp
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
chmod +x guix-install.sh
./guix-install.sh

cd ~
sudo sed -i "s#ExecStart.*guix-daemon#& --substitute-urls='https://ci.guix.gnu.org https://bordeaux.guix.gnu.org https://substitutes.nonguix.org'#" /etc/systemd/system/guix-daemon.service
sudo systemctl daemon-reload
sudo systemctl restart guix-daemon.service
curl https://substitutes.nonguix.org/signing-key.pub | sudo guix archive --authorize
sudo guix archive --authorize < "$(find / -name ci.guix.gnu.org.pub | sed 1q)"
sudo guix archive --authorize < "$(find / -name bordeaux.guix.gnu.org.pub | sed 1q)"
