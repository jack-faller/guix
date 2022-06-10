echo enter host name
read -r NAME
echo "$NAME" > /etc/hostname 
echo "127.0.0.1    localhost
::1    localhost
127.0.1.1    $NAME.localdomain    $NAME

# The following lines are desirable for IPv6 capable hosts
::1     localhost ip6-localhost ip6-loopback
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters" > /etc/hosts
mkdir .ssh
echo 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJNcDBG1O8GhsxaNwI3n76ECitK5Yt+Dqwd046Xgp6o jack.t.faller@gmail.com' >> .ssh/authorized_keys

useradd -m jack
passwd jack
usermod --shell /bin/bash jack
echo 'jack   ALL=(ALL:ALL) ALL' >> /etc/sudoers
su jack
cd ~
mkdir .ssh
echo 'ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICJNcDBG1O8GhsxaNwI3n76ECitK5Yt+Dqwd046Xgp6o jack.t.faller@gmail.com' > .ssh/authorized_keys

cd /tmp
wget https://git.savannah.gnu.org/cgit/guix.git/plain/etc/guix-install.sh
chmod +x guix-install.sh
sudo ./guix-install.sh

cd ~
guix install git
GUIX_PROFILE="$HOME/.guix-profile"
. "$GUIX_PROFILE/etc/profile"
git config --global http.sslverify false
git clone https://github.com/jack-faller/guix.git
ln -s ~/guix/channels.scm ~/.config/guix/channels.scm

cd ~
sudo sed -i "s#ExecStart.*guix-daemon#& --substitute-urls='https://ci.guix.gnu.org https://substitutes.nonguix.org'#" /etc/systemd/system/guix-daemon.service
curl https://substitutes.nonguix.org/signing-key.pub | sudo guix archive --authorize
sudo guix archive --authorize < "$(find / -name ci.guix.gnu.org.pub | sed 1q)"
sudo -i guix pull
sudo systemctl daemon-reload
sudo systemctl restart guix-daemon.service

# install ncsd
sudo systemctl enable nscd
sudo systemctl start nscd
guix home reconfigure ~/guix/home.scm
