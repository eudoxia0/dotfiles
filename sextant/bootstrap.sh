# Run as root immediately after installation

echo "Etc/GMT+3" > /etc/timezone
dpkg-reconfigure -f noninteractive tzdata
apt-get update
apt-get install curl sudo
echo "eudoxia ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/eudoxia
