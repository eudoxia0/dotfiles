# Run as root immediately after installation

echo "America/Montevideo" > /etc/timezone
dpkg-reconfigure -f noninteractive tzdata
apt-get update
apt-get install curl sudo
echo "eudoxia ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/eudoxia
