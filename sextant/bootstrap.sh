# Run as root immediately after installation

dpkg-reconfigure tzdata
# None of the above > GMT+3
# REMEMBER: THE TIMEZONES ARE MISLABELED, GMT+3 IS UTC-3
apt-get update
apt-get install curl sudo
echo "eudoxia ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/eudoxia
