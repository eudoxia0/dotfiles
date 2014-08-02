#/bin/sh

# Install drivers
# Recommend do this outside of X

sudo mhwd -a pci nonfree 0300
sudo mhwd-gpu --setgl nvidia

sudo reboot
