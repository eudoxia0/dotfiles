#!/bin/sh

# backup - copy important files to USB drive
# usage: backup.sh

if [ "$HOSTNAME" = desktop ]; then
    base=/run/media/eudoxia/backup # This is Arch
else
    base=/media/eudoxia/backup # This is Ubuntu
fi

if [ -d "$base/Kindle" ]; then
  # Is the Kindle on?
  cp "$base/Kindle/documents/My Clippings.txt" ~/self/clippings.txt
fi

echo "Code"
unison ~/code $base/code
echo "Images"
unison ~/images $base/images
echo "Writing"
unison ~/writing $base/writing
echo "Self"
unison ~/self $base/self
echo "Scripts"
unison ~/.scripts $base/.scripts
echo "Backup"
unison ~/backup $base/backup
echo "Notes"
unison ~/notes $base/notes
echo "Library"
unison ~/library $base/library
echo "hosts.txt"
unison ~/.shell/hosts.txt $base/.shell/hosts.txt
echo "Local variables"
unison ~/.localrc $base/.localrc
echo "Liferea feeds"
unison ~/.liferea_1.8 $base/.liferea_1.8
echo "SSH"
unison ~/.ssh $base/.ssh -ignore "Name known_hosts"

