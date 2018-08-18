#!/usr/bin/env bash

gpg --list-secret-keys

echo "Duplicity directory:"
read BACKUP_DUPLICITY_DIR
echo "Key: "
read BACKUP_KEY_ID
echo "Passphrase: "
read -s BACKUP_KEY_PASSPHRASE

function back_up_directory() {
    echo "Synchronizing ~/$1/"
    PASSPHRASE=$BACKUP_KEY_PASSPHRASE duplicity \
              --encrypt-key $BACKUP_KEY_ID \
              ~/$1/ \
              file://$BACKUP_DUPLICITY_DIR/$1/
}

back_up_directory .fonts
back_up_directory .ssh
back_up_directory backup
back_up_directory code
back_up_directory images
back_up_directory library
back_up_directory music
back_up_directory self
back_up_directory texmf
back_up_directory wiki
back_up_directory work
back_up_directory writing
