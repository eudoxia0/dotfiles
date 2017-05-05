FOLDER="$HOME/.walls"
DELAY=300

cd $FOLDER

change_wallpaper() {
    NUMBER=$(ls -1 | wc -l)
    RAN=$(($RANDOM%$NUMBER+1))
    FILE=$(ls -1 | sed -n ${RAN}p)
    echo $FILE
    feh --bg-fill "$FOLDER/$FILE"
    sleep $DELAY # Time between change
}

while true; do change_wallpaper; done