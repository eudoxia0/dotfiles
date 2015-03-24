FOLDER="/home/eudoxia/images/wallpapers"
DELAY=300

cd $FOLDER

loop () {
    NUMBER=$(ls -1 | wc -l)
    RAN=$(($RANDOM%$NUMBER+1))
    FILE=$(ls -1 | sed -n ${RAN}p)
    echo $FILE
    feh --bg-fill "$FOLDER/$FILE"
    sleep $DELAY # Time between change
    loop
}
loop
