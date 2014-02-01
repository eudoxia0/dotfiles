FOLDER="/home/eudoxia/images/wallpapers/rotating" # Change to your directory
cd $FOLDER

loop () {
    NUMBER=$(ls -1 | wc -l)
    RAN=$(($RANDOM%$NUMBER+1))
    FILE=$(ls -1 | sed -n ${RAN}p)
    echo $FILE
    feh --bg-fill "$FOLDER/$FILE"
    sleep 300 # Time between change
    loop
}
loop

