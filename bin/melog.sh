set -e
cd ~/Dropbox/Documents/Log 
C=$(zenity --entry --text="Add to Log")
R=`./log.sh ${C}`
zenity --info --text="${R}"
