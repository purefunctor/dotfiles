nice xwinwrap -a -s -fs -st -sp -nf -ovr -fdt -- mpv -wid WID --really-quiet --framedrop=vo --panscan="1.0" --loop ~/Wallpapers/Animated/train.gif
# if [[ $1 == "day" && $(date +%k) -ge 8 && $(date +%k) -lt 18 ]]; then
#     DISPLAY=:0 HOME=/home/pure feh --bg-fill --randomize /home/pure/Wallpapers/Scenery/Day/*
# elif [[ $1 == "night" && (( $(date +%k) -ge 18 || $(date +%k) -lt 8 )) ]]; then
#     DISPLAY=:0 HOME=/home/pure feh --bg-fill --randomize /home/pure/Wallpapers/Scenery/Night/*
# else
#     echo "Usage: wallpaper.sh (day|night)"
#     echo "Note: Also respects the current time"
# fi
