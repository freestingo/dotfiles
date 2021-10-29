#!/bin/sh
# Gives dmenu a list of selected symbols used in math/cs + misc.
# Will then show selected character in dunst if it is running

chosen=$(grep -v "#" ~/scripts/.symbols | dmenu -i -l 20 -h 30 -fn 'Hasklug Nerd Font Mono' -p 'Select symbol:' -x 540 -y 290 -z 900)
[ "$chosen" != "" ] || exit

c=$(echo "$chosen" | sed "s/ .*//")
echo "$c" | tr -d '\n' | xclip -selection clipboard
notify-send "Symbols Script" "'$c' copied to clipboard." &
