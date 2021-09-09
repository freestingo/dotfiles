#!/usr/bin/bash

# Mount/unmount your external drive similarly to how `Nautilus` does it.
# https://help.ubuntu.com/community/AutomaticallyMountPartitions

# TODO add html markup to the `udisksctl` output message

DMENU_INPUT=''
DRIVES="/home/freestingo/scripts/.drives"
DISKS_DIR='/dev/disk/by-uuid'
AUTO_MOUNT_POINT='/media/freestingo'
NOTIFICATION_TITLE='AutoMount Custom Script'

# ALIASES

newline_if_first () {
  [[ -n "$DMENU_INPUT" ]] && echo "\n"
}

open_dmenu () {
  echo -e "$1" | dmenu -i -l 20 -h 30 -fn "Hasklug Nerd Font Mono" -p "$2" -x 735 -y 450 -z 450
}

notify () {
  notify-send -u low "$NOTIFICATION_TITLE" "$1"
}

# PROGRAM

# quit script if there's no file to load data from
[[ -e "$DRIVES" ]] || { notify "Can't find drive list file!"; exit 0; }

# assumes file is written in this format: `[name] [separator] [UUID]`
# lines starting with hashes will be ignored and treated as comments
while read drive_info; do
  [[ "$drive_info" =~ ^# ]] || DMENU_INPUT="$DMENU_INPUT$(newline_if_first)$drive_info"
done < "$DRIVES"

if (( $(echo -e "$DMENU_INPUT" | wc -l) < 2 )); then
  # skip selection if there's less than two drives available
  DRIVE="$DMENU_INPUT"
else
  # prompt selection
  DRIVE=$(open_dmenu "$DMENU_INPUT" "Select drive:")
fi

# check if a drive has been selected, or has been loaded from the .drives file
if [[ -n "$DRIVE" ]]; then
  read label sep uuid <<< "$DRIVE"

  if [[ -d "$AUTO_MOUNT_POINT/$label" ]]; then
    [[ $(open_dmenu "No\nYes" "Do you wish to unmount $label?") == "Yes" ]] && \
    notify "$(udisksctl unmount -b "$DISKS_DIR/$uuid")"
  else
    if (( $(ls -l "$DISKS_DIR/" | rg "$uuid" | wc -l) )); then
      [[ $(open_dmenu "No\nYes" "Do you wish to mount $label?") == "Yes" ]] && \
      notify "$(udisksctl mount -b "$DISKS_DIR/$uuid")!"
    else
      notify "Drive <i>$label</i> is not connected to this computer!"
    fi
  fi
fi

