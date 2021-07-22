## Cleanup commands to run after cloning dotfiles

### Making Caps act as Ctrl

After cloning, enter these commands in a terminal:
- `mv ~/.config/.hwdb /etc/udev/hwdb.d/.hwdb`
- `systemd-hwdb update`

(Source: https://brokkr.net/2019/01/11/customize-your-keyboard-layout-and-have-it-work-under-wayland/)
