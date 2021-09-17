# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

# init git ssh-agent
eval "$(ssh-agent)"
. "$HOME/.cargo/env"

# set custom EDITOR for use with `sudoedit` or `sudo -e`
# https://www.reddit.com/r/linux/comments/osah05/ysk_do_not_use_sudo_vimnanoemacs_to_edit_a_file/
export EDITOR=/usr/local/bin/vim

# set `batcat` as default pager for `man`
# https://github.com/sharkdp/bat (`man` section)
export MANPAGER="sh -c 'col -bx | batcat -l man -p'"

# swap Left Alt with Windows Key
setxkbmap -option altwin:swap_lalt_lwin
# swap Left Ctrl with Caps Lock
setxkbmap -option ctrl:swapcaps

