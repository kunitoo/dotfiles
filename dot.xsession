#! /bin/sh
if [ -f "$HOME"/.Xdefaults ]; then
	xrdb -load "$HOME"/.Xdefaults
fi

LANG=ja_JP.eucJP
export LANG
xset -b
xscreensaver-command -exit
xscreensaver -no-splash &
kterm &
exec awesome
