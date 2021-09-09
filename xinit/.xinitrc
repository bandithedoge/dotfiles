#!/bin/bash
/usr/bin/pipewire & /usr/bin/pipewire-pulse & /usr/bin/pipewire-media-session &

exec lxsession &
exec nitrogen --restore &
exec picom &

exec setxkbmap -layout pl -option ctrl:swapcaps &

exec qtile start 
