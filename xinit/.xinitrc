#!/bin/bash
/usr/bin/pipewire & /usr/bin/pipewire-pulse & /usr/bin/pipewire-media-session &

exec xmodmap ~/.Xmodmap &
exec lxsession &

exec nitrogen --restore &
exec picom &

exec qtile start
