[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources
udiskie &
exec i3 -V >> ~/i3log-$(date +'%F-%k-%M-%S') 2>&1
