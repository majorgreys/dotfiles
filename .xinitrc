[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
[[ -f ~/.Xresources ]] && xrdb -load ~/.Xresources
[ "$XDG_CURRENT_DESKTOP" = "KDE" ] || [ "$XDG_CURRENT_DESKTOP" = "GNOME" ] || export QT_QPA_PLATFORMTHEME="qt5ct"
export CALIBRE_USE_SYSTEM_THEME=1
udiskie &
exec i3 -V >> ~/i3log-$(date +'%F-%k-%M-%S') 2>&1
