#!/usr/bin/env bash
VOL="${INFO:-$(osascript -e 'output volume of (get volume settings)' 2>/dev/null)}"
MUTED=$(osascript -e 'output muted of (get volume settings)' 2>/dev/null)

if [ -z "$VOL" ]; then
  sketchybar --set "$NAME" icon="󰕿" icon.color=0xff7f849c
  exit 0
fi

if [ "$MUTED" = "true" ] || [ "$VOL" = "0" ]; then
  ICON="󰝟"   # mdi-volume-mute
  COLOR="0xff7f849c"
elif [ "$VOL" -ge 66 ]; then
  ICON="󰕾"   # mdi-volume-high
  COLOR="0xffcdd6f4"
elif [ "$VOL" -ge 33 ]; then
  ICON="󰖀"   # mdi-volume-medium
  COLOR="0xffcdd6f4"
else
  ICON="󰕿"   # mdi-volume-low
  COLOR="0xffcdd6f4"
fi

sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR"
