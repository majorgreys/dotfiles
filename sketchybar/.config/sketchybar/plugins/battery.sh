#!/usr/bin/env bash
# Glyphs from Material Design Icons range of Nerd Fonts.
PMSET=$(pmset -g batt)
PERCENT=$(echo "$PMSET" | grep -Eo '[0-9]+%' | head -1 | tr -d '%')
SOURCE=$(echo "$PMSET" | head -1)

if [ -z "$PERCENT" ]; then
  sketchybar --set "$NAME" icon="󱉝" icon.color=0xff7f849c
  exit 0
fi

CHARGING=0
case "$SOURCE" in
  *"AC Power"*) CHARGING=1 ;;
esac

if [ "$CHARGING" = "1" ]; then
  ICON="󰂄"   # mdi-battery-charging
  COLOR="0xffa6e3a1"
elif [ "$PERCENT" -le 20 ]; then
  ICON="󰂃"   # mdi-battery-alert
  COLOR="0xfff38ba8"
elif [ "$PERCENT" -le 33 ]; then
  ICON="󰁻"   # low
  COLOR="0xfff9e2af"
elif [ "$PERCENT" -le 66 ]; then
  ICON="󰁾"   # medium
  COLOR="0xffcdd6f4"
else
  ICON="󰁹"   # high
  COLOR="0xffcdd6f4"
fi

sketchybar --set "$NAME" icon="$ICON" icon.color="$COLOR"
