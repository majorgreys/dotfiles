#!/usr/bin/env bash
# macOS 14+ redacts SSID without Location Services granted to the calling
# process; show connection state and hide the SSID. To get the real SSID back,
# install a small CoreLocation helper.

STATE=$(ipconfig getsummary en0 2>/dev/null | awk -F ': ' '/^ *State : / {print $2; exit}')

case "$STATE" in
  BOUND|RENEW|REBIND|INFORM)
    sketchybar --set "$NAME" \
               icon="󰖩" \
               icon.color=0xffcdd6f4 \
               label="" \
               label.drawing=off
    ;;
  *)
    sketchybar --set "$NAME" \
               icon="󰖪" \
               icon.color=0xff7f849c \
               label="" \
               label.drawing=off
    ;;
esac
