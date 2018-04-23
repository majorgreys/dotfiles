#! /bin/sh

B='#00000000'  # blank
C='#ffffff22'  # clear ish
D='#d8dee9cc'  # default
T='#d8dee9ee'  # text
W='#bf616abb'  # wrong
V='#ebcb8bbb'  # verifying

i3lock \
	--insidevercolor=$C   \
	--ringvercolor=$V     \
	--insidewrongcolor=$C \
	--ringwrongcolor=$W   \
	--insidecolor=$B      \
	--ringcolor=$D        \
	--linecolor=$B        \
	--separatorcolor=$D   \
	# --verifcolor=$T       \
	--wrongcolor=$T       \
	--timecolor=$T        \
	--datecolor=$T        \
	--layoutcolor=$T      \
	--keyhlcolor=$W       \
	--bshlcolor=$W        \
	--screen 1            \
	--blur 8              \
	--clock               \
	--indicator           \
	--timestr="%H:%M:%S"  \
	--datestr="%A, %m %Y" \
	--keylayout 2         
