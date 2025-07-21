#!/bin/sh
# Advanced QR Code Styling Experiments

URL="https://github.com/aygp-dr/fixed-point-explorer"

echo "=== Advanced QR Code Styling ==="

# Create QR with lambda symbol overlay
echo "Creating QR with Lambda overlay..."
magick qr-high-ec.png \
    \( -size 60x60 xc:white -fill '#663399' -font Symbol -pointsize 40 \
       -gravity center -annotate +0+0 'λ' \) \
    -gravity center -compose over -composite \
    qr-lambda.png

# Create colored blocks version (checker pattern)
echo "Creating checkered QR..."
magick qr-standard.png \
    \( +clone -fill '#663399' -colorize 100% \) \
    \( +clone -modulate 100,0 \) \
    -compose multiply -composite \
    qr-checkered.png

# Create neon glow effect
echo "Creating neon effect..."
magick qr-standard.png -negate \
    \( +clone -blur 0x3 -fill cyan -colorize 100% \) \
    -compose screen -composite \
    \( +clone -blur 0x5 -fill magenta -colorize 50% \) \
    -compose screen -composite \
    qr-neon.png

# Create retro terminal style
echo "Creating retro terminal style..."
magick -size 400x400 xc:black \
    -fill '#00FF00' -draw "rectangle 0,0 400,400" \
    qr-standard.png -resize 300x300 -negate \
    -gravity center -compose multiply -composite \
    -fill '#00FF00' -colorize 100% \
    qr-terminal.png

# Create ASCII art version
echo "Creating ASCII art version..."
qrencode -t ASCIIi "$URL" > qr-ascii.txt
echo "Saved ASCII version to qr-ascii.txt"

# Create multi-color gradient version
echo "Creating rainbow gradient..."
magick qr-standard.png -alpha set \
    \( +clone -fill "gradient:#FF0000-#00FF00" \) \
    -compose multiply -composite \
    qr-rainbow.png

echo -e "\n=== Functional Art QR Codes ==="

# Create business card style
echo "Creating business card..."
magick -size 350x200 xc:white \
    -fill black -draw "rectangle 10,10 340,190" \
    -fill white -draw "rectangle 15,15 335,185" \
    qr-standard.png -resize 100x100 -geometry +20+50 -composite \
    -font Helvetica -pointsize 16 -fill black \
    -annotate +140+80 "Fixed Point Explorer" \
    -font Helvetica -pointsize 12 \
    -annotate +140+100 "Y Combinator Implementation" \
    -annotate +140+120 "github.com/aygp-dr" \
    qr-businesscard.png

echo -e "\n=== Summary of Styled QR Codes ==="
ls -1 qr-*.png | sort