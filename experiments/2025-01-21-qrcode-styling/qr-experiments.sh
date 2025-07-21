#!/bin/sh
# QR Code Styling Experiments

URL="https://github.com/aygp-dr/fixed-point-explorer"

echo "=== Basic QR Code Generation ==="

# Standard black and white
qrencode -o qr-standard.png "$URL"
echo "Created: qr-standard.png"

# Larger size with custom colors
qrencode -o qr-purple.png -s 10 --foreground=663399 --background=F0F0F0 "$URL"
echo "Created: qr-purple.png (purple on light gray)"

# Different error correction levels
qrencode -o qr-high-ec.png -l H -s 8 "$URL"
echo "Created: qr-high-ec.png (high error correction)"

# SVG output for editing
qrencode -o qr-base.svg -t SVG -s 10 "$URL"
echo "Created: qr-base.svg"

# Terminal outputs
echo -e "\n=== Terminal QR Codes ==="
echo "UTF8 version:"
qrencode -t UTF8 "$URL"

echo -e "\nANSI256 colored version:"
qrencode -t ANSI256 "$URL"

# Create variations with ImageMagick
echo -e "\n=== ImageMagick Styling ==="

# Rounded corners
magick qr-standard.png -alpha set -background none -vignette 0x10 qr-rounded.png
echo "Created: qr-rounded.png"

# Add gradient overlay
magick qr-purple.png \
    \( +clone -fill "gradient:#663399-#9966CC" -draw "rectangle 0,0 %[w],%[h]" \) \
    -compose multiply -composite qr-gradient.png
echo "Created: qr-gradient.png"

# Create with logo space in center
magick qr-high-ec.png \
    -fill white -draw "circle 150,150 150,100" \
    qr-logo-space.png
echo "Created: qr-logo-space.png (space for logo)"

echo -e "\n=== Creating Artistic Variations ==="

# Create dotted style
magick qr-standard.png -morphology erode square:2 -morphology dilate disk:3 qr-dots.png
echo "Created: qr-dots.png"

# Create QR with text overlay
magick -size 400x400 xc:white \
    qr-standard.png -geometry +50+50 -composite \
    -font Helvetica-Bold -pointsize 20 -fill "#663399" \
    -annotate +100+350 "Fixed Point Explorer" \
    qr-with-text.png
echo "Created: qr-with-text.png"

echo -e "\n=== Summary ==="
ls -la *.png *.svg | grep -v ".sh"