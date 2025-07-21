#!/bin/sh
# Test animated GIF creation

echo "=== Testing animated GIF creation ==="

# Create frames
for i in 1 2 3 4; do
    angle=$((i * 90))
    magick -size 100x100 xc:white \
           -fill black -draw "translate 50,50 rotate $angle rectangle -20,-5 20,5" \
           frame$i.png
done

# Create animated GIF
echo "Creating animated GIF..."
magick -delay 25 frame*.png -loop 0 animated.gif

# Test GIF properties
echo -e "\nAnimated GIF info:"
magick identify animated.gif

# Clean up frames
rm -f frame*.png

echo -e "\n=== Testing other conversions without SVG ==="

# Test some interesting conversions
echo "PNG -> ICO (multi-size):"
magick test.png -define icon:auto-resize=64,48,32,16 multi-size.ico
magick identify multi-size.ico | head -4

echo -e "\nCreating XPM (text-based format):"
magick -size 10x10 gradient:red-blue tiny.xpm
echo "XPM file preview:"
head -20 tiny.xpm