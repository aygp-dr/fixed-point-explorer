#!/bin/sh
# Test conversion matrix for ImageMagick 7

echo "=== Creating test files ==="

# Create test files in different formats
magick -size 100x100 -background red -fill white -gravity center -pointsize 30 label:"PNG" test.png
magick -size 100x100 -background green -fill white -gravity center -pointsize 30 label:"JPG" test.jpg
magick -size 100x100 -background blue -fill white -gravity center -pointsize 30 label:"GIF" test.gif
magick -size 100x100 -background yellow -fill black -gravity center -pointsize 30 label:"BMP" test.bmp
magick -size 100x100 -background purple -fill white -gravity center -pointsize 30 label:"TIFF" test.tiff
magick -size 100x100 -background orange -fill black -gravity center -pointsize 30 label:"WEBP" test.webp

# Create SVG manually (to avoid using magick for SVG creation)
cat > test.svg << 'EOF'
<svg width="100" height="100" xmlns="http://www.w3.org/2000/svg">
  <rect width="100" height="100" fill="cyan"/>
  <text x="50" y="50" text-anchor="middle" dy=".3em" font-size="30" fill="black">SVG</text>
</svg>
EOF

# Create PDF with magick
magick -size 100x100 -background gray -fill white -gravity center -pointsize 30 label:"PDF" test.pdf

echo -e "\n=== Conversion Matrix Test ==="
echo "Format conversions (✓ = success, ✗ = failure, - = skip same format)"
echo ""

# Arrays of formats
formats="png jpg gif bmp tiff webp svg pdf"

# Header
printf "%-6s" "FROM\\TO"
for to in $formats; do
    printf "%-6s" "$to"
done
echo ""
echo "------------------------------------------------"

# Test each conversion
for from in $formats; do
    printf "%-6s" "$from"
    for to in $formats; do
        if [ "$from" = "$to" ]; then
            printf "%-6s" "-"
        else
            # Try conversion, suppress output
            if magick "test.$from" "convert-test.$to" 2>/dev/null; then
                printf "%-6s" "✓"
                rm -f "convert-test.$to"
            else
                printf "%-6s" "✗"
            fi
        fi
    done
    echo ""
done

echo -e "\n=== Testing problematic conversions individually ==="