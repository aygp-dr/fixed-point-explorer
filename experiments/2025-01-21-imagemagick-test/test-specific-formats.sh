#!/bin/sh
# Test specific format conversions with ImageMagick 7

echo "=== Creating test files for common + legacy formats ==="

# Create base test image
magick -size 100x100 gradient:blue-yellow base.png

# Common formats
echo "Creating common formats..."
magick base.png test.png
magick base.png test.jpg
magick base.png test.gif
magick base.png -density 72 test.pdf

# Legacy/Old formats
echo "Creating legacy formats..."
magick base.png test.ppm  # Portable Pixmap
magick base.png test.ico  # Windows Icon
magick base.png test.xpm  # X11 Pixmap
magick base.png test.pcx  # PC Paintbrush
magick base.png test.tga  # Truevision Targa

# PostScript (if supported)
echo "Attempting PostScript..."
magick base.png test.ps 2>/dev/null || echo "PostScript creation failed"
magick base.png test.eps 2>/dev/null || echo "EPS creation failed"

# Create SVG manually again
cat > test.svg << 'EOF'
<svg width="100" height="100" xmlns="http://www.w3.org/2000/svg">
  <defs>
    <linearGradient id="grad1" x1="0%" y1="0%" x2="0%" y2="100%">
      <stop offset="0%" style="stop-color:blue;stop-opacity:1" />
      <stop offset="100%" style="stop-color:yellow;stop-opacity:1" />
    </linearGradient>
  </defs>
  <rect width="100" height="100" fill="url(#grad1)"/>
</svg>
EOF

echo -e "\n=== Testing specific conversions ==="

# Test SVG to various formats (the problematic ones)
echo -e "\nSVG conversions:"
for fmt in png jpg gif pdf ppm ico; do
    printf "SVG -> %-4s: " "$fmt"
    if magick test.svg "svg-to-$fmt.$fmt" 2>/dev/null; then
        echo "✓ (Success)"
        ls -la "svg-to-$fmt.$fmt" | awk '{print "  Size:", $5, "bytes"}'
    else
        echo "✗ (Core dump/Failed)"
    fi
done

echo -e "\nLegacy format conversions:"
# Test PPM to modern formats
for fmt in png jpg gif pdf; do
    printf "PPM -> %-4s: " "$fmt"
    if magick test.ppm "ppm-to-$fmt.$fmt" 2>/dev/null; then
        echo "✓"
    else
        echo "✗"
    fi
done

# Test ICO conversions
echo -e "\nICO conversions:"
for fmt in png jpg gif; do
    printf "ICO -> %-4s: " "$fmt"
    if magick test.ico "ico-to-$fmt.$fmt" 2>/dev/null; then
        echo "✓"
    else
        echo "✗"
    fi
done

echo -e "\n=== File inventory ==="
ls -la test.* | grep -v ".sh"

echo -e "\n=== Format identification ==="
for f in test.png test.jpg test.gif test.pdf test.ppm test.ico test.svg; do
    if [ -f "$f" ]; then
        printf "%-12s: " "$f"
        magick identify "$f" 2>&1 | head -1 | cut -d' ' -f2-
    fi
done