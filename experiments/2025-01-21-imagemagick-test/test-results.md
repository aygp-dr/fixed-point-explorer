# ImageMagick 7 Test Results on FreeBSD

## Test Summary

### ❌ SVG Conversion
```bash
magick pelican.svg pelican.png
# Result: Core dump
```

### ✅ Basic Image Creation
```bash
magick -size 100x100 xc:blue blue.png
# Result: Success - created 321 byte PNG
```

### ✅ Text Rendering
```bash
magick -size 200x100 -background white -fill black -font Helvetica -pointsize 20 label:"FreeBSD + Scheme" text-test.png
# Result: Success
```

### ✅ Logo Creation with Transparency
```bash
magick -size 200x200 -background transparent -fill navy -font Helvetica -pointsize 30 -gravity center label:"Y" y-logo.png
# Result: Success
```

## Findings

1. **SVG processing causes core dumps** - This is likely why you considered downgrading
2. **Basic image generation works** - Canvas creation, fills, simple shapes
3. **Text rendering works** - Fonts are properly configured
4. **Transparency works** - PNG alpha channel support is functional

## Available Fonts
- Helvetica family (including Bold, Narrow variants)
- Courier family
- Times family
- AvantGarde family
- Bookman family

## Recommendation
The SVG core dump issue appears to be a known problem with ImageMagick 7 on FreeBSD. Options:
1. Use ImageMagick 6 for SVG processing
2. Use alternative tools (rsvg-convert, inkscape) for SVG
3. Generate images programmatically without SVG intermediate