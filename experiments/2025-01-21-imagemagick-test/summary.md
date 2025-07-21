# ImageMagick 7 Format Testing Summary

## Test Results

### ❌ SVG Issues (Critical)
- **ANY** SVG operation causes core dump
- This includes: identify, convert from, convert to
- Affects all output formats when SVG is involved

### ✅ Working Format Conversions
All non-SVG formats work perfectly:
- **Raster**: PNG ↔ JPG ↔ GIF ↔ BMP ↔ TIFF ↔ WebP
- **Legacy**: PPM, ICO, XPM, PCX, TGA (all work)
- **PDF**: Can create and convert (except from SVG)
- **PostScript**: PS/EPS generation works

### ✅ Advanced Features Working
1. **Animated GIFs**: Successfully created 4-frame animation
2. **Multi-resolution ICO**: Created 64/48/32/16px icon
3. **Text formats**: XPM (ASCII art) generation works
4. **Gradients**: All gradient operations successful
5. **Drawing**: Shapes, text, transformations all work

## File Sizes (100x100 gradient)
- PNG: 960 bytes (compressed)
- JPG: 1,159 bytes 
- GIF: 2,117 bytes (128 colors)
- PDF: 181 bytes (vector-like)
- PPM: 60,017 bytes (uncompressed)
- ICO: 41,662 bytes (multiple resolutions)

## Conclusion
ImageMagick 7 on FreeBSD has a **single critical bug**: SVG handling causes immediate core dumps. Everything else works perfectly. 

## Workarounds
1. Use `rsvg-convert` for SVG operations
2. Generate graphics directly without SVG intermediate
3. Use ImageMagick 6 if SVG is critical
4. Convert SVGs on a different system