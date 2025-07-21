# Apache Icons Test Results

## Successfully Downloaded & Tested
- **apache_pb.gif** (4.4KB) - "Powered by Apache" banner
- **apache_pb.png** (8.3KB) - PNG version 
- **folder.gif** (225B) - Small folder icon
- **apache_pb.svg** (260KB!) - Vector version

## Conversion Tests

### ✅ Working
1. **GIF → PNG resize**: Reduced to 50% size successfully
2. **SVG → PNG via rsvg-convert**: 588x69 output (28KB)
3. **Montage creation**: Combined multiple images

### ❌ Still Broken
```bash
magick apache_pb.svg any_format  # Core dump
```

## File Size Comparison
- GIF: 4.4KB (indexed color, very efficient)
- PNG: 8.3KB (lossless but larger)
- SVG: 260KB (surprisingly huge for a logo!)
- SVG→PNG: 28KB (larger dimensions: 588x69 vs 260x30)

## Useful Discovery
Apache provides a comprehensive icon set at https://www.apache.org/icons/ including:
- Multiple formats (GIF, PNG, SVG)
- Various sizes and styles
- Free to use
- Perfect for testing image conversion tools

The 260KB SVG file is particularly interesting - it's likely highly detailed with many paths, making it a good stress test for SVG renderers (which is probably why it crashes ImageMagick!).