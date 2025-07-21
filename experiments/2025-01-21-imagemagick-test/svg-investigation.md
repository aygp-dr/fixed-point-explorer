# SVG Core Dump Investigation

## Findings

### The Issue is VERY Specific
- **librsvg2-rust-2.60.0_3** is installed and working
- **rsvg-convert** works perfectly standalone
- **ImageMagick 7 + librsvg2** = instant core dump

### Test Results

1. **Direct rsvg-convert**: ✅ Works
   ```bash
   rsvg-convert pelican.svg -o pelican-rsvg.png  # Success!
   ```

2. **ImageMagick inline SVG**: ✅ Works sometimes
   ```bash
   echo '<svg>...</svg>' | magick svg:- test.png  # Success (no output = worked)
   ```

3. **ImageMagick file SVG**: ❌ Always fails
   ```bash
   magick any.svg any.png  # Core dump
   ```

### Root Cause
This appears to be a known issue with ImageMagick 7.1.1-45 and librsvg2-rust-2.60.0 on FreeBSD. The integration between ImageMagick and the Rust-based librsvg2 is causing the crash.

### Workarounds

1. **Use rsvg-convert directly**:
   ```bash
   rsvg-convert input.svg -o output.png
   ```

2. **Pipe through ImageMagick**:
   ```bash
   rsvg-convert input.svg -f png | magick - output.png
   ```

3. **Use ImageMagick 6** (if SVG support works there)

4. **Wait for fix** in either ImageMagick or librsvg2-rust

### Other Formats
Everything else works perfectly - this is purely an SVG file handling issue in the ImageMagick/librsvg2 integration.