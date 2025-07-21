# Observation: 2025-07-21 - ImageMagick 7 Core Tools

## Summary
ImageMagick 7 provides 11 core tools accessible via the `magick` command.

## Available Tools
```
animate   - animate image sequence
compare   - compare image differences
composite - overlap images
conjure   - execute MSL scripts
convert   - convert between formats (deprecated, use magick directly)
display   - display images (X11)
identify  - describe image characteristics
import    - capture screen/window
mogrify   - modify images in-place
montage   - create composite by combining images
stream    - stream image pixels
```

## Usage Pattern
```bash
magick [tool] [options] [input] [output]
# or directly:
magick [options] [input] [output]
```

## Key Delegates (format support)
- **Raster**: PNG, JPEG, TIFF, WebP, HEIC, JXL
- **Vector**: SVG (via rsvg - causes crashes), WMF
- **Compression**: zlib, zstd, lzma, bzip2
- **Features**: Cairo, FreeType, FontConfig, LCMS

## Observer Note
The SVG delegate (rsvg) appears to be the source of core dumps. All other formats tested successfully.