#!/usr/bin/env python3
"""
Generate compressed thumbnail copies of every photo in assets/photos/,
saved into assets/photos/thumbs/ with the same filenames (as .jpg).

Usage:
    pip install Pillow
    python3 make_thumbnails.py

Run this from the root of your site (the folder containing assets/).
"""

from pathlib import Path
from PIL import Image, ImageOps

SOURCE_DIR = Path("assets/photos")
THUMB_DIR = SOURCE_DIR / "thumbs"
MAX_DIMENSION = 900   # longest side, in pixels
JPEG_QUALITY = 82

def make_thumbnail(src_path: Path, dest_path: Path):
    with Image.open(src_path) as img:
        # Respect EXIF orientation (phone photos are often stored rotated)
        img = ImageOps.exif_transpose(img)
        img = img.convert("RGB")  # handles PNG/webp transparency, CMYK, etc.
        img.thumbnail((MAX_DIMENSION, MAX_DIMENSION), Image.LANCZOS)
        dest_path.parent.mkdir(parents=True, exist_ok=True)
        img.save(dest_path, "JPEG", quality=JPEG_QUALITY, optimize=True)

def main():
    if not SOURCE_DIR.is_dir():
        print(f"Error: {SOURCE_DIR} not found. Run this script from your site's root folder.")
        return

    extensions = {".jpg", ".jpeg", ".png", ".webp", ".JPG", ".JPEG", ".PNG", ".WEBP"}
    photos = [p for p in SOURCE_DIR.iterdir() if p.is_file() and p.suffix in extensions]

    if not photos:
        print(f"No photos found in {SOURCE_DIR}.")
        return

    print(f"Found {len(photos)} photos. Generating thumbnails in {THUMB_DIR}/ ...")

    total_before = 0
    total_after = 0
    for src in sorted(photos):
        dest = THUMB_DIR / (src.stem + ".jpg")
        make_thumbnail(src, dest)
        before = src.stat().st_size
        after = dest.stat().st_size
        total_before += before
        total_after += after
        print(f"  {src.name:45s} {before/1024:8.0f} KB -> {after/1024:6.0f} KB")

    print()
    print(f"Done. Total: {total_before/1024/1024:.1f} MB -> {total_after/1024/1024:.1f} MB")
    print(f"({100 * (1 - total_after/total_before):.0f}% smaller)")

if __name__ == "__main__":
    main()
