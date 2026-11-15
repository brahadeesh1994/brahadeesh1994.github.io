"""
Make the CAT Lab site load Latin Modern Roman from its own fonts/ folder.

Before running: copy the fonts/ folder from this package into your website
folder, next to index.html and style.css.

Run it from your website folder:

    python3 fix_latin_modern.py     (Mac)
    py fix_latin_modern.py          (Windows)

What it does:
  1. In every .html file in this folder and its subfolders, it removes the
     line that loads lmodern@0.0.3 from jsDelivr (that package does not
     exist, so the line never worked).
  2. At the top of style.css, it adds the rules that load Latin Modern Roman
     (regular, bold, italic and bold italic) from the fonts/ folder.
Nothing else is changed. Every changed file is first copied to a backup
ending in .bak. Running the script twice does no harm.
"""

import pathlib
import re
import shutil

ROOT = pathlib.Path(".")

LMODERN_LINE = re.compile(
    r'[ \t]*<link rel="stylesheet" href="https://cdn\.jsdelivr\.net/npm/lmodern@0\.0\.3/dist/lmodern\.css">[ \t]*\r?\n'
)

FONT_FACE = """
/* ============================================================
   FONTS: Latin Modern Roman, served from the site's own fonts/ folder
   (Latin Modern 10pt fonts by GUST, under the GUST Font License;
   see fonts/GUST-FONT-LICENSE.txt)
   ============================================================ */

@font-face {
    font-family: "Latin Modern Roman";
    src: url("fonts/LatinModernRoman-Regular.woff2") format("woff2");
    font-style: normal;
    font-weight: normal;
    font-display: swap;
}

@font-face {
    font-family: "Latin Modern Roman";
    src: url("fonts/LatinModernRoman-Bold.woff2") format("woff2");
    font-style: normal;
    font-weight: bold;
    font-display: swap;
}

@font-face {
    font-family: "Latin Modern Roman";
    src: url("fonts/LatinModernRoman-Italic.woff2") format("woff2");
    font-style: italic;
    font-weight: normal;
    font-display: swap;
}

@font-face {
    font-family: "Latin Modern Roman";
    src: url("fonts/LatinModernRoman-BoldItalic.woff2") format("woff2");
    font-style: italic;
    font-weight: bold;
    font-display: swap;
}

"""

changed = []
problems = []


def read(path):
    # read the raw bytes so that Windows line endings are kept exactly
    text = path.read_bytes().decode("utf-8")
    return text, ("\r\n" if "\r\n" in text else "\n")


def write(path, text):
    shutil.copy2(path, path.with_name(path.name + ".bak"))
    path.write_bytes(text.encode("utf-8"))
    changed.append(str(path))


fonts = ROOT / "fonts"
for name in ["Regular", "Bold", "Italic", "BoldItalic"]:
    if not (fonts / f"LatinModernRoman-{name}.woff2").exists():
        problems.append(f"fonts/LatinModernRoman-{name}.woff2 is missing: "
                        "copy the fonts folder into your website folder first")
if problems:
    print("Nothing was changed.")
    for p in problems:
        print("  " + p)
    raise SystemExit(1)

for path in sorted(ROOT.rglob("*.html")):
    text, nl = read(path)
    new, n = LMODERN_LINE.subn("", text)
    if n:
        write(path, new)
    elif "lmodern" in text:
        problems.append(f"{path}: mentions lmodern, but not in the expected form")

css = ROOT / "style.css"
if not css.exists():
    problems.append("style.css not found in this folder")
else:
    text, nl = read(css)
    if "fonts/LatinModernRoman-Regular.woff2" in text:
        pass  # already done
    else:
        block = FONT_FACE.replace("\n", nl)
        end = text.find("*/")
        if end == -1:
            text = block.lstrip(nl) + text
        else:
            # insert just after the opening comment of the stylesheet
            cut = text.find(nl, end)
            cut = len(text) if cut == -1 else cut + len(nl)
            text = text[:cut] + block + text[cut:]
        write(css, text)

print(f"Changed {len(changed)} file(s):")
for c in changed:
    print("  " + c)
print()
if problems:
    print("Please look at these by hand:")
    for p in problems:
        print("  " + p)
else:
    print("No problems found.")
