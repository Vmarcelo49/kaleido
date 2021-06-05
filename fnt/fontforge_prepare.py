# FontForge script to export the glyphs of a font to a format supported by
# generate.rb. To use, open the font you want to export in FontForge, then do
# File -> Execute Script and paste this script, making sure to set the
# desired output path. Click OK and after a while it should have exported all
# the glyphs.
import fontforge
import pathlib

font = fontforge.activeFont()

out_folder = pathlib.Path("") # change path here
out_folder.mkdir(parents = True, exist_ok = True)

out_formats = [
    ["1x", 127],
    ["0.5x", 63],
    ["0.25x", 31],
    ["0.125x", 15]
]

for glyph in font.glyphs():
    if glyph.unicode > -1:
        glyph_folder_name = f"glyph_{glyph.unicode}_{glyph.width}"
        glyph_folder = out_folder / glyph_folder_name
        glyph_folder.mkdir(exist_ok = True)

        for format_name, pixel_size in out_formats:
            glyph.export(str(glyph_folder / f"{format_name}.png"),
                         pixelsize = pixel_size)

        print(f"Exported glyph {glyph_folder_name}")
