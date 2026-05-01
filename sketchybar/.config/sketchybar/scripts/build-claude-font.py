#!/usr/bin/env -S uv run --quiet --with fonttools python3
"""Build a PragmataPro Mono Liga variant whose U+E861 glyph is the
Anthropic Claude mark from Font Awesome 7 Brands.

Why: sketchybar items take one font per label, so we can't combine
"Font Awesome 7 Brands" (only font on this machine with the actual
claude glyph at U+E861) and "PragmataPro Mono Liga" (the popup's text
font) in the same label. Replacing PragmataPro's placeholder glyph at
U+E861 with FA's claude glyph lets one font cover all of: claude
icon, project text, circled-digit workspace numbers.

Approach: rather than merging two fonts (different UPMs and lookup
tables make Merger blow up), copy FA's claude outline directly into
PragmataPro's CFF, rescaling to PragmataPro's coordinate system.
The output keeps PragmataPro's text, ligatures, and circled digits
intact — only U+E861 changes.

Run once after installing PragmataPro and Font Awesome 7 Brands. The
result is written to ~/Library/Fonts/ under family
"PragmataPro Mono Liga Claude".
"""
import os

from fontTools.misc.transform import Scale
from fontTools.pens.t2CharStringPen import T2CharStringPen
from fontTools.pens.transformPen import TransformPen
from fontTools.ttLib import TTFont

HOME = os.path.expanduser("~")
FONTS = os.path.join(HOME, "Library/Fonts")

PRAG_SRC = os.path.join(FONTS, "PragmataPro_Mono_R_liga_0903.otf")
FA_SRC = os.path.join(FONTS, "Font Awesome 7 Brands-Regular-400.otf")
OUT = os.path.join(FONTS, "PragmataProMonoLigaClaude-Regular.otf")

NEW_FAMILY = "PragmataPro Mono Liga Claude"
CLAUDE_CODEPOINT = 0xE861


def replace_glyph(prag: TTFont, fa: TTFont, cp: int) -> None:
    fa_cmap = fa.getBestCmap()
    fa_gname = fa_cmap[cp]
    prag_cmap = prag.getBestCmap()
    prag_gname = prag_cmap[cp]

    fa_upm = fa["head"].unitsPerEm
    prag_upm = prag["head"].unitsPerEm
    scale = prag_upm / fa_upm

    # Center FA's narrower glyph in PragmataPro's monospace cell so the
    # icon doesn't sit flush against the next character.
    fa_advance = fa["hmtx"][fa_gname][0]
    prag_advance = prag["hmtx"][prag_gname][0]
    scaled_width = fa_advance * scale
    x_offset = (prag_advance - scaled_width) / 2

    fa_glyphs = fa.getGlyphSet()
    fa_glyph = fa_glyphs[fa_gname]

    pen = T2CharStringPen(prag_advance, glyphSet=None)
    transform = Scale(scale, scale).translate(x_offset / scale, 0)
    fa_glyph.draw(TransformPen(pen, transform))
    new_cs = pen.getCharString()

    cff = prag["CFF "].cff
    top_dict = cff.topDictIndex[0]
    new_cs.private = top_dict.Private
    top_dict.CharStrings[prag_gname] = new_cs


def rename_family(font: TTFont, family: str) -> None:
    name_table = font["name"]
    name_table.setName(family, 1, 1, 0, 0)
    name_table.setName(family, 1, 3, 1, 0x409)
    name_table.setName(family, 16, 3, 1, 0x409)
    name_table.setName(f"{family} Regular", 4, 1, 0, 0)
    name_table.setName(f"{family} Regular", 4, 3, 1, 0x409)
    psname = family.replace(" ", "") + "-Regular"
    name_table.setName(psname, 6, 1, 0, 0)
    name_table.setName(psname, 6, 3, 1, 0x409)
    cff = font["CFF "].cff
    cff.fontNames = [psname]
    top_dict = cff.topDictIndex[0]
    top_dict.FullName = f"{family} Regular"
    top_dict.FamilyName = family


def main() -> None:
    if not os.path.exists(PRAG_SRC):
        raise SystemExit(f"missing source: {PRAG_SRC}")
    if not os.path.exists(FA_SRC):
        raise SystemExit(f"missing source: {FA_SRC}")

    prag = TTFont(PRAG_SRC)
    fa = TTFont(FA_SRC)
    replace_glyph(prag, fa, CLAUDE_CODEPOINT)
    rename_family(prag, NEW_FAMILY)
    prag.save(OUT)
    print(f"wrote {OUT}")


if __name__ == "__main__":
    main()
