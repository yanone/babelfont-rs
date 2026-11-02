"""Writes the fonts the ttf reader tests load: python3 make_fixtures.py

StaticLegacy.ttf: OS/2 version 1 (no sxHeight or sCapHeight), usWeightClass 500,
usWidthClass 6, fsType 0, a PANOSE, fsSelection REGULAR | USE_TYPO_METRICS;
head.fontRevision 2.005, lowestRecPPEM 9; hhea lineGap 25; style "Medium" (name
ID 17; ID 2 is "Regular"); a Japanese family name in a Windows and a MacJapanese
record; a kern table of two subtables (x H -30, H x -20, x x 5 in both,
x gravecomb -15). Glyphs:

* bar: hmtx lsb 110, glyf xMin 100.
* barflip: bar turned 180 degrees.
* bars: two bars; its own lsb is 5 units right of its xMin.
* barrot: bar turned 90 degrees (xy = -1, yx = 1).
* barx: x and bar, bar flagged USE_MY_METRICS; its own lsb is 40 units right of
  its xMin.
* gravecomb (U+0300, advance 0) and commaaccent (unencoded, advance 120).

GdefClasses.ttf: GDEF glyph classes (gravecomb a base with an advance, acutecomb a
mark without one, tildecomb a mark with one, f_f a ligature, script a base named
like a keyword); a calt rule whose five-glyph backtrack reads back as a named
class; a GPOS kern feature beside a kern table (a f_f -40); a mark feature; vhea
900/-100, every glyph 1000 high with its vertical origin at 600.

Unnamed.ttf: post format 3; A (U+0041), Aring (U+00C5, a component of A), an
unencoded alternate of A reached by salt, and A Aring kerned -50 in a kern table.

DuplicateNames.ttf: the post table names glyphs 2 and 3 both "dupe"; Acomp is a
component of glyph 3.

MarkNoGdef.ttf: no GDEF; a mark feature attaching gravecomb (U+0300) to a;
commaaccent unencoded; uni034F (U+034F) a default-ignorable nonspacing mark.

Quadratic.ttf: o, a closed contour of four off-curves only; O, runs of two
off-curves (an implied on-curve between them) and explicit on-curves, one at the
midpoint of its off-curves; dotO, O and a dot placed by point matching (the dot's
point 0 on O's point 2); u, lines running into curves along their tangents
(smooth points); v, two curves meeting at a corner.

Variable.ttf, built by varLib from three masters (wght 100, 400 default, 900;
design 20, 80, 200 through avar) and a sparse one (wght 600, design 130, brace
only); named instances Thin, Regular and Black. A and dot change width; Adot puts
dot at a different offset in each master; O's on-curves are midpoints in every
master; Q's top on-curve is one only in Thin and Regular; brace has an
intermediate master. Kerning: @L [A Adot] against @R [O Q] -40/-50/-80 in a
class subtable, and A Adot -10/-20/-30 in a glyph pair subtable before it; a
contextual kern rule that stays feature code. acutecomb attaches to A and O
by top anchors that move with the masters. parenleft has the same
intermediate master as brace, which makes its location a shared gvar tuple.
OS/2 typo ascender, x-height and subscript size, and A's advance height,
differ between the masters (MVAR, VVAR), and so do hhea's caret slope run and
offset (a slant growing with the weight). A with U+FE00 is Adot, O with U+FE00
is O (the default glyph). The named instances have PostScript names; head was
created on 2024-01-01. STAT labels the weights: Thin,
Regular (elidable, linked to 700) and Black, Black a range from 800 to 900.
VariableNoHvar.ttf: the same without HVAR, its advances in gvar phantom points.

Avar2.ttf: wght and wdth, four masters at the corners; an avar2 mapping moves
wght 900 at wdth 75 to wght 850.

Composites.ttf: wght (100-900, default 400) and wdth (75-100, default 100),
masters on a grid of three weights by two widths, and a sparse master in the
middle of the design space (wght 650, wdth 87.5). Acomp is A with acutecomb,
the accent placed by both axes and with a master of its own at the sparse
location, off the line; Aring nests Acomp and adds ring; Abar adds bar scaled and
slanted; Odot puts dot on O's point 2 by point matching; B has a master of its
own at the sparse location.

RedditSans-Subset.ttf (not written by this script): Reddit Sans (SIL Open Font
License, see its name table) built by fontmake 3.10 from Google Fonts' sources
and subset by pyftsubset to 29 glyphs with kern, mark and mkmk: a real variable
font, with glyph-specific intermediate masters and composites.

RtlKerning.ttf: Arabic alef, beh and teh; kerning as ufo2ft writes it for a
right-to-left script, <v 0 v 0>: alef beh -50, and @BEHS [beh teh] against alef
-30.

Anchors.ttf: marks on a (top: acutecomb, bottom: dotbelowcomb, ogonek at the
bottom right: uni0328 for U+0328), on the ligature f_f (top on each component),
and acutecomb on acutecomb (mark-to-mark, the same mark class as on bases).
"""
from fontTools.feaLib.builder import addOpenTypeFeaturesFromString
from fontTools.fontBuilder import FontBuilder
from fontTools.ttLib.tables.O_S_2f_2 import Panose
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib import newTable, TTFont
from fontTools.designspaceLib import DesignSpaceDocument
from fontTools import varLib
import io
from fontTools.ttLib.tables._g_l_y_f import Glyph, GlyphComponent
from fontTools.ttLib.tables._k_e_r_n import KernTable_format_0


def save(fb, path):
    # fixed dates, so that the same script writes the same bytes
    fb.font["head"].created = fb.font["head"].modified = 0
    fb.font.recalcTimestamp = False
    fb.save(path)


def legacy_kern(font, *subtables):
    font["kern"] = kern = newTable("kern")
    kern.version, kern.kernTables = 0, []
    for pairs in subtables:
        st = KernTable_format_0()
        st.version, st.coverage, st.kernTable = 0, 1, pairs
        kern.kernTables.append(st)


def rect(x0, y0, x1, y1):
    pen = TTGlyphPen(None)
    pen.moveTo((x0, y0))
    pen.lineTo((x0, y1))
    pen.lineTo((x1, y1))
    pen.lineTo((x1, y0))
    pen.closePath()
    return pen.glyph()


def composite(*parts):
    g = Glyph()
    g.numberOfContours = -1
    g.components = []
    for name, dx, dy, matrix, *flags in parts:
        c = GlyphComponent()
        c.glyphName, c.x, c.y = name, dx, dy
        c.flags = 0x4 | (flags[0] if flags else 0)  # ROUND_XY_TO_GRID
        if matrix is not None:
            c.transform = matrix
        g.components.append(c)
    return g


USE_MY_METRICS = 0x200
order = [".notdef", "x", "H", "bar", "barflip", "bars", "barrot", "barx", "gravecomb",
         "commaaccent"]
glyphs = {
    ".notdef": Glyph(),
    "x": rect(50, 0, 450, 480),
    "H": rect(60, 0, 540, 700),
    "bar": rect(100, 0, 200, 600),
    "barflip": composite(("bar", 300, 600, [[-1, 0], [0, -1]])),
    "bars": composite(("bar", 0, 0, None), ("bar", 250, 0, None)),
    # fontTools' transform is [[xx, yx], [xy, yy]]: x' = xx*x + xy*y, y' = yx*x + yy*y
    "barrot": composite(("bar", 700, 0, [[0, 1], [-1, 0]])),
    "barx": composite(("x", 300, 0, None), ("bar", 0, 0, None, USE_MY_METRICS)),
    "gravecomb": rect(-200, 500, -100, 600),
    "commaaccent": rect(20, -200, 100, -50),
}
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({ord("x"): "x", ord("H"): "H", ord("|"): "bar", 0x0300: "gravecomb"})
fb.setupGlyf(glyphs)
lsb = {".notdef": 0, "x": 50, "H": 60, "bar": 110, "barflip": 100, "bars": 105, "barrot": 100, "barx": 140,
       "gravecomb": -200, "commaaccent": 20}
advance = {"gravecomb": 0, "commaaccent": 120}
fb.setupHorizontalMetrics({g: (advance.get(g, 800), lsb[g]) for g in order})
fb.setupHorizontalHeader(ascent=800, descent=-200, lineGap=25)
fb.setupNameTable({
    "familyName": "Fixture Medium",
    "styleName": "Regular",
    "typographicFamily": "Fixture",
    "typographicSubfamily": "Medium",
})
fb.setupOS2(
    version=1, usWeightClass=500, usWidthClass=6, fsType=0, fsSelection=0x40 | 0x80,
    sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200,
    panose=Panose(bFamilyType=2, bSerifStyle=0, bWeight=6, bProportion=3),
)
fb.setupPost()
fb.font["head"].fontRevision = 2.005
fb.font["head"].lowestRecPPEM = 9
# a Japanese family name, in a Windows record and a MacJapanese one
fb.font["name"].setName("\u3057\u3063\u307d\u308a", 1, 3, 1, 0x411)
fb.font["name"].setName("\u3057\u3063\u307d\u308a", 1, 1, 1, 11)
legacy_kern(fb.font, {("x", "H"): -30, ("x", "x"): 5, ("x", "gravecomb"): -15},
            {("H", "x"): -20, ("x", "x"): 5})
save(fb, "StaticLegacy.ttf")


order = [".notdef", "a", "gravecomb", "acutecomb", "tildecomb", "f_f", "script"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({ord("a"): "a", 0x0300: "gravecomb", 0x0301: "acutecomb", 0x0303: "tildecomb"})
fb.setupGlyf({".notdef": Glyph(), "a": rect(50, 0, 450, 500), "gravecomb": rect(40, 550, 150, 700),
              "acutecomb": rect(-150, 550, -40, 700), "tildecomb": rect(20, 550, 280, 650),
              "f_f": rect(50, 0, 750, 700), "script": rect(50, 0, 450, 500)})
advance = {".notdef": 500, "a": 500, "gravecomb": 190, "acutecomb": 0, "tildecomb": 300, "f_f": 800,
           "script": 500}
fb.setupHorizontalMetrics({g: (advance[g], fb.font["glyf"][g].xMin if g != ".notdef" else 0)
                           for g in order})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupVerticalHeader(ascent=900, descent=-100)
fb.setupVerticalMetrics({g: (1000, 600 - fb.font["glyf"][g].yMax if g != ".notdef" else 0)
                         for g in order})
fb.setupNameTable({"familyName": "Fixture Gdef", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
addOpenTypeFeaturesFromString(fb.font, """
lookup up { sub gravecomb by acutecomb; } up;
feature calt {
    sub [a gravecomb acutecomb tildecomb f_f] gravecomb' lookup up;
} calt;
feature kern {
    pos a a -10;
} kern;
markClass acutecomb <anchor -100 600> @ACUTE;
feature mark {
    pos base a <anchor 250 500> mark @ACUTE;
} mark;
table GDEF {
    GlyphClassDef [a gravecomb \\script], [f_f], [acutecomb tildecomb], ;
} GDEF;
""")
legacy_kern(fb.font, {("a", "f_f"): -40})
save(fb, "GdefClasses.ttf")


order = [".notdef", "A", "Aring", "A.alt"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({0x41: "A", 0xC5: "Aring"})
fb.setupGlyf({".notdef": Glyph(), "A": rect(50, 0, 550, 700), "Aring": composite(("A", 0, 0, None)),
              "A.alt": rect(60, 0, 560, 700)})
fb.setupHorizontalMetrics({".notdef": (500, 0), "A": (600, 50), "Aring": (600, 50), "A.alt": (600, 60)})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture Unnamed", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
addOpenTypeFeaturesFromString(fb.font, "feature salt { sub A by A.alt; } salt;")
legacy_kern(fb.font, {("A", "Aring"): -50})
fb.setupPost(keepGlyphNames=False)
save(fb, "Unnamed.ttf")


order = [".notdef", "A", "dupa", "dupb", "Acomp"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({0x41: "A", 0xC0: "Acomp"})
fb.setupGlyf({".notdef": Glyph(), "A": rect(50, 0, 550, 700), "dupa": rect(60, 0, 160, 100),
              "dupb": rect(70, 0, 170, 100), "Acomp": composite(("dupb", 0, 0, None))})
fb.setupHorizontalMetrics({".notdef": (500, 0), "A": (600, 50), "dupa": (600, 60), "dupb": (600, 70),
                           "Acomp": (600, 70)})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture Duplicate Names", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
save(fb, "DuplicateNames.ttf")
data = open("DuplicateNames.ttf", "rb").read()
# both names become "dupe": the same length, so nothing in the post table moves
assert data.count(b"\x04dupa") == 1 and data.count(b"\x04dupb") == 1
open("DuplicateNames.ttf", "wb").write(data.replace(b"\x04dupa", b"\x04dupe").replace(b"\x04dupb", b"\x04dupe"))


order = [".notdef", "a", "gravecomb", "commaaccent", "uni034F"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({ord("a"): "a", 0x0300: "gravecomb", 0x034F: "uni034F"})
fb.setupGlyf({".notdef": Glyph(), "a": rect(50, 0, 450, 500), "gravecomb": rect(-150, 550, -40, 700),
              "commaaccent": rect(20, -200, 100, -50), "uni034F": Glyph()})
fb.setupHorizontalMetrics({".notdef": (500, 0), "a": (500, 50), "gravecomb": (0, -150),
                           "commaaccent": (120, 20), "uni034F": (0, 0)})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture Mark No Gdef", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
addOpenTypeFeaturesFromString(fb.font, """
markClass gravecomb <anchor -100 600> @TOP;
feature mark {
    pos base a <anchor 250 500> mark @TOP;
} mark;
""")
del fb.font["GDEF"]
save(fb, "MarkNoGdef.ttf")


def quadratic(*contours):
    """Contours of TTGlyphPen calls: ("m", pt) moveTo, ("q", *pts) qCurveTo,
    ("o", *pts) a closed contour of off-curves only"""
    pen = TTGlyphPen(None)
    for contour in contours:
        for op, *pts in contour:
            if op == "m":
                pen.moveTo(pts[0])
            elif op == "q":
                pen.qCurveTo(*pts)
            elif op == "o":
                pen.qCurveTo(*pts, None)
        pen.closePath()
    return pen.glyph()


def o_contour(sx):
    # explicit on-curves; (250, 700) is the midpoint of its off-curves, and the
    # first run has two off-curves with an implied on-curve between them
    x = lambda v: round(v * sx)
    return [("m", (x(250), 0)), ("q", (x(450), 0), (x(500), 200), (x(500), 350)),
            ("q", (x(500), 700), (x(250), 700)), ("q", (x(0), 700), (x(0), 350)),
            ("q", (x(0), 0), (x(250), 0))]


def tangent_u():
    pen = TTGlyphPen(None)
    pen.moveTo((0, 700))
    pen.lineTo((0, 250))
    pen.qCurveTo((0, 0), (250, 0))
    pen.qCurveTo((500, 0), (500, 250))
    pen.lineTo((500, 700))
    pen.closePath()
    return pen.glyph()


def corner_v():
    pen = TTGlyphPen(None)
    pen.moveTo((0, 700))
    pen.qCurveTo((100, 300), (250, 0))
    pen.qCurveTo((400, 300), (500, 700))
    pen.closePath()
    return pen.glyph()


order = [".notdef", "o", "O", "dot", "dotO", "u", "v"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({ord("o"): "o", ord("O"): "O", ord("."): "dot", ord("u"): "u", ord("v"): "v"})
dotO = composite(("O", 0, 0, None), ("dot", 0, 0, None))
dotO.components[1].firstPt, dotO.components[1].secondPt = 2, 0
del dotO.components[1].x, dotO.components[1].y
fb.setupGlyf({".notdef": Glyph(), "o": quadratic([("o", (0, 250), (250, 500), (500, 250), (250, 0))]),
              "O": quadratic(o_contour(1)), "dot": rect(0, 0, 100, 100), "dotO": dotO,
              "u": tangent_u(), "v": corner_v()})
fb.setupHorizontalMetrics({g: (600, fb.font["glyf"][g].xMin if g != ".notdef" else 0) for g in order})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture Quadratic", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
save(fb, "Quadratic.ttf")


def metric_of(weight, values):
    return {100: values[0], 400: values[1], 900: values[2]}.get(weight, values[1])


def variable_master(weight, sx, dot, dot_x, brace_right, kern, sparse=False):
    order = [".notdef", "A", "dot", "Adot", "O", "Q", "brace", "parenleft", "acutecomb"]
    a_right = {100: 400, 400: 500, 900: 650}.get(weight, 500)
    q = o_contour(sx)
    if weight == 900:
        q[2] = ("q", (round(550 * sx), 700), (round(300 * sx), 700))
    glyphs = {
        ".notdef": Glyph(),
        "A": rect(50, 0, a_right, 700),
        "dot": rect(0, 0, dot, dot),
        "Adot": composite(("A", 0, 0, None), ("dot", dot_x, 750, None)),
        "O": quadratic(o_contour(sx)),
        "Q": quadratic(q),
        "brace": rect(100, 0, brace_right, 500),
        "parenleft": rect(100, 0, brace_right, 600),
        "acutecomb": rect(-100, 720, -20, 820),
    }
    if sparse:
        order = [".notdef", "brace", "parenleft"]
        glyphs = {g: glyphs[g] for g in order}
    fb = FontBuilder(1000, isTTF=True)
    fb.setupGlyphOrder(order)
    uvs = [(ord("A"), 0xFE00, "Adot"), (ord("O"), 0xFE00, None)] if not sparse else []
    fb.setupCharacterMap({ord(c): c for c in "AOQ" if c in order} | ({ord("{"): "brace"}), uvs=uvs)
    fb.setupGlyf(glyphs)
    advance = {".notdef": 500, "A": a_right + 50, "dot": dot + 100, "Adot": a_right + 50,
               "O": round(500 * sx) + 50, "Q": round(500 * sx) + 50, "brace": brace_right + 100,
               "parenleft": brace_right + 100, "acutecomb": 0}
    fb.setupHorizontalMetrics({g: (advance[g], fb.font["glyf"][g].xMin if g != ".notdef" else 0)
                               for g in order})
    fb.setupHorizontalHeader(ascent=800, descent=-200, caretSlopeRise=1000,
                             caretSlopeRun=metric_of(weight, (100, 150, 200)),
                             caretOffset=metric_of(weight, (-5, -10, -15)))
    fb.setupVerticalHeader(ascent=900, descent=-100)
    a_height = {100: 1000, 400: 1050, 900: 1100}.get(weight, 1050)
    fb.setupVerticalMetrics({g: (a_height if g == "A" else 1000,
                                 900 - fb.font["glyf"][g].yMax if g != ".notdef" else 0)
                             for g in order})
    fb.setupNameTable({"familyName": "Fixture Variable", "styleName": "Regular"})
    metric = lambda values: {100: values[0], 400: values[1], 900: values[2]}.get(weight, values[1])
    fb.setupOS2(version=4, sTypoAscender=metric((780, 800, 840)), sTypoDescender=-200,
                usWinAscent=800, usWinDescent=200, sxHeight=metric((480, 500, 530)),
                sCapHeight=700, ySubscriptYSize=metric((600, 650, 700)))
    fb.setupPost()
    if not sparse:
        pair, klass = kern
        addOpenTypeFeaturesFromString(fb.font, f"""
@L = [A Adot];
@R = [O Q];
lookup ctx {{ pos O 5; }} ctx;
feature kern {{
    pos A Adot {pair};
    pos @L @R {klass};
    pos Q' lookup ctx A;
}} kern;
markClass acutecomb <anchor -60 720> @TOP;
feature mark {{
    pos base A <anchor {a_right // 2 + 25} 720> mark @TOP;
    pos base O <anchor {round(250 * sx)} 720> mark @TOP;
}} mark;
table GDEF {{
    GlyphClassDef [A dot Adot O Q brace parenleft], , [acutecomb], ;
}} GDEF;
""")
    # through a file, so that the font is compiled
    buf = io.BytesIO()
    fb.font.save(buf)
    buf.seek(0)
    return TTFont(buf)


from fontTools.designspaceLib import AxisLabelDescriptor

doc = DesignSpaceDocument()
doc.addAxisDescriptor(name="Weight", tag="wght", minimum=100, default=400, maximum=900,
                      map=[(100, 20), (400, 80), (600, 130), (900, 200)],
                      axisLabels=[AxisLabelDescriptor(name="Thin", userValue=100),
                                  AxisLabelDescriptor(name="Regular", userValue=400, elidable=True),
                                  AxisLabelDescriptor(name="Black", userValue=900,
                                                      userMinimum=800, userMaximum=900)])
doc.elidedFallbackName = "Regular"
for weight, design, args in [(100, 20, (0.8, 100, 150, 200, (-10, -40))),
                             (400, 80, (1.0, 120, 200, 300, (-20, -50))),
                             (600, 130, (1.0, 120, 200, 450, None)),
                             (900, 200, (1.3, 160, 300, 400, (-30, -80)))]:
    source = doc.addSourceDescriptor(name=f"wght{weight}", location={"Weight": design})
    source.font = variable_master(weight, *args, sparse=weight == 600)
for name, weight in [("Thin", 100), ("Regular", 400), ("Black", 900)]:
    doc.addInstanceDescriptor(styleName=name, postScriptFontName=f"FixtureVariable-{name}",
                              location={"Weight": {100: 20, 400: 80, 900: 200}[weight]})
vf, _, _ = varLib.build(doc)
from fontTools.otlLib.builder import buildStatTable
buildStatTable(vf, [{"tag": "wght", "name": "Weight", "values": [
    {"value": 100, "name": "Thin"},
    {"value": 400, "name": "Regular", "flags": 0x2, "linkedValue": 700},
    {"nominalValue": 900, "rangeMinValue": 800, "rangeMaxValue": 900, "name": "Black"},
]}], elidedFallbackName="Regular")
vf["head"].created = 3786912000  # 2024-01-01, in seconds since 1904
vf["head"].modified = 0
vf.recalcTimestamp = False
vf.save("Variable.ttf")
del vf["HVAR"]
vf.save("VariableNoHvar.ttf")


order = [".notdef", "a", "f_f", "acutecomb", "dotbelowcomb", "uni0328"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({ord("a"): "a", 0x0301: "acutecomb", 0x0323: "dotbelowcomb", 0x0328: "uni0328"})
fb.setupGlyf({".notdef": Glyph(), "a": rect(50, 0, 450, 500), "f_f": rect(50, 0, 950, 700),
              "acutecomb": rect(-150, 550, -50, 700), "dotbelowcomb": rect(-120, -150, -80, -100),
              "uni0328": rect(-100, -200, 0, 0)})
fb.setupHorizontalMetrics({g: ({"f_f": 1000, "a": 500}.get(g, 0), fb.font["glyf"][g].xMin if g != ".notdef" else 0)
                           for g in order})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture Anchors", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
addOpenTypeFeaturesFromString(fb.font, """
markClass acutecomb <anchor -100 500> @TOP;
markClass dotbelowcomb <anchor -100 0> @BOTTOM;
markClass uni0328 <anchor -50 0> @OGONEK;
feature mark {
    pos base a <anchor 250 500> mark @TOP <anchor 250 0> mark @BOTTOM <anchor 420 0> mark @OGONEK;
    pos ligature f_f <anchor 250 700> mark @TOP ligComponent <anchor 750 700> mark @TOP;
} mark;
feature mkmk {
    pos mark acutecomb <anchor -100 750> mark @TOP;
} mkmk;
table GDEF {
    GlyphClassDef [a], [f_f], [acutecomb dotbelowcomb uni0328], ;
} GDEF;
""")
save(fb, "Anchors.ttf")


def corner_master(weight, width):
    order = [".notdef", "I"]
    right = {100: 100, 900: 300}[weight] * width // 100
    fb = FontBuilder(1000, isTTF=True)
    fb.setupGlyphOrder(order)
    fb.setupCharacterMap({ord("I"): "I"})
    fb.setupGlyf({".notdef": Glyph(), "I": rect(50, 0, 50 + right, 700)})
    fb.setupHorizontalMetrics({".notdef": (500, 0), "I": (right + 100, 50)})
    fb.setupHorizontalHeader(ascent=800, descent=-200)
    fb.setupNameTable({"familyName": "Fixture Avar2", "styleName": "Regular"})
    fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
    fb.setupPost()
    buf = io.BytesIO()
    fb.font.save(buf)
    buf.seek(0)
    return TTFont(buf)


from fontTools.designspaceLib import AxisMappingDescriptor

doc = DesignSpaceDocument()
doc.addAxisDescriptor(name="Weight", tag="wght", minimum=100, default=100, maximum=900)
doc.addAxisDescriptor(name="Width", tag="wdth", minimum=75, default=100, maximum=100)
for weight in (100, 900):
    for width in (75, 100):
        source = doc.addSourceDescriptor(name=f"w{weight}-{width}",
                                         location={"Weight": weight, "Width": width})
        source.font = corner_master(weight, width)
mapping = AxisMappingDescriptor()
mapping.inputLocation = {"Weight": 900, "Width": 75}
mapping.outputLocation = {"Weight": 850, "Width": 75}
doc.axisMappings = [mapping]
vf, _, _ = varLib.build(doc)
vf["head"].created = vf["head"].modified = 0
vf.recalcTimestamp = False
vf.save("Avar2.ttf")


order = [".notdef", "alef-ar", "beh-ar", "teh-ar"]
fb = FontBuilder(1000, isTTF=True)
fb.setupGlyphOrder(order)
fb.setupCharacterMap({0x0627: "alef-ar", 0x0628: "beh-ar", 0x062A: "teh-ar"})
fb.setupGlyf({".notdef": Glyph(), "alef-ar": rect(50, 0, 150, 700), "beh-ar": rect(50, 0, 550, 200),
              "teh-ar": rect(50, 0, 550, 220)})
fb.setupHorizontalMetrics({".notdef": (500, 0), "alef-ar": (200, 50), "beh-ar": (600, 50),
                           "teh-ar": (600, 50)})
fb.setupHorizontalHeader(ascent=800, descent=-200)
fb.setupNameTable({"familyName": "Fixture RTL Kerning", "styleName": "Regular"})
fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
fb.setupPost()
addOpenTypeFeaturesFromString(fb.font, """
languagesystem DFLT dflt;
languagesystem arab dflt;
@BEHS = [beh-ar teh-ar];
@ALEFS = [alef-ar];
feature kern {
    pos alef-ar beh-ar <-50 0 -50 0>;
    pos @BEHS @ALEFS <-30 0 -30 0>;
} kern;
""")
save(fb, "RtlKerning.ttf")


def composites_master(weight, width, sparse=False):
    """Composites.ttf's master at a location; at the sparse location, Acomp's
    accent and B are off the line between the masters around it"""
    a_right = round((250 + 0.4 * (weight - 100)) * width / 100)
    acute_x = a_right // 2 + 60 + (40 if sparse else 0)
    sx = (0.8 + (weight - 100) / 1600) * width / 100
    order = [".notdef", "A", "O", "acutecomb", "dot", "ring", "bar", "Acomp", "Aring", "Abar",
             "Odot", "B"]
    odot = composite(("O", 0, 0, None), ("dot", 0, 0, None))
    odot.components[1].firstPt, odot.components[1].secondPt = 2, 0
    del odot.components[1].x, odot.components[1].y
    dot = 60 + weight // 20
    b_right = 300 + weight // 4 + (120 if sparse else 0)
    glyphs = {
        ".notdef": Glyph(),
        "A": rect(50, 0, a_right, 700),
        "O": quadratic(o_contour(sx)),
        "acutecomb": rect(-60, 720, 20 + weight // 20, 820),
        "dot": rect(0, 0, dot, dot),
        "ring": quadratic([("o", (0, 40), (40, 80), (80, 40), (40, 0))]),
        "bar": rect(0, 0, 20 + weight // 40, 300),
        "Acomp": composite(("A", 0, 0, None), ("acutecomb", acute_x, 0, None)),
        "Aring": composite(("Acomp", 0, 0, None), ("ring", a_right // 2 - 40, 860, None)),
        "Abar": composite(("A", 0, 0, None), ("bar", a_right // 3, 100, [[0.8, 0], [0.3, 0.8]])),
        "Odot": odot,
        "B": rect(60, 0, b_right, 700),
    }
    if sparse:
        order = [".notdef", "A", "acutecomb", "Acomp", "B"]
        glyphs = {g: glyphs[g] for g in order}
    fb = FontBuilder(1000, isTTF=True)
    fb.setupGlyphOrder(order)
    fb.setupCharacterMap({ord(c): c for c in "AOB" if c in order})
    fb.setupGlyf(glyphs)
    fb.setupHorizontalMetrics({g: (600 if g != ".notdef" else 500,
                                   fb.font["glyf"][g].xMin if g != ".notdef" else 0)
                               for g in order})
    fb.setupHorizontalHeader(ascent=800, descent=-200)
    fb.setupNameTable({"familyName": "Fixture Composites", "styleName": "Regular"})
    fb.setupOS2(sTypoAscender=800, sTypoDescender=-200, usWinAscent=800, usWinDescent=200)
    fb.setupPost()
    buf = io.BytesIO()
    fb.font.save(buf)
    buf.seek(0)
    return TTFont(buf)


doc = DesignSpaceDocument()
doc.addAxisDescriptor(name="Weight", tag="wght", minimum=100, default=400, maximum=900)
doc.addAxisDescriptor(name="Width", tag="wdth", minimum=75, default=100, maximum=100)
for weight in (100, 400, 900):
    for width in (75, 100):
        source = doc.addSourceDescriptor(name=f"w{weight}-{width}",
                                         location={"Weight": weight, "Width": width})
        source.font = composites_master(weight, width)
source = doc.addSourceDescriptor(name="sparse", location={"Weight": 650, "Width": 87.5})
source.font = composites_master(650, 87.5, sparse=True)
vf, _, _ = varLib.build(doc)
vf["head"].created = vf["head"].modified = 0
vf.recalcTimestamp = False
vf.save("Composites.ttf")
