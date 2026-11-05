#!/usr/bin/env python3
"""Build the reproducible Word reference document used for Land Use Policy review files.

Land Use Policy requires an editable, single-column Word manuscript but does not prescribe
one mandatory font family/size in its Guide for Authors. We therefore use a conservative,
consistent submission convention: Times New Roman throughout; 12 pt body text; 12 pt bold
section headings; 10 pt table text; one-inch margins; compact paragraph spacing.
"""
from __future__ import annotations

import shutil
import subprocess
import tempfile
import zipfile
from pathlib import Path
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "submission" / "land-use-policy" / "reference.docx"
W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
NS = {"w": W}
ET.register_namespace("w", W)


def qn(tag: str) -> str:
    prefix, local = tag.split(":", 1)
    if prefix != "w":
        raise ValueError(tag)
    return f"{{{W}}}{local}"


def ensure(parent: ET.Element, tag: str) -> ET.Element:
    child = parent.find(f"w:{tag}", NS)
    if child is None:
        child = ET.SubElement(parent, qn(f"w:{tag}"))
    return child


def set_style_font(style: ET.Element, size_half_points: int, bold: bool | None = None, italic: bool | None = None) -> None:
    rpr = style.find("w:rPr", NS)
    if rpr is None:
        rpr = ET.SubElement(style, qn("w:rPr"))
    fonts = ensure(rpr, "rFonts")
    for attr in ("ascii", "hAnsi", "eastAsia", "cs"):
        fonts.set(qn(f"w:{attr}"), "Times New Roman")
    sz = ensure(rpr, "sz")
    sz.set(qn("w:val"), str(size_half_points))
    szcs = ensure(rpr, "szCs")
    szcs.set(qn("w:val"), str(size_half_points))
    if bold is not None:
        b = rpr.find("w:b", NS)
        if bold and b is None:
            ET.SubElement(rpr, qn("w:b"))
        elif not bold and b is not None:
            rpr.remove(b)
    if italic is not None:
        i = rpr.find("w:i", NS)
        if italic and i is None:
            ET.SubElement(rpr, qn("w:i"))
        elif not italic and i is not None:
            rpr.remove(i)


def set_style_spacing(style: ET.Element, after_twips: int, line: int | None = None) -> None:
    ppr = style.find("w:pPr", NS)
    if ppr is None:
        ppr = ET.SubElement(style, qn("w:pPr"))
    spacing = ensure(ppr, "spacing")
    spacing.set(qn("w:after"), str(after_twips))
    if line is not None:
        spacing.set(qn("w:line"), str(line))
        spacing.set(qn("w:lineRule"), "auto")


def main() -> None:
    OUT.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as td:
        td = Path(td)
        default_docx = td / "default-reference.docx"
        # Quarto ships/uses Pandoc; invoking through Quarto avoids relying on a system Pandoc path.
        subprocess.run(
            ["quarto", "pandoc", "-o", str(default_docx), "--print-default-data-file", "reference.docx"],
            check=True,
        )
        unpacked = td / "docx"
        with zipfile.ZipFile(default_docx) as zf:
            zf.extractall(unpacked)

        styles_path = unpacked / "word" / "styles.xml"
        tree = ET.parse(styles_path)
        root = tree.getroot()
        styles = {}
        for style in root.findall("w:style", NS):
            sid = style.get(qn("w:styleId"))
            name_el = style.find("w:name", NS)
            name = name_el.get(qn("w:val")) if name_el is not None else None
            if sid:
                styles[sid] = style
            if name:
                styles[name] = style

        # Body/declaration/reference styles: Times New Roman 12 pt.
        for key in ("Normal", "BodyText", "Body Text", "FirstParagraph", "First Paragraph", "Abstract", "Bibliography", "Caption"):
            if key in styles:
                set_style_font(styles[key], 24)
                set_style_spacing(styles[key], after_twips=120, line=276)  # 6 pt after, ~1.15 spacing

        # Section headings: same family and size, bold; hierarchy is conveyed by numbering and spacing.
        for key in ("Heading1", "Heading 1", "Heading2", "Heading 2", "Heading3", "Heading 3"):
            if key in styles:
                set_style_font(styles[key], 24, bold=True)
                set_style_spacing(styles[key], after_twips=120, line=240)

        # Title modestly larger; no decorative font changes.
        if "Title" in styles:
            set_style_font(styles["Title"], 28, bold=True)
            set_style_spacing(styles["Title"], after_twips=160, line=240)

        # Pandoc table paragraphs usually inherit Normal; retain the same font family.
        # A slightly smaller table style is allowed for fit while preserving family consistency.
        for key in ("Table", "TableText", "Table Text", "Compact"):
            if key in styles:
                set_style_font(styles[key], 20)
                set_style_spacing(styles[key], after_twips=0, line=240)

        tree.write(styles_path, encoding="UTF-8", xml_declaration=True)

        # One-inch margins in the reference document section properties.
        doc_path = unpacked / "word" / "document.xml"
        dtree = ET.parse(doc_path)
        droot = dtree.getroot()
        for sect in droot.findall(".//w:sectPr", NS):
            mar = sect.find("w:pgMar", NS)
            if mar is None:
                mar = ET.SubElement(sect, qn("w:pgMar"))
            for side in ("top", "right", "bottom", "left"):
                mar.set(qn(f"w:{side}"), "1440")
        dtree.write(doc_path, encoding="UTF-8", xml_declaration=True)

        tmp_out = td / "reference.docx"
        with zipfile.ZipFile(tmp_out, "w", compression=zipfile.ZIP_DEFLATED) as zf:
            for file in sorted(unpacked.rglob("*")):
                if file.is_file():
                    zf.write(file, file.relative_to(unpacked))
        shutil.copy2(tmp_out, OUT)

    print(f"LUP_REFERENCE_DOCX_OK: {OUT}")


if __name__ == "__main__":
    main()
