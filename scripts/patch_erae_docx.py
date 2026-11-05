#!/usr/bin/env python3
"""ERAE-specific DOCX presentation patch.

Applies only journal-facing formatting:
- double line spacing via the Normal paragraph style;
- prevents table rows from splitting across pages;
- clears creator/last-modified-by metadata.

Scientific content and numerical values are not modified.
"""
from pathlib import Path
import os
import sys
import tempfile
import zipfile
import xml.etree.ElementTree as ET

W = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
DC = "http://purl.org/dc/elements/1.1/"
CP = "http://schemas.openxmlformats.org/package/2006/metadata/core-properties"
ET.register_namespace("w", W)
ET.register_namespace("dc", DC)
ET.register_namespace("cp", CP)

def q(ns, name):
    return f"{{{ns}}}{name}"

def patch(path: Path):
    with zipfile.ZipFile(path, "r") as zin:
        infos = zin.infolist()
        data = {i.filename: zin.read(i.filename) for i in infos}

    # Double-line spacing through the default Normal style.
    styles = ET.fromstring(data["word/styles.xml"])
    normal = None
    for style in styles.findall(q(W, "style")):
        if style.get(q(W, "styleId")) == "Normal":
            normal = style
            break
    if normal is None:
        raise RuntimeError("Normal paragraph style not found")
    ppr = normal.find(q(W, "pPr"))
    if ppr is None:
        ppr = ET.SubElement(normal, q(W, "pPr"))
    spacing = ppr.find(q(W, "spacing"))
    if spacing is None:
        spacing = ET.SubElement(ppr, q(W, "spacing"))
    spacing.set(q(W, "line"), "480")
    spacing.set(q(W, "lineRule"), "auto")
    data["word/styles.xml"] = ET.tostring(styles, encoding="utf-8", xml_declaration=True)

    # Prevent every table row from splitting across pages.
    document = ET.fromstring(data["word/document.xml"])
    rows = document.findall(f".//{q(W, 'tr')}")
    for row in rows:
        trpr = row.find(q(W, "trPr"))
        if trpr is None:
            trpr = ET.Element(q(W, "trPr"))
            row.insert(0, trpr)
        if trpr.find(q(W, "cantSplit")) is None:
            ET.SubElement(trpr, q(W, "cantSplit"))
    data["word/document.xml"] = ET.tostring(document, encoding="utf-8", xml_declaration=True)

    # Scrub personal core metadata.
    if "docProps/core.xml" in data:
        core = ET.fromstring(data["docProps/core.xml"])
        for tag in (q(DC, "creator"), q(CP, "lastModifiedBy")):
            node = core.find(tag)
            if node is not None:
                node.text = ""
        data["docProps/core.xml"] = ET.tostring(core, encoding="utf-8", xml_declaration=True)

    fd, tmpname = tempfile.mkstemp(suffix=".docx", dir=str(path.parent))
    os.close(fd)
    try:
        with zipfile.ZipFile(tmpname, "w", compression=zipfile.ZIP_DEFLATED) as zout:
            for info in infos:
                payload = data[info.filename]
                zout.writestr(info, payload)

        # Structural verification before replacing the original.
        with zipfile.ZipFile(tmpname, "r") as z:
            sx = ET.fromstring(z.read("word/styles.xml"))
            dx = ET.fromstring(z.read("word/document.xml"))
            normal2 = next(
                (s for s in sx.findall(q(W, "style")) if s.get(q(W, "styleId")) == "Normal"),
                None
            )
            if normal2 is None:
                raise RuntimeError("PATCH_VERIFY_FAIL: Normal style missing")
            sp = normal2.find(f"{q(W,'pPr')}/{q(W,'spacing')}")
            if sp is None or sp.get(q(W, "line")) != "480":
                raise RuntimeError("PATCH_VERIFY_FAIL: double spacing not applied")
            rows2 = dx.findall(f".//{q(W,'tr')}")
            n_cant = sum(1 for r in rows2 if r.find(f"{q(W,'trPr')}/{q(W,'cantSplit')}") is not None)
            if not rows2 or n_cant != len(rows2):
                raise RuntimeError(f"PATCH_VERIFY_FAIL: cantSplit {n_cant}/{len(rows2)}")

        os.replace(tmpname, path)
    finally:
        if os.path.exists(tmpname):
            os.unlink(tmpname)

    print(f"ERAE_DOCX_PATCH_OK rows={len(rows)} double_spacing=PASS metadata=SCRUBBED")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        raise SystemExit("Usage: patch_erae_docx.py <docx>")
    patch(Path(sys.argv[1]).resolve())
