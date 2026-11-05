#!/usr/bin/env python3
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[1]
SUB = ROOT / "submission" / "journal-of-rural-studies"
OUT = ROOT / "outputs" / "submission" / "journal-of-rural-studies"
REF = ROOT / "submission" / "land-use-policy" / "reference.docx"

def convert(source: Path, target: Path) -> None:
    if not source.exists():
        raise FileNotFoundError(source)
    target.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(["quarto", "pandoc", str(source), "--reference-doc", str(REF), "-o", str(target)], check=True)
    if not target.exists() or target.stat().st_size == 0:
        raise RuntimeError(f"Failed to create {target}")

def main() -> None:
    if not REF.exists():
        raise FileNotFoundError("reference.docx must be built before JRS submission documents")
    convert(SUB / "title-page.md", OUT / "title-page.docx")
    convert(SUB / "cover-letter.md", OUT / "cover-letter.docx")
    convert(SUB / "highlights.md", OUT / "highlights.docx")
    convert(OUT / "figure-captions.md", OUT / "figure-captions.docx")
    print("JRS_EDITABLE_SUBMISSION_DOCS_OK")

if __name__ == "__main__":
    main()
