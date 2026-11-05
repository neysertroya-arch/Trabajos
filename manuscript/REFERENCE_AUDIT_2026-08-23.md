# Reference audit — 2026-08-23

## Scope

This audit documents the bibliography expansion associated with the substantive Land Use Policy manuscript revision. The active bibliography is `manuscript/references.bib`; the immediately preceding version is preserved at `manuscript/archive/references_pre_substantive_2026-08-23.bib` and in the dedicated backup branch `backup/pre-lup-substantive-2026-08-23`.

## Audit rules

- Every active BibTeX entry must be cited in `manuscript/article.qmd`.
- Every citation key in `manuscript/article.qmd` must resolve to an active BibTeX entry.
- At least 50 unique references must be cited.
- New references are retained only when they substantively support the conceptual framework, land-market institutions, farm structure/misallocation, methodology, discussion, or policy interpretation.
- DOI metadata or an authoritative publisher/official source was checked before inclusion where a DOI exists.
- Official data-source records are retained for FAO, USDA ERS, and World Bank data products.
- No reference is included solely to inflate the citation count.

## Verification sources

Bibliographic metadata were cross-checked against authoritative publisher, journal, DOI, or institutional records, including the American Economic Association, ScienceDirect/Elsevier, Wiley Online Library, Oxford Academic, University of Chicago Press, FAO, USDA ERS, and World Bank records. The 2026 references were checked against current publication records rather than inferred from pre-2026 metadata.

## Reproducible enforcement

`R/10_audit_editorial_submission.R` enforces the citation/BibTeX correspondence in CI. The audit fails if:

1. fewer than 50 unique references are cited;
2. a citation key is absent from the BibTeX file;
3. a BibTeX entry is not cited;
4. the approved section hierarchy changes;
5. internal Results subheadings reappear;
6. selected Spanish carry-over markers are detected;
7. abstract or keyword limits are violated.

## Status

- Active BibTeX target: **50 entries**.
- Unique cited-reference target: **50**.
- Previous bibliography: **preserved**.
- Quantitative analysis: **unchanged** by bibliography revision.
- Final status remains contingent on CI and author review.
