# WCAD feasibility audit

Generated: 2026-09-08 00:07:25 UTC

## Core coverage

- Total rows: 28565
- Countries/territories in WCAD: 189
- Countries/territories with any tenure information: 148
- Countries/territories with any rental information: 138
- Countries/territories with rental-area percentage observations: 115
- Countries/territories with rental-holding percentage observations: 94
- Countries/territories with both rental percentage measures: 88
- Unclassified valid rental percentage observations: 0
- Countries with two or more rental census years: 79

## Measurement classification

WCAD disseminates area and holding-count observations under the same rental Item label. Percentage rows are therefore classified by the measurement dimension of the preceding absolute-unit row within the same Area-Year-Item source block (ha = area; No = holdings). The original source row is retained in rental_observations.csv for direct auditability, and the script stops if any valid rental percentage cannot be classified.

## Methodological caution

FAO explicitly notes that countries may apply their own definitions of agricultural holdings, which can affect international comparability. Therefore this audit establishes data feasibility, not causal identification or full harmonization.

The next scientific stage should inspect the corrected coverage and then test overlap with the productivity source before any econometric design is fixed.
