# Integrating farm structure data for data-driven farm inspection compliance analysis

This pipeline tests the automatic assembly of de-identified farm-year panel data by pulling raw data from AGIS (structural attributes, crops, animals, milk production), ACONTROL (on-farm inspections) and HODUFLU (manure and recycling-fertilizer flows).

## 1 How to run this pipeline

### 1.1 Processing the raw AGIS, Acontrol and HODUFLU data

For privacy reasons, the raw data is not included in this repository.
Hence, the processing script can only be executed by FOAG collaborators with access to the raw data.
In addition, a secret environment key needs to be stored under `resources/SECRET.env`.
It is used in the data preparation script to securely hash the farm level identifiers.

If you have access to these resources, you may run the processing script as

```r
Rscript scripts/data-preparation.R
```