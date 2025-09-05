# Integrating farm structure data for data-driven farm inspection compliance analysis

This pipeline tests the automatic assembly of de-identified farm-year panel data by pulling raw data from AGIS (structural attributes, crops, animals, milk production), ACONTROL (on-farm inspections) and HODUFLU (manure and recycling-fertilizer flows).

## 1 How to run this pipeline

### 1.1 Processing the raw AGIS, Acontrol and HODUFLU data

For privacy reasons, the raw data is not included in this repository.
Hence, the processing script can only be executed by FOAG collaborators with access to the raw data.
In addition, a secret environment key needs to be stored under `resources/SECRET.env`.
It is used in the data preparation script to securely hash the farm level identifiers.

If you have access to these resources, you may run the entire pipeline as

```r
sh scripts/pipeline.sh
```

...

# Results

## Random forest classification

The following table aggregates the results of random forest models trained with 500 trees on each theme, separately.

|                       | Accuracy| Sensitivity| Specificity| Pos Pred Value| Neg Pred Value| Precision| Recall|    F1| Prevalence| Detection Rate| Detection Prevalence| Balanced Accuracy|
|:----------------------|--------:|-----------:|-----------:|--------------:|--------------:|---------:|------:|-----:|----------:|--------------:|--------------------:|-----------------:|
|Andere                 |    0.731|       0.349|       0.834|          0.361|          0.826|     0.361|  0.349| 0.355|      0.212|          0.074|                0.205|             0.591|
|BFF                    |    0.820|       0.448|       0.844|          0.159|          0.959|     0.159|  0.448| 0.235|      0.062|          0.028|                0.174|             0.646|
|BTS                    |    0.854|       0.264|       0.886|          0.113|          0.957|     0.113|  0.264| 0.158|      0.052|          0.014|                0.121|             0.575|
|GMF                    |    0.838|       0.434|       0.858|          0.134|          0.968|     0.134|  0.434| 0.205|      0.048|          0.021|                0.156|             0.646|
|Lebensmittelsicherheit |    0.714|       0.608|       0.771|          0.589|          0.784|     0.589|  0.608| 0.598|      0.351|          0.213|                0.362|             0.689|
|Nährstoff              |    0.819|       0.587|       0.825|          0.085|          0.986|     0.085|  0.587| 0.149|      0.027|          0.016|                0.186|             0.706|
|PSM                    |    0.738|       0.774|       0.738|          0.025|          0.997|     0.025|  0.774| 0.048|      0.009|          0.007|                0.267|             0.756|
|RAUS                   |    0.819|       0.365|       0.840|          0.097|          0.966|     0.097|  0.365| 0.153|      0.045|          0.016|                0.169|             0.602|
|Tierschutz             |    0.771|       0.414|       0.831|          0.291|          0.894|     0.291|  0.414| 0.342|      0.144|          0.059|                0.204|             0.622|
|Weide                  |    0.680|       0.500|       0.714|          0.250|          0.882|     0.250|  0.500| 0.333|      0.160|          0.080|                0.320|             0.607|