# Run pre-processing of the data
# Rscript scripts/data-preparation.R

# Overall analysis of the data
Rscript scripts/data-analysis.R

# Random forest modeling
for theme in BFF BTS GMF Lebensmittelsicherheit Nährstoff PSM RAUS Tierschutz Weide Andere; do
  Rscript scripts/random-forests.R ${theme}
done

# Test for a single one
Rscript scripts/random-forests.R BFF