# Install all necessary R packages
Rscript scripts/packages.R

# Run pre-processing of the data
Rscript scripts/data-preparation.R

# Overall analysis of the data
Rscript scripts/data-analysis.R

# Random forest modeling
for theme in BFF BTS GMF Nährstoff PSM RAUS Tierschutz Weide Andere Lebensmittelsicherheit; do
  Rscript scripts/random-forests.R ${theme}
done
