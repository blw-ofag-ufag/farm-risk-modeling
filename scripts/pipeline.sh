# Run pre-processing of the data
# Rscript scripts/data-preparation.R

# Random forest modeling
for theme in Lebensmittelsicherheit Tierschutz; do
  Rscript scripts/random-forests.R ${theme}
done