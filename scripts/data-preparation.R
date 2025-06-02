# =========================================================================================================
#       title:  INTEGRATING FARM STRUCTURE, NUTRIENT FLOWS AND INSPECTION OUTCOMES
#      author:  Damian Oswald (damian.oswald@blw.admin.ch)
#        date:  2025-06-02
# description:  Pulls raw data from the FOAG production systems AGIS (structural attributes), ACONTROL
#               (on-farm inspections) and HODUFLU (manure and recycling-fertilizer flows), harmonises keys,
#               resolves duplicates, and assembles a tidy, de-identified farm-year panel. The clean data
#               frame serves as the input matrix for downstream statistical and machine-learning pipelines
#               that predict inspection pass/fail outcomes and analyse compliance-risk drivers. All steps
#               are reproducible, although access to the data is strictly limited to FOAG collaborators.
# =========================================================================================================


# =========================================================================================================
# ATTACHING AND LISTING OF NECESSARY PACKAGES TO THE SEARCH PATH
# =========================================================================================================

library(dplyr)
library(tidyr)
library(readr)
library(openssl)
library(magrittr)


# =========================================================================================================
# DEFINE HELPER FUNCTIONS
# =========================================================================================================

# Function to take in a data frame with farm level IDs and years and create a non-reversible farm-year
# identifier using HMAC-BLAKE2B (see <https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE2>). The
# farm-year-ID is subsequently used to connect farm-year data from different sources.
# ---------------------------------------------------------------------------------------------------------
assignIdentifier <- function(
    data,             # data frame containing the source columns
    identifier,       # name of the (AGIS) identifier column
    year,             # name of the year column
    varname = "ID",   # destination column (default "ID")
    secret = NULL     # 32+ random bytes used as HMAC key; by default looked up in `~/SECRET.env`
    )
{
  
  # look up secret if it's not passed to the function
  if(base::is.null(secret)) {
    secret <- base::readLines("SECRET.env")[[1]]
    if (!base::nzchar(secret))
      stop("Provide a secret key via argument `secret` or environment variable SECRET.env", call. = FALSE)
  }
  
  # prepare input vector
  string <- base::paste0(data[[identifier]], "_", data[[year]])
  
  # `openssl::sha256()` with `key` performs HMAC (hashed message authentication code)
  keys <- openssl::blake2b(
    x = string,
    key = secret
  )
  
  # assign newly created keys to own column
  data[[varname]] <- keys
  return(data)
}

# Function to convert columns in a data frame to factor variables in place
# ---------------------------------------------------------------------------------------------------------
assignFactorVariables <- function(
    data,     # data frame containing the source columns
    variables # character vector with the names of variables to be turned to factor variables
    )
{
  for (i in variables) {
    data[[i]] <- base::as.factor(data[[i]])
  }
  return(data)
}

# Function to convert columns in a data frame to numeric and re-format query artifacts (i.e. remove the
# apostrophe in large numbers like 100'000)
# ---------------------------------------------------------------------------------------------------------
assignNumericVariables <- function(
    data,     # data frame containing the source columns
    variables # character vector with the names of variables to be turned to numeric variables
    )
{
  for (i in variables) {
    data[[i]] <- base::gsub("'", "", data[[i]]) %>%
      base::as.numeric()
  }
  return(data)
}

# =========================================================================================================
# PREPARE INSPECTION DATA
# ---------------------------------------------------------------------------------------------------------
# Here, we read the CSV table containing inspection results queried from ACONTROL, assign new variable
# names, change the formatting of columns for easier use in R and assign new pseudonymous IDs.
# =========================================================================================================

# Read the data from the CSV file
inspections <- readr::read_csv("data/inspections.csv", show_col_types = FALSE) %>%
  dplyr::rename(
    date  = "Kontrolldatum",
    type  = "Type",
    theme = "v_theme"
  )

# Give proper date formatting
inspections$date <- base::as.Date(inspections$date)

# Create separate year variable (for some fast analyses later on)
inspections$year <- base::format(inspections$date, "%Y")

# Create a distinct farm-year-ID using a cryptographic hash function
inspections <- assignIdentifier(inspections, "AGIS Nummer", "year")

# Simplify the results to a binary value with 0 = Pass, 1 = Fail. The rationale for this is that the values,
# i.e. number of passes/fails per farm are not really comparable, as they depend on the context of the
# inspection, the diversity of the farm etc. Binary outcomes simplify the problem to the question of actual
# interest: Is a farm at risk of not passing an inspection?
inspections$result <- base::ceiling(inspections$CurrentPercentFail)
inspections$previous <- base::ceiling(inspections$PreviousPercentFail)

# construct a farm using HMAC-BLAKE2B
inspections$farm <- openssl::blake2b(
  x = base::as.character(inspections[["AGIS Nummer"]]),
  key = base::readLines("SECRET.env")
)

# keep only a subset of the data (remove some variables, remove older records)
inspections <- subset(
  inspections,
  select = c("ID", "farm", "date", "year", "type", "theme", "result", "previous"),
  subset = year >= 2019
) %>%
  assignFactorVariables(variables = c("theme", "type", "year", "farm"))

# convert inspection results to binary
for (i in c("result", "previous")) {
  inspections[[i]] <- base::factor(
    x = inspections[[i]],
    levels = c("0", "1"),
    labels = c("Pass", "Fail")
  )
}

# Keep environment tidy
base::rm(i)


# =========================================================================================================
# ADD STRUCTURAL FARM DATA FROM AGIS
# =========================================================================================================

# Read AIGS data from the CSV file
agis <- readr::read_csv("data/agis.csv", show_col_types = FALSE) %>%
  
  # Assign global identifier
  assignIdentifier("BBS ID (as number)", "Jahr") %>%
  
  # Assign special identifier for HODUFLU-AGIS join
  assignIdentifier("KT_ID_B", "Jahr", "KTID") %>%
  
  # Rename variables
  dplyr::rename(
    farmType                 = "Betriebsform neu",
    legalForm                = "LegalForm",
    canton                   = "Kanton",
    zone                     = "Zone neu",
    LN                       = "ha LN",
    SAK                      = "SAK",
    GVE                      = "GVE total",
    DZ                       = "Hat DZ",
    SB                       = "Hat SB",
    proofMet                 = "Nachweis erfüllt",
    organic                  = "Hat Biobeiträge",
    surveyStdMet             = "Erhebungsnorm erfüllt",
    farmerBirthYear          = "Jahrgang BL",
    foundingYear             = "Gründungsjahr"
  )

# Combine structure information to one variable
agis[["structures"]] <- base::as.logical(agis[["Hat Strukturen"]]) |
  base::as.logical(agis[["Hat Strukturen im Sömmerungsgebiet"]])

agis <- agis %>%
  
  # Convert to factor variables
  assignFactorVariables(
    variables = c(
      "farmType",
      "legalForm",
      "canton",
      "zone",
      "DZ",
      "SB",
      "structures",
      "proofMet",
      "organic",
      "surveyStdMet"
      )) %>%
  
  # clean and convert numeric variables
  assignNumericVariables(c("LN", "SAK", "GVE"))

# Reformat year variables
for (i in c("farmerBirthYear", "foundingYear")) {
  agis[[i]] <- base::as.integer(gsub("'","",agis[[i]]))
  agis[base::which(agis[,i]==0),i] <- NA
}

# Drop variables that will be no longer necessary
drop_cols <- c(
  "Hat Strukturen im Sömmerungsgebiet",
  "Hat Strukturen",
  "Jahr",
  "KT_ID_B",
  "BBS ID (as number)"
  )
agis <- agis[, !base::names(agis) %in% drop_cols]


# =========================================================================================================
# ADD NUTRIENT DATA FROM HODUFLU TO AGIS DATA
# ---------------------------------------------------------------------------------------------------------
# Only AGIS contains all necessary IDs to connect the data, so we construct an intermediate data frame 
# `hoduflagis` and subsequently join that with the previously constructed data frame `inspections`
# =========================================================================================================

# Read HODUFLU data from CSV file
hoduflu <- readr::read_csv("data/hoduflu.csv", show_col_types = FALSE) %>%
  
  # Assign special identifier for HODUFLU-AGIS join
  assignIdentifier("KT_ID_B","Jahr", "KTID") %>%
  
  # Subset the data frame
  base::subset(select = c(-Jahr, -Kanton, -KT_ID_B)) %>%
  
  # Assign abbreviated English names for ease of use
  dplyr::rename(
    hofDeliv = "Hofdünger Anz. Lieferungen tot",      # number of on-farm deliveries
    hofNtot  = "Hofdünger Bilan kg Nges",             # total N balance
    hofNeff  = "Hofdünger Bilan kg Nverf",            # applied (available) N
    hofP205  = "Hofdünger Bilan kg P205",             # P₂O₅ balance
    hofK2O   = "Hofdünger Bilan kg K2O",              # K₂O balance
    hofMg    = "Hofdünger Bilan kg Mg",               # Mg balance
    recDeliv = "Recyclingdünger Anz. Lieferungen tot", # # of recycling-fertilizer deliveries
    recNtot  = "Recyclingdünger Bilan kg Nges",       # total N balance
    recNeff  = "Recyclingdünger Bilan kg Nverf",      # applied N
    recP205  = "Recyclingdünger Bilan kg P205",       # P₂O₅ balance
    recK2O   = "Recyclingdünger Bilan kg K2O",        # K₂O balance
    recMg    = "Recyclingdünger Bilan kg Mg",         # Mg balance
  )

# join with previous AGIS data
hoduflagis <- dplyr::left_join(agis, hoduflu, by = "KTID")

# replace all missing with 0 (as there was no nutrient transaction)
for (i in base::colnames(hoduflu)) {
  missing <- base::which(base::is.na(hoduflagis[[i]]))
  if(base::length(missing)>0) {
    hoduflagis[base::which(base::is.na(hoduflagis[[i]])), i] <- 0
  }
}

# remove common identifier with AIGS
hoduflagis <- base::subset(hoduflagis, select = c(-KTID))

# join with inspection data
data <- dplyr::left_join(inspections, hoduflagis, by = "ID")

# Keep environment tidy
base::rm(inspections, hoduflagis, hoduflu, agis, drop_cols, i, missing)


# =========================================================================================================
# ADD MILK DATA FROM DBMILCH (IN AGIS)
# =========================================================================================================

# Read data from CSV file
milk <- readr::read_csv("data/dbmilk.csv", show_col_types = FALSE) %>%
  
  # Assign identifier for matching
  assignIdentifier("BBS ID (as number)", "Jahr") %>%
  
  # Remove duplicates, if there are any
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  
  # Rename the milk variable
  dplyr::rename(milk = "Milch kg") %>%
  
  # Subset the data frame
  subset(select = c("milk", "ID")) %>%
  
  # Assign numeric type, remove apostrophes
  assignNumericVariables("milk")

# Join milk data with previously joined data
data <- dplyr::left_join(data, milk, by = "ID")

# Keep environment tidy
base::rm(milk)

# If milk is NA, it is essentially 0, so we assign 0
data[base::is.na(data[["milk"]]), "milk"] <- 0


# =========================================================================================================
# ADD AREA DATA FROM AGIS
# =========================================================================================================

# Read area data from CSV file
area <- readr::read_delim(
  file = "data/area.csv",
  col_types = cols(`Andere Elemente` = col_double()),
  delim = ";", 
  show_col_types = FALSE
  ) %>%
  
  # Assign identifier for matching
  assignIdentifier("BBS_ID", "Jahr") %>%
  
  # Assign abbreviated English names for ease of use
  dplyr::rename(
    elements            = "Andere Elemente", # stone walls, hedges, …
    trees               = "Bäume",
    nonLNarea           = "Flächen ausserhalb der LN", # outside agricultural area
    perennialCropsArea  = "Flächen mit Dauerkulturen", # permanent crops
    protectedCropsArea  = "Flächen mit Kulturen in ganzjährig geschütztem Anbau",
    grassArea           = "Grünfläche",
    openArableArea      = "Offene Ackerfläche",
    otherLNarea         = "Weitere Flächen innerhalb der LN"  # other within agricultural area
  ) %>%
  
  # Subset the data frame
  base::subset(select = c(-Jahr, -BBS_ID, -Kanton, -Total, -Zone, -elements))

# replace all NA with 0
for (i in base::colnames(base::subset(area, select = -ID))) {
  area[[i]][base::is.na(area[[i]])] <- 0
  area[[i]] <- 100 * area[[i]]
}

# Join area data with previously joined data
data <- dplyr::left_join(data, area, by = "ID")

# Keep environment tidy
base::rm(area)

# =========================================================================================================
# ADD CROP AREA DATA FROM AGIS
# =========================================================================================================

# Read crop data from CSV file
crops <- read_csv("data/crops.csv", show_col_types = FALSE) %>%
  
  # Assign identifier for matching
  assignIdentifier("BBS ID (as number)", "Jahr") %>%
  
  # Assign abbreviated English names for ease of use
  dplyr::rename(crop = "Kultur++") %>%
  
  # Select only necessary variables
  base::subset(select = c(ID, crop, ha)) %>%
  
  # Convert character area information to numeric
  assignNumericVariables("ha")

# Mapping for crop group names. In the query, crops were aggregated to agronomically similar groups.
crop_names <- c(
  "Dauerkulturen"                             = "perennials",
  "Andere Elemente"                           = "elements",
  "Brotgetreide"                              = "breadCereals",
  "Dauergrünflächen"                          = "perennialGrassland",
  "Flächen ausserhalb der LN"                 = "nonAgArea",
  "Kartoffeln"                                = "potatos",
  "Ölsaaten"                                  = "oilseeds",
  "Silomais"                                  = "siloMaize",
  "Weitere Flächen innerhalb der LN"          = "otherAgArea",
  "BFF Elemente"                              = "BFFelements",
  "Futtergetreide"                            = "fodderCereals",
  "Körnerleguminosen"                         = "legumes",
  "Körnermais"                                = "grainMaize",
  "Kunstwiesen"                               = "artificialGrassland",
  "Zuckerrüben"                               = "sugarBeets",
  "Kulturen in ganzjährig geschütztem Anbau"  = "protectedCrops",
  "Spezialkulturen"                           = "specialCrops",
  "Saat- und Pflanzgut"                       = "seedMaterials",
  "Übrige offene Ackerfläche"                 = "otherArea",
  "Nachwachsende Rohstoffe"                   = "renewals",
  "Futterrüben"                               = "fodderBeets",
  "Flächen im Sömmerungsgebiet"               = "alps"
)

# Apply new names
crops$crop <- crop_names[crops$crop]

# Pivot to wide: one column per crop key, filling missing with 0
crops <- tidyr::pivot_wider(
    data         = crops,
    id_cols      = ID,
    names_from   = crop,
    values_from  = ha,
    values_fill  = 0,       # if there are no records, we fill it with 0
    values_fn    = sum      # in case there were accidental duplicates
  )

# Join crops data with previously joined data
data <- dplyr::left_join(data, crops, by = "ID")

# Compute the relative (%) share of each crop instead of the area of the crop. The goal here is to have
# similar values for farms with similar activities independent on farm size.
for (i in crop_names) {
  data[[i]] <- (data[[i]] / data$LN) * 100
}

# Keep environment tidy
base::rm(crops, crop_names, i)

# =========================================================================================================
# ADD ANIMALS DATA FROM AGIS
# =========================================================================================================

# Mapping for animal group names. In the query, animals were aggregated to agronomically similar groups.
animal_names <- c(
  roughage              = "Andere Raufutter verzehrende Nutztiere",
  other_animals         = "Andere Tiere",
  hobby_pets            = "Hobbytierhaltung (Liebhabertiere)",
  rabbits               = "Kaninchengattung",
  poultry               = "Nutzgeflügel",
  horses                = "Pferdegattung",
  cattle                = "Rindergattung und Wasserbüffel",
  sheep                 = "Schafgattung",
  pigs                  = "Schweinegattung",
  other_poultry         = "Übriges Geflügel",
  goats                 = "Ziegengattung"
)

# Function to read an animal table (the data is split into two separate tables, `animals_GJB.csv` and
# `animals_SB.csv`). Both tables can be parsed using the same function.
read_animals <- function(path) {
  
  # Read CSV file from path
  readr::read_delim(path, show_col_types = FALSE) %>%
    
    # Assign identifier for matching
    assignIdentifier("BBS_ID", "Jahr") %>%
    
    # Remove unnecessary variables
    base::subset(select = c("ID", animal_names)) %>%
    
    # Assign abbreviated English names for ease of use
    dplyr::rename(all_of(animal_names)) %>%
    
    # Convert all values to numeric
    assignNumericVariables(names(animal_names))
}

# Read the two data sets and combine them by row
animals <- base::rbind(
  read_animals("data/animals_GJB.csv"),
  read_animals("data/animals_SB.csv")
  )

# Replace all NA with 0
animals[base::is.na(animals)] <- 0

# Convert to %
for (i in base::names(animal_names)) {
  animals[[i]] <- 100 * animals[[i]]
}

# Join animals data with previously joined data
data <- dplyr::left_join(data, animals, by = "ID")

# Keep environment tidy
base::rm(animals, animal_names, i, read_animals)

# =========================================================================================================
# SAVE RESULTING DATA PRODUCT
# =========================================================================================================

saveRDS(data, "data/data.rds")


# =========================================================================================================
# TODO: USE DIFFERENTIAL PRIVACY TO ANONYMIZE DATA TO AN ACCEPTABLE DEGREE
# =========================================================================================================

