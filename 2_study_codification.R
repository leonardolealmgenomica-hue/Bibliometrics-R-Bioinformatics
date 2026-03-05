rm(list = ls())

# ============================
# 1) Paquetes necesarios
# ============================
library(stringr)
library(dplyr)
library(readr)
library(tidyverse)  # ya incluye dplyr, readr, etc.

# ============================
# 2) Cargar bases de datos
# ============================

## ---------- dat1: set original de MASLD con PMIDs ----------

# Detectar separador automáticamente en csv-MASLDTitle-set.csv
first_line <- readLines("csv-MASLDTitle-set.csv", n = 1)

sep_used <- if (grepl(";", first_line)) {
  ";"
} else if (grepl("\t", first_line)) {
  "\t"
} else {
  ","
}

message("Detected separator in csv-MASLDTitle-set.csv: '", sep_used, "'")

# Usar readr, que maneja mejor comillas y textos largos
if (sep_used == ";") {
  dat1 <- readr::read_delim(
    "csv-MASLDTitle-set.csv",
    delim         = ";",
    locale        = locale(decimal_mark = ".", grouping_mark = ","),
    trim_ws       = TRUE,
    show_col_types = FALSE
  )
} else if (sep_used == "\t") {
  dat1 <- readr::read_tsv(
    "csv-MASLDTitle-set.csv",
    locale        = locale(decimal_mark = ".", grouping_mark = ","),
    trim_ws       = TRUE,
    show_col_types = FALSE
  )
} else {
  dat1 <- readr::read_csv(
    "csv-MASLDTitle-set.csv",
    locale        = locale(decimal_mark = ".", grouping_mark = ","),
    trim_ws       = TRUE,
    show_col_types = FALSE
  )
}

message("Column names in dat1:")
print(colnames(dat1))

# Ver problemas de parseo (por si hay filas conflictivas como la 147)
prob <- readr::problems(dat1)
if (nrow(prob) > 0) {
  message("⚠️ readr reporta problemas al leer csv-MASLDTitle-set.csv. Ejemplo:")
  print(head(prob))
}

# Detectar columna PMID en dat1
pmid_col_dat1 <- grep("PMID", colnames(dat1), ignore.case = TRUE, value = TRUE)

if (length(pmid_col_dat1) == 0) {
  stop("ERROR: No se encuentra ninguna columna que contenga 'PMID' en dat1.")
} else if (length(pmid_col_dat1) > 1) {
  message("Multiple PMID-like columns found in dat1: ", paste(pmid_col_dat1, collapse = ", "))
  message("Using the first one: ", pmid_col_dat1[1])
  pmid_col_dat1 <- pmid_col_dat1[1]
}

dat1 <- dat1 %>%
  mutate(PMID = as.character(.data[[pmid_col_dat1]]))

## ---------- dat2: metadatos descargados de PubMed ----------

dat2 <- readr::read_csv(
  "pubmed_articles_with_metadata.csv",
  locale        = locale(decimal_mark = ".", grouping_mark = ","),
  trim_ws       = TRUE,
  show_col_types = FALSE
)

message("Column names in dat2:")
print(colnames(dat2))

pmid_col_dat2 <- grep("PMID", colnames(dat2), ignore.case = TRUE, value = TRUE)

if (length(pmid_col_dat2) == 0) {
  stop("ERROR: No se encuentra ninguna columna que contenga 'PMID' en dat2.")
} else if (length(pmid_col_dat2) > 1) {
  message("Multiple PMID-like columns found in dat2: ", paste(pmid_col_dat2, collapse = ", "))
  message("Using the first one: ", pmid_col_dat2[1])
  pmid_col_dat2 <- pmid_col_dat2[1]
}

dat2 <- dat2 %>%
  mutate(
    PMID       = as.character(.data[[pmid_col_dat2]]),
    Title      = if ("Title" %in% colnames(.)) as.character(Title) else NA_character_,
    Abstract   = if ("Abstract" %in% colnames(.)) as.character(Abstract) else NA_character_,
    Affiliation= if ("Affiliation" %in% colnames(.)) as.character(Affiliation) else NA_character_
  )

str(dat1)
str(dat2)

# Unir por PMID (left join: conserva todos los de dat1)
df <- left_join(dat1, dat2, by = "PMID")

# ============================
# 3) Catálogo ciudad → país
# ============================

city_to_country <- c(
  "São Paulo"        = "Brazil",
  "Rio de Janeiro"   = "Brazil",
  "Buenos Aires"     = "Argentina",
  "Guadalajara"      = "Mexico",
  "Ciudad de México" = "Mexico",
  "Mexico City"      = "Mexico",
  "Lima"             = "Peru",
  "Bogotá"           = "Colombia",
  "Quito"            = "Ecuador",
  "Caracas"          = "Venezuela",
  "Montevideo"       = "Uruguay",
  "Santiago"         = "Chile",
  "La Paz"           = "Bolivia",
  "Asunción"         = "Paraguay",
  "San José"         = "Costa Rica",
  "Tegucigalpa"      = "Honduras",
  "San Salvador"     = "El Salvador",
  "Managua"          = "Nicaragua",
  "Panamá"           = "Panama",
  "Havana"           = "Cuba",
  "Port-au-Prince"   = "Haiti",
  "Santo Domingo"    = "Dominican Republic"
)

# ============================
# 4) Catálogo de países LATAM / Caribe
# ============================

latin_america <- c(
  # Centroamérica + México
  "México", "Mexico", "Belize", "Guatemala", "El Salvador", "Honduras", 
  "Nicaragua", "Costa Rica", "Panama",
  
  # Sudamérica
  "Colombia", "Venezuela", "Ecuador", "Peru", "Perú", "Bolivia",
  "Brazil", "Brasil", "Paraguay", "Chile", "Argentina", "Uruguay",
  "Guyana", "Suriname", "French Guiana",
  
  # Caribe - países independientes
  "Cuba", "Haiti", "Dominican Republic", "Jamaica", "Bahamas",
  "Barbados", "Saint Lucia", "Saint Vincent and the Grenadines", 
  "Grenada", "Trinidad and Tobago", "Trinidad y Tobago",
  "Antigua and Barbuda", "Saint Kitts and Nevis", "Dominica",
  
  # Caribe - territorios dependientes
  "Puerto Rico", "Guadeloupe", "Martinique", "Aruba", "Curaçao", 
  "Sint Maarten", "Bonaire", "Saint Barthélemy", "Saint Martin", 
  "US Virgin Islands", "British Virgin Islands", "Turks and Caicos Islands", 
  "Cayman Islands", "Montserrat", "Saba", "Sint Eustatius"
)

# ============================
# 5) Detección de país en Title, Abstract, Affiliation
# ============================

df <- df %>%
  mutate(
    Title      = if ("Title" %in% colnames(.)) as.character(Title) else NA_character_,
    Abstract   = if ("Abstract" %in% colnames(.)) as.character(Abstract) else NA_character_,
    Affiliation= if ("Affiliation" %in% colnames(.)) as.character(Affiliation) else NA_character_
  )

# País en Title
df <- df %>%
  mutate(
    Country_in_title = sapply(Title, function(x) {
      if (is.null(x) || is.na(x) || !nzchar(x)) return(NA)
      country <- latin_america[
        which(sapply(latin_america, function(y) grepl(y, x, ignore.case = TRUE)))
      ]
      if (length(country) > 0) country[1] else NA
    })
  )

# País en Abstract
df <- df %>%
  mutate(
    Country_in_Abstract = sapply(Abstract, function(x) {
      if (is.null(x) || is.na(x) || !nzchar(x)) return(NA)
      country <- latin_america[
        which(sapply(latin_america, function(y) grepl(y, x, ignore.case = TRUE)))
      ]
      if (length(country) > 0) country[1] else NA
    })
  )

# País por Afiliación (país o ciudad → país)
df <- df %>%
  mutate(
    Country_affilation = sapply(Affiliation, function(x) {
      if (is.null(x) || is.na(x) || !nzchar(x)) return(NA)
      
      # 1) Buscar país explícito
      country_match <- latin_america[
        which(sapply(latin_america, function(y) grepl(y, x, ignore.case = TRUE)))
      ]
      if (length(country_match) > 0) {
        return(country_match[1])
      }
      
      # 2) Buscar ciudad y mapear a país
      city_match <- names(city_to_country)[
        which(sapply(names(city_to_country), function(city) grepl(city, x, ignore.case = TRUE)))
      ]
      if (length(city_match) > 0) {
        return(city_to_country[city_match[1]])
      }
      
      # 3) Nada encontrado
      return(NA)
    })
  )

# ============================
# 6) Consolidar información de país
# ============================

df <- df %>%
  mutate(
    Country_app_title_abstract_affiliation = paste(
      Country, Country_in_title, Country_in_Abstract, Country_affilation,
      sep = ", "
    )
  )

str(df)

df <- df %>%
  mutate(
    Country_study = coalesce(
      Country_in_title,
      Country_in_Abstract,
      Country_affilation,
      Country
    ),
    Country_study = case_when(
      Country_study == "México" ~ "Mexico",
      Country_study == "Perú"   ~ "Peru",
      Country_study == "Brasil" ~ "Brazil",
      TRUE ~ Country_study
    )
  ) %>%
  filter(
    !is.na(Country_study),
    !Country_study %in% c("Saba", "Aruba", "Grenada")
  )

# ============================
# 7) Exploración rápida y guardado
# ============================

print(table(df$Country_study, useNA = "ifany"))

write.csv(df, "dataset_MASLD_Latin_America.csv", row.names = FALSE)
