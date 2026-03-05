############################################################
## PUBMED METADATA HARVESTER
## - Lee PMIDs desde CSV (sep = ";", columnas: n, PMID)
## - Descarga metadatos desde PubMed con rentrez
## - Extrae títulos, abstracts, autores, journal, año, MeSH,
##   afiliación, país (enfocado en LATAM/Caribe)
## - Exporta: pubmed_articles_with_metadata.csv
############################################################

rm(list = ls())

# ============================
# 1) Paquetes requeridos
# ============================
library(rentrez)
library(XML)
library(dplyr)
library(stringr)
library(progress)

# ============================
# 2) Funciones auxiliares
# ============================

# Extraer elementos XML de forma segura
safe_xpath_extract <- function(doc, xpath, default = NA) {
  result <- tryCatch({
    values <- xpathSApply(doc, xpath, xmlValue)
    if (length(values) == 0) return(default)
    values
  }, error = function(e) {
    message("Error extracting ", xpath, ": ", e$message)
    return(default)
  })
  return(result)
}

# Extraer país de afiliación (LATAM/Caribe)
extract_country <- function(affiliation) {
  if (is.na(affiliation)) return(NA)
  
  latin_american_countries <- c(
    # Mexico
    "Mexico",
    # Central America
    "Belize", "Guatemala", "El Salvador", "Honduras", 
    "Nicaragua", "Costa Rica", "Panama",
    # South America
    "Colombia", "Venezuela", "Ecuador", "Peru", "Bolivia", 
    "Brazil", "Paraguay", "Chile", "Argentina", "Uruguay",
    "Guyana", "Suriname", "French Guiana",
    # Caribbean
    "Cuba", "Haiti", "Dominican Republic", "Jamaica", 
    "Bahamas", "Barbados", "Saint Lucia", 
    "Saint Vincent and the Grenadines", "Grenada", 
    "Trinidad and Tobago", "Antigua and Barbuda", 
    "Saint Kitts and Nevis", "Dominica", "Puerto Rico", 
    "Guadeloupe", "Martinique", "Aruba", "Curaçao", 
    "Sint Maarten", "Bonaire", "Saint Barthélemy", 
    "Saint Martin", "US Virgin Islands", 
    "British Virgin Islands", "Turks and Caicos Islands", 
    "Cayman Islands", "Montserrat", "Saba", "Sint Eustatius"
  )
  
  # País al final de la afiliación
  country_match <- str_match(
    affiliation, 
    paste0(".*[,\\.]\\s*(", paste(latin_american_countries, collapse = "|"), ")\\s*$")
  )
  
  if (!is.na(country_match[1, 2])) {
    return(country_match[1, 2])
  }
  
  # Buscar en cualquier parte
  for (country in latin_american_countries) {
    if (grepl(country, affiliation, ignore.case = TRUE)) {
      return(country)
    }
  }
  
  return(NA)
}

# Combinar autores en un string
combine_authors <- function(last_names, first_names, initials) {
  authors <- vector("character", length = length(last_names))
  
  for (i in seq_along(last_names)) {
    if (!is.na(first_names[i]) && nzchar(first_names[i])) {
      authors[i] <- paste0(last_names[i], " ", first_names[i])
    } else if (!is.na(initials[i]) && nzchar(initials[i])) {
      authors[i] <- paste0(last_names[i], " ", initials[i])
    } else {
      authors[i] <- last_names[i]
    }
  }
  
  paste(authors, collapse = "; ")
}

# Descargar metadatos de un batch de PMIDs
fetch_metadata_batch <- function(pmids_batch) {
  tryCatch({
    pmid_str <- paste(pmids_batch, collapse = ",")
    message("Fetching batch (first 80 chars): ", substr(pmid_str, 1, 80), "...")
    record <- entrez_fetch(
      db      = "pubmed", 
      id      = pmid_str, 
      rettype = "xml", 
      retmode = "xml"
    )
    return(record)
  }, error = function(e) {
    message("Error fetching batch: ", e$message)
    Sys.sleep(5)
    tryCatch({
      message("Retrying...")
      pmid_str <- paste(pmids_batch, collapse = ",")
      record <- entrez_fetch(
        db      = "pubmed", 
        id      = pmid_str, 
        rettype = "xml", 
        retmode = "xml"
      )
      return(record)
    }, error = function(e2) {
      message("Retry failed: ", e2$message)
      return(NULL)
    })
  })
}

# Parsear XML y extraer metadatos
parse_metadata <- function(xml_data) {
  if (is.null(xml_data)) return(NULL)
  
  tryCatch({
    doc <- xmlParse(xml_data)
    article_nodes <- getNodeSet(doc, "//PubmedArticle")
    results <- list()
    
    for (i in seq_along(article_nodes)) {
      article <- article_nodes[[i]]
      
      # PMID
      pmid_node <- getNodeSet(article, "./MedlineCitation/PMID")[[1]]
      pmid <- xmlValue(pmid_node)
      
      # Metadatos básicos
      title    <- safe_xpath_extract(article, "./MedlineCitation/Article/ArticleTitle", "")
      abstract <- safe_xpath_extract(article, "./MedlineCitation/Article/Abstract/AbstractText", "")
      journal  <- safe_xpath_extract(article, "./MedlineCitation/Article/Journal/Title", "")
      volume   <- safe_xpath_extract(article, "./MedlineCitation/Article/Journal/JournalIssue/Volume", NA)
      issue    <- safe_xpath_extract(article, "./MedlineCitation/Article/Journal/JournalIssue/Issue", NA)
      
      year <- safe_xpath_extract(article, "./MedlineCitation/Article/Journal/JournalIssue/PubDate/Year", NA)
      if (is.na(year[1])) {
        medline_date <- safe_xpath_extract(article, "./MedlineCitation/Article/Journal/JournalIssue/PubDate/MedlineDate", NA)
        if (!is.na(medline_date[1])) {
          year <- str_extract(medline_date[1], "^\\d{4}")
        }
      }
      
      pub_types <- safe_xpath_extract(
        article, 
        "./MedlineCitation/Article/PublicationTypeList/PublicationType", 
        ""
      )
      pub_types <- paste(pub_types, collapse = "; ")
      
      doi <- NA
      identifiers <- getNodeSet(article, "./MedlineCitation/Article/ELocationID")
      if (length(identifiers) > 0) {
        for (id_node in identifiers) {
          id_type <- xmlGetAttr(id_node, "EIdType")
          if (!is.null(id_type) && id_type == "doi") {
            doi <- xmlValue(id_node)
            break
          }
        }
      }
      
      mesh_terms <- safe_xpath_extract(
        article, 
        "./MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName", 
        ""
      )
      mesh_terms <- paste(mesh_terms, collapse = "; ")
      
      # Autores y afiliaciones
      author_nodes <- getNodeSet(article, "./MedlineCitation/Article/AuthorList/Author")
      
      if (length(author_nodes) > 0) {
        last_names   <- character(length(author_nodes))
        first_names  <- character(length(author_nodes))
        initials     <- character(length(author_nodes))
        affiliations <- character(length(author_nodes))
        
        for (j in seq_along(author_nodes)) {
          author <- author_nodes[[j]]
          
          last_names[j]  <- safe_xpath_extract(author, "./LastName", "")[1]
          first_names[j] <- safe_xpath_extract(author, "./ForeName", NA)[1]
          initials[j]    <- safe_xpath_extract(author, "./Initials", NA)[1]
          
          current_aff <- safe_xpath_extract(author, "./AffiliationInfo/Affiliation", NA)
          if (is.na(current_aff[1])) {
            current_aff <- safe_xpath_extract(author, "./Affiliation", NA)
          }
          
          affiliations[j] <- if (!is.na(current_aff[1])) current_aff[1] else NA
        }
        
        authors_formatted <- combine_authors(last_names, first_names, initials)
        first_affiliation <- affiliations[1]
        country           <- extract_country(first_affiliation)
      } else {
        authors_formatted <- NA
        first_affiliation <- NA
        country           <- NA
      }
      
      results[[i]] <- data.frame(
        PMID        = pmid,
        Title       = if (length(title)    > 0) paste(title,    collapse = " ") else NA,
        Abstract    = if (length(abstract) > 0) paste(abstract, collapse = " ") else NA,
        Authors     = authors_formatted,
        Journal     = if (length(journal)  > 0) journal[1] else NA,
        Year        = if (length(year)     > 0) year[1]    else NA,
        Volume      = if (length(volume)   > 0) volume[1]  else NA,
        Issue       = if (length(issue)    > 0) issue[1]   else NA,
        DOI         = doi,
        PubType     = pub_types,
        MeSHTerms   = mesh_terms,
        Affiliation = first_affiliation,
        Country     = country,
        stringsAsFactors = FALSE
      )
    }
    
    if (length(results) > 0) {
      bind_rows(results)
    } else {
      NULL
    }
  }, error = function(e) {
    message("Error parsing XML: ", e$message)
    NULL
  })
}

# ============================
# 3) Cargar CSV con PMIDs
# ============================

data <- read.csv(
  "1_articles_pmid.csv",
  sep = ";",              # <-- tu archivo viene con ';'
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

message("Column names in input file:")
print(colnames(data))     # Esperado: "n"  "PMID"

if (!"PMID" %in% colnames(data)) {
  stop("Error: no se encontró una columna llamada 'PMID' en el archivo")
}

data$PMID <- as.character(data$PMID)

message("Sample of PMID column:")
print(head(data$PMID))

pmids <- data$PMID
pmids <- pmids[!is.na(pmids) & pmids != ""]
pmids <- unique(pmids)

if (length(pmids) == 0) {
  stop("Error: No valid PMIDs found in the input file after cleaning")
}

# 🔻🔻🔻 AQUÍ LIMITAMOS A 5000 PMIDs 🔻🔻🔻
if (length(pmids) > 5000) {
  message("More than 5000 PMIDs detected (", length(pmids), "). Limiting to first 5000.")
  pmids <- pmids[1:5000]
}
# 🔺🔺🔺 FIN DEL LÍMITE A 5000 🔺🔺🔺

message("Processing ", length(pmids), " unique PMIDs in batches of 100")

# ============================
# 4) Descarga por batches
# ============================

batch_size   <- 100
pmid_batches <- split(pmids, ceiling(seq_along(pmids) / batch_size))

pb_fetch <- progress_bar$new(
  format = "Fetch [:bar] :percent | ETA: :eta | Batch: :current/:total",
  total  = length(pmid_batches),
  clear  = FALSE,
  width  = 80
)

metadata_list <- list()

for (i in seq_along(pmid_batches)) {
  metadata_list[[i]] <- fetch_metadata_batch(pmid_batches[[i]])
  Sys.sleep(3)  # Respetar rate limits
  pb_fetch$tick()
}

# ============================
# 5) Parseo de metadatos
# ============================

message("Parsing metadata batch by batch...")

parsed_list <- list()
pb_parse <- progress_bar$new(
  format = "Parse  [:bar] :percent | ETA: :eta | Batch: :current/:total",
  total  = length(metadata_list),
  clear  = FALSE,
  width  = 80
)

for (i in seq_along(metadata_list)) {
  if (!is.null(metadata_list[[i]])) {
    parsed_list[[i]] <- parse_metadata(metadata_list[[i]])
  } else {
    parsed_list[[i]] <- NULL
  }
  pb_parse$tick()
}

parsed_list <- parsed_list[!sapply(parsed_list, is.null)]

if (length(parsed_list) == 0) {
  stop("Error: No metadata could be retrieved and parsed")
}

final_metadata_df <- bind_rows(parsed_list)

# ============================
# 6) Merge con el dataset original
# ============================

message("Merging data...")
data$PMID              <- as.character(data$PMID)
final_metadata_df$PMID <- as.character(final_metadata_df$PMID)

final_data <- left_join(data, final_metadata_df, by = "PMID")

# ============================
# 7) Guardar resultados
# ============================

output_file <- "pubmed_articles_with_metadata.csv"
message("Saving results to ", output_file)
write.csv(final_data, output_file, row.names = FALSE)

# ============================
# 8) Resumen
# ============================

message("Processing complete! Results saved to: ", output_file)
message("Retrieved metadata for ", nrow(final_metadata_df),
        " articles (unique PMIDs in metadata: ", length(unique(final_metadata_df$PMID)),
        ") out of ", length(pmids), " requested PMIDs")

message("\nSample of retrieved data:")
print(head(final_data[, c("PMID", "Title", "Authors", "Journal", "Year", "Country")]))
