# PubMed Text Mining and Bibliometrics in R 🧬📊

A specialized R-based pipeline for automated scientific literature recovery, metadata extraction, and frequentist text analysis, with a focus on metabolic diseases (MASLD) in Latin America.

## Project Overview
This repository provides a modular approach to bibliometric analysis using R. It includes scripts for querying the PubMed database, cleaning large datasets, and generating visualizations to identify research trends and frequentist patterns in scientific publications.

## Key Features
- **PubMed Recovery:** Automated retrieval of PMIDs and full metadata using the `easyPubMed` and `rentrez` logic.
- **Study Codification:** Structural cleaning and categorization of retrieved articles.
- **Bibliometric Visualization:**
    - Frequentist barplots for bigrams and keywords.
    - Publication trend maps and label distribution analysis.
- **Advanced Data Handling:** Optimized for processing datasets with thousands of entries (e.g., MASLD research in Latin America).

## Scripts Description
1.  `1_recuperacion_articulos_pubmed.R`: Main script for connecting to NCBI and fetching research data.
2.  `2_study_codification.R`: Logic for data cleaning and metadata structured formatting.
3.  `4_barplot_mapa_lavels.R`: Visualization engine for geographic and thematic distribution.
4.  `9_bigram_barplot_approch_frequentista.R`: Advanced NLP analysis for word co-occurrence and bigram frequency.

## Requirements
- **R** (Version 4.0+)
- Libraries: `tidyverse`, `easyPubMed`, `ggplot2`, `tidytext`, `wordcloud`.

## Author
- **Leal-Mercado L.**
- *Bioinformatics Integration Specialist*

---
*Note: Large datasets (.csv) and generated figures (.jpg) are excluded to maintain a clean repository focused on the analytical workflow.*
