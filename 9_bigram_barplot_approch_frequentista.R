############################################################
## TEXT MINING + BIGRAM NETWORKS
## - Aplica a dataset_*_consensus_classified_experimentsmodels.csv
## - Requiere columnas: Title, Abstract,
##   Human_consensus_cleaned, Animal_consensus, InVitro_consensus
############################################################

# Limpiar entorno
rm(list = ls())

# ============================
# 1) Paquetes necesarios
# ============================
library(stringr)
library(dplyr)
library(readr)
library(tidyverse)
library(ggpubr)
library(ggprism)
library(ggpattern)
library(ggplot2)
library(igraph)
library(ggraph)
library(tidytext)
library(scales)
library(cowplot)
library(ggforce)

# ============================
# 2) Archivo de datos
# ============================
# Cambia aquí según el proyecto que estés analizando:
#   - HBV  : "dataset_HBV_consensus_classified_experimentsmodels.csv"
#   - MASLD: "dataset_MASLD_consensus_classified_experimentsmodels.csv"  (si ya lo generaste)
DATA_FILE <- "dataset_HBV_consensus_classified_experimentsmodels.csv"

df0 <- read.csv(
  DATA_FILE,
  header    = TRUE,
  sep       = ",",
  dec       = ".",
  row.names = 1,
  stringsAsFactors = FALSE
)

str(df0)

# ---------------------------------------------------------
# HELPER: pipeline de text mining + red de bigramas
# ---------------------------------------------------------
run_textmining_network <- function(df_input,
                                   filter_label = "Human",
                                   stopwords_vec,
                                   bigram_min_n = 10,
                                   legend_side = c("top", "left"),
                                   label_size = 2.0) {
  
  legend_side <- match.arg(legend_side)
  
  # Asegurar texto
  df_input$Abstract <- as.character(df_input$Abstract)
  df_input$Title    <- as.character(df_input$Title)
  
  df_input <- df_input %>%
    filter(!is.na(Abstract)) %>%
    mutate(combined_text = paste(Title, Abstract, sep = " "))
  
  # Añadir ID
  df_text <- tibble::rowid_to_column(df_input, "ID")
  
  # Tokenización + limpieza
  review_words <- df_text %>%
    distinct(combined_text, .keep_all = TRUE) %>%
    unnest_tokens(word, combined_text, drop = FALSE) %>%
    distinct(ID, word, .keep_all = TRUE) %>%
    filter(!word %in% stopwords_vec) %>%
    filter(str_detect(word, "[^0-9]")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup()
  
  # Frecuencia de palabras (para inspección rápida)
  word_counts <- review_words %>%
    count(word, sort = TRUE)
  
  word_countsb <- word_counts %>%
    head(40) %>%
    mutate(word = reorder(word, n))
  
  print(head(word_countsb, 20))
  
  # Opcional: barplot de top palabras
  p_bar <- ggplot(word_countsb, aes(word, n)) +
    geom_col(fill = "#386cb0") +
    geom_text(aes(label = n, y = n + 1),
              size = 3, vjust = 0) +
    scale_y_continuous(labels = comma_format()) +
    coord_flip() +
    labs(
      title    = paste("Most frequent words —", filter_label),
      subtitle = "Custom stopwords removed and no numbers",
      x = "Word",
      y = "Frequency"
    ) +
    theme_classic()
  
  # Para word cloud, si alguna vez lo usas
  df_grouped_V <- review_words %>%
    count(word) %>%
    mutate(frecuencia = n / nrow(review_words))
  
  # Bigramas
  review_bigrams <- df_text %>%
    unnest_tokens(bigram, combined_text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stopwords_vec, !word2 %in% stopwords_vec) %>%
    filter(str_detect(word1, "[^0-9]"),
           str_detect(word2, "[^0-9]"))
  
  bigram_counts <- review_bigrams %>%
    count(word1, word2, sort = TRUE)
  
  # Frecuencia por nodo
  node_freq <- bigram_counts %>%
    pivot_longer(cols = c(word1, word2),
                 names_to = "type", values_to = "word") %>%
    group_by(word) %>%
    summarise(freq = sum(n), .groups = "drop")
  
  # Crear grafo (filtrando por frecuencia mínima)
  bigram_graph <- bigram_counts %>%
    filter(n >= bigram_min_n) %>%
    graph_from_data_frame(directed = FALSE)
  
  if (gorder(bigram_graph) == 0) {
    warning(paste("No hay bigramas con n >=", bigram_min_n, "para", filter_label))
    return(list(barplot = p_bar, graph = NULL))
  }
  
  # Comunidades
  comms <- cluster_louvain(bigram_graph)
  V(bigram_graph)$group <- as.factor(membership(comms))
  
  # Frecuencia como tamaño de nodo
  V(bigram_graph)$freq <- node_freq$freq[match(V(bigram_graph)$name,
                                               node_freq$word)]
  
  # Leyenda: posición
  if (legend_side == "top") {
    leg_pos  <- "top"
    leg_box  <- "horizontal"
    leg_dir  <- "horizontal"
  } else {
    leg_pos  <- "left"
    leg_box  <- "vertical"
    leg_dir  <- "vertical"
  }
  
  # Red de bigramas
  g_plot <- ggraph(bigram_graph, layout = "fr", niter = 10000) +
    geom_mark_ellipse(
      aes(x = x, y = y, group = group),
      expand = unit(1, "mm"),
      color  = "darkorange1",
      fill   = "darkorange1",
      alpha  = 0.1,
      size   = 0.3,
      linetype = "dashed"
    ) +
    geom_edge_link(
      aes(edge_alpha = n, edge_width = n),
      edge_colour = "#084594"
    ) +
    scale_edge_width_continuous(
      name  = "Bigram frequency",
      breaks = c(5, 10, 25, 50, 75, 150, 300, 600, 1000, 1500),
      range  = c(0.3, 2.0),
      trans  = "log10",
      guide  = guide_legend(order = 1,
                            title.position = "top",
                            title.hjust    = 0.5)
    ) +
    scale_edge_alpha_continuous(
      name   = "Bigram frequency",
      breaks = c(5, 10, 25, 50, 75, 150, 300, 600, 1000, 1500),
      range  = c(0.5, 0.9),
      trans  = "log10",
      guide  = "none"
    ) +
    geom_node_point(aes(size = freq),
                    color = "darkred", alpha = 0.8) +
    scale_size_continuous(
      name  = "Word frequency",
      range = c(2, 8),
      guide = guide_legend(order = 2,
                           title.position = "top",
                           title.hjust    = 0.5)
    ) +
    geom_node_text(
      aes(label = name),
      size    = label_size,
      family  = "Arial",
      fontface = "bold",
      repel   = TRUE,
      colour  = "black",
      bg.color = "white",
      bg.r     = 0.15
    ) +
    theme_void() +
    theme(
      text                = element_text(family = "Arial"),
      legend.position     = leg_pos,
      legend.justification = "center",
      legend.box          = leg_box,
      legend.direction    = leg_dir,
      legend.box.margin   = margin(t = 10),
      legend.spacing.x    = unit(0.2, "cm"),
      legend.title        = element_text(size = 10),
      legend.text         = element_text(size = 9),
      legend.key.width    = unit(1, "cm"),
      legend.background   = element_rect(fill = "transparent", color = NA),
      legend.key          = element_rect(fill = "transparent", color = NA),
      plot.margin         = margin(t = 10, r = 10, b = 10, l = 10)
    )
  
  list(barplot = p_bar, graph = g_plot)
}

# =========================================================
# 3) BLOQUE A — HUMAN STUDIES
# =========================================================

df_h <- df0 %>%
  filter(Human_consensus_cleaned == "Human")

custom_stopwords_h <- c(
  "in the", "of the", "on the", "for the", "to the",
  "and", "in", "of", "follow", "among", "significantly",
  "all", "rigths", "reserved", "one", "least", "control",
  "term", "compared",
  "and the", "with the", "from the", "by the", "of a",
  "is", "by", "or", "between", "with",
  "rio", "janeiro", "paulo",
  "that", "a", "to", "the", "for", "as", "it", "was", "at", "which",
  "an", "this", "be", "are", "not", "has", "have", "had", "having",
  "i", "on", "you", "we", "they", "their", "your", "theirs", "ours",
  "themselves", "what", "who", "whom", "these", "those",
  "am", "were", "been", "being", "does", "did", "doing", "but",
  "if", "because",
  "ic", "median", "mean", "ml", "copies", "iu", "sd",
  "confidence", "interval", "suggest", "our", "real", "time",
  "regression", "logistic", "significance", "p", "value", "error",
  "standard", "degree", "serum", "samples", "from",
  "test", "range", "n", "df", "than", "more", "old", "year",
  "out", "polymerase", "chain", "t", "z", "distribution", "power",
  "assumption"
)

res_h <- run_textmining_network(
  df_input     = df_h,
  filter_label = "Human studies",
  stopwords_vec = custom_stopwords_h,
  bigram_min_n  = 80,
  legend_side   = "top",
  label_size    = 2.0
)

bigram_h_freq <- res_h$graph
bigram_h_freq  # red de humanos


# =========================================================
# 4) BLOQUE B — ANIMAL STUDIES
# =========================================================

df_a <- df0 %>%
  filter(Animal_consensus == "Animal")

custom_stopwords_a <- c(
  "in the", "of the", "on the", "for the", "to the",
  "and", "in", "of", "follow", "among", "significantly",
  "g", "all", "rigths", "reserved", "kg", "mg", "i.p", "it",
  "plant", "hss", "cov", "coli", "v3",
  "and the", "with the", "from the", "by the", "of a",
  "is", "by", "or", "between", "with",
  "that", "a", "to", "the", "for", "as", "it", "was", "at",
  "which", "an", "this", "be", "are", "not", "has", "have", "had",
  "having", "i", "on", "you", "we", "they", "their", "your", "theirs",
  "ours", "themselves", "what", "who", "whom", "these", "those",
  "am", "were", "been", "being", "does", "did", "doing", "but", "if",
  "because",
  "ic", "median", "mean", "ml", "copies", "iu", "sd", "confidence",
  "interval", "suggest", "our", "real", "time", "paulo",
  "regression", "logistic", "significance", "p", "value", "error",
  "standard", "degree", "serum", "samples", "from",
  "test", "range", "n", "df", "than", "more", "old", "year",
  "out", "polymerase", "chain", "t", "z", "distribution", "power",
  "assumption"
)

res_a <- run_textmining_network(
  df_input      = df_a,
  filter_label  = "Animal studies",
  stopwords_vec = custom_stopwords_a,
  bigram_min_n  = 7,
  legend_side   = "left",
  label_size    = 2.2
)

bigram_a_freq <- res_a$graph
bigram_a_freq  # red de animales


# =========================================================
# 5) BLOQUE C — IN VITRO STUDIES
# =========================================================

df_v <- df0 %>%
  filter(InVitro_consensus == "In Vitro")

custom_stopwords_v <- c(
  "in the", "of the", "on the", "for the", "to the",
  "and", "in", "of", "follow", "among", "significantly",
  "g", "all", "rigths", "reserved", "fever", "yellow", "4n",
  "wild", "hif", "r", "present",
  "and the", "with the", "from the", "by the", "of a",
  "is", "by", "or", "between", "with",
  "that", "a", "to", "the", "for", "as", "it", "was", "at",
  "which", "an", "this", "be", "are", "not", "has", "have", "had",
  "having", "i", "on", "you", "we", "they", "their", "your", "theirs",
  "ours", "themselves", "what", "who", "whom", "these", "those",
  "am", "were", "been", "being", "does", "did", "doing", "but", "if",
  "because",
  "ic", "median", "mean", "ml", "copies", "iu", "sd", "confidence",
  "interval", "suggest", "our", "real", "time", "paulo",
  "regression", "logistic", "significance", "p", "value", "error",
  "standard", "degree", "serum", "samples", "from",
  "test", "range", "n", "df", "than", "more", "old", "year",
  "out", "polymerase", "chain", "t", "z", "distribution", "power",
  "assumption"
)

res_v <- run_textmining_network(
  df_input      = df_v,
  filter_label  = "In vitro studies",
  stopwords_vec = custom_stopwords_v,
  bigram_min_n  = 10,
  legend_side   = "left",
  label_size    = 2.0
)

bigram_v_freq <- res_v$graph
bigram_v_freq  # red in vitro


# =========================================================
# 6) FIGURA COMPUESTA A–C
# =========================================================

# A = Human, B = Animal, C = In Vitro
combined_plot <- ggarrange(
  bigram_h_freq,  # fila 1
  ggarrange(
    bigram_a_freq,
    bigram_v_freq,
    nrow   = 2,
    labels = c("B", "C")
  ),
  labels  = c("A", ""),
  ncol    = 2,
  widths  = c(1.2, 0.7),
  heights = c(1.2, 0.9)
)

combined_plot

# Nombre del archivo de salida depende del proyecto
out_file <- if (grepl("MASLD", DATA_FILE, ignore.case = TRUE)) {
  "bigram_combined_plot_MASLD.jpg"
} else {
  "bigram_combined_plot_HBV.jpg"
}

ggsave(
  filename = out_file,
  plot     = combined_plot,
  width    = 13.7,
  height   = 7.7,
  dpi      = 450,
  units    = "in"
)
