############################################################
## FIGURE 1 — MASLD / NAFLD in Latin America (PubMed)
## Panel A: Mapa LATAM (% estudios por país)
## Panel B: Conteo de publicaciones por año
## Panel C: Top 20 journals por número de publicaciones
############################################################

# Clean environment
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
library(stringdist)
library(sf)
library(maps)
library(ggrepel)
library(RColorBrewer)

# ============================
# 2) Cargar dataset MASLD LATAM
# ============================

# Ajusta el nombre si lo guardaste con otro:
df <- read.csv(
  file      = "dataset_MASLD_Latin_America.csv",
  header    = TRUE,
  sep       = ",",
  dec       = ".",
  stringsAsFactors = FALSE
)

str(df)

# Aseguramos tipos
if ("Year" %in% names(df)) {
  df$Year <- as.numeric(df$Year)
} else {
  stop("No se encontró la columna 'Year' en el dataset MASLD.")
}

if (!"Journal" %in% names(df)) {
  stop("No se encontró la columna 'Journal' en el dataset MASLD.")
}

if (!"Country_study" %in% names(df)) {
  stop("No se encontró la columna 'Country_study' en el dataset MASLD.")
}

# ============================
# 3) Panel B — Publicaciones por año
# ============================

dat2 <- df %>%
  filter(!is.na(Year)) %>%
  count(Year, name = "n")

# Rango dinámico para ejes
year_min <- min(dat2$Year, na.rm = TRUE)
year_max <- max(dat2$Year, na.rm = TRUE)

y_max   <- max(dat2$n, na.rm = TRUE)
y_break <- pretty(c(0, y_max))[2]  # paso "bonito" (10, 20, etc.)

text_color <- "black"

b <- ggplot(dat2, aes(x = Year, y = n)) + 
  geom_area_pattern(
    data          = dat2,
    pattern       = "gradient", 
    fill          = "#00000000",
    pattern_fill  = "#00000000",
    pattern_fill2 = "#644296"
  ) +
  geom_line(size = 0.4, color = "#644296", alpha = 0.4) +
  geom_point(shape = 16, size = 1.2, colour = "#644296", alpha = 0.4) +
  geom_point(shape = 16, size = 0.4, colour = "white", alpha = 0.4) +
  geom_text(
    aes(label = n, y = n + 0.03 * y_max),
    vjust = 0,
    size = 2.3,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = seq(year_min, year_max, by = 2),
    minor_breaks = seq(year_min, year_max, by = 1),
    guide = "prism_minor",
    expand = expansion(mult = c(0.00, 0.00))
  ) +
  scale_y_continuous(
    breaks = seq(0, y_max + y_break, by = y_break),
    guide  = "prism_minor",
    expand = expansion(mult = c(0.01, 0.05))
  ) +
  labs(x = "", y = "Publication count") +
  theme_classic() +
  theme(
    legend.title      = element_blank(),
    legend.position   = "top",
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_blank(),
    axis.text.y       = element_text(color = text_color, size = 8),
    axis.text.x       = element_text(color = text_color, size = 7.8,
                                     angle = 45, hjust = 1, vjust = 1),
    axis.title.y      = element_text(color = text_color, size = 9, face = "bold"),
    axis.line.y       = element_line(color = "black", size = 0.21),
    axis.ticks.y      = element_line(color = "black", size = 0.21),
    axis.line.x       = element_line(color = "black", size = 0.21),
    axis.ticks.x      = element_line(color = "black", size = 0.21),
    axis.ticks.length = unit(0.26, "cm"),
    prism.ticks.length.y = unit(2.2, "pt"),
    prism.ticks.length.x = unit(2.2, "pt"),
    plot.background   = element_rect(fill = "white", color = NA)
  )

b

# ============================
# 4) Panel C — Top 20 journals
# ============================

# Limpieza de algunos nombres de journal (puedes agregar más si aparece ruido)
df <- df %>%
  mutate(
    Journal = recode(
      Journal,
      "Rev Med Chil"         = "Rev Med Chile",
      "GUT"                  = "Gut",
      "MEDICINA"             = "Medicina",
      "rev cuba educac med superr`" = "rev cuba educac med superr",
      "rev cubana med trop"  = "Rev Cuba Med Trop",
      .default = Journal
    )
  )

tab <- df %>%
  filter(!is.na(Journal) & Journal != "") %>%
  count(Journal, name = "n") %>%
  arrange(desc(n))

# Opcional: versión "clean" del nombre, por si luego quieres clustering
tab$Journal_clean <- tab$Journal %>%
  str_to_lower() %>%
  str_trim() %>%
  str_replace_all("\\.", "")

top_20_tab <- tab %>% head(20)

text_color <- "black"

c <- ggplot(top_20_tab, aes(x = reorder(Journal, n), y = n)) +
  geom_bar(stat = "identity", fill = "#FF7777", width = 0.7) +
  geom_text(
    aes(label = n, y = n + 0.03 * max(n)),
    vjust = 0,
    size = 3,
    color = "black"
  ) +
  labs(x = "", y = "Publication count") +
  scale_y_continuous(
    breaks = pretty(c(0, max(top_20_tab$n))),
    guide  = "prism_minor",
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_classic() +
  theme(
    legend.title      = element_blank(),
    legend.position   = "top",
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_blank(),
    axis.text.y       = element_text(color = text_color, size = 8),
    axis.text.x       = element_text(color = text_color, size = 8,
                                     angle = 45, hjust = 1),
    axis.title.y      = element_text(color = text_color, size = 9, face = "bold"),
    axis.line.y       = element_line(color = "black", size = 0.21),
    axis.ticks.y      = element_line(color = "black", size = 0.21),
    axis.line.x       = element_line(color = "black", size = 0.21),
    axis.ticks.x      = element_line(color = "black", size = 0.21),
    axis.ticks.length = unit(0.26, "cm"),
    prism.ticks.length.y = unit(2.2, "pt"),
    prism.ticks.length.x = unit(2.2, "pt"),
    plot.background   = element_rect(fill = "white", color = NA)
  )

c

# ============================
# 5) Panel A — Mapa LATAM MASLD
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

# Mapa base
some.LA.maps <- map_data("world", region = latin_america)

# Conteo por país (Country_study viene de tu script de merge)
tab_country <- df %>%
  filter(!is.na(Country_study)) %>%
  count(Country_study, name = "n") %>%
  filter(Country_study %in% latin_america) %>%
  mutate(
    percentage = (n / sum(n)) * 100
  ) %>%
  arrange(desc(percentage))

# Merge con mapa
some.LA.maps <- some.LA.maps %>%
  left_join(tab_country, by = c("region" = "Country_study"))

# Centroides para labels
region.lab.data <- some.LA.maps %>%
  group_by(region) %>%
  summarise(
    long = mean(long),
    lat  = mean(lat),
    .groups = "drop"
  )

# Ajustes manuales para evitar solapes
region.lab.data$lat[region.lab.data$region == "Ecuador"] <- -1.83
region.lab.data$long[region.lab.data$region == "Ecuador"] <- -78.18
region.lab.data$lat[region.lab.data$region == "Peru"] <- -9.19
region.lab.data$long[region.lab.data$region == "Peru"] <- -75.01
region.lab.data$lat[region.lab.data$region == "Chile"] <- -33.5
region.lab.data$long[region.lab.data$region == "Chile"] <- -70.5
region.lab.data$lat[region.lab.data$region == "Jamaica"] <- 18.1
region.lab.data$long[region.lab.data$region == "Jamaica"] <- -77.6

combined_data <- region.lab.data %>%
  left_join(tab_country, by = c("region" = "Country_study"))

# Paleta
ylorrd_palette <- rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffff'))
my_breaks <- pretty(tab_country$percentage)
my_breaks <- my_breaks[my_breaks > 0]

map <- ggplot(some.LA.maps, aes(x = long, y = lat)) +
  geom_polygon(
    aes(group = group, fill = percentage),
    color = "black",
    size  = 0.1
  ) +
  geom_point(
    data = region.lab.data %>% filter(region %in% combined_data$region),
    aes(x = long, y = lat),
    color = "black",
    size  = 0.2
  ) +
  geom_label_repel(
    data = combined_data %>% filter(!is.na(percentage)),
    aes(label = paste(region, sprintf("%.1f%%", percentage)), fill = percentage),
    size            = 2,
    min.segment.length = 0,
    seed            = 42,
    box.padding     = 0.5,
    max.overlaps    = Inf,
    color           = "black",
    label.r         = 0.25,
    alpha           = 0.8,
    segment.size    = 0.1,
    label.size      = 0.2
  ) +
  scale_fill_gradientn(
    name   = "MASLD / NAFLD\nstudies",
    colours = ylorrd_palette,
    breaks  = my_breaks,
    na.value = "grey90",
    labels  = function(x) paste0(round(x, 1), "%")
  ) +
  theme_void() +
  theme(
    legend.position   = c(0.25, 0.4),
    legend.key.height = unit(20, "pt"),
    legend.key.width  = unit(15, "pt"),
    legend.text       = element_text(size = 7),
    legend.title      = element_text(size = 8)
  )

map

# ============================
# 6) Armar figura compuesta A–C
# ============================

empty_plot <- ggplot() + theme_void()

arranged_plots_1 <- ggarrange(
  map, labels = c("A"),
  ggarrange(
    empty_plot, b, empty_plot, c,
    ncol   = 1,
    align  = "hv",
    labels = c("", "B", "", "C"),
    heights = c(0.1, 0.9, 0.1, 0.9),
    hjust  = -0.09,
    vjust  = -1
  ),
  ncol   = 2,
  nrow   = 1,
  widths = c(1, 0.99)
)

arranged_plots_1

ggsave(
  "figure_1_MASLD_pubmed.jpg",
  plot   = arranged_plots_1,
  dpi    = 400,
  width  = 12.5,
  height = 7.8,
  units  = "in"
)

