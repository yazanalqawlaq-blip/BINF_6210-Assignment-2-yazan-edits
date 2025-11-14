# Assignment 2 – BINF*6210, November 15, 2025
# Primary author: William Feinman (GitHub: WFeinman)
# Secondary contributor: Yazan Alqawlaq (Student ID: 1021214; GitHub: yazanalqawlaq-blip)
# Description:
#   Original script explores geographic and phylogenetic patterns in Sciuridae records from BOLD.
#   As secondary contributor, I made three main edits:
#   EDIT 1 – Corrected the per-species latitude summaries (highest/lowest latitude species)
#             to explicitly use per-species max/min latitude and avoid subtle data errors.
#   EDIT 2 – Reduced redundancy and added process checks by adding a summarize_counts()
#             helper function and using it to summarize key geographic and credit variables.
#   EDIT 3 – Altered geographic visualizations by cleaning coordinates (removing impossible
#             lat/lon values), using a cleaned data object for mapping, applying viridis
#             colour scales, shortening long institution names, and adjusting titles/legends
#             so maps are clearer and legends are fully visible.

## _ Packages used -------
library(stats)
library(tidyverse)
library(viridis)
# Use a light theme by default for all plots
theme_set(theme_light())


## _ Setup and data cleaning -------

# Reading in the BOLD data.
Sciuridae <- read_tsv("../data/BOLD_Sciuridae.tsv")

# Setting up a new tibble with only the data relevant to our inquiries.
Sciuridae2 <- Sciuridae[, c(
  1, 8, 11, 19, 21:23, 26, 28, 41, 48, 50:53, 55, 62:65, 67, 71
)]

# Make sure the columns we want transferred properly and check for potential issues.
names(Sciuridae2)

# Splitting the coordinates column "coord" into latitude and longitude for ease of use,
# since the original is a character vector.
Sciuridae_lat_lon <- Sciuridae2 %>%
  filter(!is.na(coord)) %>%
  filter(!is.na(bin_uri)) %>%
  mutate(lat = str_remove_all(coord, ", .+")) %>%
  mutate(lat = str_remove(lat, "[\\[\\]]")) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = str_remove_all(coord, ".+, ")) %>%
  mutate(lon = str_remove(lon, "[\\[\\]]")) %>%
  mutate(lon = as.numeric(lon))

## EDIT 2 – Helper for repeated count summaries (reduces redundancy and clarifies checks)
summarize_counts <- function(data, var, n = 10) {
  data %>%
    count({{ var }}, sort = TRUE) %>%
    slice_head(n = n)
}

## EDIT 3 – Clean coordinates to remove impossible lat/lon values for mapping
Sciuridae_lat_lon_clean <- Sciuridae_lat_lon %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180)


## _ Bin considerations-------

# Check for number of unique BINs to get a better idea of the data.
length(unique(Sciuridae_lat_lon$bin_uri))

# Distribution of BINs to understand sampling across BINs.
summarize_counts(Sciuridae_lat_lon, bin_uri)

# Marker codes represented in the dataset.
summarize_counts(Sciuridae_lat_lon, marker_code)


## _ Geographic considerations-------

# Inspect raw lat and lon distributions to check for potential outliers or default coordinates.
summarize_counts(Sciuridae_lat_lon, lat)
summarize_counts(Sciuridae_lat_lon, lon)

# After removing impossible coordinates, re-check the cleaned distributions.
summarize_counts(Sciuridae_lat_lon_clean, lat)
summarize_counts(Sciuridae_lat_lon_clean, lon)

# Geographic distribution across several spatial scales (top values for each category).
summarize_counts(Sciuridae_lat_lon_clean, `country/ocean`)
summarize_counts(Sciuridae_lat_lon_clean, `province/state`)
summarize_counts(Sciuridae_lat_lon_clean, region)
summarize_counts(Sciuridae_lat_lon_clean, sector)
summarize_counts(Sciuridae_lat_lon_clean, site)
summarize_counts(Sciuridae_lat_lon_clean, realm)
summarize_counts(Sciuridae_lat_lon_clean, ecoregion)
summarize_counts(Sciuridae_lat_lon_clean, habitat)


## _ Credit considerations-------

# Institutions primarily responsible for specimen records.
summarize_counts(Sciuridae_lat_lon_clean, inst)

# How the data were collected and identified.
summarize_counts(Sciuridae_lat_lon_clean, collectors)
summarize_counts(Sciuridae_lat_lon_clean, identified_by)
summarize_counts(Sciuridae_lat_lon_clean, identification_method)


## _ Phylogeny considerations-------

# Phylogenetic distribution at several taxonomic levels.
summarize_counts(Sciuridae_lat_lon_clean, subfamily)
summarize_counts(Sciuridae_lat_lon_clean, genus)
summarize_counts(Sciuridae_lat_lon_clean, species)


## _ Graphing it out-------

# EDIT 3 (part 1) – Top 10 institutional contributors with shortened names and cleaner legend.

# Identify the top 10 institutions by record count.
top_institutions <- Sciuridae_lat_lon_clean %>%
  filter(!is.na(inst)) %>%
  count(inst, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(inst)

# Keep only records from those top 10 institutions and create shortened labels.
Sciuridae_top_inst <- Sciuridae_lat_lon_clean %>%
  filter(inst %in% top_institutions) %>%
  mutate(
    inst_short = recode(
      inst,
      "Arizona State University" = "ASU",
      "Centre for Biodiversity Genomics" = "CBG",
      "Centro de Investigaciones Biologicas del Noroeste S. C." = "CIBNOR",
      "Institute of Biodiversity and Ecosystem Research, Sofia" = "IBER-BAS",
      "Mined from GenBank, NCBI" = "GenBank (NCBI)",
      "Penza State Pedagogical University" = "PSPU",
      "Royal Ontario Museum" = "ROM",
      "Russian Academy of Sciences, Koltzov Institute of Developmental Biology" = "IDB RAS",
      "University of Guelph" = "U of G",
      "Zoological Museum of Moscow University" = "ZMMU"
    )
  )

# Check the distribution of the shortened institution names.
summarize_counts(Sciuridae_top_inst, inst_short)

## EDIT 3 – edited map: geographic distribution of top institutional contributors
ggplot(data = Sciuridae_top_inst) +
  geom_point(
    mapping = aes(x = lon, y = lat, colour = inst_short),
    alpha = 0.8
  ) +
  scale_colour_viridis_d(guide = guide_legend(title = NULL)) +
  coord_quickmap() +
  labs(
    title = "Geographic distribution of top institutional contributors",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
  )

# Plot to check that the cleaned coordinates capture the bulk of the data distribution.
ggplot(data = Sciuridae_lat_lon_clean) +
  geom_point(mapping = aes(x = lon, y = lat), alpha = 0.6) +
  coord_quickmap() +
  labs(
    title = "Recreation of cleaned coordinate data",
    x = "Longitude",
    y = "Latitude"
  )

## EDIT 1 – Correct highest and lowest latitude species summaries

# Highest latitude species in dataset (per-species maximum latitude).
highest_lat_species <- Sciuridae_lat_lon %>%
  filter(!is.na(species), !is.na(lat)) %>%
  group_by(species) %>%
  summarise(max_lat = max(lat), .groups = "drop") %>%
  arrange(desc(max_lat))

# Species with the highest maximum latitude.
print(highest_lat_species$species[1])

# Inspect the top 10 highest-latitude species.
highest_lat_species %>%
  slice_head(n = 10) %>%
  print()

# Lowest latitude species in dataset (per-species minimum latitude).
lowest_lat_species <- Sciuridae_lat_lon %>%
  filter(!is.na(species), !is.na(lat)) %>%
  group_by(species) %>%
  summarise(min_lat = min(lat), .groups = "drop") %>%
  arrange(min_lat)

# Species with the lowest minimum latitude.
print(lowest_lat_species$species[1])

# Inspect the top 10 lowest-latitude species.
lowest_lat_species %>%
  slice_head(n = 10) %>%
  print()

# Geographic phylogeny distribution: subfamilies.
ggplot(data = Sciuridae_lat_lon_clean) +
  geom_point(
    mapping = aes(x = lon, y = lat, colour = subfamily),
    alpha = 0.8
  ) +
  scale_colour_viridis_d(guide = guide_legend(title = NULL)) +
  coord_quickmap() +
  labs(
    title = "Geographic distribution of Sciuridae subfamilies",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
  )

Sciuridae_large_genus <- Sciuridae_lat_lon_clean %>%
  filter(!is.na(genus)) %>%
  group_by(genus) %>%
  filter(n() >= 34)

## EDIT 3 – Edited map of highly sampled genera (clean coords, Edited legend and title)
ggplot(data = Sciuridae_large_genus) +
  geom_point(
    mapping = aes(x = lon, y = lat, colour = genus),
    alpha = 0.8
  ) +
  scale_colour_viridis_d(guide = guide_legend(title = NULL)) +
  coord_quickmap() +
  labs(
    title = "Distribution of highly sampled Sciuridae genera",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
  )

Sciuridae_large_species <- Sciuridae_lat_lon_clean %>%
  filter(!is.na(species)) %>%
  group_by(species) %>%
  filter(n() >= 16)

## EDIT 3 – Edited map of highly sampled species (clean coords, neat legend)
ggplot(data = Sciuridae_large_species) +
  geom_point(
    mapping = aes(x = lon, y = lat, colour = species),
    alpha = 0.8
  ) +
  scale_colour_viridis_d(guide = guide_legend(title = NULL)) +
  coord_quickmap() +
  labs(
    title = "Distribution of highly sampled Sciuridae species",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
  )
