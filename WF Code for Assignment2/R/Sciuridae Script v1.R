

## _ Packages used -------
library(stats)
library(tidyverse)
library(viridis)
# + scale_color/fill_viridis(discrete = T/F)
theme_set(theme_light())


## _ Setup and data cleaning -------
#Reading in the BOLD data.
Sciuridae <- read_tsv("../data/BOLD_Sciuridae.tsv")

#Setting up a new tibble with only the data relevant to our inquiries.
Sciuridae2 <- Sciuridae[, c(1, 8, 11, 19, 21:23, 26, 28, 41, 48, 50:53, 55, 62:65, 67, 71)]

#Make sure the columns we want transferred properly, check data for potential issues.
names(Sciuridae2)

#Splitting the coordinates column "coord" into latitude and longitude for ease of use, since the original is a character vector:
Sciuridae_lat_lon <- Sciuridae2 %>%
  filter(!is.na(coord)) %>%
  filter(!is.na(bin_uri)) %>%
  mutate(lat = str_remove_all(coord, ', .+')) %>%
  mutate(lat = str_remove(lat, '[\\[\\]]')) %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = str_remove_all(coord, '.+, ')) %>%
  mutate(lon = str_remove(lon, '[\\[\\]]')) %>%
  mutate(lon = as.numeric(lon)) 
  

## _ Bin considerations-------  


#Check for number of unique bin_uri in dataset to get a better idea of our data.
length(unique(Sciuridae_lat_lon$bin_uri))

#Figure out why our unique bin counts are so different from the dataframe length with a quick check of the data
Sciuridae_lat_lon %>%
  count (bin_uri, sort = TRUE)

#Check how many unique BINS we have coordinate data for, see if data lost
length(unique(Sciuridae_lat_lon$bin_uri))

#Get a distribution breakdown
Sciuridae_lat_lon %>%
  count (bin_uri, sort = TRUE)

#What sort of information are we getting?
Sciuridae_lat_lon %>%
  count (marker_code, sort = TRUE)


## _ Geographic considerations-------  

#Get an idea of lat and lon data, check for outliers.
Sciuridae_lat_lon %>%
  count (lat, sort = TRUE)
Sciuridae_lat_lon %>%
  count (lon, sort = TRUE)

#Above disparity is likely due to taking the central geoid for a country if precise collection coordiantes are unavailable. 

#Getting a better idea of geographic distribution
Sciuridae_lat_lon %>%
  count (`country/ocean`, sort = TRUE)

Sciuridae_lat_lon %>%
  count (`province/state`, sort = TRUE)

Sciuridae_lat_lon %>%
  count (region, sort = TRUE)

Sciuridae_lat_lon %>%
  count (sector, sort = TRUE)

Sciuridae_lat_lon %>%
  count (site, sort = TRUE)

Sciuridae_lat_lon %>%
  count (realm, sort = TRUE)

Sciuridae_lat_lon %>%
  count (ecoregion, sort = TRUE)

Sciuridae_lat_lon %>%
  count (habitat, sort = TRUE)


## _ Credit considerations-------  

#Which institutions are primarily responsible?

Sciuridae_lat_lon %>%
  count (inst, sort = TRUE)

#How was this data collected/identified?
Sciuridae_lat_lon %>%
  count (collectors, sort = TRUE)

Sciuridae_lat_lon %>%
  count (inst, sort = TRUE)

Sciuridae_lat_lon %>%
  count (identified_by, sort = TRUE)

Sciuridae_lat_lon %>%
  count (identification_method, sort = TRUE)


## _ Phylogeny considerations-------  

#What does our phylogenic distribution look like?
Sciuridae_lat_lon %>%
  count (subfamily, sort = TRUE)

Sciuridae_lat_lon %>%
  count (genus, sort = TRUE)

Sciuridae_lat_lon %>%
  count (species, sort = TRUE)


## _ Graphing it out-------

#First, credit where it's due. Plot of institutional contributors and where they are active.

Sciuridae_large_inst <- Sciuridae_lat_lon %>%
  group_by(inst) %>%
  filter(n() >= 8)

Sciuridae_large_inst %>%
    count (inst, sort = TRUE)

ggplot(data = Sciuridae_large_inst) +
  geom_point(mapping = aes(x = lon, y = lat, colour = inst)) +
  labs(title = "Distribution of Top Instituional Contibutors", x = "Longitude", y = "Latitude")

#Plot to check above captures bulk of data distribution
ggplot(data = Sciuridae_large_inst) +
  geom_point(mapping = aes(x = lon, y = lat, )) +
  labs(title = "Recreation of Coordinate Data", x = "Longitude", y = "Latitude")


#Highest lat species in dataset:
print(Sciuridae_lat_lon$species[which.max(Sciuridae_lat_lon$lat)])

Sciuridae_lat_lon %>%
  group_by(species) %>%
  summarize(lat) %>%
  arrange(desc(lat)) %>%
  print()

#Lowest lat species in dataset:
print(Sciuridae_lat_lon$species[which.min(Sciuridae_lat_lon$lat)])

Sciuridae_lat_lon %>%
  group_by(species) %>%
  summarize(lat) %>%
  arrange(lat) %>%
  print()


#Geographic Phylogeny Distribution:

ggplot(data = Sciuridae_lat_lon) +
  geom_point(mapping = aes(x = lon, y = lat, colour = subfamily)) +
  labs(title = "Distribution of Sciuridae Subfamilies", x = "Longitude", y = "Latitude")


Sciuridae_large_genus<- Sciuridae_lat_lon %>%
  group_by(genus) %>%
  filter(n() >= 34)

ggplot(data = Sciuridae_large_genus) +
  geom_point(mapping = aes(x = lon, y = lat, colour = genus)) +
  labs(title = "Distribution of Highly Sampled Sciuridae Genuses", x = "Longitude", y = "Latitude")

Sciuridae_large_species<- Sciuridae_lat_lon %>%
  group_by(species) %>%
  filter(!is.na(species)) %>%
  filter(n() >= 16)

ggplot(data = Sciuridae_large_species) +
  geom_point(mapping = aes(x = lon, y = lat, colour = species)) +
  labs(title = "Distribution of Highly Sampled Sciuridae Species", x = "Longitude", y = "Latitude")
  
