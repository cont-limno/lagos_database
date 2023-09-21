# clusters_refresh_20220422.R
# Created by Nicole Smith
# Based on provisional_clustering_method.R
# WARNING: DATA-DRIVEN METHODOLOGY!! If new sites added, new clusters may be added where only one cluster would have resulted if sites were present in earlier versions.
# Groups sites within lake into geographic clusters
# Presence of a cluster indicates that multiple identifiers are likely being used among multiple datasets for a single, common real-life "site" entity
# Methods worked out with Lottig, Smith, Delany, Soranno in 2019
# See clustering_demo.Rmd for defense of 200m cutoff


library(sf)
library(tidyverse)
library(mapview)

# UPDATE THIS AS NEEDED
gdb <- 'C:/Users/smithn78/Dropbox/CL_HUB_GEO/Lake_Georeferencing/dupes_issue/Lake_Linking.gpkg'
ALBERS_USGS = '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

#+--------------- KEW stuff----------------

gdb <- 'C:/Users/webst152/Dropbox/CL_HUB_GEO/Georef_Limno_Xfer/dupes_issue/Lake_Linking.gpkg'
gdb <- 'C:/Users/webst152/Dropbox/CL_HUB_GEO/Georef_Limno_Xfer/2022_Limno_Updates/Limno_2022_Sites_Clustering.gpkg'

library(readr)
lagos_wqp_20220412 <- read_csv("C:/Users/webst152/Dropbox/CL_HUB_GEO/Georef_Limno_Xfer/lagos_wqp_20220412.csv")
summary(lagos_wqp_20220412)

tst <- read_sf(gdb)
str(tst)

#---------------END

links <- read_sf(gdb, 'LAGOS_limno_linked_merged') %>%
  st_transform(ALBERS_USGS)
str(links)
links_df <- links %>%
  st_set_geometry(NULL)

MAXDIST = 200

# get vector of lakeIDs only of lakes with > 1 site within
links_more_than_one <- links_df %>%
  filter(!is.na(Linked_lagoslakeid)) %>%
  group_by(Linked_lagoslakeid) %>%
  mutate(nsites = n()) %>%
  filter(nsites > 1)

lagoslakeids <- links_more_than_one %>%
  distinct(Linked_lagoslakeid) %>%
  pull()

# for each lake individually (so that cluster members must be from the same lake)

# create column for cluster label (an integer)
links$samplesiteID_n <- NA_integer_
for (id in lagoslakeids) {
  # pull the sites from this lake only
  lk_pts <- links %>% filter(Linked_lagoslakeid == id) %>% select(samplesite_lat, samplesite_lon)
  filter_indices <- which(links$Linked_lagoslakeid ==id)
  
  # cluster
  dist <- dist(st_coordinates(lk_pts))
  result <- cutree(hclust(dist, method="complete"), h=MAXDIST) # hierarchical clustering, cut @ 200m 
  
  # result contains cluster labels (a unique integer), transfer label to corresponding site row
  links$samplesiteID_n[filter_indices] <- result
}

LETTERS2 <- as.vector(sapply(LETTERS, function(x) paste0(x, LETTERS)))

# calculating distance from cluster center to site, and mean distance for the cluster
links_dist <- links %>%
  mutate(samplesiteID_n = if_else(!is.na(samplesiteID_n) & !is.na(Linked_lagoslakeid), samplesiteID_n, as.integer(1))) %>%
  mutate(samplesiteID_CLUSTER = if_else(!is.na(Linked_lagoslakeid),
                                        paste(Linked_lagoslakeid, LETTERS2[samplesiteID_n], sep='-'),
                                        NA_character_)
  ) %>%
  mutate(site_x = st_coordinates(.)[,1],
         site_y = st_coordinates(.)[,2]) %>%
  group_by(samplesiteID_CLUSTER) %>%
  mutate(cluster_n = n(),
         in_cluster = if_else((!is.na(samplesiteID_CLUSTER) & cluster_n > 1), 1, 0),
         cluster_n = if_else(in_cluster == 1, cluster_n, NA_integer_),
         cluster_x = if_else(in_cluster == 1, mean(site_x), NA_real_),
         cluster_y = if_else(in_cluster == 1, mean(site_y), NA_real_),
         cluster_meandist = mean(sqrt((cluster_x - site_x)^2 + (cluster_y - site_y)^2))) %>%
  ungroup() %>%
  mutate(site_cluster_dist = sqrt((cluster_x - site_x)^2 + (cluster_y - site_y)^2)) %>%
  select(-c(in_cluster, site_x, site_y, cluster_x, cluster_y, samplesiteID, num_samplesites, samplesiteID_n)) %>%
  st_transform('+init=epsg:4269') %>%
  mutate(cluster_lat = round(st_coordinates(.)[,2], 6),
         cluster_lon = round(st_coordinates(.)[,1], 6)) %>%
  arrange(samplesiteID_CLUSTER)

write_sf(links_dist, gdb, 'provisional_clusters')
write_csv(st_set_geometry(links_dist, NULL), file.path(dirname(gdb), 'provisional_clusters.csv'))
