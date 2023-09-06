limno_siteclusters_7DEC22_readme.txt

Output file: limno_siteclusters_7DEC22.csv
Created 07DEC22 by K Webster
Notes on clustering of sites for LIMNO export

Source data: Us_final_lat_lon.csv // file created by Arnab

R Script: clusters_refresh_20220422_working.R  // script created by Nicole Smith to generate clusters of sites based on Lat/Lon

Max Distance: 200m was chosen as the maximum distance between sites in the cluster based on prior work by Nicole //
	I looked at other distances (eg., 300, 500) and it seemed there was more chance of picking up sites in different bays or near shore combined with mid lake
	I hesitate to deviate from 200m as that was what was initially selected by LIMNO team based on some analyses Nicole did.
	I can easily run the script for different max distances.

Notes on data file:
	(1) There are 34334 NA values for cl_n and cl_mndis / these lake clusters consist of only 1 lake so there is no cl_mndis to quantify.
	(2) cl_n = the number of sites in a cluster // cl_mndis (cluster_meandist) = mean distance among lakes in the cluster // 
		site_cldis (site_clusterdist) = distance from site to mean cluster coords //
		clus_lat = latitude of cluster mean // clus_lon = latitude of cluster mean

Some stats:
	(1) There were 20,336 lakes in the file of which 9910 had only 1 site and 10426 had >1 site (range 1:583, median = 2, 75th = 3)
	(2) Clusters per lake ranged 1:396, median=1, 75th = 2  
	(3) Distance from a site to the cluster mean ranged 0:161.8 with median = 27.24 and 75th = 58.61
	(4) The mean distance of sites from the cluster mean ranged 0:101.82, median = 32.22 and 75th = 58.22
	(5) Lake of the Ozarks, lagoslakeid = 6067 had 433 sites and 396 clusters // many sites seem to plot near shore.
	(6) 9828 sites have identical lat/lon with n duplicates ranging 2:25 with median = 4 and 75th = 5 / so these are different sample site ids within a lake
		that have identical coordinates.  These sites are from 2385 lakes.  4831 of the sites start with "USGS-...





