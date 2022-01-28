###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.
DATA PRODUCT INFORMATION
------------------------
ID: NEON.DOM.SITE.DP1.20252.001
Name: Secchi depth
Description: Measurement of water column Secchi depth in non-wadeable streams and lakes
NEON Science Team Supplier: Aquatic Observation System
Abstract: This data product contains the quality-controlled, native sampling resolution data from NEON's Secchi depth data collection. Secchi measurements indicate water clarity, and secchi depth is used to determine the depth to which light penetrates. This value can also be used to calculate the depth of the euphotic zone in a lake or river, which is especially important for phytoplankton sampling. Secchi data are collected when collecting data for any standard operating procedure that samples the water column in a lake or river (e.g., surface water chemistry and phytoplankton) by lowering a Secchi disk through the water column, and recorded the depth(s) to which it disappears from view. Secchi depth measurements are collected only during ice-free periods, and are collected a minimum of 4 times per year, up to 12+ times per year. For additional details, see the user guide, protocols, and science design listed in the Documentation section in this data product's details webpage.
Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.
dep_secchi:  30
Brief Design Description: Secchi data are collected during sample collection for any standard operating procedure that samples the water column in a lake or river (phytoplankton, zooplankton, pelagic surface water microbes, and pelagic surface water chemistry), and may be collected during other sampling visits to lake and river sites. Measurements are collected only near the buoy sensors in lakes and rivers, and data are not collected under the ice. Secchi depth measurements are only collected during ice-free periods, and are collected a minimum of 4 times per year, up to 12+ times per year.
Brief Study Area Description: Measured at all NEON lake and river sites.
Sensor(s): 
Keywords: attenuation, secchi disk, lakes, transparency, light penetration, pelagic, water column, lake, water quality, aquatic, river
Domain: D03
DATA PACKAGE CONTENTS
---------------------
This folder contains the following documentation files:
This data product contains up to 1 data tables:
- Term descriptions, data types, and units: NEON.D03.BARC.DP1.20252.001.variables.20220107T002743Z.csv
dep_secchi - Secchi field data collection per site per sampling day
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic data package includes all measurements. An expanded download package is not available for this product.
Expanded download package definition: 
FILE NAMING CONVENTIONS
-----------------------
NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.
ISSUE LOG
----------
This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.20252.001.
Issue Date: 2021-01-06
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or eliminated sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
       Date Range: 2020-03-23 to 2021-06-01
       Location(s) Affected: All
Resolution Date: 
Resolution: 
ADDITIONAL INFORMATION
----------------------
Queries for this data product will return all data for `dep_secchi` collected during the date range specified. The protocol dictates that secchi measurements are collected at lake and river  sites whenever another protocol that samples the water column is implemented (i.e., surface water chemistry, surface water microbes, phytoplankton, zooplankton) during ice-free periods, which results in a minimum of 4 data points per year collected near the sensor buoy location. Each record in `dep_secchi` corresponds to one record in `dep_profileHeader` (see related data products). Duplicates may exist where protocol and/or data entry aberrations have occurred; users should check data carefully for anomalies before analyzing data.
NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------
A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.20252.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.
DATA QUALITY AND VERSIONING
---------------------------
NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI.
To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.
