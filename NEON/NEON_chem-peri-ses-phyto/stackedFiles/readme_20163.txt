###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.
DATA PRODUCT INFORMATION
------------------------
ID: NEON.DOM.SITE.DP1.20163.001
Name: Periphyton, seston, and phytoplankton chemical properties
Description: Carbon (C), nitrogen (N), phosphorus (P), isotopes, chlorophyll a, and pheophytin of periphyton and phytoplankton from benthic and water column samples in lakes, rivers, and wadeable streams
NEON Science Team Supplier: Aquatic Observation System
Abstract: This data product contains the quality-controlled, native sampling resolution data from NEON's aquatic periphyton and phytoplankton chemical analyses provided by a contracted lab. Benthic and water column field samples are collected in wadeable streams, rivers, and lakes three times per year during the growing season using the type of sampler most suitable to the habitat and substratum types present at the site. Samples are separated into aliquots at the domain support facility and filtered onto glass-fiber filters for chlorophyll, pheophytin, carbon, nitrogen, phosphorus, and carbon, nitrogen, and sulfur isotopes for analysis at external facilities. For additional details, see the user guide, protocols, science design, and laboratory SOPs listed in the Documentation section in this data product's details webpage.
Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.
alg_fieldData:  60
alg_domainLabChemistry:  60
alg_algaeExternalLabDataPerSample:  180
alg_algaeExternalLabQA:  180
asi_externalLabPOMSummaryData:  14
Brief Design Description: Periphyton and phytoplankton samples are collected three times per year at wadeable stream, river, and lake sites during aquatic biology bout windows, roughly in spring, summer, and fall. Samples are collected using the most appropriate sampler for the habitat and substratum type, including rock scrubs, grab samples, and epiphyton, and water column samplers for phytoplankton. In wadeable streams, periphyton samples are collected in the two most dominant benthic habitat types (e.g. riffles, runs, pools, step pools). In lakes, water-column phytoplankton samples are collected near the buoy and littoral sensors using a Kemmerer sampler, and in littoral areas using the best benthic sampling method for the substratum type. In rivers, phytoplankton samples are collected near the buoy and two other deep-water locations using a Kemmerer or Van Dorn sampler, and in littoral areas using the most appropriate benthic sampling method for the substratum type. All samples are returned to the domain support facility for subsampling. Subsamples are  filtered onto glass-fiber filters then frozen (chlorophyll/pheophytin) or dried (C, N, P, isotopes) and shipped to external analytical facilities for analysis.
Brief Study Area Description: Measured at all NEON aquatic sites (wadeable streams, lakes, and rivers).
Sensor(s): 
Keywords: nitrogen-15 (15N), algae, seston, suspended, nitrogen (N), phaeophytin, streams, carbon (C), stable isotopes, sulfur-34 (34S), pheophytin, aquatic, benthic, chlorophyll, periphyton, water column, rivers, carbon-13 (13C), phytoplankton, sulfur (S), microalgae, phosphorous (P), lakes
Domain: D05
DATA PACKAGE CONTENTS
---------------------
This folder contains the following documentation files:
This data product contains up to 5 data tables:
- Term descriptions, data types, and units: NEON.D05.CRAM.DP1.20163.001.variables.20220125T181130Z.csv
alg_domainLabChemistry - Periphyton, seston, and phytoplankton domain lab data for chemistry samples
alg_fieldData - Periphyton, seston, and phytoplankton field collection data from streams, lakes, and non-wadeable streams
alg_algaeExternalLabDataPerSample - Algae external lab chemistry data
asi_externalLabPOMSummaryData - Plant, algae and POM chemistry external lab summary data
alg_algaeExternalLabQA - Algae chemistry external lab data quality assurance
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic data package includes the analytical results.
Expanded download package definition: The expanded download package includes the analytical results plus quality control data from the analytical facility.
FILE NAMING CONVENTIONS
-----------------------
NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.
ISSUE LOG
----------
This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.20163.001.
Issue Date: 2021-01-06
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or eliminated sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
       Date Range: 2020-03-23 to 2021-06-01
       Location(s) Affected: All
Resolution Date: 
Resolution: 
Issue Date: 2020-10-22
Issue: Near-shore sampling locations in all lake sites were named 'inlet' and 'outlet' even if the site was a seepage lake that has no single inlet or outlet.
       Date Range: 2014-07-01 to 2020-12-31
       Location(s) Affected: BARC, CRAM, LIRO, PRLA, PRPO, SUGG, TOOK
Resolution Date: 2020-12-31
Resolution: Near-shore collection sites at seepage lakes are now called 'littoral1' and 'littoral2', where 'littoral1' corresponds to previous site name 'inlet' and 'littoral2' corresponds to previous site name 'outlet'.
Issue Date: 2020-10-22
Issue: Raw data used in chlorophyll and pheophytin calculations not reported in NEON data
       Date Range: 2014-07-01 to 2019-01-22
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2019-01-22
Resolution: Dilution factor, solvent volume, and pre- and post-acidification fluorescence recorded in data. Labs calculate chlorophyll concentration and also provide equations for calculations using these raw data in the lab SOP.
Issue Date: 2020-10-22
Issue: Data uncorrected for pheophytin a were reported in data as analyte = “total chlorophyll _a_”.
       Date Range: 2017-10-10 to 2018-11-20
       Location(s) Affected: ARIK, BARC, BIGC, BLDE, BLUE, BLWA, CARI, COMO, CRAM, CUPE, FLNT, GUIL, HOPB, KING, LECO, LEWI, LIRO, MART, MAYF, MCDI, MCRA, OKSR, POSE, PRIN, PRLA, PRPO, REDB, SUGG, TOMB, TOOK, WALK, WLOU
Resolution Date: 2018-11-20
Resolution: Data corrected for pheophytin are reported in the data as analyte = “chlorophyll _a_”, in addition to “total chlorophyll _a_” indicating data not corrected for pheophytin.
Issue Date: 2018-07-16
Issue: Chlorophyll a, pheophytin, and total chlorophyll data returned from EcoAnalysts between 3/1/18 to 7/9/18 were not corrected for filter volume or acetone extraction volume.
       Date Range: 2018-03-01 to 2018-07-09
       Location(s) Affected: ARIK, BARC, BIGC, BLUE, BLWA, CRAM, CUPE, FLNT, GUIL, HOPB, KING, LECO, LEWI, LIRO, MART, MAYF, MCDI, MCRA, POSE, PRIN, PRLA, PRPO, REDB, SUGG, TOMB, and WLOU
Resolution Date: 2018-07-16
Resolution: All data have been edited and now reflect both domain filter volume and the volume of the acetone extraction.
Issue Date: 2020-10-22
Issue: Wadeble stream seston samples contain sparse algal cells
       Date Range: 2014-07-01 to 2018-07-05
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2018-07-05
Resolution: Analysis of algal C, N, P, and isotopes in wadeable stream seston halted and removed from scope of data product. Algal chlorophyll and pheophytin continued to be analyzed to support chlorophyll sensor measurements in wadeable streams.
Issue Date: 2020-10-22
Issue: Algal chemistry samples (C, N, P, and algal isotopes) were filtered, frozen at -20 C, and shipped overnight to the analytical lab for processing.
       Date Range: 2014-07-01 to 2017-10-19
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2017-10-19
Resolution: Algal chemistry samples (C, N, P, and algal isotopes) were filtered, dried at 65 C overnight, stored in a desiccator, and shipped to the analytical lab under ambient, ground conditions.
Issue Date: 2020-10-22
Issue: Gap in algal chlorophyll data because a lab contract for algal chlorophyll not in place. Chlorophyll samples not stored due to short holding time.
       Date Range: 2017-02-02 to 2017-10-10
       Location(s) Affected: ARIK, BARC, BLUE, BLWA, CARI, COMO, CUPE, FLNT, GUIL, KING, HOPB, LECO, LEWI, MAYF, OKSR, POSE, PRIN, PRLA, PRPO, REDB, SUGG, TOMB, TOOK, WALK
Resolution Date: 2017-10-10
Resolution: Algal chlorophyll contract in place for the new fiscal year, algal chlorophyll samples collected, shipped, and analyzed after end date.
Issue Date: 2020-10-22
Issue: In early 2017, NEON stopped shipping algal isotope samples to the Academy of Natural Sciences of Drexel University for analysis and began shipping to the SIRFER Lab at the University of Utah. The switch in external facility involved a major change in the SOP for algal C and N isotope analysis. At the Drexel lab, algal isotope filters were fumigated with concentrated HCl to remove carbonates prior to analysis. At the SIRFER lab, samples were analyzed without first fumigating to remove carbonates.
       Date Range: 2014-07-01 to 2017-02-28
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2017-02-28
Resolution: Laboratory data downloaded in the table ‘alg_algaeExternalLabDataPerSample’ that contains ‘Academy of Natural Sciences of Drexel University’ in the ‘laboratoryName’ field underwent acid fumigation prior to analysis. The most recent record to contain Drexel as the laboratoryName was the Resolved Date, but the date of external facility change will vary across the observatory.
Issue Date: 2020-10-22
Issue: Audit failure for chlorophyll and pheophytin analyses, ‘analyteConcentration’ data removed.
       Date Range: 2014-07-01 to 2017-02-01
       Location(s) Affected: ARIK, BARC, BLWA, CARI, COMO, CRAM, CUPE, GUIL, KING, LECO, LEWI, MAYF, OKSR, POSE, PRIN, PRLA, PRPO, REDB, SUGG, TOMB, TOOK, WALK
Resolution Date: 2017-02-01
Resolution: Analysis halted due to failed audit, new contract procured.
ADDITIONAL INFORMATION
----------------------
Queries for this data product will return all data for `alg_fieldData`, `alg_domainLabChemistry`, `alg_algaeExternalLabDataPerSample`, and `alg_algaeExternalLabQA` during the date range specified. Each record in `alg_fieldData` may have several child records in `alg_domainLabChemistry` and `alg_algaeExternalLabDataPerSample`. One unique child record is created for each sampleID - analyte - filterNumber combination. The expanded package also returns summary data for each analytical method from the contractor in `asi_externalLabPOMSummaryData`, with one record per date range, analyte, instrument, and method. Quality data, standards, and blanks are recorded in `alg_algaeExternalLabQA` with a batchID that corresponds to records in`alg_algaeExternalLabDataPerSample`. Duplicates may exist where protocol and/or data entry aberrations have occurred; users should check data carefully for anomalies before analyzing data.
NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------
A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.20163.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.
DATA QUALITY AND VERSIONING
---------------------------
NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI. 
To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.
