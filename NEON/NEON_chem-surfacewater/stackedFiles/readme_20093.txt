###################################
########### Disclaimer ############
This is the most recent readme publication based on all site-date combinations used during stackByTable.
Information specific to the query, including sites and dates, has been removed. The remaining content reflects general metadata for the data product.
##################################

This data package been produced by and downloaded from the National Ecological Observatory Network (NEON). NEON is funded by the National Science Foundation (Awards 0653461, 0752017, 1029808, 1138160, 1246537, 1638695, 1638696, 1724433) and managed cooperatively by Battelle. These data are provided under the terms of the NEON data policy at https://www.neonscience.org/data-policy.
DATA PRODUCT INFORMATION
------------------------
ID: NEON.DOM.SITE.DP1.20093.001
Name: Chemical properties of surface water
Description: Grab samples of surface water chemistry including general chemistry, anions, cations, and nutrients.
NEON Science Team Supplier: Aquatic Observation System
Abstract: This data product contains the quality-controlled, native sampling resolution data from NEON's surface water chemistry sampling protocol. Subsamples are analyzed at NEON domain headquarters for alkalinity and acid neutralizing capacity (ANC); other subsamples are sent to external facilities for a broad suite of analytes, including dissolved and total nutrients and carbon, cations and anions, and general chemistry. For additional details, see the user guide, protocols, and science design listed in the Documentation section in this data product's details webpage.
Latency:
The expected time from data and/or sample collection in the field to data publication is as follows, for each of the data tables (in days) in the downloaded data package. See the Data Product User Guide for more information.
swc_domainLabData:  60
swc_externalLabData:  150
swc_fieldData:  30
swc_fieldSuperParent:  30
swc_externalLabSummaryData:  14
Brief Design Description: Grab samples of surface water at NEON aquatic sites are collected in streams and rivers 15- 26 times per year and 12 times per year in lakes. In streams and rivers, 12 samples are collected at regular intervals during the sampling season while the remaining 14 are collected on an irregular basis to capture major flow events. In lakes samples are collected approximately monthly and to capture ice-on and ice-off events. The field protocol used by NEON for collecting surface water chemistry samples follows the general requirements set forth by the 2011 USGS National Water-Quality Assessment (NAWQA) Program and the Arctic LTER standard operating procedures (SOP). Sample handling and preparation portions of this protocol follow the general requirements set forth by the USGS National Water-Quality Assessment (NAWQA) Program (USGS 2006).
Brief Study Area Description: Measured at all NEON aquatic sites.
Sensor(s): 
Keywords: acid neutralizing capacity (ANC), chemistry, surface water, anions, phosphorous (P), nutrients, chemical properties, analytes, water quality, alkalinity, nitrogen (N), grab samples, carbon (C), total carbon (TC), cations
Domain: D03
DATA PACKAGE CONTENTS
---------------------
This folder contains the following documentation files:
This data product contains up to 7 data tables:
- Term descriptions, data types, and units: NEON.D03.SUGG.DP1.20093.001.variables.20220125T180828Z.csv
swc_fieldSuperParent - Field data for the parent sample of surface water chemistry
swc_domainLabData - Surface water chemistry ALK and ANC domain lab data summary data
swc_externalLabSummaryData - Surface water chemistry external lab summary data
swc_externalLabData - Surface water chemistry from external lab
swc_fieldData - Surface water chemistry summary data per site per bout
swc_externalLabDataByAnalyte - Surface water chemistry from external lab in long format
swc_asiPOMFieldData - Surface water stable isotopes field data
If data are unavailable for the particular sites and dates queried, some tables may be absent.
Basic download package definition: The basic download package includes the primary measurements.
Expanded download package definition: The expanded package for this data product contain metadata associated with the in-house alkalinity and ANC titrations, and method detection limits from the analytical laboratory.
FILE NAMING CONVENTIONS
-----------------------
NEON data files are named using a series of component abbreviations separated by periods. File naming conventions for NEON data files differ between NEON science teams. A file will have the same name whether it is accessed via NEON's data portal or API. Please visit https://www.neonscience.org/data-formats-conventions for a full description of the naming conventions.
ISSUE LOG
----------
This log provides a list of issues that were identified during data collection or processing, prior to publication of this data package. For a more recent log, please visit this data product's detail page at https://data.neonscience.org/data-products/DP1.20093.001.
Issue Date: 2021-01-06
Issue: Safety measures to protect personnel during the COVID-19 pandemic resulted in reduced or eliminated sampling activities for extended periods at NEON sites. Data availability may be reduced during this time.
       Date Range: 2020-03-23 to 2021-06-01
       Location(s) Affected: All
Resolution Date: 
Resolution: 
Issue Date: 2021-12-01
Issue: Sample information for each subsample was not published in `swc_fieldData_pub` for data included in the 2021 NEON data release.
       Date Range: 2012-01-01 to 2019-12-31
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2021-12-01
Resolution: Sample ID, barcode, and sample condition for each subsample collected and reported in `swc_externalLabDataByAnalyte_pub` is now published in `swc_fieldData_pub` for all previously-released data.
Issue Date: 2021-12-01
Issue: Analysis of particulate carbon and nitrogen concentrations was halted due to the external laboratory instrument failure. The proposed timeline for returning the analysis to normal operations did not did not meet NEON’s desired timeline for data return. Therefore, NEON moved the analysis of particulate carbon and nitrogen to a different laboratory.
       Date Range: 2012-01-01 to 2021-10-20
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2021-08-18
Resolution: Total particulate carbon and nitrogen analysis was transferred to another contracted external laboratory. Users can identify the change in external laboratory when the `swc_externalLabDataByAnalyte_pub:laboratoryName` field stops being populated as ‘EcoCore_CSU’ for the TPC and TPN analytes. NEON is now publishing particulate carbon and nitrogen concentration data from the ‘SIRFER Lab at University of Utah’. These data are also published in the Stable isotopes in surface water (DP1.20206.001) data product. For all previously released data, the particulate carbon and nitrogen data from SIRFER are now also published in the Chemical properties of surface water data product to provide a continuous data series. All particulate carbon and nitrogen data analyzed by EcoCore has been converted from units of ‘milligrams’ to ‘microgramsPerLiter’ to align with the units returned by SIRFER. Users should note that for a single parent sample, total particulate carbon (TPC) and nitrogen (TPN) may be published from both EcoCore and SIRFER. Inherent heterogeneity in stream water particulates from different samples and differences in sample collection, handling, shipment, and processing may cause differences in concentrations returned from the two labs for samples with the same parentSampleID.
Issue Date: 2021-12-03
Issue: Based on a retrospective analysis, many NEON water chemistry samples were below the limit of detection specified by the NEON scope of work due to oligotrophic conditions at many sites. Therefore, a new sample collection and laboratory analysis procedure was put in place. Starting the summer of 2019, samples for nitrate plus nitrate as nitrogen, nitrate as nitrogen, ammonium as nitrogen, orthophosphate, total phosphorus, and total dissolved phosphorus are collected, filtered (for nitrate plus nitrate as nitrogen, nitrate as nitrogen, ammonium as nitrogen, and orthophosphate), and both raw and filtered samples are frozen for preservation. Samples are then shipped to the laboratory frozen, stored frozen, thawed, and analyzed within 48 hours using methods with lower detection limits (> 90 % of NEON water chemistry samples are expected to be above these lower detection limits). Shipments of the frozen samples began 6/1/2021 and will proceed with approximately quarterly shipments in the future.
       Date Range: 2012-01-01 to 2019-07-01
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2021-06-01
Resolution: Nitrate plus nitrate as nitrogen, nitrate as nitrogen, ammonium as nitrogen, orthophosphate, total phosphorus, and total dissolved phosphorus are now being analyzed from frozen filtered and raw samples collected at the same time as other samples for water chemistry analysis.
Issue Date: 2021-12-03
Issue: Based on community and aquatic technical working group (ATWG) feedback the external lab will no longer be reporting total suspended solids (TSS) dry mass, TSS normalized to filtered water volume (mg/L) will continue to be reported and published. Specific conductance and pH will also no longer be reported by the laboratory as of 3/1/2021. For specific conductance and pH measurements, please see the `swc_fieldSuperParent` table of Chemical properties of surface water (DP1.20093.001) for specific conductance measurements made with a handheld meter at the time of grab sample collection, `swc_domainLabData` of Chemical properties of surface water (DP1.20093.001) for initialSamplepH collected prior to lab titrations, or NEON Water quality (DP1.20288.01) sensor data, which contains specific conductance and pH along with other parameters. For sensor data, horizontal (HOR) index 102 is sensor set 2 (downstream) in streams and HOR index 103 is the buoy in lakes and rivers, which are the closest sensor locations to the grab sample collection location.
       Date Range: 2012-01-01 to 2021-03-01
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2021-03-01
Resolution: External laboratory data will no longer contain the following analytes: TSS - Dry Mass, specificConductance, and pH
Issue Date: 2020-09-10
Issue: External lab data for surface water and groundwater chemistry has been transformed from wide format to long format. The wide format table (`swc_externalLabData`) has been deprecated, and replaced with this table: `swc_externalLabDataByAnalyte`.
       Date Range: 2020-04-01 to 2020-09-10
       Location(s) Affected: All aquatic sites
Resolution Date: 2020-04-01
Resolution: All previous data has been transferred to the table
Issue Date: 2020-10-28
Issue: ANC samples only taken monthly
       Date Range: 2012-01-01 to 2018-01-01
       Location(s) Affected: All Aquatic Sites
Resolution Date: 2018-01-01
Resolution: Field sampling discontinued at seepage lake inlet and outlet locations
Issue Date: 2020-10-28
Issue: Field sampling discontinued at seepage lake inlet and outlet locations
       Date Range: 2012-01-01 to 2018-01-01
       Location(s) Affected: BARC, SUGG, CRAM, LIRO, PRLA, PRPO
Resolution Date: 2018-01-01
Resolution: Field sampling discontinued at seepage lake inlet and outlet locations New change log entry for surface water chemistry data products.
ADDITIONAL INFORMATION
----------------------
The protocol dictates that each siteID x stationID combination is sampled at least once per event (one record expected per parentSampleID in `swc_fieldSuperParent`).  A record from `swc_fieldSuperParent` may have zero to three child records in `swc_fieldData`, depending on whether a water sample was collected. In the event that a water sample cannot be taken, a record will still be created in `swc_fieldSuperParent`, and `swc_fieldSuperParent.samplingImpractical` will be something other than NULL, but there will be no corresponding record in `swc_fieldData`. Periodically, triplicate water samples are collected and three child records in `swc_fieldData` are expected for a single record in `swc_fieldSuperParent` Each record from `swc_fieldData` is expected to have at least one child record in `swc_domainLabData` (one for ALK and ANC, if sampled). Three times per year, duplicate ALK samples are analyzed and two ALK child records in `swc_domainLabData` are expected for a single record in `swc_fieldData`. Each record from `swc_fieldData` is also expected to have up to 33 child records in `swc_externalLabDataByAnalyte`, one per analyte. However, duplicates and/or missing data may exist where protocol and/or data entry aberrations have occurred; Users should check data carefully for anomalies before joining tables.
NEON DATA POLICY AND CITATION GUIDELINES
----------------------------------------
A citation statement is available in this data product's detail page at https://data.neonscience.org/data-products/DP1.20093.001. Please visit https://www.neonscience.org/data-policy for more information about NEON's data policy and citation guidelines.
DATA QUALITY AND VERSIONING
---------------------------
NEON data are initially published with a status of Provisional, in which updates to data and/or processing algorithms will occur on an as-needed basis, and query reproducibility cannot be guaranteed. Once data are published as part of a Data Release, they are no longer provisional, and are associated with a stable DOI. 
To learn more about provisional versus released data, please visit https://www.neonscience.org/data-revisions-releases.
