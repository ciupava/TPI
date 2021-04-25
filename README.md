# TPI_code

This folders contains the R scripts which are needed to run the TPI analysis.
Please refer to the paper of Pritchard et al. for more details.

List of scripts:
- *DatabaseCreation_groups.R*
Tool to generate the database necessary to run the analysis. It puts together US census data ad different scales (blocks to tracts) for ~50 metropolitan areas. Note: the data is locally stored and this file doesn't access them online.
- *TPI.R*
Script to run the TPI analysis as in the paper. Generates also complementary plots.