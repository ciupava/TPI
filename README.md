# TPI code

[![DOI](https://zenodo.org/badge/207566992.svg)](https://zenodo.org/badge/latestdoi/207566992)

In this repo you find the material we have generated to implement a TPI analysis.
Please do not hesitate to open an issue or contact us in case you think anything could be improved.

---

**Run it yourself!**
The materiale present in page is available with a more detailed description of the data sources, how to perform the data preparation and the process behind a TPI analysis at [this](https://ciupava.github.io/TPI/) GitHub book.

---


## Intro

**TPI** stands for **Target Population Index**. It is a tool developed to be used in several scientific disciplines to evaluate how particular segments (subgroups) of a population fare compared with the rest of the population. We refer to the subgroup of interest as the _target population_, that is the target of future policies for instance, hence the index's name.

A theoretical _worst-case_ and a theoretical _best-case_ situation are used as two distinct population-specific benchmarks, to determine the relative position of the subgroup of interest in relation to the rest of the population. This allows to characterise the situation of specific subgroups and compare them one to another, or to assess the same subgroup in different positions in time.

Please refer to the paper of **Pritchard et al.**<sup>[1](#paper_footnote)</sup> for more details about the methodology and the theoretical framework that underlines the TPI concept.

In the paper we discuss also an application of the index to transport studies, using 49 of the largest US metropolitan regions as a case study <sup>[2](#data_footnote)</sup>.
<br> In particular, the variable _job accessibility by public transport_ is assessed for two population subgroups that are especially likely to benefit from public transport: adults without a car and adults living in poverty.
<br> The analysis finds that the car-less population is comparatively better served by public transport than the poor population. Additionally, regions with a higher TPI score for car-less populations also tend to have public transport systems that better serve their population living in poverty. These results highlight that the proposed index allows for easy comparison across regions, despite large differences in the scale of the variable of interest (accessibility by public transport), the total population, and the size of the target groups (car-less or adults in poverty).


## Notes on the code
- (DatabaseCreation_groups.)[https://github.com/ciupava/TPI/blob/master/DatabaseCreation_Groups.R]
Tool to generate the database necessary to run the analysis. It combines US census data ad different scales (blocks to tracts) for ~50 metropolitan areas. Note: the data is locally stored and this file doesn't access them online, but sources are listed in this repository (see below).
- (TPI.R)[https://github.com/ciupava/TPI/blob/master/TPI.R]
Script to run the TPI analysis as in the analysis presented in the paper. Generates also complementary plots optionally.


## Data
All the data used in the analysis is open-source (...)

**TBC**



## References

This work was implemented at the Technion - Israel Institute of Technology under the guidance of prof. Karel Martens within the [Fair Transport Lab](https://karelm.net.technion.ac.il/)

<a name="paper_footnote">1</a>: Still under review.
J.P. Pritchard, A. Zanchetta, K. Martens, _A new index to assess the situation of subgroups, with an application to public transport disadvantage in US metropolitan areas_, 202?

<a name="data_footnote">2</a>: (Link?) and description of data sources

## TO_DO list:

- [ ] complete the Data section: link to data sources (check availability and compatibility with code... outdated?)
- [ ] explanation of used fields in the tables (why, meaning of the fields, ...)
- [ ] add workflow and what one should do to reproduce the same work (see [Issue #1](https://github.com/ciupava/TPI/issues/1))
- [ ] put example of pieces of table and how the data should look like for the code to run
- [ ] eliminate hardcoded variables
- [ ] check affiliation is correct
