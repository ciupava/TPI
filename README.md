# TPI_code

In this repo you can find the material we have generated to implement a TPI analysis.

## Intro

**TPI** stands for **Target Population Index**. It is a tool developed that can be used in several scientific disciplines in order to evaluate how particular segments of a population fare, compared to the rest of the population. We refer to the subgroup of interest as the _target population_, hence the index's name.

A theoretical _worst-case_ and a theoretical _best-case_ situation are used as two distinct population-specific benchmarks, to determine the relative position of the subgroup of interest in relation to the rest of hte population. This allows to characterise the situation of specific subgroups and compare them both in time and size.

Please refer to the paper of **Pritchard et al.**<sup>[1](#paper_footnote)</sup> for more details.

In the paper we discuss also an application of the index to transport studies, using 49 of the largest US metropolitan regions as a case study <sup>[2](#data_footnote)</sup>.
<br> In particular, the variable job accessibility by public transport is assessed for two population subgroups especially likely to benefit from public
transport: adults without a car and adults living in poverty.
<br>The analysis finds that the car-less population is comparatively better served by public transport than the poor population. Additionally, regions with a higher TPI score for car-less populations also tend to have public transport systems that better serve their population living in poverty. These results highlight that the proposed index allows for easy comparison across regions, despite large differences in the scale of the variable of interest (accessibility by public transport), the total population, and the size of the target groups (car-less or adults in poverty).


## Notes on the code:
- *DatabaseCreation_groups.R*
Tool to generate the database necessary to run the analysis. It combines US census data ad different scales (blocks to tracts) for ~50 metropolitan areas. Note: the data is locally stored and this file doesn't access them online, but sources are listed in this repository.
- *TPI.R*
Script to run the TPI analysis as it was done in the analysis presented in the paper. Generates also complementary plots.

<a name="paper_footnote">1</a>: Still under review.
J.P. Pritchard, A. Zanchetta, K. Martens, _A new index to assess the situation of subgroups, with an application to public transport disadvantage in US metropolitan areas_, 202?

<a name="data_footnote">2</a>: (Link?) and description of data sources
