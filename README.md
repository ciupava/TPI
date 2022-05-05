# TPI_code

**TPI** stands for **Target Population Index** . It is a tool developed to evaluate how particular segments of society fare, compared to a larger population. We refer to the subgroup of interest as the _target population_, hence the index's name.

The index uses two distinct population-specific benchmarks to determine the relative position of the subgroup: a theoretical _worst-case_ and a theoretical _best-case_ situation.

In this repo you can find the R scripts which are needed to run a TPI analysis.



Please refer to the paper of **Pritchard et al.**<sup>[1](#paper_footnote)</sup> for more details.

In this paper we discuss also an application of the index to transport, using 49 of the largest US metropolitan regions as case studies. Job accessibility by
public transport is assessed for two population subgroups especially likely to benefit from public
transport: adults without a car and adults living in poverty. It is found that the car-less population
is comparatively better served by public transport than the poor population. Additionally, regions
with a higher TPI score for car-less populations also tend to have public transport systems that
better serve their population living in poverty. These results highlight that the proposed index
allows for easy comparison across regions, despite large differences in the scale of the variable of
interest (accessibility by public transport), the total population, and the size of the target groups
(car-less or adults in poverty)

List of scripts:
- *DatabaseCreation_groups.R*
Tool to generate the database necessary to run the analysis. It puts together US census data ad different scales (blocks to tracts) for ~50 metropolitan areas. Note: the data is locally stored and this file doesn't access them online.
- *TPI.R*
Script to run the TPI analysis as in the paper. Generates also complementary plots.





<a name="paper_footnote">1</a>: Still under review.
*A new index to assess the situation of subgroups, with an application to public transport disadvantage in US metropolitan areas *