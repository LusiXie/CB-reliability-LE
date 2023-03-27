# Analysis for Temporal Reliability of Contingent Behavior Trip Data in Kuhn-Tucker Recreation Demand Models (Xie and Adamowicz, Land Economics)

## Overview

This repository includes R code to conduct analyses in [Temporal Reliability of Contingent Behavior Trip Data in Kuhn-Tucker Recreation Demand Models](https://le.uwpress.org/content/early/2022/09/14/le.030521-0025R#:~:text=Unlike%20other%20stated%20preference%20methods%2C%20the%20temporal%20reliability,data%20collected%20over%20three%20years%20in%20KT%20models.). 

1. Survey data is not included. Datasets include the following variables (see Table 2 in the manuscript for descriptions):  

| Name of variable | Description | Variable type|
| ----------- | ----------- | ----------- |
| id | Id variable, one value for each respondent in each CB scenario | Integer|
| choice | Site variable, one value for each alternative site | Integer|
| trips | Number of hunting trips to each site | Integer|
| price | Travel costs (round trip), one value for each origin (postal code) and destination (site) combination | Continuous|
| income | Annual household income | Continuous|
| cwdrate | CWD prevalence rate | Continuous|
| ext | Indicator: extended season | Binary|
| oct | Indicator: October CB scenario in eligible sites for respondents who received the scenario | Binary|
| dec | Indicator: December CB scenario in eligible sites for respondents who received the scenario | Binary|
| tag | Indicator: Extra tag CB scenario in eligible sites for respondents who received the scenario | Binary|
| gift | Indicator: Gift card CB scenario in eligible sites for respondents who received the scenario | Binary|
| children | Indicator if respondents have children in the household | Binary|
| urban | Indicator if respondents live in the urban areas | Binary|
| college | Indicator if respondents have a college degree | Binary|
| yrshunt | Years of hunting experience | Continuous|
| asc | Alternative specific constants: one for each site | Binary|

2. Names of the code files indicate the order to replicate outputs prepared for Tables 2, 3, and 4, Figures 1, 2, and 3, as well as appendices. 

3. Estimation and welfare measurement is conducted using the R package rmdcev developed by Pat Lloyd-Smith. The package is available at https://github.com/plloydsmith/rmdcev. The github page includes a simulation example of parameter estimation and welfare measurement. 
