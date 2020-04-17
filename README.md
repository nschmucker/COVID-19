# COVID-19 data analysis

This repository contains exploratory data analysis on U.S. cases of COVID-19, relying primarily on data from The New York Times, based on reports from state and local health agencies.

#### Introduction

The scale of the COVID-19 pandemic in the United States has grown rapidly since the first reported cases in Washington State in late January. As of mid-April, the U.S. has over 555,000 confirmed cases in over 1,500 counties, and over 22,000 confirmed deaths--more deaths than any other country in the world. This dashboard was created to help visualize the spread of the virus throughout the country. For more information about the sARS-CoV-2 virus and COVID-19 disease, the [CDC website](https://www.cdc.gov/coronavirus/2019-ncov/index.html) publishes up-to-date scientific information, guidance and data reports on the U.S. outbreak. 

#### Data sources

The New York Times aggregates data based on reports from state and local health agencies, and it has made the data public in a [GitHub repository](https://github.com/nytimes/covid-19-data). The COVID-19 data is for the Unites States only at the state and county level and begins with the first reported coronavirus case in Washington State on Jan. 21, 2020. For reporting purposes, all New York City counties have been consolidated to one reporting unit. Other exceptions are documents on the New York Times' GitHub page. The analyses in this repository exclude Alaska, Hawaii, and U.S. Territories. The New York Times updates its data regularly (as of mid-April, the update is daily around noon), and the code in this repository retrieves the latest New York Times data each time it is run. 

Population data comes from the [U.S. Census Bureau's](https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html) 2019 estimate of resident population for counties.

#### Definitions

* **Total cases:** Number of confirmed COVID-19 cases
* **Total deaths:** Number of confirmed deaths due to COVID-19
* **Total cases per 100k:** Number of confirmed COVID-19 cases per 100k residents
* **Total deaths per 100k:** Number of confirmed deaths due to COVID-19 per 100k residents
* **New cases:** Number of newly-confirmed COVID-19 cases
* **New deaths:** Number of newly-reported deaths due to COVID-19
* **Average new cases:** 7-day rolling average number of newly-confirmed COVID-19 cases (Null if fewer than 7 days)
* **Average new deaths:** 7-day rolling average number of newly-confirmed deaths due to COVID-19 (Null if fewer than 7 days)
* **Average infection rate:** Average new cases as a percent of total cases
* **Average death rate:** Average new deaths as a percent of total cases
* **Case fatality rate:** Total deaths as a percent of total cases

#### License
The New York Times has made their data publicly available for broad, noncommercial public use including by medical and public health researchers, policymakers, analysts and local news media.