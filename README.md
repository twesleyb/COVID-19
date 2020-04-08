# COVID-19 Data Repository
_originally forked from
[CSSEGISandData/COVID-19](https://github.com/CSSEGISandData/COVID-19)_  

## Description
Tidy COVID-19 case data available as an `R` package.  

## COVID in the United States
See the [README.md](./analysis/README.md) in `COVID-19/analysis` to see plots of 
COVID-19 cases in the United States.

## Usage
In R, download the `covid19` package from github:
```R
devtools::install_github("twesleyb/COVID-19")
```

Access the COVID-19 case data with `data()`:
```R
library(covid19)

# Load worldwide cases:
data(global_cases)

# Load COVID cases in the US:
data(united_states_cases)
```

## Data Sources

Data in this repository are pulled from the `CSSEGISandData/COVID-19` 
data repository. This repository is currated by the __Johns Hopkins 
University Center for Systems Science__ and __Engineering__ 
[(JHU CSSE)](https://systems.jhu.edu/).

The orignal authors reference the following data sources:  

* World Health Organization [(WHO)](https://www.who.int/)
* [COVID-19 Global Pandemic Real-time Report](http://3g.dxy.cn/newh5/view/pneumonia)
* [BNO News](https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/)
* National Health Commission of the People’s Republic of China [(NHC)](http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml)
* China CDC [(CCDC)](http://weekly.chinacdc.cn/news/TrackingtheEpidemic.html)
* [Hong Kong Department of Health](https://www.chp.gov.hk/en/features/102465.html)
* [Macau Government](https://www.ssm.gov.mo/portal/)
* [Taiwan CDC](https://sites.google.com/cdc.gov.tw/2019ncov/taiwan?authuser=0)
* [United States CDC](https://www.cdc.gov/coronavirus/2019-ncov/index.html)
* [Government of Canada](https://www.canada.ca/en/public-health/services/diseases/coronavirus.html)
* [Australia Government Department of Health](https://www.health.gov.au/news/coronavirus-update-at-a-glance)
* European Centre for Disease Prevention and Control [(ECDC)](https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases)
* Ministry of Health Singapore [(MOH)](https://www.moh.gov.sg/covid
* [Italy Ministry of Health](http://www.salute.gov.it/nuovocoronavi
* [1Point3Arces](https://coronavirus.1point3acres.com
* [WorldoMeters](https://www.worldometers.info/coronavir

## License

Per the authors' original terms of use, this GitHub repo and its contents herein, 
including all data, mapping, and analysis, copyright 2020 Johns Hopkins University, 
all rights reserved, is provided to the public strictly for educational and academic 
research purposes.  The Website relies upon publicly available data from multiple sources, 
that do not always agree. The Johns Hopkins University hereby disclaims any and all 
representations and warranties with respect to the Website, including accuracy, 
fitness for use, and merchantability.  Reliance on the Website for medical guidance 
or use of the Website in commerce is strictly prohibited.
