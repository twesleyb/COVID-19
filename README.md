# COVID-19 Data Repository
_Global _COVID-19_ case data pulled from [CSSEGISandData/COVID-19](https://github.com/CSSEGISandData/COVID-19), 
tidied, and available as an_ `R` _package._

See the [README](./analysis/README.md) in `analysis/` for plots of COVID-19 cases in all US states and territories.

For example, a plot of COVID-19 cases in __North Carolina__:
<p align="center">
<img src="./figs/US-States/US_North_Carolina.png" height="500" />
</p>

## Usage
In R, download the `covid19` package from github: 
```R
devtools::install_github("twesleyb/COVID-19")

# Load the covid19 library:
library(covid19)

# Load worldwide cases:
data(global_cases)

# Load COVID cases in the US:
data(united_states_cases)
```

## Data Sources

Data in this repository are from the Center for Systems Science and Engineering at Johns Hopkins University.
The data is currated by John Hopkin's University Center for Systems Science and Engineering.

The orignal authors reference the following data sources:  

> This list includes a complete list of all sources ever used in the data set, since January 21, 2010. 
> Some sources listed here (e.g., WHO, ECDC, US CDC, BNO News) are not currently relied upon as a source of data.

- Aggregated data sources:
  - World Health Organization (WHO): https://www.who.int/
  - European Centre for Disease Prevention and Control (ECDC): https://www.ecdc.europa.eu/en/geographical-distribution-2019-ncov-cases 
  - DXY.cn. Pneumonia. 2020. http://3g.dxy.cn/newh5/view/pneumonia
  - US CDC: https://www.cdc.gov/coronavirus/2019-ncov/index.html
  - BNO News: https://bnonews.com/index.php/2020/02/the-latest-coronavirus-cases/
  - WorldoMeters: https://www.worldometers.info/coronavirus/  
  - 1Point3Arces: https://coronavirus.1point3acres.com/en  
  - COVID Tracking Project: https://covidtracking.com/data. (US Testing and Hospitalization Data. We use the maximum reported value from "Currently" and "Cumulative" Hospitalized for our hospitalization number reported for each state.)
  - Los Angeles Times: https://www.latimes.com/projects/california-coronavirus-cases-tracking-outbreak/
  - The Mercury News: https://www.mercurynews.com/tag/coronavirus/

- US data sources at the state (Admin1) or county/city (Admin2) level:  
  - Washington State Department of Health: https://www.doh.wa.gov/emergencies/coronavirus
  - Maryland Department of Health: https://coronavirus.maryland.gov/
  - New York State Department of Health: https://health.data.ny.gov/Health/New-York-State-Statewide-COVID-19-Testing/xdss-u53e/data
  - NYC Department of Health and Mental Hygiene: https://www1.nyc.gov/site/doh/covid/covid-19-data.page and https://github.com/nychealth/coronavirus-data
  - Florida Department of Health Dashboard: https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/arcgis/rest/services/Florida_COVID19_Cases/FeatureServer/0
    and https://fdoh.maps.arcgis.com/apps/opsdashboard/index.html#/8d0de33f260d444c852a615dc7837c86
  - Colorado: https://covid19.colorado.gov/covid-19-data
  - Virginia: https://www.vdh.virginia.gov/coronavirus/
  - Northern Mariana Islands CNMI Department of Public Health: https://cnmichcc.maps.arcgis.com/apps/opsdashboard/index.html#/4061b674fc964efe84f7774b7979d2b5
  - Missouri Department of Health: https://www.arcgis.com/apps/MapSeries/index.html?appid=8e01a5d8d8bd4b4f85add006f9e14a9d
  - St. Louis City Department of Health: https://www.stlouis-mo.gov/covid-19/data/#totalsByDate
  - St. Louis County: https://stlcorona.com/resources/covid-19-statistics1/
  - Massachusetts: https://www.mass.gov/info-details/covid-19-response-reporting
  - Michigan: https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html 
  - Illinois Department of Public Health: https://dph.illinois.gov/covid19
  - Indiana State Department of Health: https://hub.mph.in.gov/dataset?q=COVID
  - Connecticut Department of Public Health: https://data.ct.gov/stories/s/COVID-19-data/wa3g-tfvc/
  - Ohio Department of Health: https://coronavirus.ohio.gov/wps/portal/gov/covid-19/home
  - Oregon Office of Emergency Management: https://experience.arcgis.com/experience/fff9f83827c5461583cd014fdf4587de
  - Tennessee Department of Health: https://www.tn.gov/health/cedep/ncov.html
  - Rhode Island Department of Health: https://ri-department-of-health-covid-19-data-rihealth.hub.arcgis.com/
  - Wisconsin Department of Health Services: https://www.dhs.wisconsin.gov/covid-19/data.htm
  - North Carolina City of Greenville GIS: https://www.arcgis.com/apps/opsdashboard/index.html#/7aeac695cafa4065ba1505b1cfa72747
  - Iowa State Government: https://coronavirus.iowa.gov/
  - Minnesota Department of Health: https://www.health.state.mn.us/diseases/coronavirus/situation.html
  - Alabama Samford University's Department of Geography and Sociology: https://experience.arcgis.com/experience/e03f87e48a234feebbad27d0ee7ff824
  - Mississippi State Department of Health: https://msdh.ms.gov/msdhsite/_static/14,0,420.html
  - Nebraska Department of Health and Human Services: https://experience.arcgis.com/experience/ece0db09da4d4ca68252c3967aa1e9dd
  - South Carolina Department of Health and Environmental Control: https://scdhec.gov/infectious-diseases/viruses/coronavirus-disease-2019-covid-19/sc-testing-data-projections-covid-19
  - Nevada Department of Health and Human Services: https://nvhealthresponse.nv.gov/
  - New Jersey Department of Health: https://covid19.nj.gov/
  - Delaware Emergency Management Agency: https://coronavirus.delaware.gov/
  - Utah Department of Health: https://coronavirus-dashboard.utah.gov/#overview
  - Arizona Emergency Support Function (ESF)/Recovery Support Function (RSF) Operations Dashboard: https://www.arcgis.com/apps/MapSeries/index.html?appid=62e6bfa682a34e6aae9d9255ac865467
  - Departamento de Salud, Puerto Rico: https://bioseguridad.maps.arcgis.com/apps/opsdashboard/index.html#/d7308c1abb4747e584329adf1215125e
  - Arkansas Department of Health: https://experience.arcgis.com/experience/c2ef4a4fcbe5458fbf2e48a21e4fece9
  - Maine Department of Health and Human Services: https://www.maine.gov/dhhs/mecdc/infectious-disease/epi/airborne/coronavirus/data.shtml
  - Pennsylvania Department of Health: https://www.health.pa.gov/topics/disease/coronavirus/Pages/Cases.aspx
  - City of Philadelphia: https://www.phila.gov/programs/coronavirus-disease-2019-covid-19/
  - Lancaster County: https://covid-19-lancastercountypa.hub.arcgis.com/
  - Chester County: https://chesco.maps.arcgis.com/apps/opsdashboard/index.html#/975082d579454c3ca7877db0a44e61ca
  - Louisiana Department of Health: http://ldh.la.gov/coronavirus/
  - Government of The District of Columbia: https://coronavirus.dc.gov/page/coronavirus-data 
  - North Dakota Department of Health: https://www.health.nd.gov/diseases-conditions/coronavirus/north-dakota-coronavirus-cases
  - Oklahoma State Department of Health: https://looker-dashboards.ok.gov/embed/dashboards/44
  - Guam Department of Public Health and Social Services: http://dphss.guam.gov/covid-19/ 
  - New Mexico Department of Health: https://cvprovider.nmhealth.org/public-dashboard.html
  - Kentucky Department of Public Health: https://kygeonet.maps.arcgis.com/apps/opsdashboard/index.html#/543ac64bc40445918cf8bc34dc40e334
  - Georgia Department of Public Health: https://dph.georgia.gov/covid-19-daily-status-report
  - State of Hawai'i Department of Health: https://health.hawaii.gov/coronavirusdisease2019/what-you-should-know/current-situation-in-hawaii/
  - Reno County Health Department: http://reno.maps.arcgis.com/apps/opsdashboard/index.html#/dfaef27aede1414b89abf393b2ccb994
  - Texas Department of State Health Services: https://dshs.texas.gov/coronavirus/
  - Texas: City of San Antonio: https://covid19.sanantonio.gov/Home
  - Texas: Brazioria County: https://www.brazoriacountytx.gov/departments/health-department/brazoria-county-coronavirus-map
  - Texas: Brazos County Health District: http://www.brazoshealth.org
  - Texas: Cameron County Public Health: https://www.cameroncounty.us/publichealth/index.php/coronavirus/
  - Texas: Collin County: https://www.collincountytx.gov/healthcare_services/Pages/news.aspx
  - Texas: Dallas County: https://www.dallascounty.org/departments/dchhs/2019-novel-coronavirus.php
  - Texas: Denton County: https://gis-covid19-dentoncounty.hub.arcgis.com/pages/covid-19cases
  - Texas: Ector County:  http://www.co.ector.tx.us/page/ector.CoronavirusCOVID19
  - Texas: City of El Paso: http://epstrong.org/results.php
  - Texas: Fayette County: https://www.co.fayette.tx.us/
  - Texas: Fort Bend County Health & Human Services: https://www.fbchealth.org/ncov/
  - Texas: Galveston County Health District: https://www.gchd.org/about-us/news-and-events/coronavirus-disease-2019-covid-19/galveston-county-confirmed-covid-19-cases
  - Texas: Harris County: https://publichealth.harriscountytx.gov/Resources/2019-Novel-Coronavirus
  - Texas: Hays County: https://hayscountytx.com/covid-19-information-for-hays-county-residents/
  - Texas: Hidalgo County Health and Human Services: https://www.hidalgocounty.us/2630/Coronavirus-Updates
  - Texas: Midland County: https://www.midlandtexas.gov/955/Coronavirus-COVID-19
  - Texas: Montgomery County: https://coronavirus-response-moco.hub.arcgis.com/
  - Texas: City of Corpus Christi: https://www.cctexas.com/coronavirus
  - Texas: Amarillo Public Health Department: https://covid-data-amarillo.hub.arcgis.com/
  - Texas: Tarrant County Public Health: https://www.tarrantcounty.com/en/public-health/disease-control---prevention/coronaviruas.html
  - Texas: City of Mount Pleasant: https://www.mpcity.net/632/COVID-19-UPDATES
  - Texas: City of San Angelo: https://www.cosatx.us/departments-services/health-services/coronavirus-covid-19#ad-image-0
  - Texas: San Angelo Standard-Times: https://www.gosanangelo.com/
  - Texas: Travis County: https://www.traviscountytx.gov/news/2020/1945-novel-coronavirus-covid-19-information
  - Texas: City of Laredo: https://www.cityoflaredo.com/coronavirus/
  - Texas: Williamson County & Cities Health District: http://www.wcchd.org/COVID-19/dashboard.php
  - California Department of Publich Health: https://www.cdph.ca.gov/covid19
  - California: Mariposa County: https://www.mariposacounty.org/1592/COVID-19-Information
  - California: Alameda County Public Health Department: https://covid-19.acgov.org/
  - California: City of Fresno: https://www.fresno.gov/coronavirus
  - California: Humboldt County: https://humboldtgov.org/
  - California: Madera County: https://www.maderacounty.com/government/public-health/corona-virus-covid-19/covid-revised
  - California: Marin County Health & Human Services: https://coronavirus.marinhhs.org/
  - California: Orange County Health Care Agency: https://occovid19.ochealthinfo.com/coronavirus-in-oc
  - California: Placer County: https://www.placer.ca.gov/coronavirus
  - California: Riverside County: https://www.rivcoph.org/coronavirus
  - California: Sacramento County: https://www.saccounty.net/COVID-19/
  - California: San Francisco Department of Public Helath: https://www.sfdph.org/dph/alerts/coronavirus.asp
  - California: San Benito County Health & Human Services: https://hhsa.cosb.us/publichealth/communicable-disease/coronavirus/
  - California: San Joaquin County Public Health Services: http://www.sjcphs.org/coronavirus.aspx
  - California: San Mateo County: https://www.smchealth.org/coronavirus
  - California: Santa Cruz County Health Services Agency: http://www.santacruzhealth.org/HSAHome/HSADivisions/PublicHealth/CommunicableDiseaseControl/CoronavirusHome.aspx
  - California: Shasta County: https://www.co.shasta.ca.us/covid-19/overview
  - California: Solano County: https://www.co.shasta.ca.us/covid-19/overview
  - California: Sonoma County: https://socoemergency.org/emergency/novel-coronavirus/coronavirus-cases/
  - California: Stanislaus County Health Services Agency: http://schsa.org/publichealth/pages/corona-virus/
  - California: Ventura County: https://www.venturacountyrecovers.org/
  - California: Yolo County: https://www.yolocounty.org/health-human-services/adults/communicable-disease-investigation-and-control/novel-coronavirus-2019/
  - California: Los Angeles County: http://publichealth.lacounty.gov/media/coronavirus/
  - California: San Diego County: https://www.sandiegocounty.gov/coronavirus.html
  - California: Santa Clara County: https://www.sccgov.org/sites/covid19/
  - California: Imperial County Public Health Department: http://www.icphd.org/health-information-and-resources/healthy-facts/covid-19/
  - California: San Bernardino County: https://sbcovid19.com/
  - Montana Department of Public Health and Human Services: https://dphhs.mt.gov/publichealth/cdepi/diseases/coronavirusmt
  - South Dakota Department of Health: https://doh.sd.gov/news/coronavirus.aspx
  
- Non-US data sources at the country/region (Admin0) or state/province (Admin1) level:
  - National Health Commission of the People’s Republic of China (NHC):
    http://www.nhc.gov.cn/xcs/yqtb/list_gzbd.shtml
  - China CDC (CCDC): http://weekly.chinacdc.cn/news/TrackingtheEpidemic.htm
  - Hong Kong Department of Health: https://www.chp.gov.hk/en/features/102465.html
  - Macau Government: https://www.ssm.gov.mo/portal/
  - Taiwan CDC: https://sites.google.com/cdc.gov.tw/2019ncov/taiwan?authuser=0
  - Government of Canada: https://www.canada.ca/en/public-health/services/diseases/coronavirus.html
  - Australia Government Department of Health: https://www.health.gov.au/news/coronavirus-update-at-a-glance
  - COVID Live (Australia): https://www.covidlive.com.au/
  - Ministry of Health Singapore (MOH): https://www.moh.gov.sg/covid-19
  - Italy Ministry of Health: http://www.salute.gov.it/nuovocoronavirus
  - Dati COVID-19 Italia (Italy): https://github.com/pcm-dpc/COVID-19/tree/master/dati-regioni
  - French Government: https://dashboard.covid19.data.gouv.fr/ and https://github.com/opencovid19-fr/data/blob/master/dist/chiffres-cles.json
  - OpenCOVID19 France: https://github.com/opencovid19-fr
  - Palestine (West Bank and Gaza): https://corona.ps/details
  - Israel: https://govextra.gov.il/ministry-of-health/corona/corona-virus/
  - Ministry of Health, Republic of Kosovo: https://kosova.health/ and https://covidks.s3.amazonaws.com/data.json
  - Berliner Morgenpost (Germany): https://interaktiv.morgenpost.de/corona-virus-karte-infektionen-deutschland-weltweit/
  - rtve (Spain): https://www.rtve.es/noticias/20200514/mapa-del-coronavirus-espana/2004681.shtml
  - Ministry of Health, Republic of Serbia: https://covid19.rs/homepage-english/ 
  - Chile: https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/
  - Chile: https://www.gob.cl/coronavirus/cifrasoficiales/
  - Brazil Ministry of Health: https://covid.saude.gov.br/
  - Brazil: https://github.com/wcota/covid19br. Data described in [DOI: 10.1590/SciELOPreprints.362](https://doi.org/10.1590/SciELOPreprints.362)
  - Gobierono De Mexico:https://covid19.sinave.gob.mx/
  - Japan COVID-19 Coronavirus Tracker: https://covid19japan.com/#all-prefectures
  - Monitoreo del COVID-19 en Perú -  Policía Nacional del Perú (PNP) - Dirección de Inteligencia (DIRIN): https://www.arcgis.com/apps/opsdashboard/index.html#/f90a7a87af2548699d6e7bb72f5547c2 and Ministerio de Salud del Perú: https://covid19.minsa.gob.pe/sala_situacional.asp
  - Colombia National Institute of Health: http://www.ins.gov.co/Noticias/Paginas/Coronavirus.aspx
  - Russia: https://xn--80aesfpebagmfblc0a.xn--p1ai/information/
  - Ukraine: https://covid19.rnbo.gov.ua/
  - Public Health Agency of Sweden: https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa
  - India Ministry of Health and Family Welfare: https://www.mohfw.gov.in/
  - Government of Pakistan: http://covid.gov.pk/stats/pakistan
  - The UK Government: https://coronavirus.data.gov.uk/#category=nations&map=rate
  - Scottish Government: https://www.gov.scot/publications/coronavirus-covid-19-trends-in-daily-data/
  - Netherlands National Institute for Health and Environment: https://experience.arcgis.com/experience/ea064047519040469acb8da05c0f100d
  - Iceland Directorate of Health and Department of Civil Protection and Emergency Management: https://www.covid.is/data

## License

Per the authors' original terms of use, this GitHub repo and its contents herein, 
including all data, mapping, and analysis, copyright 2020 Johns Hopkins University, 
all rights reserved, is provided to the public strictly for educational and academic 
research purposes.  The Website relies upon publicly available data from multiple sources, 
that do not always agree. The Johns Hopkins University hereby disclaims any and all 
representations and warranties with respect to the Website, including accuracy, 
fitness for use, and merchantability.  Reliance on the Website for medical guidance 
or use of the Website in commerce is strictly prohibited.
