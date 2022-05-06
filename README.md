# PPOL 670 | Final Project
#### Authors:
* [Ethan Rosenbaum](https://github.com/erosenbaum97)
* [Xinyu Zheng](https://github.com/XZXinyuZheng)
* [Beryl Nana Ama Akuffo-Kwapong](https://github.com/bnakwaps)

## Links to Github Pages
Code and Output: https://erosenbaum97.github.io/FinalProject/FinalProject.html

Supporting Essay: https://erosenbaum97.github.io/FinalProject/Essay.html

## Overview:
-----------------------------

#### Project Title

Climate Change in New York City: Examining Flooding and Demographics

##### Summary & Motivating Question
This project is on environmental justice and climate change resilience. Our project focuses on New York City, a coastal city vulnerable to floods and hurricanes due to its extensively used waterfronts and coast. It is estimated that by 2050, 659,000 people in New York will be at risk to coastal flooding due to rising sea levels, and this risk is expected to rise in the future with worsening climate change. Using Geospatial Analysis, API Pulls and Supervised Machine Learning, we explore the impact of flooding on various areas in the state of New York. We explore the levels of impact experienced based on factors such as race, income, age, educational attainment, and population density to see if disparities exist. We also identify and predict the areas most prone to flooding, and potential damages based on population data

##### The Project Goal:
The purpose of the project is to provide insight into the staggering impact of climate change, provide insight and visibility into the needs of populations at risk, and help inform policy.

##### Findings
We find that the most important factor with correlation to flooding is location. While this may seem intuitive, flooding in one area can have damaging effects in adjacent regions. From our analysis, we found key points of interest:
* 518 census tracts across the city of New York are at risk of flooding by 2100. While this is what the data projects, there is a likelihood the costs of impact will extend to other tracts due to negative externalities of flooding such as homelessness
* Potential Increase in Economic Inequality: While the income distribution of people living in floodplains are spread across various income groups, low income dwellers will be most burdened with flood prevention costs, relocation or evacuations costs. This could potentially widen the already existing income inequality in New York city
* High unemployment is best predictor of an area's flooding likelihood, so resilience projects should make sure to include those as stakeholders, since they might be at high risk

##### Recommendations
* Cities should use them to model both flood damage and examine what kinds of communities live in flood-exposed areas. Ensuring that these communities are protected and have means to escape floods is vital.

##### Policy Implications
- [ ] To be able to access and understand populations at the most risk of losing property and livelihood due to natural disasters such as flooding and hurricanes
- [ ] To inform policy on ways to anticipate, better target, and reduce the vulnerability of populations most at risk  

## Project Design
-----------------------------

##### Data Used
- [ ] [2050s 500-year Floodplain](https://data.cityofnewyork.us)
- [ ] [American Community Survey](https://www.census.gov/programs-surveys/acs/about.html)
- [ ] [USGS Flood Event Viewer](https://stn.wim.usgs.gov/fev/#Sandy)
- [ ] [Sandy Damage Estimates by Block Group](https://www.huduser.gov/maps/map_sandy_blockgroup.html)

##### Data Techniques and Approaches:
* Geospatial Analysis
* API Pulls
* Supervised Machine Learning

###### Technology Employed
- [ ] R Programming Technology

## Instructions for Replication
-----------------------------
- [ ] Download the RStudio on project device
- [ ] Create a  subfolder in the repository folder named `data` where all datasets used for the project will be kept
- [ ]  Install all packages using the code:
```
install.packages "packagename"
```
- [ ] Load in Packages with the first code chunk in the `FinalProject.RMD` file
- [ ] For the Census API pull, [sign up](https://api.census.gov/data/key_signup.html) for a unique key which will be store in a .txt file. The `.txt file` should be stored in the `data subfolder` as `credential`
- [ ] Run the code for replication

##### Description of files:
A  brief description of the files/directories in the repository
* The `README.md` file provides a brief overview of project and its policy implications
* The `FinalProject.Rmd` file contains all code chunks for the project
* The `FinalProject.html file` contains the knitted code chunks  and results of the project.
* The `.gitignore file` contains all the data and env that was used for the project


###### Project Limitations:
We anticipated several potential technical hurdles that may arise during the development of this project. The size of the shapefiles was occassionally taxing on our computers, but we were able to clean the read-in data enough to mitigate this issue. Due to time constraints, we were also unable to meaningfully impute missing values.

## Bibliography
-------------------------------
Hurricanes and Climate Change. Center for Climate and Energy Solutions. https://www.c2es.org/content/hurricanes-and-climate-change/

Maps of Sandy Inundation Zone. United States Department of Housing and Urban Development. Updated February 10, 2022. https://www.huduser.gov/maps/map_sandy_blockgroup.html)

Sandy Inundation Zone Data. NYC Open Data. Updated September 10, 2018. https://data.cityofnewyork.us/Environment/Sandy-Inundation-Zone/uyj8-7rv5

Sea Level Rise Maps (2050s 500-year Floodplain). NYC Open Data. Updated September 16, 2021. https://data.cityofnewyork.us/Environment/Sea-Level-Rise-Maps-2050s-500-year-Floodplain-/qwca-zqw3
