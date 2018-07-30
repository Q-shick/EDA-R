
Heart Disease Mortality And Farmer's Market
===========================================

##### <i>Kyoosik Kim<br>July 2018</i>

### Abstract

I explore the data sets, 'Heart Disease Mortality' and 'Farmer's Market In The US', to study the relationship between them. The goal of this project is to capture a possible link of farmer's markets to heart disease mortality.

### Introduction

The Center For Disease Control And Prevention provides the data set 'Heart Disease Mortality Data Among US Adults (35+) by State/County in 2014' which is downloadable at <a href='https://catalog.data.gov/dataset/heart-disease-mortality-data-among-us-adults-35-by-state-territory-and-county-5fb7c'>DATA.GOV</a>. This data set contains the numbers of deaths from <a href='http://www.heart.org/HEARTORG/Conditions/What-is-Cardiovascular-Disease_UCM_301852_Article.jsp#.Wz57p9JKjIU'>Cardiovascular Disease</a>, also simply called heart disease, every 100,000 population by gender and race in state/county level. Another data set 'Farmer's Market In The US' is procured from The United States Department of Agriculture at <a href='https://www.ams.usda.gov/services/local-regional/farmers-markets-and-direct-consumer-marketing'>USDA</a>. 8.7k+ Farmer's markets are listed with information of state, exact locations and items. Even though the farmer's market data is collected between 2011 and 2018, most of the data was updated in 2015 which is almost the same year of that of the heart disease data. Also considering the small variability of the data over time, therefore, these two data sets can be compared without any serious errors.

I will be studying these data to discover trends of which states have more or less heart disease mortality and whether the states have a relationship with the number of farmer's markets. The importance of these questions stem from the fact that the number one cause of death in the US is none other than heart diseases.

It has been talked a lot about what causes heart diseases such as diet habits as interests in organic/fresh foods have been growing fast. Although people now believe that bad diets could cause heart diseases thanks to researches and education, there lacks the reverse thoughts of reducing the risks with good diets.

First step to answer these questions is to explore the data sets individually. I will look into the data, one by one, to see how they are distributed and visualize on the US map to gain some ideas where heart diseases or farmer's markets are found the most and least.

Second, I will join the two data sets to find a pattern between the heart disease mortality and the number of farmer's markets. This will be shown on the map for better understanding. The anticipated result is that the mortality is found to be lower where there are many farmer's markets than where there are less.

------------------------------------------------------------------------

### Preparation

``` r
# load packages
library(ggplot2)
library(stringi)
library(readr)
```

##### Define Functions

``` r
# modify state names to fit a format
process_region <- function(df, state_list) {
  # modify county values
  df$county <- gsub("County", "", df$county)
  df$county <- stri_trans_totitle(df$county)
  df$county <- trimws(df$county)
  # modify the mainland states and DC
  df$state <- state.abb[match(df$state, state_list)]
  df$state[which(df$county == "District Of Columbia")] <- "DC"
  df$state <- factor(df$state)
  # drop the data that is out of criteria (ex. abnormal NA's, non-US locations)
  df <- df[complete.cases(df), ]
  # return the final data frame
  return(df)
}
```

##### Load Data Set - Heart Disease Mortality

``` r
# load heart disease mortality data
heart_mortality <- read.csv('../Data/Heart_Disease_Mortality_by_County.csv')
# select columns
heart_mortality <- subset(heart_mortality, GeographicLevel == 'County')
heart_mortality <- heart_mortality[, -c(1, 4:7, 9:13, 15, 17:18)]
heart_mortality <- heart_mortality[!is.na(heart_mortality$Data_Value), ]
# change column names
colnames(heart_mortality) <- c('state', 'county', 'mortality', 
                               'gender', 'race', 'location')
# process the regional columns
heart_mortality <- process_region(heart_mortality, state.abb)
# convert location to x, y coordinate format
loc_split <- strsplit(as.character(heart_mortality$location), split = " ")
heart_mortality$x <- parse_number(sapply(loc_split, '[', 1))
heart_mortality$y <- parse_number(sapply(loc_split, '[', 2))
# show structure
str(heart_mortality)
```

    ## 'data.frame':    31062 obs. of  8 variables:
    ##  $ state    : Factor w/ 51 levels "AK","AL","AR",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ county   : chr  "Aleutians East" "Aleutians West" "Anchorage" "Bethel" ...
    ##  $ mortality: num  105 212 258 352 306 ...
    ##  $ gender   : Factor w/ 3 levels "Female","Male",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ race     : Factor w/ 6 levels "American Indian and Alaskan Native",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ location : Factor w/ 3282 levels "","(-14.2766, -170.367)",..: 3256 3255 3271 3270 3275 3268 3278 3266 3263 3269 ...
    ##  $ x        : num  55.4 53 61.2 60.9 63.7 ...
    ##  $ y        : num  -162 -170 -149 -160 -150 ...

``` r
head(heart_mortality)
```

    ##   state         county mortality  gender    race                 location
    ## 1    AK Aleutians East     105.3 Overall Overall (55.440626, -161.962562)
    ## 2    AK Aleutians West     211.9 Overall Overall (52.995403, -170.251538)
    ## 3    AK      Anchorage     257.9 Overall Overall (61.159049, -149.103905)
    ## 4    AK         Bethel     351.6 Overall Overall (60.924483, -159.749655)
    ## 6    AK         Denali     305.5 Overall Overall (63.678399, -149.962076)
    ## 7    AK     Dillingham     411.6 Overall Overall (59.803151, -158.181608)
    ##          x         y
    ## 1 55.44063 -161.9626
    ## 2 52.99540 -170.2515
    ## 3 61.15905 -149.1039
    ## 4 60.92448 -159.7497
    ## 6 63.67840 -149.9621
    ## 7 59.80315 -158.1816

##### Load Data Set - Heart Disease Mortality

``` r
# load farmer's market data set
farmers_market <- read.csv('../Data/Farmers_Markets_by_County.csv')
# select columns
farmers_market <- farmers_market[, -c(1:9, 12:20, 23, 59)]
farmers_market <- farmers_market[, c(1:4, 11, 17:18, 24, 32:33)]
farmers_market <- farmers_market[farmers_market$County != "", ]
farmers_market <- farmers_market[!is.na(farmers_market$x), ]
# change column names
colnames(farmers_market)[1:2] <- c('county', 'state')
# process the regional columns
farmers_market <- process_region(farmers_market, state.name)
# show the structure
str(farmers_market)
```

    ## 'data.frame':    8174 obs. of  10 variables:
    ##  $ county    : chr  "Caledonia" "Cuyahoga" "Barton" "New York" ...
    ##  $ state     : Factor w/ 51 levels "AK","AL","AR",..: 47 36 25 35 43 35 9 8 8 35 ...
    ##  $ x         : num  -72.1 -81.7 -94.3 -73.9 -86.8 ...
    ##  $ y         : num  44.4 41.4 37.5 40.8 36.1 ...
    ##  $ Bakedgoods: Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 1 2 2 1 ...
    ##  $ Herbs     : Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 2 1 ...
    ##  $ Vegetables: Factor w/ 2 levels "N","Y": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ Nuts      : Factor w/ 2 levels "N","Y": 1 1 1 2 1 2 1 2 1 1 ...
    ##  $ Beans     : Factor w/ 2 levels "N","Y": 2 1 1 1 1 1 1 2 1 2 ...
    ##  $ Fruits    : Factor w/ 2 levels "N","Y": 2 2 2 1 2 2 2 2 2 2 ...

``` r
head(farmers_market)
```

    ##      county state         x        y Bakedgoods Herbs Vegetables Nuts
    ## 1 Caledonia    VT -72.14034 44.41104          Y     Y          Y    N
    ## 2  Cuyahoga    OH -81.73394 41.37480          Y     Y          Y    N
    ## 4    Barton    MO -94.27462 37.49563          Y     Y          Y    N
    ## 5  New York    NY -73.94930 40.79390          Y     Y          Y    Y
    ## 6  Davidson    TN -86.79071 36.11837          Y     Y          Y    N
    ## 7  New York    NY -73.94825 40.80895          Y     Y          Y    Y
    ##   Beans Fruits
    ## 1     Y      Y
    ## 2     N      Y
    ## 4     N      Y
    ## 5     N      N
    ## 6     N      Y
    ## 7     N      Y

Univariate Plots Section
========================

Univariate Analysis
===================

### What is the structure of your dataset?

### What is/are the main feature(s) of interest in your dataset?

### What other features in the dataset do you think will help support your investigation into your feature(s) of interest?

### Did you create any new variables from existing variables in the dataset?

### Of the features you investigated, were there any unusual distributions? Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?

Bivariate Plots Section
=======================

Bivariate Analysis
==================

### Talk about some of the relationships you observed in this part of the investigation. How did the feature(s) of interest vary with other features in the dataset?

### Did you observe any interesting relationships between the other features (not the main feature(s) of interest)?

### What was the strongest relationship you found?

Multivariate Plots Section
==========================

Multivariate Analysis
=====================

### Talk about some of the relationships you observed in this part of the investigation. Were there features that strengthened each other in terms of looking at your feature(s) of interest?

### Were there any interesting or surprising interactions between features?

### OPTIONAL: Did you create any models with your dataset? Discuss the strengths and limitations of your model.

------------------------------------------------------------------------

Final Plots and Summary
=======================

### Plot One

### Description One

### Plot Two

### Description Two

### Plot Three

### Description Three

------------------------------------------------------------------------

Reflection
==========
