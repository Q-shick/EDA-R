# Explaratory Data Analysis with R
#### *by Kyoosik Kim*


### Object
This repository is a part of the coursework 'EDA with R' of Udacity. It is created for saving files and codes and for sharing ideas. For that reason, the best way to utilize this repository is to refer to the content, not to copy and paste it. Especially, the mini project series and final project are solely created and owned by the author who reserves all the rights.
  

### Contents
As Lesson 1 and 2 are introductory, they are not included in this repository. The rest of the lessons and relevant data are in each folder with the following files.
* Data: data sets that are used in the analysis
* Lesson: answers to the problem set, mini-project code
* Final Project: project code


### Final Project
The goal of the project is to explore two datasets; Heart Disease Mortality and Farmer's Markets, respectively. By researching the data and finding a pattern, the outcome of the project could be best used for healthcare fields potentially. The steps I took to conduct this project are largely as following.

1. Merge two datasets on counties in the US
2. Set the heart disease mortality as the dependent variable (by county, gender)
3. Count number of farmer's markets by county for quantative aspect
4. Assess each farmer's market size with number of items available for quality aspect
5. Find correlation between the independent variable and the farmer's market data extracted

Expressing the result in state level for summary, heart disease mortalities in most of the states are negatively correlated with the number of farmer's markets. This holds true for the size of farmer's markets, though it is less so. This means the mortality is lower where there are more farmer's market and the farmer's markets have wider range of items. For both aspects, women has stronger correlation than men.

One thing to note is that the farmer's market dataset is highly skewed, having small number of faremr's markets in each county. This  trait made me focus on observing the relationship of farmer's markets less than 10, which covers most of the observations. The final result is as the following plot.

![Final Result](https://github.com/Q-shick/Explanatory-Data-Analysis/blob/master/final_project/heart_disease_mortaltiy_and_farmers_market_files/figure-markdown_github/unnamed-chunk-38-1.png)
