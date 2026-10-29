# ST-558-Project-2

## Fertility Clinics in the United States 2020 - 2022

This repository creates an app about U.S. fertility clinics using R and Shiny.

The app can be accessed here:
[https://esurrell.shinyapps.io/st-558-project-2/](https://esurrell.shinyapps.io/st-558-project-2/)

### About the App

Use this app to analyze and download cleaned CDC data about Assisted Reproductive Technology (ART). This data provides information about fertility clinics across the United States in the years 2020-2022. Links to each year of the raw data are provided below:

- [2020 data](https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2020-Final-Assisted-Reproductive-Technology-ART-Se/92ri-yjps/about_data)
- [2021 data](https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2021-Final-Assisted-Reproductive-Technology-ART-Se/ui6g-vumy/about_data)
- [2022 data](https://data.cdc.gov/Assisted-Reproductive-Technology-ART-/2022-Final-Assisted-Reproductive-Technology-ART-Se/ix4g-rt8v/about_data)

This data has been cleaned and does not look exactly like its raw form. Some data cleaning that has been performed includes combining the three years of data into a single data set, removing summary rows, variable formatting, and calculating ratios. The final data set contains one row of data per fertility clinic per year.

To filter the data, use the panel to the left to make your selections and then click the "Apply Filters" button. The data will not filter until you click this button.

To download data, set your filters and then proceed to the "Data Download" tab.

Summary tables and plots are available on the "Data Exploration" tab. This includes:

- One and two-way contingency tables, which provide counts of clinics and data records by categories of your choosing;
- Numeric Data Summaries, which provide statistical measures of a numerical variable of your choosing by a categorical variable of your choosing; and
- Plots. There are 6 types of plots available, and each of them allows you to select your variable(s) of interest.

![Image of Logo of Data.CDC.gov website](data/Logo.jpg)
