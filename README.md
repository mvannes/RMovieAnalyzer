# RMovieAnalyzer
## What is this?
A school project for the HvA course Data Storage and Processing. 
The purpose of this project is to combine information from two dataset and display this data in cool ways.
## What does it contain?
For our datasets I have chosen to webscrape IMDB for the first dataset. This webscraping was done using the [rvest](https://github.com/hadley/rvest) library.
The other dataset consists of the latest movie information csv dataset found at [GroupLens](https://grouplens.org/datasets/movielens/)
From the latter dataset we take the movies.csv and ratings.csv files and import each into their own MongoDB collection using
mongoimport. 

This mongodb data is then parsed using the MongoSetup.R script. Note that this will not create a new mongo collection for you. 
This is a process that was intentionally left as a manual operation as the creation of large collections is something worth thinking about.

IMDB data is webscraped using the IMDBScraper.R script, and is provided to the main server.R script using the `scrape_imdb` function.

The ui.R and server.R are part of RShiny and allow for the rendering of interactive R plots.

Two files containing helper functions are also included in the repo: 
1. helper.R, which provides generic helper functions
2. GraphDataHelper.R, which provides our plots with correct data depending on the user input.

## What libraries does it use?
The RShiny app in this repo makes use of the following libraries:
1.  [rvest](https://github.com/hadley/rvest) - For webscraping
2. [magritrr[(https://github.com/tidyverse/magrittr) - To allow for method piping
3. [mongolite](https://github.com/jeroen/mongolite) - To handle MongoDB connections
4. [dplyr](https://github.com/tidyverse/dplyr) - For data processing
5. [stringr](https://github.com/tidyverse/stringr) - For string processing / better regexp matching
6. [shiny](https://github.com/rstudio/shiny) - For data visualization
7. [ggplot2](https://github.com/tidyverse/ggplot2) - For much cooler plots.
