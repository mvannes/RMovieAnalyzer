library(dplyr)
library(shiny)
library(ggplot2)
library(magrittr)
library(mongolite)

source("IMDBScraper.R")
source("helper.R")
source("GraphDataHelper.R")

# Webscrape IMDB for imdb stats.
scraped_stats <<- scrape_imdb(get_start_date(), get_end_date())

# Open connections to the collections
database_name <- "test"
movie_connection <- mongo(collection = "filtered_movies", db = database_name)
# Query full dataframe from the prepared mongodb database of filtered stats
mongo_stats <<- movie_connection$find(query =
    paste(
        '{"ReleaseYear": { "$gte": ', get_start_date(), ', "$lte": ', get_end_date(), '}}'
    )
)
# Remove to ensure the connection to mongo is closed.
rm(movie_connection)

# Add a discriminator column to colorize when data is combined.
# We intentionally don't add these to our database as this is obvious on a database level.
mongo_stats <<- data.frame(is_imdb = FALSE, mongo_stats)
scraped_stats <<- data.frame(is_imdb= TRUE, scraped_stats)

full_stats <<- rbind(scraped_stats, mongo_stats)

# Define server logic required to draw a scatterplot!
shinyServer(function(input, output) {
    output$combinedPlot <- renderPlot({
        filtered_output <- filter(
            full_stats,
            ReleaseYear >= input$year[1],
            ReleaseYear <= input$year[2]
        )
        qplot(
            x=select_data(filtered_output, input$x),
            y=select_data(filtered_output, input$y),
            xlab=names(input$x),
            ylab=names(input$y),
            color=filtered_output$is_imdb
        ) + theme_classic()
    })
    output$imdbPlot <- renderPlot({
        filtered_output <- filter(
            scraped_stats,
            ReleaseYear >= input$year[1],
            ReleaseYear <= input$year[2]
        )
        qplot(
            x=select_data(filtered_output, input$x),
            y=select_data(filtered_output, input$y),
            xlab=input$x,
            ylab=input$y,
            color=filtered_output$ReleaseYear
        ) + theme_classic()
    })
    output$mongoPlot <- renderPlot({
        filtered_output <- filter(
            mongo_stats,
            ReleaseYear >= input$year[1],
            ReleaseYear <= input$year[2]
        )
        qplot(
            x=select_data(filtered_output, input$x),
            y=select_data(filtered_output, input$y),
            xlab=input$x,
            ylab=input$y,
            color=filtered_output$ReleaseYear
        ) + theme_classic()
    })
})
