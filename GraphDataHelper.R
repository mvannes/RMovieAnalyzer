library(shiny)
library(dplyr)
library(ggplot2)
library(mongolite)

source("IMDBScraper.R")

get_start_date <- function () {
    return(1995)
}

get_end_date <- function() {
    return(2017)
}

select_data <- function(data_set, selection) {
    switch(
        selection,
        rating = {
            return(data_set$Rating)
        },
        votes = {
            return(data_set$Votes)
        },
        release_year = {
            return(data_set$ReleaseYear)
        },
        title_length = {
            return(nchar(as.character(data_set$Title)))
        }
    )
}

select_label <- function(selection) {
    switch(
        selection,
        rating = {
            return("Rating")
        },
        votes = {
            return("Amount of votes")
        },
        release_year = {
            return("Release year")
        },
        title_length = {
            return("Title length (chars)")
        }
    )
}

get_plot <- function(data_set, input, colour_on="ReleaseYear") {
    plot <- renderPlot({
        filtered_output <- filter(
            data_set,
            ReleaseYear >= input$year[1],
            ReleaseYear <= input$year[2]
        )
        ggplot(
            data = filtered_output,
            aes(
                x=select_data(filtered_output, input$x),
                y=select_data(filtered_output, input$y),
                colour= filtered_output[,colour_on]
            )
        ) + labs(x=select_label(input$x), y=select_label(input$y), colour=colour_on) +
            geom_point() +
            theme_classic()
    })
    return(plot)
}

get_text_output <- function (data_set, input, type) {
    text <- renderText({
        filtered_output <- filter(
            data_set,
            ReleaseYear >= input$year[1],
            ReleaseYear <= input$year[2]
        )
        switch(
            type,
            movie_count = {
                return(paste("Amount of movies: ", nrow(filtered_output)))
            },
            max_rating = {
                return(paste("Maximum rating: ", max(filtered_output$Rating)))
            },
            min_rating = {
                return(paste("Minimum rating: ", min(filtered_output$Rating)))
            },
            mean_rating = {
                return(paste("Mean rating: ", mean(filtered_output$Rating)))
            }
        )
    })
}

get_imdb_data <- function(force_refresh = FALSE) {
    if (!exists("scraped_stats") | force_refresh) {
        # Webscrape IMDB for imdb stats.
        scraped_stats <- scrape_imdb(get_start_date(), get_end_date())
        # Add a discriminator column to colourize when data is combined.
        scraped_stats <- mutate(scraped_stats, is_imdb = TRUE)
    }
    return(scraped_stats)
}

get_mongo_data <- function(force_refresh = FALSE) {
    if (!exists("mongo_stats") | force_refresh) {
        # Open connections to the collections
        database_name <- "test"
        movie_connection <- mongo(collection = "filtered_movies", db = database_name)
        # Query full dataframe from the prepared mongodb database of filtered stats
        mongo_stats <- movie_connection$find(query =
            paste(
                '{"ReleaseYear": { "$gte": ', get_start_date(), ', "$lte": ', get_end_date(), '}}'
            )
        )
        # Remove to ensure the connection to mongo is closed.
        rm(movie_connection)

        # Add a discriminator column to colourize when data is combined.
        # We intentionally don't add these to our database as this is obvious on a database level.
        mongo_stats <- mutate(mongo_stats, is_imdb = FALSE)
    }
    return(mongo_stats)
}
