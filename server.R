library(shiny)

source("GraphDataHelper.R")

scraped_stats <<- get_imdb_data(FALSE)
mongo_stats <<- get_mongo_data(FALSE)

full_stats <<- rbind(scraped_stats, mongo_stats)

# Define server logic required to draw a scatterplot!
shinyServer(function(input, output) {
    output$combinedPlot <- get_plot(full_stats, input, colour_on = "is_imdb")
    output$combinedMovies <- get_text_output(full_stats, input, "movie_count")
    output$combinedMaxRating <- get_text_output(full_stats, input, "max_rating")
    output$combinedMinRating <- get_text_output(full_stats, input, "min_rating")
    output$combinedMeanRating <- get_text_output(full_stats, input, "mean_rating")

    output$imdbPlot <- get_plot(scraped_stats, input)
    output$imdbMovies <- get_text_output(scraped_stats, input, "movie_count")
    output$imdbMaxRating <- get_text_output(scraped_stats, input, "max_rating")
    output$imdbMinRating <- get_text_output(scraped_stats, input, "min_rating")
    output$imdbMeanRating <- get_text_output(scraped_stats, input, "mean_rating")

    output$mongoPlot <- get_plot(mongo_stats, input)
    output$mongoMovies <- get_text_output(mongo_stats, input, "movie_count")
    output$mongoMaxRating <- get_text_output(mongo_stats, input, "max_rating")
    output$mongoMinRating <- get_text_output(mongo_stats, input, "min_rating")
    output$mongoMeanRating <- get_text_output(mongo_stats, input, "mean_rating")
})
