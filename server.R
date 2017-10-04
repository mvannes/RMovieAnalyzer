library(shiny)

source("GraphDataHelper.R")

scraped_stats <<- get_imdb_data(FALSE)
mongo_stats <<- get_mongo_data(FALSE)

full_stats <<- rbind(scraped_stats, mongo_stats)

# Define server logic required to draw a scatterplot!
shinyServer(function(input, output) {
    output$combinedPlot <- get_plot(full_stats, input, colour_on = "is_imdb")
    output$imdbPlot <- get_plot(scraped_stats, input)
    output$mongoPlot <- get_plot(mongo_stats, input)
})
