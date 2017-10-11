library(rvest)
library(magrittr)

scrape_imdb <- function(start_date, end_date) {

    # Base url containing "DATE_HERE" placeholder tokens.
    base_url <- paste(
        'http://www.imdb.com/search/title?count=100&release_date=',
        'DATE_HERE,DATE_HERE&title_type=feature&sort=boxoffice_gross_us,desc',
        sep=""
    )

    release_years <- seq(start_date, end_date, 1)
    # Empty data frame that will hold all of our results.
    imdb <- data.frame()

    # For every year between the start- and end date we scrape the top 100 movies sorted
    # by gross earnings.
    # We look for the fields:
    # - Movie title
    # - Amount of votes
    # - Movie genres (multiple if applicable)
    for(year in release_years) {
        url <- gsub("DATE_HERE", year, base_url)
        page <- read_html(url)

        # Get page movie titles
        page_titles <- html_nodes(page, '.lister-item-header a') %>%
            html_text()

        # Get page votes
        page_votes <- html_nodes(page,'.sort-num_votes-visible span:nth-child(2)') %>%
            html_text() %>%
            gsub("\\,", "", .) %>%
            as.numeric()

        # Get page movie ratings
        page_ratings <- html_nodes(page, '.ratings-imdb-rating strong') %>%
            html_text() %>%
            as.numeric()

        # Get page movie genres
        page_genres <- html_nodes(page, '.genre') %>%
            html_text() %>%
            gsub("\n", "", .) %>%
            strsplit(., ", ")

        df <- data.frame(
            Title = page_titles,
            Rating = page_ratings,
            ReleaseYear = year,
            Votes = page_votes
        )
        df$Genre <- page_genres
        imdb <- rbind(imdb, df)
    }
    return(imdb)
}
