library(magrittr)
library(mongolite)
library(stringr)
library(dplyr)

# Responsible for reading two mongodb collections imported through CLI from
# csv files. Saves them to the workspace so manual operations can be done.
# Recommended manual operations:
# filtered_db <- mongo(collection = "filtered_movies", db = database_name)
# filtered_db$insert(filtered_movies)
# This will ensure a new collection named filtered_movies is created using the filtered movies

# Default cli mongo service database
database_name <- "test"

# Open connections to the collections
movies <- mongo(collection = "movies", db = database_name)
ratings <- mongo(collection = "ratings", db = database_name)

# Query the smallest possible subset
all_movies <- movies$find(
    fields = '{"movieId": true, "genres": true, "title": true, "_id": false}',
    limit = 900000
)
# Query a grouped by aggregation of all ratings.
# Grouped by the movie id, the sum amount of votes, and average rating.
all_ratings <- ratings$aggregate(
    '[
        {"$group":{"_id":"$movieId", "votes": {"$sum":1}, "average_rating":{"$avg":"$rating"}}}
    ]',
    options = '{"allowDiskUse":true}'
)

filtered_movies <- data.frame()

for(i in 1:nrow(all_movies)) {
    movie <- all_movies[i,]
    # At least one row is corrupted, we throw these out.
    if (movie$movieId == 'movieId') {
        next
    }

     # Separate the title and year into two fields.
    regexp_pattern <- ' \\([0-9]*\\){1}'
    year <- str_match(movie$title, regexp_pattern)[1] %>% # Always want the first match.
        gsub(' \\(', '', .) %>%
        gsub('\\)', '', .) %>%
        as.numeric()

    # If we don't have a year, we remove it from our dataset as comparing by year is hard
    # when you don't have a year to compare to.
    if(is.na(year)) {
        next
    }
    title <- gsub(regexp_pattern, '', movie$title)

    # Parse genres to only use the first one.
    genres <- strsplit(movie$genres, "\\|")

    # Parse rating info
    movie_ratings <- filter(all_ratings, (all_ratings["_id"] == movie$movieId))

    votes <- movie_ratings$votes
    # Account for difference in rating system by multiplying by 2 and rounding the rating.
    rating <- round(movie_ratings$average_rating * 2, 2)

    # If there are no votes / ratings, we exclude it from the filered result as that leaves us
    # little to compare with.
    if(length(votes) == 0 | length(rating) == 0) {
        next
    }

    parsed_movie <- data.frame(
        Title = title,
        Rating = rating,
        ReleaseYear = year,
        Votes = votes
    )
    parsed_movie$Genre <- genres
    filtered_movies <- rbind(filtered_movies, parsed_movie)
}
# Remove these connections to ensure they are closed.
rm(movies)
rm(ratings)
