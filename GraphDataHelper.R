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
