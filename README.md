# A comparison of GroupLens and IMDB data
__Authored by:__
Michael van Nes
500670754

# Research
## Datasets
### GroupLens / MovieLens
To start researching something we must first decide what it is that we intend to research. We must also find out where our data will come from. Our first dataset, which had it’s usage set as a requirement for this research, consists of movie data from [MovieLens](https://grouplens.org/datasets/movielens/), a part of the data aggregator GroupLens. For the purposes of having a dataset that is as expansive as possible, we decided to make use of the largeset dataset available. 
>Full: 26,000,000 ratings and 750,000 tag applications applied to 45,000 movies by 270,000 users. >Includes tag genome data with 12 million relevance scores across 1,100 tags. Last updated 8/2017.

This description seemed like it would give us more than sufficient data to use in forming conclusions. The MovieLens data came in the form of multiple `.csv` files. From these files we decided to make use of the following:
* `movies.csv`
For movie data including; Title, Release year, Genres, the movies's unique id in this dataset.
The contents of this file require quite a bit of processing, as the Title and Release year are held in the same column. More on that later however.
* `ratings.csv` 
This is by far the largest file in the dataset, where the entire uncompressed dataset is roughly ~1gb of data, this file alone is ~700 mb. It also contains what is by far the most interesting data. Namely this file contains every rating done for every movie. The fields that are of interest for our analysis are; The movie id (so we can link it with our other csv), the given rating. We are explicitly not making use of the user id that is also in this csv as we want to ensure that we are not processing any personal data.

### IMDB
For our second data set we made use of data webscraped from IMDB, for this we took the top 100 highest grossing movies for each year between 1995 and 2017. This webscraped data contained the Title, the release year, the amount of votes, and the movies genre(s). At the start it also contained the movie's runtime and gross earnings but as these were not available in the MovieLens dataset, it was decided that these should be excluded. An example of the a webscraped page can be found at `http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature&sort=boxoffice_gross_us,desc` 

## Research question
Research is not complete without a research question.
> When looking at the two datasets, IMDB and Grouplens, which shows a closer grouping of data  points when various pieces of information are set against eachother.

To analyze this we want to be able to do a couple of things. 
* Show a graph
Very obvious, but important nonetheless. We want to be able to show an interactive graph that allows the user to change what values are depicted on the x- and y-axis. Filtering based on the release year will also allow us to further enhance our results.
* Combine data from MovieLens and IMDB
We want to have the ability to show both graphs containing individual data sets, meaning separate graphs for IMDB and MovieLens data, as well as a graph of combined data. 

This sounds simple, and the difficulty will mostly come from the processing of our data. As this research is mostly intended to showcase some skill in R, the main factor for success is the "cool-factor" of our graphs.

# Processing
The following chapter will be about the processing of our datasets into something we can actually use to compare the two data sets. Looking at the data as we get it in it's most pure form, we have a couple of csv files and a couple of web pages. This makes comparisons hard. Of course we could manually draw the graphs that we need, but as we are programmers, and kind of lazy, we're not going to do that. The first step is to normalize the data, starting with the MovieLens data
## MovieLens
A requirement for this research was that one of the datasets must be actively taken from a real database connection. As the IMDB dataset already requires special parsing, the proper target for database connections is the MovieLens data.
### Choosing a database
First, a decisions must be made as to what database we will use. As we're making use of a lot of rough data that doesn't rely too much on interconnectivity, we will opt to make use of a MongoDB database. We start off by loading the completely unparsed data into two different mongo collections using CLI commands.
* `mongoimport -c ratings --type=csv --file=ratings.csv --fields=userId,movieId,rating,timestamp`
Which loads our ratings.csv into a local mongo collection called `ratings`
* `mongoimport -c movies --type=csv --file=movies.csv --fields=movieId,title,genres`
Which loads our movies.csv into local mongo collection called `movies`

### Problems with the data
With this initial setup done, we must now filter the data in these collections to get normalized data. There are a few problems we can identify with the dataset that need to be fixed.
1. The year and title are part of the same field
This is a problem because we want to be able to filter based on year, which is made considerably harder when the year is preceded by an arbitrarily large string of text.
2. The genres are a single string
If we want to make any comparison of genres, we must separate the genres from eachother, turning a single string into a vector. 
3. There is no average rating
All we have is a lot of unrelated ratings, this must be resolved so we can find a single averaged rating for each movie. 
4. The amount of votes is unknown
Similar to the previous issue, we do not know how many people voted on a movie, these also need to be grouped to ensure a comparison is possible.

### MongoSetup.R
Because this is a relatively large amount of data, and the parsing we need to do is heavier than simply copying and pasting the information from one place to another, cleaning this data will be done in a separate R script. The clean data can then be inserted into a new Mongo collection which will make initializing the data for our graphs much faster, as it won't have to be cleaned every run.

#### Step 1: Querying
We start off by forming database connections using the `mongolite` CRAN library. This will allow us to easily query the existing collections, as well as write to a new collection once our data has been filtered.
To establish active connections to our collection we use the following code:
``` R
movies <- mongo(collection = "movies", db = database_name) 
ratings <- mongo(collection = "ratings", db = database_name)
```
Then, we query everything we need from our collections. Querying once is better for performance reasons, a basic design goal is to keep the amount of database interactions to a minimum. 
Our first query limits the amount of fields we retrieve to ensure we only ,get the smallest subset of data we require for analysis. This could be done in R, but when working with a live database that might not be locally hosted it is better to send less data over the line. For a much larger data set even adding a single extra field will dramatically slow down the process of retrieving data.
``` R
all_movies <- movies$find(
    fields = '{"movieId": true, "genres": true, "title": true, "_id": false}'
)
```
Our second query deals with querying our ratings collection for information about votes and ratings, grouped by the movie id.
An option would be to handle the aggregation of data in R, but again, we look to send as little data as possible, and thus leave handling of the aggregation to our database. This is especially important here as this database contains over 700 mb worth of data, a lot of which is not interesting for our analysis.
The following query results in a simple list of movie id's, with their amount of votes and average ratings. 
``` R
all_ratings <- ratings$aggregate(
    '[
        {"$group":{"_id":"$movieId", "votes": {"$sum":1}, "average_rating":{"$avg":"$rating"}}}
    ]',
    options = '{"allowDiskUse":true}'
)
```

#### Step 2: Disregard conventions, do looping
R language conventions dictate that you should almost never have to loop. 
R language conventions dicatate that you should really be using apply functions instead of loops.
Performance tests indicate that looping is often more efficient than apply functions.
The main reason for using apply functions and abstracting bits of code into repeatable functions is because when you're running R scripts on a distributed platform, such as when using mapreduce functions, performance is much better. 
So for that reason, and because we are kind of lazy, and love ourselves some performance gains, this one time script will loop over our dataframe's rows.
The first thing we must do is confirm that our data is valid. Some of the invalid data that we can have occur is having a movie without a movie id. In this case the movie id will be `"movieId"`, which means that we can never associate real ratings to the movie. 
The solution we implement here is to simply skip over the row like so;
``` R
    if (movie$movieId == 'movieId') {
        next
    }
```
Solving our first problem, the fact that the title and release date are in the same field with the unparsed dataset, we make use of the `stringr` library to substring the year from the title.
``` R
    regexp_pattern <- ' \\([0-9]*\\){1}'
    year <- str_match(movie$title, regexp_pattern)[1] %>% 
        gsub(' \\(', '', .) %>%
        gsub('\\)', '', .) %>%
        as.numeric()
```
This is a simple regexp that matches the year, an arbitrary amount of numbers between parentheses. For some movies no year is found. This means that we can't use these movies in our analysis. So these are excluded 
``` R
    if(is.na(year)) {
        next
    }
```
Having extracted the year, we use the same regexp to gsub the year out, leaving us with a proper title. 
``` R 
    genres <- strsplit(movie$genres, "\\|")
```

