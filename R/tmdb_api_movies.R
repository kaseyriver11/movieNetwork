

#' Pull information from TMDB API
#' @description Send an API request to tmdb based on movie genre and min and max release dates
#' @param api_key Api key requested from the website
#' @param genres genres of interest can include: 'Action', 'Adventure', 'Animation',
#'  'Comedy', 'Crime','Documentary', 'Drama', 'Family', 'Fantasy', 'History',
#'  'Horror', 'Music', 'Mystery', 'Romance', 'Science Fiction', 'TV Movie', 'Thriller',
#'   'War', 'Western'
#' @param min_date Min release date in the format 'yyyy-mm-dd'
#' @param max_date Max release date in the format 'yyyy-mm-dd'
#' @param movie_request_lim Limit on the number of movies to pull
#' @return A data frame containing movie titles along with movie attributes
#' @export
#'
library(dplyr)
library(tidyr)
pull_movie_titles <- function(api_key, genres, min_date, max_date, movie_request_lim = 100){

  #Set the page limit (one page returns 20 movies)
  page_limit <- ceiling(movie_request_lim / 20)

  #match the genre requested with the genre id
  genre_ids <- match_genre_id(genres)

  #create the url needed for the api request
  url <- create_movie_url(api_key, genre_ids, min_date, max_date, page_num = 1)

  #submit api request and return movie data in the form of a dataframe
  tmdb_pull <- jsonlite::fromJSON(url, simplifyDataFrame = T)
  tmdb_data <- tmdb_pull$request

  #determine how many pages we need to loop through
  total_pages <- tmdb_pull$total_pages
  total_pages <- ifelse(total_pages > page_limit, page_limit, total_pages)

  #loop through remaining pages and form one movie data frame
  for(page in 1:total_pages){
   url <- create_movie_url(api_key, genre_ids, min_date, max_date, page_num = page)
   tmdb_add <- jsonlite::fromJSON(url, simplifyDataFrame = T)$results
   tmdb_data <- rbind(tmdb_data, tmdb_add)
  }

  #The genres were a list withim a dataframe. This converts the list into a vector
  tmdb_data %>%
    mutate(genre_ids = sapply(tmdb_data$genre_ids, function(x) paste0(x, collapse = ',')) %>%
             unlist) %>%
    rename(movie_id = id) %>%
    select(-poster_path, -adult, -backdrop_path, -video) %>% 
    slice(1:movie_request_lim)-> tmdb_data

  return(tmdb_data)
}




#' Create movie genres data frame
#' @description Create dataframe of the genres available on tmdb
#' @return data frame of genres and respective genre id
#' @export

load_genres <- function(){
  id <- c(28, 12, 16, 35, 80, 99, 18, 10751, 14, 36, 27,
          10402, 9648, 10749, 878, 10770, 53, 10752, 37)

  name <- c("Action", "Adventure", "Animation", "Comedy", "Crime",
            "Documentary", "Drama", "Family", "Fantasy", "History",
            "Horror", "Music", "Mystery", "Romance", "Science Fiction",
            "TV Movie", "Thriller", "War", "Western")

  return(data.frame(id, name))
}






#' Create url for api request
#' @description Form the url needed for the api request
#' @param api_key Api key requested from the website
#' @param genres genres of interest can include: 'Action', 'Adventure', 'Animation',
#'  'Comedy', 'Crime','Documentary', 'Drama', 'Family', 'Fantasy', 'History',
#'  'Horror', 'Music', 'Mystery', 'Romance', 'Science Fiction', 'TV Movie', 'Thriller',
#'   'War', 'Western'
#' @param min_date Min release date in the format 'yyyy-mm-dd'
#' @param max_date Max release date in the format 'yyyy-mm-dd'
#' @return url needed for api request as a single string
create_movie_url <- function(api_key, genre_ids, min_date, max_date, page_num){
      return(  paste0('https://api.themoviedb.org/3/discover/movie?api_key=',
                 api_key,
                '&language=en-US',
                '&sort_by=popularity.desc',
                '&include_adult=false',
                '&include_video=false',
                '&page=',page_num,
                '&primary_release_date.gte=',min_date,
                '&primary_release_date.lte=',max_date,
                '&with_genres=',genre_ids) )

}






#' Match genre name to id needed for tmdb api
#' @description Match the genre string to the genre id needed for the api request
#' @param genres genres of interest can include: 'Action', 'Adventure', 'Animation',
#'  'Comedy', 'Crime','Documentary', 'Drama', 'Family', 'Fantasy', 'History',
#'  'Horror', 'Music', 'Mystery', 'Romance', 'Science Fiction', 'TV Movie', 'Thriller',
#'   'War', 'Western'
#' @return A string containing a comma separated string of genre ids; passable to the api
match_genre_id <- function(genres){
  genre_df <- load_genres()

  genre_df %>%
    filter(name %in% genres) %>%
    .$id %>%
    paste0(collapse = ',') -> genre_ids

  return(genre_ids)
}





