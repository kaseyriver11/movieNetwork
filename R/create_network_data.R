
#' Pull information from TMDB API
#' @description Combine Api pull functions 'pull_movie_titles', and 'pull_cast' to create
#' a dataframe that a network can be built from
#' @param api_key Api key requested from the website
#' @param genres genres of interest can include: 'Action', 'Adventure', 'Animation',
#'  'Comedy', 'Crime','Documentary', 'Drama', 'Family', 'Fantasy', 'History',
#'  'Horror', 'Music', 'Mystery', 'Romance', 'Science Fiction', 'TV Movie', 'Thriller',
#'   'War', 'Western'
#' @param min_date Min release date in the format 'yyyy-mm-dd'
#' @param max_date Max release date in the format 'yyyy-mm-dd'
#' @param movie_request_lim Limit on the number of movies to pull
#' @return A data frame containing movie titles and associated cast & crew
#' @export

create_network_df <- function(api_key, genres, min_date, max_date, movie_request_lim = 100){
  movie_df <- pull_movie_titles(api_key, genres, min_date, max_date, movie_request_lim)

  movie_df$movie_id %>%  #apply pull_cast over each movie id
    lapply(function(movie_id){
      Sys.sleep(.255) # there is a limit of 40 queries/10sec (By ip address so key does not matter)
      return(pull_cast(api_key, movie_id)) }) %>%
    bind_rows() %>% #bind each api pull into a single data.frame
    inner_join(movie_df, by = 'movie_id') -> final_df

  return(final_df)
}





