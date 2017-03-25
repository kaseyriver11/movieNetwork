
#' Pull information on the cast of a movie
#' @description Use tmdb api to return the cast of a movie given the tmdb movie id
#' @param api_key Api key of tmdb
#' @param movie_id Movie id of interest
#' @return Dataframe of the cast for the movie of interest
#' @export

pull_cast <- function(api_key, movie_id){

  url <- create_cast_url(api_key, movie_id)
  cast_crew_data <- jsonlite::fromJSON(url, simplifyDataFrame = T)

  cast_crew_data$cast %>%
    select(id, name, character) %>%
    mutate(cast_crew = 'cast') %>%
    rename(role = character) %>%
    rbind(  cast_crew_data$crew %>%
              select(id, name, job) %>%
              mutate(cast_crew = 'crew') %>%
              rename(role = job)  ) %>%
    mutate(movie_id = movie_id) -> cast_crew_data


  return(cast_crew_data)
}



#' Create the cast url
#' @description Given the api key and movie id, create the url needed to pass to the api
#' @param api_key Api key of tmdb
#' @param movie_id Movie id of interest
#' @return url needed to pass to the api
#' @export

create_cast_url <- function(api_key, movie_id){
  return( paste0('https://api.themoviedb.org/3/',
         'movie/', movie_id,'/credits?',
         'api_key=',api_key) )
}








