

#' create pairwise name combinations for each movie
#' @description Take the data frame created by create_network_df and return all
#'  combinations of actors or cast within each movie
#' @param Movie data frame created by create_network_df function
#' @param cast_crew value of 'cast' or 'crew' dependent on whether the user wanted a cast network or crew network
#' @param k The Number of cast or crew to take. We make this an option because it can be computationally
#'  expensive to make connections between a long list of cast or crew members
#' @return a dataframe of each combination of cast and crew names
#' @export
#' 
#'   selection <- input$cast_crew

create_name_combinations <- function(df, cast_crew_select, k = 20){
    
    df %>%
        filter(cast_crew == cast_crew_select) %>% 
        group_by(movie_id) %>% 
        slice(1:k) -> important_people
        
    df %>% 
        filter(cast_crew == cast_crew_select) %>% 
        filter(name %in% important_people$name) %>% 
        select(movie_id, name) %>% 
        distinct() %>% 
        group_by(movie_id) %>% 
        summarise(name_comb = name_combinations(name)) %>%
        .$name_comb %>% 
        bind_rows() %>% 
        mutate_each(funs(as.character)) %>% 
        return()
}


#' Name Combinations
#' @description Takes a vector of names and returns each 2-way connection as a dataframe
#' @param name_vect A vector of cast or crew names
#' @return a list of cast/crew  name combinations
#' @export
name_combinations <- function(name_vect){
    combn(name_vect, 2) %>%
        apply(2,sort) %>% 
        t() %>% 
        data.frame('source_name' = .[,1], 'target_name' = .[,2]) %>% 
        select(source_name, target_name) %>% 
        list %>% 
        return()
}




#' 
#' Add unique id's to each name
#' @param name_match_df Data frame containing 2 columns named target_name and source_name.
#' each row is a pairwise combination of people who worked togethor on a movle. This argument is passed the output from
#' the function create_name_combinations. 
#' It contains pairwise names of people who togethor on a movie
#' @param id_df Data Frame containing a name column, and an ID column. We will be assigning each name a numeric value
#' @return a Data frame containing the same pairwise names that were submitted in name_match_df, except now there are columns 
#' indicating person id's. One column named 'value' is the number of times that pair occurred (potential for people to work on
#' more than one movie togethor)
#' @export 
add_ids_to_names <- function(name_match_df, id_df){
    name_match_df %>% 
        inner_join(id_df, by = c('source_name' = 'name')) %>% 
        rename(source = id) %>% 
        inner_join(id_df, by = c('target_name' = 'name')) %>%
        rename(target = id)  %>% 
        group_by(source_target = paste0(source, '_', target)) %>% 
        mutate(value = n()) %>% 
        summarise_each(funs(.[1])) %>%  #For people that were in > 1 movies togethor
        select(source, target, value, source_name, target_name) %>% 
        return()
}


#'
#'Create labels for each node (each node is a person)
#'@description Taking information for each person, create a node label to be plotted along with the network
#'@param pairwise_df Pairwise name combinations with id's dataframe. This parameter is the output
#'from add_ids_to_names function
#'@param id_df The id dataframe containing a column of names, and a column of id's
#'@param between_df a dataframe containing a column of names, and a column of betweenness scores
#'@param network_df a dataframe of movie data. the output from the function create_network_df
#'@return A dataframe containing information for node labels, including which group they belong to (number of movies)
#'@export

create_node_labels <- function(pairwise_df, id_df, between_df, network_df){
pairwise_df %>% 
    select(source_name, target_name) %>%   #select pairwise names
    gather(value = 'name') %>%  #Make them into one long column (to count n connectione) 
    count(name) %>% #count number of connections
    inner_join(network_df %>%  #join on the network dataframe and id dataframe for information needed
                   select(title, name), # for aggregation
               by = 'name') %>% 
    inner_join(id_df, by = 'name') %>% 
    group_by(name) %>%   #group by person and calculate information needed for node labels
    summarise(connections = n[1],
              id = id[1],
              movies = paste(unique(title), collapse = ' | '),
              group = length(unique(title)),
              size = 1) %>% 
    inner_join(between_df, by = 'id')  %>% 
    mutate(name = paste0('Cast: ', name,
                         ', Movies: ', movies,
                         ', Connections: ', connections,
                         ', Betweenness Score: ', between_value)) %>%
    arrange(id) %>% 
    select(name, group, size) %>% 
    return()
}




#'
#'Return the k most popular people 
#'@description Return the k most popular people based on connections and betweenness
#'@param pairwise_df Pairwise name combinations with id's dataframe. This parameter is the output
#'from add_ids_to_names function
#'@param id_df The id dataframe containing a column of names, and a column of id's
#'@param between_df a dataframe containing a column of names, and a column of betweenness scores
#'@param network_df a dataframe of movie data. the output from the function create_network_df
#'@return A dataframe containing information for node labels, including which group they belong to (number of movies)
#'@export
find_most_popular <- function(pairwise_df, id_df, between_df, network_df, k){
    pairwise_df %>% 
        select(source_name, target_name) %>%   #select pairwise names
        gather(value = 'name') %>%  #Make them into one long column (to count n connectione) 
        count(name) %>% #count number of connections
        inner_join(network_df %>%  #join on the network dataframe and id dataframe for information needed
                       select(title, name), # for aggregation
                   by = 'name') %>% 
        group_by(name) %>%   #group by person and calculate information needed for node labels
        summarise(connections = n[1],
                  group = length(unique(title))) %>%
        rename('Total Movies' = group) %>% 
        arrange(desc(`Total Movies`), desc(connections)) %>% 
        slice(1:k) %>% 
        return()
}


        