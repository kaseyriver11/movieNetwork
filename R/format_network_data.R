

#' create pairwise name combinations for each movie
#' @description Take the data frame created by create_network_df and return all
#'  combinations of actors or cast within each movie
#' @param df data frame created by create_network_df function
#' @param cast_crew_select value of 'cast' or 'crew' dependent on whether the user wanted a cast network or crew network
#' @param k The Number of cast or crew to take. We make this an option because it can be computationally
#'  expensive to make connections between a long list of cast or crew members
#' @return a dataframe of each combination of cast and crew names
#' @export
#' 

create_link_df <- function(df, cast_crew_select, k = 20){
    
    #Filter to only keep the top k people, If this isn't done,
    #The network will become too large to properly display
    df %>%
        filter(cast_crew == cast_crew_select) %>% 
        group_by(movie_id) %>% 
        slice(1:k) -> top_k_people
    #creates all 2-way name combinations, to create links between people who worked on/
    # in the same movie
    name_link_df <- df %>% 
                        filter(cast_crew == cast_crew_select) %>% 
                        filter(name %in% top_k_people$name) %>% 
                        select(movie_id, name) %>% 
                        distinct() %>% #sometimes people are recorded twice in the same movie
                        group_by(movie_id) %>% 
                        summarise(name_comb = name_combinations(name)) %>%
                        .$name_comb %>% 
                        bind_rows() %>% 
                        mutate_all(funs(as.character))

    #We have to give new id's because the force network diagram requires that the ID's start at 0
    id_df <- name_link_df %>% 
                   gather() %>% 
                   select(value) %>% 
                   distinct() %>% 
                   mutate(id = 0:(n()-1)) %>% 
                   rename(name = value)
    
    #add back the id's to names
    name_id_link_df <- name_link_df %>% 
                        inner_join( id_df %>% 
                                        select(id, name) %>% 
                                        distinct(),
                                    by = c('source_name' = 'name')) %>% 
                        rename(source = id) %>% 
                        inner_join( id_df %>% 
                                        select(id, name) %>% 
                                        distinct,
                                    by = c('target_name' = 'name') ) %>% 
                        rename(target = id) %>% 
                        mutate(group_id = paste(source, target, sep = '_')) %>% 
                        group_by(group_id) %>% 
                        mutate(value = n()) %>% 
                        summarise_all(funs(.[1])) %>% 
                        select(source, target, value, source_name, target_name) %>% 
                        arrange(source)
    
        return(list(link_df = name_id_link_df, id_df = id_df))
}


#' Name Combinations
#' @description Takes a vector of names and returns each 2-way connection as a dataframe
#' @param name_vect A vector of cast or crew names
#' @return a list of cast/crew  name combinations
#' @export
name_combinations <- function(name_vect){
    if(length(name_vect) > 1){
        combn(name_vect, 2) %>%
            apply(2,sort) %>% 
            t() %>% 
            data.frame('source_name' = .[,1], 'target_name' = .[,2]) %>% 
            select(source_name, target_name) %>% 
            list %>% 
            return()
    } else {
        return(list(data.frame()))
    }
}


#'
#'Create labels for each node (each node is a person)
#'@description Taking information for each person, create a node label to be plotted along with the network
#'@param links_df Pairwise name combinations with id's dataframe.
#'@param movie_df a dataframe of movie data. the output from the function create_network_df
#'@return A dataframe containing information for node labels, including which group they belong to (number of movies)
#'@export

create_node_labels <- function(links_df,  movie_df, id_df){
node_label_df <- links_df %>% 
                 select(source_name, target_name) %>%   #select pairwise names
                 gather(value = 'name') %>%  #Make them into one long column (to count n connectione) 
                 count(name) %>% #count number of connections
                 inner_join(movie_df %>%  #join on the network dataframe and id dataframe for information needed
                            select(title, name), # for aggregation
                            by = 'name') %>% 
                 group_by(name) %>%   #group by person and calculate information needed for node labels
                 summarise(connections = n[1],
                           movies = paste(unique(title), collapse = ' | '),
                           group = length(unique(title)),
                           size = 1) %>%
                  rename(person_name = name) %>% 
                  mutate(name = paste0('Cast: ', person_name,
                                       ', Movies: ', movies,
                                       ', Connections: ', connections)) %>%
                  select(person_name, name, group, size) %>% 
                  data.frame()

node_label_df_id <- node_label_df %>% 
                        inner_join(id_df, by = c('person_name' = 'name')) %>% 
                        select(id, name, group, size) %>% 
                        arrange(id)
          
     return(node_label_df_id)
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



create_output_df <- function(links_df, movie_df){
    id_df <- data.frame(id = c(links_df$source, links_df$target), name = (c(links_df$source_name, links_df$target_name)), stringsAsFactors = F)%>%
             distinct()
    
    movie_df <- movie_df %>% 
        select(-id) %>% 
        inner_join(id_df, by = 'name')
    
    # Calculate Betweeness Centrality
    igraph_obj <- igraph::graph_from_data_frame(links_df, directed = FALSE)
    between_values <- igraph::betweenness(igraph_obj, normalized = TRUE)
    
    #Calculate Degree Centrality
    degree_values <- round(igraph::degree(igraph_obj, normalized = TRUE),4)
    
    #Calculate Eigen Centrality
    eigen_cent_values <- round(igraph::eigen_centrality(igraph_obj)$vector,4)
    
    #Create data frame to display information on the network
    centrality_df <- data.frame(id = as.integer(names(between_values)),
                            betweeness = between_values,
                            eigen = eigen_cent_values,
                            degree = degree_values)
    
    popularity_df <- movie_df %>% 
                     group_by(id) %>% 
                     summarise(name = name[1],
                               n_movies = length(unique(title)))
    
    output_df <- centrality_df %>% 
                     inner_join(popularity_df, by = 'id') %>% 
                     select(id, name, n_movies, degree, betweeness, eigen)
    
    return(output_df)
}
        