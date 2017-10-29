

shinyServer(function(input, output, session){
    
    # The list of Genres
    output$genres <- renderUI({
        selectizeInput("genreSelect", "Choose Genres:", as.list(genreList),
                       multiple = TRUE, options = list(maxItems = 3))
    })
    
    # Using the action button
    genres <- eventReactive(input$goButton, {
        input$genreSelect
    })
    dates <- eventReactive(input$goButton,{
        input$dateRange
    })
    movie_count <- eventReactive(input$goButton,{
        input$movie_count
    })
    actor_count <- eventReactive(input$goButton,{
        input$actor_count
    })
    
    movie_w_cast_df <- reactive({
        withProgress(message = 'Querying TMDB API', value = 0, {
        print('pulling movies')
        api_key = '6df77c0d4d734469b206f490ea084869'
        movie_df <- pull_movie_titles(api_key = api_key, 
                                      genres = genres(),
                                      min_date = dates()[1],
                                      max_date = dates()[2],
                                      movie_request_lim = movie_count())
        movie_w_cast_df <- movie_df$movie_id %>%  #apply pull_cast over each movie id
            lapply(function(movie_id){
                Sys.sleep(.255) # there is a limit of 40 queries/10sec (By ip address so key does not matter)
                incProgress(1/movie_count())
                return(pull_cast(api_key = api_key, movie_id)) }) %>%
            bind_rows() %>% #bind each api pull into a single data.frame
            inner_join(movie_df, by = 'movie_id')
        
        movie_w_cast_df
         })
    })
    
    output$force <- renderForceNetwork({
        if(input$goButton){
            # Look at only the cast members
         
            #format movie df into links
            links <<- create_link_df(df = movie_w_cast_df(),
                                       cast_crew_select = input$cast_crew,
                                       k = input$actor_count)
            
            #Create nodes labels containing the movies, connections, and betweenness
            node_labels <- create_node_labels(links_df = links$link_df, movie_df = movie_w_cast_df(), id_df = links$id_df) 
            
            # Make the visual
            forceNetwork(Links = links$link_df, Nodes = node_labels, height = 800, width = 1000,
                         Source = "source", Target = "target",
                         linkWidth = JS("function(d) { return Math.pow(d.value,1.2); }"), # width of links
                         linkDistance = JS("function(d){return d.value * 10}"), # tightness of movie clusters
                         Value = "value", NodeID = "name", zoom = TRUE,
                         charge = -15, legend = TRUE,
                         colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),
                         bound = TRUE,
                         Group = "group", opacity = 0.8, fontSize = 12)
        }
    })
    
    output$recentTable1 <-  renderDataTable({
        if(input$goButton){
            create_output_df(links_df = links$link_df, movie_df = movie_w_cast_df())
        }
        })
    
    
})

