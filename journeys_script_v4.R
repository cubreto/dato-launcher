require(dplyr)

##____________________________________ helper functions

mock_up_data = function(n)
{
  # Mock up a data frame with 3 columns: journey_number, status, time, and with n rows
  # INPUTS
  # n (integer) - how many rows to mock up
  # OUTPUTS
  # data.frame with mock up journeys
  
  date_range = seq(as.Date('2019/03/01'), as.Date('2019/03/20'), by='day')
  journeys = list()
  for(journey_id in seq_len(n))
  {
    journey_size = sample(1:10, 2) ## size of journey, i.e. number of rows in a single journey
    journeys[[journey_id]] = data.frame(journey_number = journey_id,
                         status = sort(sample(LETTERS[1:7], journey_size, replace=TRUE)),
                         time = sort(sample(date_range, journey_size, replace=FALSE)),
                         stringsAsFactors = FALSE)
  }
  
  d = dplyr::bind_rows(journeys)
  filter_out_journeys = d %>% count(journey_number) %>% filter(n==1)
  d = d[!d$journey_number %in% filter_out_journeys$journey_number, ]
  
  return(d)
}

df_to_journeys = function(d)
{
  # this function breaks a data.frame by journey id (a fixed column called "journey number")
  # and creates a list of igraph graphs
  # INPUTS:
  ## d (data.frame) - a data.frame with graphs. It must have columns called: "journey_number (that identifies
  ##                  an individual journey), "status_from", "status_to"
  # OUTPUTS:
  ## a list of graphs, one graph per journey
  
  graphs = list()
  for(i in sort(unique(d[['journey_number']])))
  {
    graphs[[i]] = d %>%
      filter(journey_number == i) %>%
      distinct(status_from, status_to) %>%
      igraph::graph_from_data_frame()
  }
  return(graphs)
}

find_isomorphs = function(graphs, unique_journey_numbers)
{
  # this function finds isomorphs. It does not look for subgraphs.
  # INPUTS:
  ## graphs - a list of igraph graphs
  ## unique_journey_numbers - a vector of unique ids or journeys
  # OUTPUT:
  ## a data.frame of lists - the two columns are: archetype_journey and similar_journey_ids
  ##                         archetype_journey stores a graph archetype. Getting it out for archetype 5
  ##                         looks like:
  ##                           data = find_isomorphs(graphs, unique(d$journey_number))
  ##                           data[5, 'archetype_journey'][[1]][[1]] # 5 is the id of archetype
  ##
  ##                         similar_journey_ids stores indices of graphs similar to the archetype
  ##                         getting the list out for archetype id 18 looks like
  ##                           data = find_isomorphs(graphs, unique(d$journey_number))
  ##                           data[5, 'archetype_journey'][[1]]
  unique_journey_numbers = sort(unique_journey_numbers)
  archetypes = list()
  inspected_ids = c()
  for(i in unique_journey_numbers)
  {
    if(i %in% inspected_ids) next() ## skip since it the inner loop (on j) looks ahead
    temp_isomorph_id = c(i)
    for(j in unique_journey_numbers)
    {
      if(j <= i) next() ## j <= i have already been compared in previous iteration
      if(igraph::is_isomorphic_to(graphs[[i]], graphs[[j]]))
      {
        temp_isomorph_id = c(temp_isomorph_id, j)
      }
    }
    # these will not be
    ## inspected in the next iteration since they have already been covered
    inspected_ids = unique(c(inspected_ids, temp_isomorph_id))
    # append archetype to data
    # and enrich data with extra info
    unique_touchpoints = igraph::as_data_frame(graphs[[1]])$from %>% unique() %>% length()
    archetypes[[i]] = data_frame(archetype_journey = list(graphs[[i]]),
                                 similar_journey_ids = list(temp_isomorph_id),
                                 unique_touchpoints = unique_touchpoints,
                                 arch_id = i)
  }
  return(dplyr::bind_rows(archetypes))
}

translate_arch_to_journey = function(archs_df)
{
  # This function takes in data.frame with archetypes (i.e. output of function find_isomorphs)
  # and creates a map between archetypes and journeys
  # INPUTS:
  ## archs_df - output of find_isomorphs function
  # OUTPUTS:
  ## data.frame with archetype_id mapped to respective journey_ids
  arch_to_journey_translator = list()
  for(a_id in archs_df$arch_id){
    arch_to_journey_translator[[a_id]] = data.frame(arch_id = a_id,
                                                    journey_ids = unlist(archs_df[archs_df$arch_id == a_id,
                                                                                  'similar_journey_ids']))
  }
  return(dplyr::bind_rows(arch_to_journey_translator))
}


##____________________________________ data mock up and processing


#d = mock_up_data(100)
d = read.csv(file.choose(), stringsAsFactors = FALSE)

d = d %>%
  rename(status_from = status) %>%
  group_by(journey_number) %>%
  mutate(status_to = lead(status_from, 1),
         status_to = ifelse(is.na(status_to), 'end', status_to)) %>%
  select(status_from, status_to, journey_number, time) %>%
  ungroup()

gg = df_to_journeys(d)

archs_df = find_isomorphs(gg, unique(d$journey_number))
## how many times an archetype occured
archs_df$arch_popularity = sapply(archs_df[['similar_journey_ids']] , length)
## create translator: archetype - journey
arch_journey_tranlsation_table = translate_arch_to_journey(archs_df)
## and bind arch_id back to original data
d = d %>%
  left_join(arch_journey_tranlsation_table, by = c('journey_number' = 'journey_ids'))

##____________________________________ SHINY APP

require(shiny)
require(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Sample journeys"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(
        title = "Journeys overview",
        dataTableOutput('out_journey_overview')
      ),
      box(title = 'journeys by touchpoints', plotOutput("journey_by_touchpoints"))
    ),
    fluidRow(
      box(title = 'most frequent journey',
          plotOutput('most_frequent_journey'),
          height = 400),
      box(title = 'Fastest journey',
          plotOutput('fastest_journey'),
          height = 400)
    ),
    fluidRow(
      box(title = 'sankey'),
      networkD3::sankeyNetworkOutput('sankey')
    )
  )
)


server <- function(input, output)
{
  
  output$out_journey_overview = renderDataTable({
    number_of_journeys = length(unique(d$journey_number))
    number_of_archetypes = length(unique(d$arch_id))
    # most popular journey
    most_popular_journey_id = d %>% ungroup() %>% count(arch_id, sort=TRUE) %>% slice(1) %>% select(n) %>% as.numeric()
    most_frequent_journey = d[d$journey_number == most_popular_journey_id,][['status_from']] %>%
      unique() %>%
      paste(collapse=' -> ')
    
    data.frame(measure = c('total number of journeys', 'number of unique journeys', 'most frequent journey'),
               values  = c(number_of_journeys, number_of_archetypes, most_frequent_journey))
  })

  output$journey_by_touchpoints = renderPlot({
    tmp = d %>%
      count(journey_number) %>%
      slice(1:10) %>% ## NOTE: COMMENT THIS OUT IF YOU WANT PLOT FOR ALL JOURNEYS
      arrange(desc(n))
    tmplst = list()
    for(i in tmp$journey_number){
      tmplst[[i]] = d[d$journey_number == i, ][['status_from']] %>% unique %>% paste(collapse = ' -> ')
    }
    tmp['journey_name'] = unlist(tmplst)

    tmp %>% distinct(journey_name, .keep_all = TRUE) %>%
      ggplot2::ggplot(data=., ggplot2::aes(x=journey_name, y = n)) +
      ggplot2::geom_bar(stat='identity', color = "blue", fill = "white") +
      ggplot2::coord_flip() +
      ggplot2::ggtitle('Sample journeys by number of touchpoints') +
      ggplot2::ylab('number of touchpoints') +
      ggplot2::xlab('journey')
  })

  output$most_frequent_journey = renderPlot({
    number_of_occurences = archs_df[which.max(archs_df$arch_popularity),][[ 'arch_popularity']]
    archs_df[which.max(archs_df$arch_popularity), 'archetype_journey'][[1]][[1]] %>%
      plot(main=paste0('most popular journey\n', number_of_occurences, ' occurences'))
  })

  output$fastest_journey = renderPlot({
    tmp = d %>%
      group_by(journey_number) %>%
      summarize(maxt = max(time),
                mint = min(time),
                len = length(journey_number),
                arch_id = unique(arch_id)
    #            journey_name = paste(unique(status_from), collapse = ' -> '),
    #            arch_id = unique(arch_id)
    ) %>%
      mutate(duration = difftime(maxt, mint, units='days')) %>%
      ungroup() %>%
      filter(duration != 0, len > 4) %>%
      arrange(duration) %>%
      slice(1)

    archs_df[archs_df$arch_id == tmp$arch_id, ][['archetype_journey']][[1]] %>%
      plot(main=paste0('fastest journey\n', tmp$duration, ' days long'))
  })

  output$sankey = networkD3::renderSankeyNetwork({
    ds = d %>%
      filter(status_from != status_to) %>%
      count(status_from, status_to) %>%
      as.data.frame()

    tmp_nodes = data.frame(node = 0:6, name = LETTERS[1:7])

    ds = ds %>%
      left_join(tmp_nodes, by = c('status_from' = 'name')) %>%
      rename(source = node) %>%
      left_join(tmp_nodes, by = c('status_to' = 'name')) %>%
      rename(target = node,
             value = n) %>%
      select(source, target, value) %>%
      mutate(target = ifelse(is.na(target), 7, target))

    tmp_nodes = dplyr::bind_rows(tmp_nodes, data.frame(node=7, name='(end)'))

    networkD3::sankeyNetwork(Links = ds, Nodes = tmp_nodes,
                             Source = 'source',
                             Target = 'target',
                             Value = 'value',
                             NodeID = 'name',
                             fontSize = 12,
                             nodeWidth = 30
    )
  })

}



shinyApp(ui, server)

