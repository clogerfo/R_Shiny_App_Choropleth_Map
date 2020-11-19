library(stringr)
library(dplyr)
library(geojsonio)


states <- geojson_read("US_map.json", what = "sp")
data <- read.csv(file="MLBFA_data.csv", header=TRUE, sep=",")

#Used in functions to complete and match data
state_list = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",  "FL", "GA", "HI", "ID", "IL", 
               "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
               "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
               "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR")


#function that returns the mode of a numeric vector
getmode <- function(d) {
  unique_id <- unique(d)
  unique_id[which.max(tabulate(match(d, unique_id)))]
}

#match state abbreviations to the names that are used in .json geographic data
abr_code_mx <- cbind(state_list, states$STATE)
match_abr_to_code <- function(abr_value_df){
  state_abrs <- abr_value_df[,1]
  return_code <- rep(0, length(state_abrs))
  for( i in 1:length(state_abrs) ){
    for( j in 1:(dim(abr_code_mx)[1]) ){
      if( state_abrs[i] == abr_code_mx[j, 1] ){
        return_code[i] <- abr_code_mx[j, 2]
      }
    }
  }
  abr_value_df[,1] <- as.numeric(return_code)
  return(abr_value_df)
}


#incomplete_d needs to be: a matrix/dataframe with 2 columns. First is abr and second is data
fill_state_data <- function(incomplete_d){
  len <- length(state_list)
  list_v <- rep("N/A", len )
  for( j in 1:length(state_list) ){
    if( !as.logical(str_detect(state_list[j], paste(unlist(incomplete_d[,1]), collapse='|'))) ){
      #need a filler, incomplete d doesn't have this state recorded
      list_v[j] <- state_list[j] 
    }
  }
  nr <- length(list_v [! list_v %in% "N/A" ])
  print(nr)
  df_add <- matrix("N/A", nrow = nr, ncol = dim(incomplete_d)[2])
  
  df_add[, 1] <- list_v [! list_v %in% "N/A" ]
  return( as.data.frame( rbind(incomplete_d, df_add) ) )
}


#Function to combine our features with hte geo data for mapping
fill_data <- function(geo, data){
  data$race <- as.factor(data$race)
  data$US <- as.logical(str_detect(data$BirthPlace, paste(state_list, collapse='|')))
  US_data <- data[data$US == TRUE,]
  
  state <- rep(0, length(US_data$US) )
  for( i in 1:length(US_data$US) ){
    state[i] <- unlist(strsplit(as.character(US_data$BirthPlace[i]), ", "))[2]
  }
  
  US_data$State <- as.factor(state)
  US_data <- US_data %>% arrange(State, desc(avg.value))
  
  #top contract (AAV) by state
  top_contracts <- US_data %>% 
    group_by(State) %>% 
    slice(which.max(avg.value))
  
  #list, then lapply
  #need to include year here
  top_contract_subset <- as.data.frame(cbind(as.character(top_contracts$State),
                                             as.numeric(top_contracts$avg.value),
                                             as.character(top_contracts$name),
                                             as.character(top_contracts$position), 
                                             as.numeric(top_contracts$contractAge),
                                             as.numeric(top_contracts$contractDuration)))
  
  #missing mean salary
  temp <- US_data %>% 
    group_by(State) %>%
    summarise(mean_sal = mean(avg.value))
  
  #popular position by state
  pos <- US_data %>%
    group_by(State) %>%
    summarise(pop_pos = getmode(as.character(position)) ) 
  
  #Number unique FA contracts by state
  num_players <- US_data %>%
    group_by(State) %>%
    summarise(n())
  
  top_contract_subset <- top_contract_subset %>%
    setNames(., c("V1", "V2", "V3", "V4", "V5", "V6")) %>%
    fill_state_data() %>%
    match_abr_to_code() %>%
    arrange(V1) %>%
    select(-c(V1))
  
  pos <- pos %>%
    setNames(., c("V1", "V2")) %>%
    fill_state_data() %>%
    match_abr_to_code() %>%
    arrange(V1) %>%
    select(-c(V1))
  
  num_players <- num_players %>% 
    setNames(., c("V1", "V2")) %>%
    fill_state_data() %>%
    match_abr_to_code() %>%
    arrange(V1)%>%
    select(-c(V1))
  
  mean_contract_subset <- temp %>%
    setNames(., c("V1", "V2")) %>%
    fill_state_data() %>%
    match_abr_to_code() %>%
    arrange(V1) %>%
    select(-c(V1))
  
  list_titles <- c("top_contract_aav", "name", "position", "age", "duration")
  colnames(top_contract_subset) <- list_titles
  
  #Change vectors to appropraite data types
  top_contract_subset$top_contract_aav <- as.numeric(as.character(top_contract_subset$top_contract_aav))/1000000
  top_contract_subset$age <- as.numeric(as.character(top_contract_subset$age))
  top_contract_subset$name <- as.character(top_contract_subset$name)
  
  #put together full data set
  clean_data <- cbind(states@data, 
                      top_contract_subset, 
                      num_players = as.numeric(as.character(num_players$V2)),
                      pop_pos = as.character(pos$V2),
                      mean_sal = as.numeric(as.character(mean_contract_subset$V2))/1000000)
  
  #return full data set
  return(clean_data)
}

#Save final data set as .Rds to the working directory
states_data <- fill_data(states, data)
states@data <- states_data
saveRDS(states, file="states_complete.Rds")
