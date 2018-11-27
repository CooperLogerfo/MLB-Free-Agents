install.packages("stringr")
install.packages("geojsonio")
install.packages("leaflet")
install.packages("dplyr")
library(stringr)
library(geojsonio)
library(leaflet)
library(dplyr)

setwd("/Users/cooperlogerfo/desktop/R_play")

states <- geojson_read("us_map.json", what = "sp")
data <- read.csv(file="final_data.csv", header=TRUE, sep=",")

map <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

map %>% addPolygons()


data$race <- as.factor(data$race)

#
state_list = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC",  "FL", "GA", "HI", "ID", "IL", 
           "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
           "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
           "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "PR")
data$US <- as.logical(str_detect(data$BirthPlace, paste(state_list, collapse='|')))


#1 subset data
#2 strip cities from loc names

data$BirthPlace <- as.character(data$BirthPlace)

US_data <- data[data$US == TRUE,]

state <- rep(0, length(US_data$US) )
for( i in 1:length(US_data$US) ){
  state[i] <- unlist(strsplit(as.character(US_data$BirthPlace[i]), ", "))[2]
}

US_data$State <- as.factor(state)
US_data <- US_data %>% arrange(State, desc(avg.value))

# top_player_state <-
#   US_data %>% group_by(State) %>%
#   arrange(State, desc(avg.value)) %>%
#   filter(row_number() == 1)

#sort/goup by state
#find aav by state , do by_state( by aav( get first player infofrmation and output it ))

# US_data$state_sal <- rep( 0, dim(US_data)[1] )
# 
# for( i in 1:(dim(US_data)[1]) ){
#   for( j in 1:length(aav_state$State) ){
#     if(US_data$State[i] == aav_state$State[j]){
#       #I think this should be "top aav"
#       US_data$state_sal[i] <- aav_state$mean_sal[j]
#     }
#   }
# }

abr_code_mx <- cbind(state_list, states$STATE)
#so this is totally useless because Code in downloaded df is not numeric even tho its 1 - 52
match_abr_to_code <- function(abr_value_df){
  state_abrs <- abr_value_df[,1]
  #print(head(state_abrs))
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

abr_name_mx <- cbind(state_list, states$NAME)

match_abr_to_name <- function(state_abrs){
  return_name <- rep(0, length(state_abrs))
  for( i in 1:length(state_abrs) ){
    for( j in 1:(dim(abr_name_mx)[1])){
      if( state_abrs[i] == abr_name_mx[j, 1] ){
        return_name[i] <- abr_name_mx[j, 2]
      }
    }
  }
  return(return_name)
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

#sort data by state, then by avg value.. so we can try to pick off the top the player
#with the highest avg salary from each state
US_data %>% arrange(State, avg.value)

top_contracts <- US_data %>% 
  group_by(State) %>% 
  slice(which.max(avg.value))

#list, then lapply
#need to include year here
top_contract_subset <- as.data.frame(cbind(as.character(top_contracts$State), as.numeric(top_contracts$avg.value),
                                        as.character(top_contracts$name), as.character(top_contracts$position), 
                                        as.numeric(top_contracts$contractAge), as.numeric(top_contracts$contractDuration)))

#missing mean salary
temp <- US_data %>% 
  group_by(State) %>%
  summarise(mean_sal = mean(avg.value))

getmode <- function(v) {
       uniqv <- unique(v)
       uniqv[which.max(tabulate(match(v, uniqv)))]
}

#popular positions
pos <- US_data %>%
  group_by(State) %>%
  summarise(pop_pos = getmode(as.character(position)) ) 

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

#"state"
list_titles <- c("top_contract_aav", "name", "position", "age", "duration")
colnames(top_contract_subset) <- list_titles

top_contract_subset$top_contract_aav <- as.numeric(as.character(top_contract_subset$top_contract_aav))/1000000

states@data <- cbind(states@data, 
                     top_contract_subset, 
                     num_players = num_players$V2,
                     pop_pos = pos$V2,
                     mean_sal = mean_contract_subset$V2)


#add to data frame
#names(states@data)[12]  <- "mean_sal"
states@data$name <- as.character(states@data$name)
states@data$position <- as.character(states@data$position)
states@data$mean_sal <- as.numeric(as.character(states@data$mean_sal))/1000000
states@data$age <- 25 + as.numeric(top_contract_subset$age)

labels <- sprintf(
  "<strong>%s</strong><br/>%g AAV FA Contract (Millions)<br/>
  %g State's Largest Contract (Millions)<br/>%s Player Receiving Top Contract <br/>
  %s State's Most Common Position",
  states@data$NAME, states@data$mean_sal, states@data$top_contract_aav, 
  states@data$name, states@data$position
) %>% lapply(htmltools::HTML)


bins <- c(0, .001, 3, 6, 8, Inf)
pal <- colorBin("YlOrRd", domain = states@data$mean_sal, bins = bins)

map %>% 
  addPolygons( fillColor = ~pal(states@data$mean_sal), 
                     weight = 2, opacity = 1, 
                     color = "white", dashArray = "3", fillOpacity = 0.7,
                     highlight = highlightOptions( weight = 5, color = "#666",
                                                   dashArray = "", fillOpacity = .7, 
                                                   bringToFront = TRUE),
                     label = labels, 
                     labelOptions = labelOptions(style = list(
                       "font-weight" = "normal", padding = "3px 8px"), textsize = "15px", 
                       direction = "auto")) %>%
  addLegend(pal = pal, values = ~states@data$mean_sal, opacity = 0.7, 
            title = NULL, position = "bottomright")

