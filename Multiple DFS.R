library(worldfootballR)
library(dplyr)
library(plyr)


###### DEFINING FUNCTIONS

# Iterate trhough prem urls and create individual dfs
individual_dfs <- function(szn_start_year){
  urls <- fb_teams_urls(paste0("https://fbref.com/en/comps/9/", 
                                               as.character(szn_start_year), "-", as.character(szn_start_year+1), "/",
                                               as.character(szn_start_year), "-", as.character(szn_start_year+1),
                                               '/Premier-League-Stats'))
  
  for (url in urls){
    dynamicVariableName <- paste0(toupper(substr(url, 48, 50)), "_", substr(url, 38, 41), "_stand")
    names_df.append(assign(dynamicVariableName, fb_team_player_stats(team_urls = url, stat_type = 'standard')))
  }
}

individual_dfs(2019)

binding_dfs <- function(){
  
}

# Creating a data frame (DF) for each club using standard data (ONLY WORKS FOR NON_CURRENT SZN)
create_standard_df <- function(szn){
  
  # List with URLs of each team for that season
  urls <- fb_teams_urls(paste0("https://fbref.com/en/comps/9/", 
                               as.character(szn), "-", as.character(szn+1), "/",
                               as.character(szn), "-", as.character(szn+1),
                               '/Premier-League-Stats'))
  
  # append team variable names to a list to iterate over later in rbind
  names_df <- list()
  
  # Create DF with Standard Stats for every Player that season
  for (url in urls){
    dynamicVariableName <- paste0(toupper(substr(url, 48, 50)), "_", substr(url, 38, 41), "_stand")
    df = assign(dynamicVariableName, fb_team_player_stats(team_urls = url, stat_type = 'standard'))
    names_df <- append(names_df, df)
  }
    
  # Manually add man city 
  MANC_stand = fb_team_player_stats(team_urls = paste0("https://fbref.com/en/squads/b8fd03ef/", 
                                                         as.character(szn), "-", as.character(szn+1), "/Manchester-City-Stats"),
                                      stat_type = 'standard')
  # APPEND MANC TO LIST!!!!!
    
  # Create a League wide DF 
  #league_var_name <- bind_rows(ARS_stand, AST_stand, BOU_stand, BRE_stand, 
  #                              BRI_stand, CHE_stand, CRY_stand, EVE_stand, 
  #                              FUL_stand, LEE_stand, LEI_stand, LIV_stand, 
  #                              MANC_stand, MAN_stand, NEW_stand, NOT_stand, 
  #                              SOU_stand, TOT_stand, WES_stand, WOL_stand)
  league_var_name <- bind_rows(names_df)

  
  return(league_var_name)
}


################## TESTING 
league_stand_2019 <- create_standard_df(2019)

typeof(league_stand_2019)
##################




############### DEVELOPING

prem_urls <- fb_teams_urls(paste0("https://fbref.com/en/comps/9/", 
                             "2019", "-", "2020", "/",
                             "2019", "-", "2020",
                             '/Premier-League-Stats'))

names_df <- list()

# Create DF with Standard Stats for every Player that season
for (url in prem_urls){
  dynamicVariableName <- paste0(toupper(substr(url, 48, 50)), "_", substr(url, 38, 41), "_stand")
  names_df = append(names_df, assign(dynamicVariableName, fb_team_player_stats(team_urls = url, stat_type = 'standard')))
}


league_stand_2019 <- do.call("rbind", names_df)
##### 37 ROWS PER PLAYER
df = data.frame(league_stand_2019)

