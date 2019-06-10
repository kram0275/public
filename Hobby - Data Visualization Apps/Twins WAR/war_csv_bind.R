###  See: https://www.fangraphs.com/warleaders.aspx?season=2017&team=8&type=

library(dplyr)

rm()
# Create vector(s) of filenames for raw data .csv files
setwd("~/R_WIP/Baseball/FanGraphs/Twins War CSVs")
csvnames <- paste0("TwinsWar", as.character(86:99), ".csv")
csvnames2 <- paste0("TwinsWar0", as.character(0:9), ".csv")
csvnames3 <- paste0("TwinsWar", as.character(10:17), ".csv")
csvnamesTotal <- c(csvnames, csvnames2, csvnames3)
# Initiate list as a container for data frames
war_list <- list()

for(i in 1:length(csvnamesTotal)) {
  war_list[[i]] <- as.data.frame(read.csv(file = csvnamesTotal[i]))
  war_list[[i]]$year <- as.character(i + 1985)
  # Create new metric called pct_war, which indicates a player's WAR as a pct. 
  # of the team's total WAR for a given year
  war_list[[i]]$pct_war <- round(war_list[[i]]$Total.WAR / 
    sum(war_list[[i]]$Total.WAR) * 100, 1)
}

# Consolidate list data frames into one large data frame
full_data <- dplyr::bind_rows(war_list)

setwd("~/R_WIP/Baseball/FanGraphs")
headers <- colnames(full_data)
headers[1] <- "Player"
colnames(full_data) <- headers
colnames(full_data)

# Save df, list, raw data filename vector
save(full_data, war_list, csvnamesTotal, file = "war_shiny.Rda")
# Clean up
rm(i, csvnames, csvnames2, csvnames3, headers)