
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

get_table <- function(year_start, year_end, table_url){
  all_table <- list()
  entry_index <- 1
  
  for(i in(year_start:year_end)){
    url <- str_c(table_url, i,
                 ".html")
    page <- read_html(url)
    tables <- html_nodes(page, "table") |>
      html_table(fill = TRUE)
    
    event_names <- page |>
      html_nodes("h3.font-weight-500") |>
      html_text(trim = TRUE)
    
    table_temp <- tables[sapply(tables, function(tbl) nrow(tbl) > 1)]
    
    for (j in 1:length(table_temp)) {
      table_temp[[j]] <- table_temp[[j]] |>
        mutate(Year = i) |>
        mutate(Event = event_names[[j]])
      
      
      all_table[[entry_index]] <- table_temp[[j]]
      entry_index <- entry_index + 1
    }
    
    all_table_clean <- lapply(all_table, function(df) {
      if ("Time" %in% names(df)) {
        df$Time <- as.character(df$Time)
      }
      return(df)
    })
    
  }
  return(all_table_clean)
}

final_indoor_miac <- get_table(2021, 2025,"htmlTables/MIAC/MIACIndoor")
final_miac <- bind_rows(final_indoor_miac[1:length(final_indoor_miac)])

final_outdoor_nats <- get_table(2012, 2025,"htmlTables/NATS/NATSOutdoor")
final_nats <- bind_rows(final_outdoor_nats[1:length(final_outdoor_nats)])

