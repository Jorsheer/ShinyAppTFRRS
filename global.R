
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

get_table <- function(year_vector, table_url){
  all_table <- list()
  entry_index <- 1
  
  for(i in(year_vector)){
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
      if ("Wind" %in% names(df)) {
        df$Wind <- as.character(df$Wind)
      }
      if ("Points" %in% names(df)) {
        df$Points <- as.character(df$Points)
      }
      return(df)
    })
    
  }
  return(all_table_clean)
}

convert_time <- function(time_str) {
  if (is.na(time_str)) return(NA)
  if (grepl(":", time_str)) {
    parts <- strsplit(time_str, ":")[[1]]
    return(as.numeric(parts[1]) * 60 + as.numeric(parts[2]))
  } else {
    return(as.numeric(time_str))
  }
}

years <- c(2017,2018,2019,2021,2022,2023,2024,2025)
final_indoor_miac <- get_table(years,"htmlTables/MIAC/MIACIndoor")
final_miac <- bind_rows(final_indoor_miac[1:length(final_indoor_miac)])

final_miac$Time <- sapply(final_miac$Time, convert_time)
final_miac$Mark <- gsub("[^0-9\\.]", "", final_miac$Mark)
final_miac$Mark <- as.numeric(final_miac$Mark)

final_outdoor_nats <- get_table(years,"htmlTables/NATS/NATSOutdoor")
final_nats <- bind_rows(final_outdoor_nats[1:length(final_outdoor_nats)])

final_nats$Time <- sapply(final_nats$Time, convert_time)
final_nats$Mark <- gsub("[^0-9\\.]", "", final_nats$Mark)
final_nats$Mark <- as.numeric(final_nats$Mark)