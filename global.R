
library(rvest)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

get_table <- function(year_vector, table_url){ #convert the years list into a tibble
  all_table <- list()
  entry_index <- 1
  
  for(i in year_vector){
    url <- str_c(table_url, i, ".html") #separate html
    page <- read_html(url)
    
    event_blocks <- html_elements(page, "div.row.gender_m, div.row.gender_f")
    
    for (block in event_blocks) { #looks through different events
      event_name <- block |> 
        html_element("h3.font-weight-500") |> 
        html_text() |> 
        str_squish()
      
      table_node <- block |> html_element("table")
      
      if (is.na(table_node)) next
      
      table_data <- html_table(table_node, fill = TRUE)
      
      if (nrow(table_data) > 0) { #gets rid of data with no rows
        table_data <- table_data |>
          mutate(Year = i, Event = event_name)
        
        
        all_table[[entry_index]] <- table_data
        entry_index <- entry_index + 1
      }
    }
  }
  
  all_table_clean <- lapply(all_table, function(df) {
    if ("Time" %in% names(df)) df$Time <- as.character(df$Time)
    if ("Wind" %in% names(df)) df$Wind <- as.character(df$Wind)
    if ("Points" %in% names(df)) df$Points <- as.character(df$Points)
    return(df)
  })
  
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

move_points_to_mark <- function(df) {
  if ("Points" %in% names(df)) {
    if (!"Mark" %in% names(df)) {
      df$Mark <- NA_character_
    }
    df$Mark <- ifelse(is.na(df$Mark) | df$Mark == "", df$Points, df$Mark)
  }
  return(df)
}

miac_years <- c(2017,2018,2019,2021,2022,2023,2024,2025)
nats_years <- c(2012,2013,2014,2015,2016,2017,2018,2019,2021,2022,2023,2024,2025)

#INDOOR

final_miac_indoor <- get_table(miac_years,"htmlTables/MIAC/MIACIndoor")
final_miac_indoor <- bind_rows(final_miac_indoor[1:length(final_miac_indoor)])

final_miac_indoor <- move_points_to_mark(final_miac_indoor)

final_miac_indoor$Time <- sapply(final_miac_indoor$Time, convert_time)
final_miac_indoor$Mark <- gsub("[^0-9\\.]", "", final_miac_indoor$Mark)
final_miac_indoor$Mark <- as.numeric(final_miac_indoor$Mark)

# OUTDOOR

final_miac_outdoor <- get_table(miac_years,"htmlTables/MIAC/MIACoutdoor")
final_miac_outdoor <- bind_rows(final_miac_outdoor[1:length(final_miac_outdoor)])

final_miac_outdoor <- final_miac_outdoor |>
  mutate(Event = str_squish(Event))

final_miac_indoor <- move_points_to_mark(final_miac_indoor)

final_miac_outdoor$Time <- sapply(final_miac_outdoor$Time, convert_time)
final_miac_outdoor$Mark <- gsub("[^0-9\\.]", "", final_miac_outdoor$Mark)
final_miac_outdoor$Mark <- as.numeric(final_miac_outdoor$Mark)

#NATS

final_outdoor_nats <- get_table(miac_years,"htmlTables/NATS/NATSOutdoor")
final_nats <- bind_rows(final_outdoor_nats[1:length(final_outdoor_nats)])

final_outdoor_nats <- move_points_to_mark(final_outdoor_nats)

final_nats$Time <- sapply(final_nats$Time, convert_time)
final_nats$Mark <- gsub("[^0-9\\.]", "", final_nats$Mark)
final_nats$Mark <- as.numeric(final_nats$Mark)
