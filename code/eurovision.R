# trying to update script by https://github.com/tashapiro/eurovision-contest/blob/main/code/eurovision_scraping.R
# eurovision website changed

# load libraries ----
library(rvest)
library(tidyverse)

# extracting webpage for each year of comp ----
# initialize an empty vector to store the links
event_links <- c()
# loop over the pages
for (i in 0:5) {
  # construct the URL for the page
  url <- paste0('https://eurovision.tv/history?page=', i)
  # read the HTML of the webpage
  url_events <- read_html(url)
  # extract the "More" links from the table on the page
  links <- url_events %>%
    html_nodes("table") %>%
    html_nodes("a.show-more") %>%
    html_attr("href")
  # append the links to the event_links vector
  event_links <- c(event_links, links)
}

# make the full url
event_links <- paste0("https://eurovision.tv", event_links)
event_links

# Extract host country ----
country <- c()
for (i in 1:length(event_links)) {
  if (i <= 15) {
    c <- read_html(event_links[i]) %>%
      html_elements('li:nth-child(5) .field-content') %>%
      html_text2() %>%
      gsub(".*,", "", x = .) %>%
      trimws()
    country <- c(country, c)
  } else if (i >= 54 & i <= 57) {
    c <- read_html(event_links[57]) %>%
      html_elements('li:nth-child(3) .field-content') %>%
      html_text2() %>%
      gsub(".*,", "", x = .) %>%
      trimws()
    country <- c(country, c[2])
  } else if (i >= 20) {
    c <- read_html(event_links[i]) %>%
      html_elements('li:nth-child(3) .field-content') %>%
      html_text2() %>%
      gsub(".*,", "", x = .) %>%
      trimws()
    country <- c(country, c)
  } else {
    c <- read_html(event_links[i]) %>%
      html_elements('li:nth-child(4) .field-content') %>%
      html_text2() %>%
      gsub(".*,", "", x = .) %>%
      trimws()
    country <- c(country, c)
  }
}

# store events in data frame ----
events <- data.frame()

for (i in 0:4) {
  # Construct the URL for the page
  url <- paste0('https://eurovision.tv/history?page=', i)
  # Read the HTML of the webpage
  url_events <- read_html(url)
  # extract table
  temp_df <- url_events %>%
    html_element('table') %>%
    html_table()
  # append to events data frame
  events <- rbind(events, temp_df)
}

# put into a data frame for easier use later
events <- events %>%
  select(Year:Winners) %>%
  mutate(host_country = country,
         event_link = event_links,
         event = str_extract(event_link, "(?<=/)[^/]*(?<=\\d)") %>% gsub("-", " ", .)) %>%
  select(Year, City, host_country, everything())


# semi finals ----
# 2004 - 2008 one semi final
# 2008 onwards two semi finals
# pre 2004 only final
finals <- c("/first-semi-final", "/second-semi-final", 
            "/semi-final", "/final", "/grand-final")

qual_vect <- c("1st", "2nd", "3rd", "4th", "5th",
               "6th", "7th", "8th", "9th", "10th")

two_semi <- events[events$Year >= 2008, "event_link", drop = TRUE] # two semi finals
one_semi <- events[events$Year > 2004 & events$Year < 2008, "event_link", drop = TRUE] # one semi final

# pull first semi final for years after 2007
first_semi <- data.frame()
for (i in event_links[event_links %in% two_semi]) {
  first_link <- paste0(i, finals[1])
  f_s <- read_html(first_link) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(
      Participant = str_remove(Participant, "qualified"),
      round = gsub("/", "", finals[1]) %>% gsub("-", " ", .),
      #host_city = str_extract(ev_l, "(?<=/)[^/]*(?=-)"),
      #host_country = country[1],
      Year = as.integer(str_sub(i, start = -4, end = -1)),
      #event = paste0(host_city, " ", year),
      qualified = ifelse(Rank %in% qual_vect, TRUE, FALSE),
      winner = ifelse(Rank == "1st", TRUE, FALSE)
    ) %>%
    rename(running_order = `R/O\n\nSort descending`)
  first_semi <- rbind(first_semi, f_s)
}

# pull second semi final for years after 2007
second_semi <- data.frame()
for (i in event_links[event_links %in% two_semi]) {
  second_link <- paste0(i, finals[2])
  s_s <- read_html(second_link) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(
      Participant = str_remove(Participant, "qualified"),
      round = gsub("/", "", finals[2]) %>% gsub("-", " ", .),
      #host_city = str_extract(ev_l, "(?<=/)[^/]*(?=-)"),
      #host_country = country[1],
      Year = as.integer(str_sub(i, start = -4, end = -1)),
      #event = paste0(host_city, " ", year),
      qualified = ifelse(Rank %in% qual_vect, TRUE, FALSE),
      winner = ifelse(Rank == "1st", TRUE, FALSE)
    ) %>%
    rename(running_order = `R/O\n\nSort descending`)
  second_semi <- rbind(second_semi, s_s)
}

# pull semi final between 2005 and 2007
lone_semi <- data.frame()
for (i in event_links[event_links %in% one_semi]) {
  semi_link <- paste0(i, finals[3])
  s <- read_html(semi_link) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(
      Participant = str_remove(Participant, "qualified"),
      round = gsub("/", "", finals[3]) %>% gsub("-", " ", .),
      Year = as.integer(str_sub(i, start = -4, end = -1)),
      qualified = ifelse(Rank %in% qual_vect, TRUE, FALSE),
      winner = ifelse(Rank == "1st", TRUE, FALSE)
    ) %>%
    rename(running_order = `R/O\n\nSort descending`)
  lone_semi <- rbind(lone_semi, s)
}

# join up semis
all_semis <- rbind(lone_semi, first_semi, second_semi)

# grand final & final ----
# was called final pre 2004
final <- events[events$Year < 2004, "event_link", drop = TRUE]
g_final <- events[events$Year >= 2004, "event_link", drop = TRUE]

# final pre 2004
df_final <- data.frame()
for (i in event_links[event_links %in% final]) {
  final_link <- paste0(i, finals[4])
  f <- read_html(final_link) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(
      Participant = str_remove(Participant, "qualified"),
      round = gsub("/", "", finals[4]),
      Year = as.integer(str_sub(i, start = -4, end = -1)),
      qualified = rep(NA, nrow(.)),
      winner = ifelse(Rank == "1st", TRUE, FALSE)
    ) %>%
    rename(running_order = `R/O\n\nSort descending`)
  df_final <- rbind(df_final, f)
}

# grand final post 2004
df_gfinal <- data.frame()
for (i in event_links[event_links %in% g_final]) {
  gfinal_link <- paste0(i, finals[5])
  gf <- read_html(gfinal_link) %>%
    html_element("table") %>%
    html_table() %>%
    mutate(
      Participant = str_remove(Participant, "qualified"),
      round = gsub("/", "", finals[5]) %>% gsub("-", " ", .),
      Year = as.integer(str_sub(i, start = -4, end = -1)),
      qualified = rep(NA, nrow(.)),
      winner = ifelse(Rank == "1st", TRUE, FALSE)
    ) %>%
    rename(running_order = `R/O\n\nSort descending`)
  df_gfinal <- rbind(df_gfinal, gf)
}

# combine final datasets
all_finals <- rbind(df_final, df_gfinal)

# combine semifinals and finals into one dataset ----
df_rankings <- rbind(all_semis, all_finals)

# join dataset to events ----
data <- left_join(df_rankings, events, by = "Year", relationship = "many-to-many") %>%
  janitor::clean_names() %>%
  rename(host_city = city,
         participant_name = participant,
         participant_country = country,
         total_points = points) %>%
  select(event, host_city, host_country, year, 
         round, participant_name, song, participant_country,
         running_order, total_points, rank, qualified, winner,
         event_link)

# export data ----
write.csv(data, "data/eurovision_data.csv", row.names = FALSE)
