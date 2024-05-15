# libraries ----
#devtools::install_github('rensa/ggflags', force = TRUE)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(ggflags)
library(geomtextpath)
library(showtext)
library(MetBrewer)
library(ggthemes)
library(maps)
library(patchwork)
library(ggiraph)
library(countrycode)

# load data ----
eurovision <- read_csv("data/eurovision_data.csv")

# add int for ranking and iso codes (need to change this in scraping script) ----
eurovision <- eurovision |>
  mutate(rank_int = str_extract_all(rank, "\\d+"),
         rank_int = as.integer(rank_int),
         winner_int = as.integer(winner),
         countrycode_iso3 = countrycode(participant_country, 
                                        origin = 'country.name', 
                                        destination = 'iso3c'),
         countrycode_iso2 = countrycode(participant_country, 
                                        origin = 'country.name', 
                                        destination = 'iso2c'),
         flag_emoji = tolower(countrycode_iso2))

# winners by year ----
# filter for just finals, winners, and year over 1970
winners <- eurovision %>%
  filter(round == "final" | round == "grand final") %>%
  filter(winner == TRUE) %>%
  filter(year >= 1970) %>%
  filter(nchar(flag_emoji) <= 2) %>% # deal with any with no flags
  # make text for plot
  mutate(winner_text = paste0(year, ": ", participant_name,
                              " (", total_points, " Points)"))

# lollipop viz of winners by year
png(filename = "visuals/eurovision_winners.png", 
    width = 4000, height = 4000, res = 340, pointsize = 14)
ggplot(winners, aes(year, total_points)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = total_points),
               colour = "grey45") +
  geom_flag(aes(country = flag_emoji)) + 
  geom_text(aes(label = winner_text),
            hjust = -0.1, vjust = 0.15, angle = 0,
            family = "Avenir", size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(x = "", y = "",
       title = "Eurovision winners: 1970 - 2022") +
  # add annotation information
  annotate(geom = "text", x = 1989, y = 975, size = 3,
           label = "1989 winners Yugoslavia\n removed due to no flag",
           family = "Avenir") +
  annotate(geom = "curve", x = 1989, y = 925, xend = 1989, yend = 300, 
           curvature = -0.2, colour = "forestgreen") +
  annotate(geom = "text", x = 2020, y = 975, size = 3,
           label = "No competition in 2020\n due to COVID-19",
           family = "Avenir") +
  annotate(geom = "curve", x = 2020, y = 925, xend = 2020, yend = 500, 
           curvature = 0.035, colour = "forestgreen") +
  theme(plot.title.position = "plot",
        plot.title = element_text(family = "Avenir", 
                                  size = 18,
                                  face = "bold"),
        text = element_text(family = "Avenir"),
        axis.text = element_text(size = 11),
        plot.background = element_rect(fill = "#FFFCFD"),
        panel.background = element_rect(fill = "#FFFCFD"),
        panel.grid.major.y = element_line(colour = "grey90"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90"),
        panel.grid.minor = element_line(colour = "grey90"),
        axis.ticks = element_blank())
dev.off()

# ranking over the years ----
yearly_rank <- eurovision |>
  filter(round == "final" | round == "grand final") |>
  group_by(participant_country, year) |>
  summarise(avg_rank = mean(rank_int, na.rm = TRUE)) |>
  ungroup()

highlight <- count(yearly_rank, participant_country, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(participant_country)
n <- length(highlight)

yearly_rank <- yearly_rank |>
  #filter(participant_country %in% highlight) |>
  mutate(group = if_else(participant_country %in% c("United Kingdom", "Sweden", "France"), 
                         participant_country, "other"),
         group = as.factor(group),
         group = fct_relevel(group, "other", after = Inf))

theme_set(theme_minimal(base_family = "Avenir"))

theme_update(
  # Remove title for both x and y axes
  axis.title = element_blank(),
  # Axes labels are grey
  axis.text = element_text(color = "grey40"),
  # The size of the axes labels are different for x and y.
  axis.text.x = element_text(size = 20, margin = margin(t = 5)),
  axis.text.y = element_text(size = 17, margin = margin(r = 5)),
  # Also, the ticks have a very light grey color
  axis.ticks = element_line(color = "grey91", size = .5),
  # The length of the axis ticks is increased.
  axis.ticks.length.x = unit(1.3, "lines"),
  axis.ticks.length.y = unit(.7, "lines"),
  # Remove the grid lines that come with ggplot2 plots by default
  panel.grid = element_blank(),
  # Customize margin values (top, right, bottom, left)
  plot.margin = margin(20, 40, 20, 40),
  # Use a light grey color for the background of both the plot and the panel
  plot.background = element_rect(fill = "grey98", color = "grey98"),
  panel.background = element_rect(fill = "grey98", color = "grey98"),
  # Customize title appearence
  plot.title = element_text(
    color = "grey10", 
    size = 28, 
    face = "bold",
    margin = margin(t = 15)
  ),
  # Customize subtitle appearence
  plot.subtitle = element_markdown(
    color = "grey30", 
    size = 16,
    lineheight = 1.35,
    margin = margin(t = 15, b = 40)
  ),
  # Title and caption are going to be aligned
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(
    color = "grey30", 
    size = 13,
    lineheight = 1.2, 
    hjust = 0,
    margin = margin(t = 40) # Large margin on the top of the caption.
  ),
  # Remove legend
  legend.position = "none"
)

end_df <- yearly_rank |> filter(group != "other") |> filter(year > 2023)
pal <- c('#1982c4','#ffca3a','#ff595e','#8ac926','#6a4c93')
ranking_p <- ggplot(yearly_rank |> filter(group != "other"),
       aes(year, avg_rank)) +
  geom_line(data = yearly_rank |> filter(group == "other"),
            colour = "grey30", linewidth = 0.6, alpha = 0.4) +
  geom_line(aes(colour = group), linewidth = 1.2, show.legend = F) +
  labs(title = "The volitlity of Eurovision rankings",
       subtitle = "Ranking of 1 = winner. Grey lines show rest of ranking data over time") +
  geom_text_repel(
    data = end_df,
    aes(label = participant_country, colour = group),
    family = "Avenir",
    size = 5,
    direction = "y",
    hjust = 0,
    xlim = c(2024.5, NA),
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
    ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(1956, 2033.5), 
    breaks = seq(1956, 2024, by = 5)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(1, 30, by = 5)) +
  scale_colour_manual(values = pal) +
  coord_cartesian(
    clip = "off") 
ranking_p

ggsave("visuals/eurovision_yearly_ranking.png", ranking_p, dpi = 325,
       device = ragg::agg_png, height = 10, width = 15.5)

# map of who has won Eurovision at least once ----
# create map and fix uk
map_df <- map_data('world') %>%
  mutate(region = ifelse(region == "UK", "United Kingdom", region))

# make aggregation for maps
eurovision_rankings <- eurovision %>%
  filter(round == "final" | round == "grand-final") %>%
  group_by(participant_country) %>%
  summarise(average_rank = mean(rank_int, na.rm = TRUE),
            total_wins = sum(winner_int, na.rm = TRUE)) %>%
  mutate(winners_cat = case_when(
    total_wins < 1 ~ "No wins",
    total_wins == 1 ~ "One win",
    total_wins > 1 ~ "Multiple wins"
  )) %>%
  ungroup() %>%
  mutate(winners_cat = factor(winners_cat, levels = c("No wins", "One win", "Multiple wins"))) 

# join up the datasets
eurovision_rankings_map <- map_df %>%
  filter(region %in% eurovision_rankings$participant_country) %>%
  left_join(eurovision_rankings, by = c("region" = "participant_country"))

euro_pal <- c('#4799e4', '#ffd966', '#c27ba0')

winners_map <- eurovision_rankings_map %>%
  ggplot(aes(long, lat, group = group, fill = winners_cat)) +
  geom_polygon(colour = "black", linewidth = 0.15) +
  coord_map() +
  scale_fill_manual(values = euro_pal) +
  labs(title = "Which countries have won Eurovision?",
       fill = "How many wins?") +
  theme_map(base_size = 16, 
            base_family = "Avenir") +
  theme(legend.position = 'bottom')
winners_map

ggsave(filename = "visuals/winners_map.png", winners_map, bg = "white",
       width = 4000, height = 3508, units = "px", dpi = 400)

# map of average ranking of countries ----
ranking_map <- eurovision_rankings_map %>%
  ggplot(aes(long, lat, group = group, fill = average_rank)) +
  geom_polygon(colour = "black", linewidth = 0.15) +
  coord_map() +
  scale_fill_continuous(trans = 'reverse',
                        low = euro_pal[1], high = euro_pal[2]) +
  labs(title = "What are the average final ranking of countries?",
       fill = "Average Eurovision final ranking") +
  theme_map(base_size = 16, 
            base_family = "Avenir") +
  theme(legend.position = 'bottom')
ranking_map

ggsave(filename = "visuals/ranking_map.png", ranking_map, bg = "white",
       width = 4000, height = 3508, units = "px", dpi = 400)

# combining map visuals ----
eurovision_maps <- winners_map + ranking_map +
  plot_annotation(
    caption = 'Data: Eurovision song content | Visual: Andrew Moles',
    theme = theme(plot.caption = element_text(size = 10, family = "Avenir"))
  )
eurovision_maps

ggsave(filename = "visuals/eurovision_maps.png", eurovision_maps, bg = "white",
       width = 5000, height = 3508, units = "px", dpi = 320)


