#Load packages
library(tidyverse)
library(hexbin)
library(jsonlite)
library(httr)
library(shiny)
library(scales)
library(extrafont)
library(magick)
library(cowplot)
library(nbastatR)

#Get Players from nbaStat R
ps <- nba_players()

source("helpers.R")

#Change themes
court_theme <- court_themes$light
alpha_range <- c(0.98, 0.98)

#Get player names
players <- players[!duplicated(players$display_first_last), ]

#Function
make_chart <- function(player) {

dfs <- get_data(players$person_id[players$display_first_last==player], "2018-19")

dfs$year <- substr(dfs$season, 1, 4)
dfs$seasonyear <- paste0(dfs$year, "-", as.numeric(dfs$year)+1)
dfs$seasonyear <- as.factor(dfs$seasonyear)

names <- players %>% select(person_id, display_first_last)

dfs <- left_join(dfs,names, by = "person_id")

p <- base_court +
  geom_polygon(
    data = dfs,
    aes(
      x = adj_x*-1,
      y = adj_y,
      group = hexbin_id,
      fill = bounded_fg_diff,
    ),
    size = court_theme$hex_border_size,
    color = court_theme$hex_border_color
  )   +
  scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
  theme(text=element_text(size=14,  family="Gill Sans MT"), 
        plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5), 
        plot.caption = element_text(face = "italic", size = 8), 
        legend.spacing.x = unit(0, 'cm'), 
        legend.title=element_text(size=12), 
        legend.text = element_text(size = rel(0.6)), 
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-25,0,10,0))  +
  labs(title = player,
       subtitle = "2018-19") +
  guides(fill=guide_legend(
    keywidth=.35,
    keyheight=.15,
    default.unit="inch", 
    label.position = 'bottom', 
    title.position = 'top',
    title.hjust = .5,
    title.vjust = 2,
    label.vjust = 3,
    nrow = 1)) +
  scale_fill_distiller("FG% vs. League Average",
                       limits = c(-.15, .15),
                       breaks = c(-.15, -.10, -.05, .0, .05, .10, .15),
                       labels = c("-15%", "-10%", "-5%", "0%", "+5%", "+10%", "+15%"),
                       palette = 7, 
                       type = "div",
                       direction = -1)


cowplot::ggdraw(p) + 
  theme(plot.background = element_rect(fill="floralwhite", color = NA))



ggsave(paste0("PlayerCharts/", player, ".png"),  width = 6, height = 6, dpi=300)


hs.url <- ps$urlPlayerHeadshot[ps$namePlayer==player]

hs <- image_read(hs.url) 

graf <- image_read(paste0("PlayerCharts/", player, ".png"))

image_composite(graf, hs, offset = "+250+50")  %>% image_write(paste0("PlayerCharts/", player, ".png"))

footy <- image_read("footer.png")

graf <- image_read(paste0("PlayerCharts/", player, ".png"))

image_composite(graf, footy, offset = "+0+1745") %>% image_write(paste0("PlayerCharts/", player, ".png"))

}

#Enter Player's name
suppressWarnings({make_chart("Myles Turner")})


