library(tidyverse)
library(ggimage)
library(gt)
library(ggrepel)
#library(jthomasmock/gtExtras)
library(scales)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(gganimate)
library(gtExtras)
library(datapasta)
library(lubridate)
library(vip)
library(viridis)
library(hrbrthemes)
library(sportyR)
library(mlbplotR)
library(baseballr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(devtools)
library(hockeyR)
library(shiny)

library(data.table)
library(tidyverse) # all the things

library(ggExtra)   # marginal plots

library(ggtext)    # color your text

library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales)

library(hexbin)

library(shinydashboard)
library(shinyWidgets)
library(reactable)
library(reactablefmtr)
library(shinythemes)

library(reshape2)


#install.packages("tidyverse")
library(tidyverse)

#install.packages("dplyr")
library(dplyr)

library(ggplot2)

library(cowplot)
library(rsconnect)

player_types <- read.csv("NBA_Play_Types_16_23.csv", header = TRUE, sep = ",")%>%
  filter(SEASON == "2022-23")

shots_data <- read.csv("NBA_2023_ShotsNew.csv", header = TRUE, sep = ",")


defense_data <- read.csv("Defense_NBA_Dashboard.csv", header = TRUE, sep = ",")

player_name <- unique(player_types$PLAYER_NAME)

team_name <- unique(player_types$TEAM_ABB)




#rsconnect::deployApp('/Users/ryanmiele/Library/Mobile Documents/com~apple~CloudDocs/R Projects/NBA/NBA Player Dashboard/2022 NBA Player Dashboard')



# Court Plotting Function (Run All at Once)

# ------------------------- Creating Court and Plotting
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

court_themes = list(
  light = list(
    court = 'floralwhite',
    lines = 'black',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "#000000"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray15',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray15"
  ),
  white = list(
    court = 'white',
    lines = 'black',
    text = 'black',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "gray15"
  )
)


plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
  if (use_short_three) {
    three_point_radius = 22
    three_point_side_height = 0
  }
  
  court_points = data_frame(
    x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
    y = c(height, 0, 0, height, height),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines, sizes = 2
    ) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal(base_size = 22) +
    theme(
      text = element_text(color = court_theme$text),
      plot.background = element_rect(fill = 'gray15', color = 'gray15'),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      #legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      #legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0))
    )
}




# ------------------------- Filter for Single Game Shots by Date & Player Name

shots <- shots_data %>%
  filter(GAME_DATE == "04-04-2023") %>%
  filter(PLAYER_NAME == "Joel Embiid" )


# ------------------------- Create Plot

plot_court(court_themes$white, use_short_three = F) +
  # ---- plot shot "points" with x & y locations
  geom_point(data = shots, aes(x = LOC_X, y = LOC_Y, color = SHOT_MADE, fill = SHOT_MADE),
             size =3, shape = 21, stroke = .5) +
  # ---- plot player hedashot (remove these 2 lines if you're plotting multiple players!)
  draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"),
             x = -19, y = -1.3, width = 9, height = 9) +
  # ---- plot team logo (remove these 2 lines if you're plotting multiple teams!)
  draw_image(paste0("https://cdn.nba.com/logos/nba/", unique(shots$TEAM_ID), "/primary/L/logo.svg"),
             x = 12, y = 0.2, width = 6, height = 6) +
  # ---- fill the points with color
  scale_color_manual(values = c("green4","red3"), aesthetics = "color", breaks = c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
  scale_fill_manual(values = c("green2","red2"), aesthetics = "fill", breaks = c("TRUE", "FALSE"), labels=c("Made", "Missed")) +
  # ----
scale_x_continuous(limits = c(-27.5, 27.5)) +
  scale_y_continuous(limits = c(0, 45)) +
  # ---- Add title and subtitle (manual!)
  labs(
    title = "Joel Embiid - Shot Chart",
    subtitle = "April 4th, 2023 vs. Boston Celtics"
  ) +
  # ----
annotate(geom = 'text', x = 0, y = 2, hjust = .5,
         label = "Tutorial: @DSamangy", size = 2, color = "black", face = "bold") +
  # ---- Theme options for manipulating the look of the plot
  theme(
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(hjust = .5, size = 7, face = "bold", colour = "black"),
    legend.background = element_rect(fill = "white", colour = "white"),
    legend.box.background = element_rect(fill = "white", colour = "white"),
    legend.key = element_rect(fill = "white", colour = "white"),
    #legend.margin = margin(t = -.5, unit='cm'),
    #legend.box.margin=margin(-15,0,15,0),
    #
    plot.background = element_rect(fill="white", color = "white"),
    panel.background = element_rect(fill="white", color = "white"),
    #
    plot.title = element_text(hjust = 0.5, size = 22, vjust = -9, face = "bold", colour = "black"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, vjust = -15, face = "bold", colour = "black"),
    #plot.margin = margin(0, 0, .5, 0, "cm"),
  )







compiled_shots <- shots_data%>%
  mutate(made_shot = case_when(SHOT_MADE == "TRUE"~ 1, TRUE ~ 0),
         point_value = case_when(made_shot == "1" & SHOT_TYPE == "3PT Field Goal" ~ 3,
                                 made_shot == "1" & SHOT_TYPE == "2PT Field Goal" ~ 2,
                                 TRUE ~ 0))%>%
  group_by(BASIC_ZONE, ZONE_NAME)%>%
  mutate(mean_zone = mean(made_shot), .groups = 'drop')%>%
  ungroup()%>%
  group_by(PLAYER_NAME, BASIC_ZONE, ZONE_NAME)%>%
  mutate(mean_individual_zone = mean(made_shot))%>%
  ungroup()%>%
  group_by(PLAYER_NAME)%>%
  mutate(total_shots_individual = n())%>%
  ungroup()


compiled_shots <- compiled_shots%>%
  mutate(final_zone = case_when(
    BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Left Side Center" ~ "Left Side Above the Break 3",
    BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Right Side Center" ~ "Right Side Above the Break 3",
    BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center" ~ "Center Above the Break 3",
    BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side" ~ "Left Side Mid-Range",
    BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side Center" ~ "Left Side Center Mid-Range",
    BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center" ~ "Center Mid-Range",
    BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side Center" ~ "Right Side Center Mid-Range",
    BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side" ~ "Right Side Mid-Range",
    TRUE ~ compiled_shots$BASIC_ZONE))

pps <- compiled_shots%>%
  group_by(final_zone)%>%
  mutate(n_league = n(),
         sum_point_value_league = sum(point_value),
         pps_league = sum_point_value_league/n_league,
         freq_league = n_league/217220)%>%
  ungroup()%>%
  group_by(PLAYER_NAME, final_zone)%>%
  mutate(n_individual = n(),
         sum_point_value_individual = sum(point_value),
         pps_individual = sum_point_value_individual/n_individual,
         freq_individual = n_individual/total_shots_individual)%>%
  ungroup()










ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel(h1("2022-23 NBA Player Dashboard", align = "center")),
  
  sidebarPanel(width = 2,
               selectInput("playerchoose1", "Player", 
                           choices = c(sort(unique(as.character(player_name)))), selected = "Tyrese Haliburton")
               
  ),
  
  
  
  mainPanel(
    navbarPage("Ryan Miele",
               tabPanel("Play Type",
                        fluidRow(
                          column(4, align = "center",
                                 plotOutput("head_shot")),
                          br(),
                          column(8),
                          tableOutput("playtypetbl"),
                          br(),
                          column(12, align = "center"),
                          tableOutput("actiontypetbl"),
                          br(),
                          column(12, align = "center"),
                          tableOutput("shotdistancetbl"),
                        )),
               tabPanel("Shot Frequency",
                        fluidRow(
                          column(4, align = "center",
                                 plotOutput("head_shot2")),
                          column(8,
                                 tags$h4("Shot Frequency")),
                          plotOutput("shotfreq"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Three Pointer Shot Frequency")),
                          plotOutput("shotfreqbarthree"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Mid-Range Shot Frequency")),
                          plotOutput("shotfreqbarmid"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Paint Shot Frequency")),
                          plotOutput("shotfreqbarpaint"),
                        )),
               tabPanel("Shot Efficiency",
                        fluidRow(
                          column(4, align = "center",
                                 plotOutput("head_shot3")),
                          column(8,
                                 tags$h4("Overall Points Per Shot")),
                          plotOutput("shotpps"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Three Pointer Points Per Shot")),
                          plotOutput("threebarpps"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Mid-Range Points Per Shot")),
                          plotOutput("midbarpps"),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Paint Points Per Shot")),
                          plotOutput("paintbarpps"),
                        )),
               tabPanel("Defense and Hustle",
                        fluidRow(
                          column(12, align = "center",
                                 plotOutput("head_shot4")),
                          column(12, align = "center",
                                 tags$h4("Defensive Percentiles")),
                          plotOutput("defenseplot"),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          br(),
                          column(12, align = "center",
                                 tags$h4("Hustle Percentiles")),
                          plotOutput("hustleplot"),
                        )),
               tabPanel("About",
                        fluidRow(
                          column(12, align = "left",
                                 tags$h5("If anyone spots any issues or has any questions don't hesitate to reach out to me @BuckAnalytics or ryancmiele@gmail.com."),
                                 br(),
                                 tags$h5("All of the data came from NBA.com or Dom Samangy's github (https://github.com/DomSamangy).
                                         I want to give a special thanks to Dom Samangy and Arjun Menon who provided a lot of the inspiration behind this project."))
                          
                        )))))


server <-  function(input, output) {
  
  
  
  output$head_shot <- renderPlot({
    
    
    
    shots <- shots_data %>%
      mutate(head_x = 0,
             head_y = 0)%>%
      filter(PLAYER_NAME == input$playerchoose1 )
    #filter(PLAYER_NAME == "Joel Embiid")
    
    
    ggplot()+
      geom_point(data = shots, aes(x = LOC_X, y = LOC_Y), color = "white", fill = "white") +
      draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"), 
                 x = -30, y = -10, width = 60, height = 60)+
      theme_minimal()+
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    
  }, width = 200)
  
  output$playtypetbl <- render_gt({
    
    
    
    
    table_play_types <- player_types %>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(PLAY_TYPE, FREQ,FREQ_PCTL,PPP,PPP_PCTL)%>%
      filter(PLAY_TYPE != "Misc")%>%
      setorder(cols = - "FREQ")
    
    
    
    
    
    table_play_types%>%
      gt() %>%
      gtExtras::gt_theme_espn()%>%
      cols_label(PLAY_TYPE = "Play Type",
                 FREQ = "Frequency",
                 FREQ_PCTL = "Percentile",
                 PPP = "Points per Possesion",
                 PPP_PCTL = "Percentile")%>%
      fmt_percent(
        columns = FREQ,
        decimals = 1)%>%
      data_color(columns = FREQ_PCTL,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))%>%
      data_color(columns = PPP_PCTL,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))
    
  }, width = 450)
  
  output$actiontypetbl <- render_gt({
    
    
    
    compiled_shots <- shots_data%>%
      mutate(made_shot = case_when(SHOT_MADE == "TRUE"~ 1, TRUE ~ 0),
             point_value = case_when(made_shot == "1" & SHOT_TYPE == "3PT Field Goal" ~ 3,
                                     made_shot == "1" & SHOT_TYPE == "2PT Field Goal" ~ 2,
                                     TRUE ~ 0))%>%
      group_by(BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_zone = mean(made_shot), .groups = 'drop')%>%
      ungroup()%>%
      group_by(PLAYER_NAME, BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_individual_zone = mean(made_shot))%>%
      ungroup()%>%
      group_by(PLAYER_NAME)%>%
      mutate(total_shots_individual = n())%>%
      ungroup()
    
    
    
    compiled_shots <- compiled_shots%>%
      mutate(final_zone = case_when(
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Left Side Center" ~ "Left Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Right Side Center" ~ "Right Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center" ~ "Center Above the Break 3",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side" ~ "Left Side Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side Center" ~ "Left Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center" ~ "Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side Center" ~ "Right Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side" ~ "Right Side Mid-Range",
        TRUE ~ compiled_shots$BASIC_ZONE))
    
    
    action_type <- compiled_shots%>%
      group_by(PLAYER_NAME, ACTION_TYPE)%>%
      mutate(total_shots_individual_action_type = n(),
             sum_point_value_individual_action_type = sum(point_value),
             pps_individual_action_type = sum_point_value_individual_action_type/total_shots_individual_action_type,
             freq_individual_action_type = total_shots_individual_action_type/total_shots_individual)%>%
      ungroup()
    
    action_type$action_type_freq_percentile <- ntile(action_type$freq_individual_action_type, 100)
    action_type$action_type_pps_percentile <- ntile(action_type$pps_individual_action_type, 100)
    
    
    table_action_type <- action_type%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(ACTION_TYPE, freq_individual_action_type,action_type_freq_percentile,pps_individual_action_type, action_type_pps_percentile)%>%
      setorder(cols = - "freq_individual_action_type")%>%
      filter(freq_individual_action_type > .02)
    
    table_action_type <- table_action_type[!duplicated(table_action_type), ]
    table_action_type <- table_action_type[!duplicated(table_action_type$ACTION_TYPE), ]
    
    table_action_type%>%
      gt() %>%
      gtExtras::gt_theme_espn()%>%
      cols_label(ACTION_TYPE = "Action Type",
                 freq_individual_action_type = "Frequency",
                 action_type_freq_percentile = "Percentile",
                 pps_individual_action_type = "Points per Shot",
                 action_type_pps_percentile = "Percentile")%>%
      
      fmt_percent(
        columns = freq_individual_action_type,
        decimals = 1)%>%
      fmt_number(columns = pps_individual_action_type,
                 decimals = 2)%>%
      data_color(columns = action_type_freq_percentile,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))%>%
      data_color(columns = action_type_pps_percentile,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))
    
    
    
    
    
  }, width = 900)
  
  
  
  output$shotdistancetbl <- render_gt({
    
    
    compiled_shots <- shots_data%>%
      mutate(made_shot = case_when(SHOT_MADE == "TRUE"~ 1, TRUE ~ 0),
             point_value = case_when(made_shot == "1" & SHOT_TYPE == "3PT Field Goal" ~ 3,
                                     made_shot == "1" & SHOT_TYPE == "2PT Field Goal" ~ 2,
                                     TRUE ~ 0))%>%
      group_by(BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_zone = mean(made_shot), .groups = 'drop')%>%
      ungroup()%>%
      group_by(PLAYER_NAME, BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_individual_zone = mean(made_shot))%>%
      ungroup()%>%
      group_by(PLAYER_NAME)%>%
      mutate(total_shots_individual = n())%>%
      ungroup()
    
    
    
    compiled_shots <- compiled_shots%>%
      mutate(final_zone = case_when(
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Left Side Center" ~ "Left Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Right Side Center" ~ "Right Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center" ~ "Center Above the Break 3",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side" ~ "Left Side Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side Center" ~ "Left Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center" ~ "Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side Center" ~ "Right Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side" ~ "Right Side Mid-Range",
        TRUE ~ compiled_shots$BASIC_ZONE))
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    
    
    
    zone_range <- compiled_shots%>%
      group_by(PLAYER_NAME, ZONE_RANGE)%>%
      mutate(total_shots_individual_zone_range = n(),
             sum_point_value_individual_zone_range = sum(point_value),
             pps_individual_zone_range = sum_point_value_individual_zone_range/total_shots_individual_zone_range,
             freq_individual_zone_range = total_shots_individual_zone_range/total_shots_individual)%>%
      ungroup()
    
    zone_range$zone_range_freq_percentile <- ntile(zone_range$freq_individual_zone_range, 100)
    zone_range$zone_range_pps_percentile <- ntile(zone_range$pps_individual_zone_range, 100)
    
    table_zone_range <- zone_range%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(ZONE_RANGE, freq_individual_zone_range,zone_range_freq_percentile,pps_individual_zone_range, zone_range_pps_percentile)%>%
      setorder(cols = - "freq_individual_zone_range")
    
    table_zone_range <- table_zone_range[!duplicated(table_zone_range), ]
    table_zone_range <- table_zone_range[!duplicated(table_zone_range$ZONE_RANGE), ]
    
    table_zone_range%>%
      gt() %>%
      gtExtras::gt_theme_espn()%>%
      cols_label(ZONE_RANGE = "Shot Distance",
                 freq_individual_zone_range = "Frequency",
                 zone_range_freq_percentile = "Percentile",
                 pps_individual_zone_range = "Points per Shot",
                 zone_range_pps_percentile = "Percentile")%>%
      
      fmt_percent(
        columns = freq_individual_zone_range,
        decimals = 1)%>%
      fmt_number(columns = pps_individual_zone_range,
                 decimals = 2)%>%
      data_color(columns = zone_range_freq_percentile,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))%>%
      data_color(columns = zone_range_pps_percentile,
                 palette = c("blue","steelblue1","white", "indianred1", "red"),
                 domain = c(-0,100))
    
    
    
    
    
  }, width = 900)
  
  
  
  output$head_shot2 <- renderPlot({
    
    
    
    shots <- shots_data %>%
      mutate(head_x = 0,
             head_y = 0)%>%
      filter(PLAYER_NAME == input$playerchoose1 )
    #filter(PLAYER_NAME == "Joel Embiid")
    
    
    ggplot()+
      geom_point(data = shots, aes(x = LOC_X, y = LOC_Y), color = "white", fill = "white") +
      draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"), 
                 x = -30, y = -10, width = 60, height = 60)+
      theme_minimal()+
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    
  }, width = 200)
  
  
  output$shotfreq <- renderPlot({
    
    circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
      angles = seq(0, 2 * pi, length.out = npoints)
      return(data_frame(x = center[1] + radius * cos(angles),
                        y = center[2] + radius * sin(angles)))
    }
    
    width = 50
    height = 94 / 2
    key_height = 19
    inner_key_width = 12
    outer_key_width = 16
    backboard_width = 6
    backboard_offset = 4
    neck_length = 0.5
    hoop_radius = 0.75
    hoop_center_y = backboard_offset + neck_length + hoop_radius
    three_point_radius = 23.75
    three_point_side_radius = 22
    three_point_side_height = 14
    
    court_themes = list(
      light = list(
        court = 'floralwhite',
        lines = 'black',
        text = '#222222',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 1,
        hex_border_color = "#000000"
      ),
      dark = list(
        court = '#000004',
        lines = '#999999',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "#000000"
      ),
      ppt = list(
        court = 'gray15',
        lines = 'white',
        text = '#f0f0f0',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      ),
      white = list(
        court = 'white',
        lines = 'black',
        text = 'black',
        made = '#00bfc4',
        missed = '#f8766d',
        hex_border_size = 0,
        hex_border_color = "gray15"
      )
    )
    
    
    plot_court = function(court_theme = court_themes$light, use_short_three = FALSE) {
      if (use_short_three) {
        three_point_radius = 22
        three_point_side_height = 0
      }
      
      court_points = data_frame(
        x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
        y = c(height, 0, 0, height, height),
        desc = "perimeter"
      )
      
      court_points = bind_rows(court_points , data_frame(
        x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
        y = c(0, key_height, key_height, 0),
        desc = "outer_key"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(-backboard_width / 2, backboard_width / 2),
        y = c(backboard_offset, backboard_offset),
        desc = "backboard"
      ))
      
      court_points = bind_rows(court_points , data_frame(
        x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
      ))
      
      foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
      
      foul_circle_top = filter(foul_circle, y > key_height) %>%
        mutate(desc = "foul_circle_top")
      
      foul_circle_bottom = filter(foul_circle, y < key_height) %>%
        mutate(
          angle = atan((y - key_height) / x) * 180 / pi,
          angle_group = floor((angle - 5.625) / 11.25),
          desc = paste0("foul_circle_bottom_", angle_group)
        ) %>%
        filter(angle_group %% 2 == 0) %>%
        select(x, y, desc)
      
      hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
        mutate(desc = "hoop")
      
      restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
        filter(y >= hoop_center_y) %>%
        mutate(desc = "restricted")
      
      three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
        filter(y >= three_point_side_height, y >= hoop_center_y)
      
      three_point_line = data_frame(
        x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
        y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
        desc = "three_point_line"
      )
      
      court_points = bind_rows(
        court_points,
        foul_circle_top,
        foul_circle_bottom,
        hoop,
        restricted,
        three_point_line
      )
      
      
      court_points <- court_points
      
      ggplot() +
        geom_path(
          data = court_points,
          aes(x = x, y = y, group = desc),
          color = court_theme$lines
        ) +
        coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
        theme_minimal(base_size = 22) +
        theme(
          text = element_text(color = court_theme$text),
          plot.background = element_rect(fill = 'gray15', color = 'gray15'),
          panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
          legend.position = "bottom",
          legend.key = element_blank(),
          legend.text = element_text(size = rel(1.0))
        )
    }
    
    
    
    
    shots <- shots_data %>% 
      #filter(GAME_DATE == "04-04-2023") %>%
      filter(PLAYER_NAME == input$playerchoose1 )
    
    
    
    palette <-paletteer::paletteer_c("ggthemes::Red-Blue Diverging", 17, direction = -1)
    
    
    plot_court(court_themes$white, use_short_three = F) +
      geom_hex(data = shots,
               aes(x = LOC_X, y = LOC_Y,
                   fill = cut(..count.., c(
                     0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32, Inf))),
               colour = "white",
               binwidth = 5,
               alpha = 0.6) +
      scale_fill_manual(values = c(palette), aesthetics = c("fill", "color"))+ 
      scale_x_continuous(limits = c(-27.5, 27.5)) +
      scale_y_continuous(limits = c(0, 45)) +
      #labs(caption = "By: @BuckAnalytics")+
      theme(
        legend.position="none",
        plot.background = element_rect(fill="white", color = "white"),
        panel.background = element_rect(fill="white", color = "white")
        #plot.title = element_text(hjust = 0.5, size = 22, vjust = -13, face = "bold", colour = "black"),
        #plot.subtitle = element_text(hjust = 0.5, size = 10, vjust = -22, face = "bold", colour = "black"), 
      ) 
    
    
    
  }, width = 450)
  
  
  output$shotfreqbarthree <- renderPlot({
    
    
    compiled_shots <- shots_data%>%
      mutate(made_shot = case_when(SHOT_MADE == "TRUE"~ 1, TRUE ~ 0),
             point_value = case_when(made_shot == "1" & SHOT_TYPE == "3PT Field Goal" ~ 3,
                                     made_shot == "1" & SHOT_TYPE == "2PT Field Goal" ~ 2,
                                     TRUE ~ 0))%>%
      group_by(BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_zone = mean(made_shot), .groups = 'drop')%>%
      ungroup()%>%
      group_by(PLAYER_NAME, BASIC_ZONE, ZONE_NAME)%>%
      mutate(mean_individual_zone = mean(made_shot))%>%
      ungroup()%>%
      group_by(PLAYER_NAME)%>%
      mutate(total_shots_individual = n())%>%
      ungroup()
    
    compiled_shots <- compiled_shots%>%
      mutate(final_zone = case_when(
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Left Side Center" ~ "Left Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Right Side Center" ~ "Right Side Above the Break 3",
        BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center" ~ "Center Above the Break 3",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side" ~ "Left Side Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side Center" ~ "Left Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center" ~ "Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side Center" ~ "Right Side Center Mid-Range",
        BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side" ~ "Right Side Mid-Range",
        TRUE ~ compiled_shots$BASIC_ZONE))
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    
    dffreq <- pps%>%
      #filter(PLAYER_NAME == "Joel Embiid")%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, freq_individual, freq_league)%>%
      rename("Selected Player Frequency" = freq_individual,
             "League Frequency" = freq_league)
    
    
    dffreq <- dffreq[!duplicated(dffreq), ]
    
    dffreq <- reshape2::melt(dffreq[,c("final_zone", "Selected Player Frequency", "League Frequency")],id.vars = 1)
    
    
    threefreq <- dffreq%>%
      filter(final_zone == "Left Corner 3" | final_zone == "Left Side Above the Break 3" |
               final_zone == "Center Above the Break 3" | final_zone == "Right Side Above the Break 3" |
               final_zone == "Right Corner 3")
    
    
    ggplot(threefreq)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
                vjust = 0, hjust = -.05, size = 4)+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
    
    
  }, width = 900)
  
  output$shotfreqbarmid <- renderPlot({
    
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    
    dffreq <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, freq_individual, freq_league)%>%
      rename("Selected Player Frequency" = freq_individual,
             "League Frequency" = freq_league)
    
    
    dffreq <- dffreq[!duplicated(dffreq), ]
    
    dffreq <- reshape2::melt(dffreq[,c("final_zone", "Selected Player Frequency", "League Frequency")],id.vars = 1)
    
    
    
    
    
    midrangefreq <- dffreq%>%
      filter(final_zone == "Right Side Mid-Range" | final_zone == "Right Side Center Mid-Range" |
               final_zone == "Center Mid-Range" | final_zone == "Left Side Mid-Range" |
               final_zone == "Left Side Center Mid-Range")
    
    ggplot(midrangefreq)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
                vjust = 0, hjust = -.05, size = 4)+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
    
  }, width = 900)
  
  output$shotfreqbarpaint <- renderPlot({
    
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    dffreq <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, freq_individual, freq_league)%>%
      rename("Selected Player Frequency" = freq_individual,
             "League Frequency" = freq_league)
    
    
    dffreq <- dffreq[!duplicated(dffreq), ]
    
    dffreq <- reshape2::melt(dffreq[,c("final_zone", "Selected Player Frequency", "League Frequency")],id.vars = 1)
    
    
    paintfreq <- dffreq%>%
      filter(final_zone == "Restricted Area" | final_zone == "In The Paint (Non-RA)" )
    
    ggplot(paintfreq)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
                vjust = 0, hjust = -.05, size = 4)+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            legend.text = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
    
  }, width = 900)  
  
  
  
  output$head_shot3 <- renderPlot({
    
    
    
    shots <- shots_data %>%
      mutate(head_x = 0,
             head_y = 0)%>%
      filter(PLAYER_NAME == input$playerchoose1 )
    #filter(PLAYER_NAME == "Joel Embiid")
    
    
    ggplot()+
      geom_point(data = shots, aes(x = LOC_X, y = LOC_Y), color = "white", fill = "white") +
      draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"), 
                 x = -30, y = -10, width = 60, height = 60)+
      theme_minimal()+
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    
  }, width = 200)
  
  
  
  output$shotpps <- renderPlot({
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    
    dfpps <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(PLAYER_NAME,final_zone, pps_individual, pps_league)%>%
      rename("Selected Player PPS" = pps_individual,
             "League PPS" = pps_league)
    
    
    dfpps <- dfpps[!duplicated(dfpps), ]
    
    
    court_heat <- pps%>%
      group_by(final_zone)%>%
      mutate(mloc_x = mean(LOC_X),
             mloc_y = mean(LOC_Y))%>%
      summarize(final_zone, mloc_x, mloc_y)%>%
      ungroup()
    
    
    
    court_ppp <- merge(x = dfpps, court_heat, by = "final_zone", all.x = TRUE)
    
    
    court_ppp <- court_ppp[!duplicated(court_ppp), ]
    
    
    court_ppp$ind_ppsround <- round(court_ppp$`Selected Player PPS`, digit = 2)
    
    
    court_ppp <- court_ppp[!duplicated(court_ppp), ]
    
    
    
    plot_court(court_themes$white, use_short_three = F)+
      geom_point(data = court_ppp,
                 aes(x = mloc_x, y = mloc_y, fill = court_ppp$ind_ppsround), pch = 21, color = "black", size = 12)+
      scale_fill_gradient2(midpoint = median(pps$pps_individual), low = "blue", mid = "white", high = "red")+
      geom_text(data = court_ppp,
                aes(x = mloc_x, y = mloc_y, label = court_ppp$ind_ppsround), size = 8, vjust = -1.5)+
      geom_text_repel()+
      #scale_color_manual(values = c(palette), aesthetics = c("fill", "color"))+
      scale_color_gradient2(midpoint = median(pps$pps_individual), low = "blue", mid = "white", high = "red")+
      scale_x_continuous(limits = c(-27.5, 27.5)) +
      scale_y_continuous(limits = c(0, 45))+
      theme(
        legend.position="none",
        #legend.title = element_blank(),
        plot.background = element_rect(fill="white", color = "white"),
        panel.background = element_rect(fill="white", color = "white"))
    #plot.title = element_text(hjust = 0.5, size = 22, vjust = -13, face = "bold", colour = "black"))
    #plot.subtitle = element_text(hjust = 0.5, size = 10, vjust = -22, face = "bold", colour = "black"))
    
    
    
  }, width = 450) 
  
  
  output$threebarpps <- renderPlot({
    
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    
    dfpps <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, pps_individual, pps_league)%>%
      rename("Selected Player PPS" = pps_individual,
             "League PPS" = pps_league)
    
    
    dfpps <- dfpps[!duplicated(dfpps), ]
    
    dfpps <- reshape2::melt(dfpps[,c("final_zone", "Selected Player PPS", "League PPS")],id.vars = 1)
    
    
    dfpps$value <- round(dfpps$value, digit = 2)
    
    threepps <- dfpps%>%
      filter(final_zone == "Left Corner 3" | final_zone == "Left Side Above the Break 3" |
               final_zone == "Center Above the Break 3" | final_zone == "Right Side Above the Break 3" |
               final_zone == "Right Corner 3")
    
    ggplot(threepps)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = value), position = position_dodge(0.9),
                vjust = 0, hjust = 1.5, size = 4, color = "white")+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      #scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
    
    
  }, width = 900)  
  
  output$midbarpps <- renderPlot({
    
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    dfpps <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, pps_individual, pps_league)%>%
      rename("Selected Player PPS" = pps_individual,
             "League PPS" = pps_league)
    
    
    dfpps <- dfpps[!duplicated(dfpps), ]
    
    dfpps <- reshape2::melt(dfpps[,c("final_zone", "Selected Player PPS", "League PPS")],id.vars = 1)
    
    
    dfpps$value <- round(dfpps$value, digit = 2)
    
    
    midrangepps <- dfpps%>%
      filter(final_zone == "Right Side Mid-Range" | final_zone == "Right Side Center Mid-Range" |
               final_zone == "Center Mid-Range" | final_zone == "Left Side Mid-Range" |
               final_zone == "Left Side Center Mid-Range")
    
    
    ggplot(midrangepps)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = value), position = position_dodge(0.9),
                vjust = 0, hjust = 1.5, size = 4, color = "white")+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      #scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
  }, width = 900) 
  
  
  output$paintbarpps <- renderPlot({
    
    
    pps <- compiled_shots%>%
      group_by(final_zone)%>%
      mutate(n_league = n(),
             sum_point_value_league = sum(point_value),
             pps_league = sum_point_value_league/n_league,
             freq_league = n_league/217220)%>%
      ungroup()%>%
      group_by(PLAYER_NAME, final_zone)%>%
      mutate(n_individual = n(),
             sum_point_value_individual = sum(point_value),
             pps_individual = sum_point_value_individual/n_individual,
             freq_individual = n_individual/total_shots_individual)%>%
      ungroup()
    dfpps <- pps%>%
      filter(PLAYER_NAME == input$playerchoose1)%>%
      summarize(final_zone, pps_individual, pps_league)%>%
      rename("Selected Player PPS" = pps_individual,
             "League PPS" = pps_league)
    
    
    dfpps <- dfpps[!duplicated(dfpps), ]
    
    dfpps <- reshape2::melt(dfpps[,c("final_zone", "Selected Player PPS", "League PPS")],id.vars = 1)
    
    
    dfpps$value <- round(dfpps$value, digit = 2)
    
    paintpps <- dfpps%>%
      filter(final_zone == "Restricted Area" | final_zone == "In The Paint (Non-RA)" )
    
    
    ggplot(paintpps)+
      geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
      labs(y = "",
           x = "")+
      theme_bw()+
      geom_text(aes(x = value, y = final_zone, fill = variable, label = value), position = position_dodge(0.9),
                vjust = 0, hjust = 1.5, size = 4, color = "white")+
      theme_minimal() +
      scale_fill_manual(values = c("blue", "gray50"))+
      #scale_x_continuous(label = scales::percent_format(accuracy = 1))+
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
      theme(legend.title = element_blank(),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 14))
    
    
    
  }, width = 900) 
  
  
  
  output$head_shot4 <- renderPlot({
    
    
    
    shots <- shots_data %>%
      mutate(head_x = 0,
             head_y = 0)%>%
      filter(PLAYER_NAME == input$playerchoose1 )
    #filter(PLAYER_NAME == "Joel Embiid")
    
    
    ggplot()+
      geom_point(data = shots, aes(x = LOC_X, y = LOC_Y), color = "white", fill = "white") +
      draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"), 
                 x = -15, y = 11, width = 30, height = 30)+
      theme_minimal()+
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
    
  }, width = 900)
  
  
  output$defenseplot <- renderPlot({
    
    
    defense <- defense_data 
    
    
    defense <- defense[!duplicated(defense$PLAYER), ]
    
    
    defense$DFG_perc<- ntile(defense$DFG., 100)
    defense$Deflection_perc<- ntile(defense$DEFLECTIONS, 100)
    defense$Contested2_perc<- ntile(defense$CONTESTED.2PT.SHOTS, 100)
    defense$Contested3_perc<- ntile(defense$CONTESTED.3PT.SHOTS, 100)
    defense$TotContested_perc<- ntile(defense$TOTAL.CONTESTED.SHOTS, 100)
    
    
    defense$screen_assists_perc<- ntile(defense$SCREEN.ASSISTS.PTS, 100)
    defense$loose_balls_perc<- ntile(defense$LOOSE.BALLS.RECOVERED, 100)
    defense$charges_perc<- ntile(defense$CHARGES.DRAWN, 100)
    defense$feet_per_min_perc<- ntile(defense$FEET.PER.MIN, 100)
    defense$avg_off_speed_perc<- ntile(defense$AVG.SPEED.OFF, 100)
    defense$avg_def_speed_perc<- ntile(defense$AVG.SPEED.DEF, 100)
    defense$avg_speed_perc<- ntile(defense$AVG.SPEED, 100)
    
    defense <- defense%>%
      rename(Total.Contested.Shots = TotContested_perc,
             Contested.3PT = Contested3_perc,
             Defensive.FG.Percentage = DFG_perc,
             Deflections = Deflection_perc,
             Contested.2PT = Contested2_perc,
             Screen.Assist.Points = screen_assists_perc,
             Loose.Balls.Recovered = loose_balls_perc,
             Charges = CHARGES.DRAWN,
             Charges.Drawn = charges_perc,
             Feet.Per.Minute = feet_per_min_perc,
             Average.Speed.Offense = avg_off_speed_perc,
             Average.Speed.Defense = avg_def_speed_perc,
             Average.Speed.Total = avg_speed_perc)
    
    
    defense_melt <- reshape2::melt(defense[,c("PLAYER", "Deflections","Defensive.FG.Percentage","Total.Contested.Shots", "Contested.3PT", 
                                              "Contested.2PT", "Screen.Assist.Points", "Loose.Balls.Recovered","Charges.Drawn",
                                              "Feet.Per.Minute","Average.Speed.Total","Average.Speed.Defense", "Average.Speed.Offense")],id.vars = 1)
    
    
    defense_melt2 <- defense_melt%>%
      filter(variable == "Deflections" | variable == "Total.Contested.Shots"| variable == "Defensive.FG.Percentage" | variable == "Total.Contested.Shots"| 
               variable == "Contested.3PT" | variable == "Contested.2PT")
    
    defense_table2 <- defense_melt2
    
    
    defense_table2 <- defense_table2%>%
      filter(PLAYER == input$playerchoose1)
    
    
    ggplot()+
      geom_point(data = defense_table2,
                 aes(x = variable, y = value, fill = value), pch = 21, color = "black", size = 20)+
      scale_fill_gradient2(midpoint = 50, low = "blue", mid = "gray", high = "red", space ="Lab" )+
      geom_text(data = defense_table2,
                aes(x = variable, y = value, label = value), color = "white", size = 7)+
      geom_segment(data = defense_table2,
                   aes(x = variable, xend = variable, y = 0, yend = value-3),
                   color = "gray", lwd = 1) +
      labs(y = "",
           x = "")+
      coord_flip() +
      theme_minimal()+
      theme(legend.position="none",
            axis.text.y = element_text( size = 16),
            axis.text.x = element_blank())+
      scale_y_continuous(limits=c(0, 100)) 
    
    
    
    
    
    
  }, width = 900) 
  
  output$hustleplot <- renderPlot({
    
    
    
    defense <- defense_data 
    
    
    defense <- defense[!duplicated(defense$PLAYER), ]
    
    
    defense$DFG_perc<- ntile(defense$DFG., 100)
    defense$Deflection_perc<- ntile(defense$DEFLECTIONS, 100)
    defense$Contested2_perc<- ntile(defense$CONTESTED.2PT.SHOTS, 100)
    defense$Contested3_perc<- ntile(defense$CONTESTED.3PT.SHOTS, 100)
    defense$TotContested_perc<- ntile(defense$TOTAL.CONTESTED.SHOTS, 100)
    
    
    defense$screen_assists_perc<- ntile(defense$SCREEN.ASSISTS.PTS, 100)
    defense$loose_balls_perc<- ntile(defense$LOOSE.BALLS.RECOVERED, 100)
    defense$charges_perc<- ntile(defense$CHARGES.DRAWN, 100)
    defense$feet_per_min_perc<- ntile(defense$FEET.PER.MIN, 100)
    defense$avg_off_speed_perc<- ntile(defense$AVG.SPEED.OFF, 100)
    defense$avg_def_speed_perc<- ntile(defense$AVG.SPEED.DEF, 100)
    defense$avg_speed_perc<- ntile(defense$AVG.SPEED, 100)
    
    defense <- defense%>%
      rename(Total.Contested.Shots = TotContested_perc,
             Contested.3PT = Contested3_perc,
             Defensive.FG.Percentage = DFG_perc,
             Deflections = Deflection_perc,
             Contested.2PT = Contested2_perc,
             Screen.Assist.Points = screen_assists_perc,
             Loose.Balls.Recovered = loose_balls_perc,
             Charges = CHARGES.DRAWN,
             Charges.Drawn = charges_perc,
             Feet.Per.Minute = feet_per_min_perc,
             Average.Speed.Offense = avg_off_speed_perc,
             Average.Speed.Defense = avg_def_speed_perc,
             Average.Speed.Total = avg_speed_perc)
    
    
    defense_melt <- reshape2::melt(defense[,c("PLAYER", "Deflections","Defensive.FG.Percentage","Total.Contested.Shots", "Contested.3PT", 
                                              "Contested.2PT", "Screen.Assist.Points", "Loose.Balls.Recovered","Charges.Drawn",
                                              "Feet.Per.Minute","Average.Speed.Total","Average.Speed.Defense", "Average.Speed.Offense")],id.vars = 1)
    
    
    hustle_melt <- defense_melt%>%
      filter(variable == "Screen.Assist.Points" | variable == "Loose.Balls.Recovered"| variable == "Charges.Drawn" | variable == "Feet.Per.Minute"| 
               variable == "Average.Speed.Total" | variable == "Average.Speed.Defense" | variable == "Average.Speed.Offense")
    
    
    hustle_table <-hustle_melt%>%
      filter(PLAYER == input$playerchoose1)
    
    
    ggplot()+
      geom_point(data = hustle_table,
                 aes(x = variable, y = value, fill = value), pch = 21, color = "black", size = 20)+
      scale_fill_gradient2(midpoint = 50, low = "blue", mid = "gray", high = "red", space ="Lab" )+
      geom_text(data = hustle_table,
                aes(x = variable, y = value, label = value), color = "white", size = 7)+
      geom_segment(data = hustle_table,
                   aes(x = variable, xend = variable, y = 0, yend = value-3),
                   color = "gray", lwd = 1) +
      labs(y = "",
           x = "",
           caption = "By: @BuckAnalytics")+
      coord_flip() +
      theme_minimal()+
      theme(legend.position="none",
            axis.text.y = element_text( size = 16),
            axis.text.x = element_blank())+
      scale_y_continuous(limits=c(0, 100)) 
    
    
    
    
    
  }, width = 900) 
  
}

shinyApp(ui = ui, server = server)







#rsconnect::showLogs()























# 
# 
# 
# compiled_shots <- shots_data%>%
#   mutate(made_shot = case_when(SHOT_MADE == "TRUE"~ 1, TRUE ~ 0),
#          point_value = case_when(made_shot == "1" & SHOT_TYPE == "3PT Field Goal" ~ 3,
#                                  made_shot == "1" & SHOT_TYPE == "2PT Field Goal" ~ 2,
#                                  TRUE ~ 0))%>%
#   group_by(BASIC_ZONE, ZONE_NAME)%>%
#   mutate(mean_zone = mean(made_shot), .groups = 'drop')%>%
#   ungroup()%>%
#   group_by(PLAYER_NAME, BASIC_ZONE, ZONE_NAME)%>%
#   mutate(mean_individual_zone = mean(made_shot))%>%
#   ungroup()%>%
#   group_by(PLAYER_NAME)%>%
#   mutate(total_shots_individual = n())%>%
#   ungroup()
# 
# 
# compiled_shots <- compiled_shots%>%
#   mutate(final_zone = case_when(
#     BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Left Side Center" ~ "Left Side Above the Break 3",
#     BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Right Side Center" ~ "Right Side Above the Break 3",
#     BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center" ~ "Center Above the Break 3",
#     BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side" ~ "Left Side Mid-Range",
#     BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side Center" ~ "Left Side Center Mid-Range",
#     BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center" ~ "Center Mid-Range",
#     BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side Center" ~ "Right Side Center Mid-Range",
#     BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side" ~ "Right Side Mid-Range",
#     TRUE ~ compiled_shots$BASIC_ZONE))
# 
# pps <- compiled_shots%>%
#   group_by(final_zone)%>%
#   mutate(n_league = n(),
#          sum_point_value_league = sum(point_value),
#     pps_league = sum_point_value_league/n_league,
#     freq_league = n_league/217220)%>%
#   ungroup()%>%
#   group_by(PLAYER_NAME, final_zone)%>%
#   mutate(n_individual = n(),
#          sum_point_value_individual = sum(point_value),
#          pps_individual = sum_point_value_individual/n_individual,
#          freq_individual = n_individual/total_shots_individual)%>%
#   ungroup()
# 
# 
# 
# # Frequency bar graphs
# 
# 
# dffreq <- pps%>%
#   filter(PLAYER_NAME == "Joel Embiid")%>%
#   summarize(final_zone, freq_individual, freq_league)%>%
#   rename("Selected Player Frequency" = freq_individual,
#          "League Frequency" = freq_league)
# 
# 
# dffreq <- dffreq[!duplicated(dffreq), ]
# 
# dffreq <- melt(dffreq[,c("final_zone", "Selected Player Frequency", "League Frequency")],id.vars = 1)
# 
# 
# paintfreq <- dffreq%>%
#   filter(final_zone == "Restricted Area" | final_zone == "In The Paint (Non-RA)" )
# 
# threefreq <- dffreq%>%
#   filter(final_zone == "Left Corner 3" | final_zone == "Left Side Above the Break 3" |
#            final_zone == "Center Above the Break 3" | final_zone == "Right Side Above the Break 3" |
#            final_zone == "Right Corner 3")
# 
# 
# midrangefreq <- dffreq%>%
#   filter(final_zone == "Right Side Mid-Range" | final_zone == "Right Side Center Mid-Range" |
#            final_zone == "Center Mid-Range" | final_zone == "Left Side Mid-Range" |
#            final_zone == "Left Side Center Mid-Range")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #Points Per Shot bar graphs
# 
# dfpps <- pps%>%
#   filter(PLAYER_NAME == "Joel Embiid")%>%
#   summarize(final_zone, pps_individual, pps_league)%>%
#   rename("Selected Player PPS" = pps_individual,
#          "League PPS" = pps_league)
# 
# 
# dfpps <- dfpps[!duplicated(dfpps), ]
# 
# dfpps <- melt(dfpps[,c("final_zone", "Selected Player PPS", "League PPS")],id.vars = 1)
# 
# 
# paintpps <- dfpps%>%
#   filter(final_zone == "Restricted Area" | final_zone == "In The Paint (Non-RA)" )
# 
# threepps <- dfpps%>%
#   filter(final_zone == "Left Corner 3" | final_zone == "Left Side Above the Break 3" |
#            final_zone == "Center Above the Break 3" | final_zone == "Right Side Above the Break 3" |
#            final_zone == "Right Corner 3")
# 
# 
# midrangepps <- dfpps%>%
#   filter(final_zone == "Right Side Mid-Range" | final_zone == "Right Side Center Mid-Range" |
#            final_zone == "Center Mid-Range" | final_zone == "Left Side Mid-Range" |
#            final_zone == "Left Side Center Mid-Range")
# 
# 
# 
# 
# ggplot(paintpps)+
#   geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
#   labs(title = "Paint Points Per Shot",
#        y = "",
#        x = "")+
#   theme_bw()+
#   geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
#             vjust = 0, hjust = 1.5, size = 4, color = "white")+
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "gray50"))+
#   scale_x_continuous(label = scales::percent_format(accuracy = 1))+
#   theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 14))
# 
# 
# 
# 
# 
# 
# ggplot(midrangepps)+
#   geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
#   labs(title = "Mid-Range Points Per Shot",
#        y = "",
#        x = "")+
#   theme_bw()+
#   geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
#           vjust = 0, hjust = 1.5, size = 4, color = "white")+
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "gray50"))+
#   scale_x_continuous(label = scales::percent_format(accuracy = 1))+
#   theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 14))
# 
# 
# ggplot(threepps)+
#   geom_col(aes(x = value, y = final_zone, fill = variable), position = position_dodge(), width = .75) +
#   labs(title = "Three Pointer Points Per Shot",
#        y = "",
#        x = "")+
#   theme_bw()+
#   geom_text(aes(x = value, y = final_zone, fill = variable, label = scales::percent(value, accuracy = .1)), position = position_dodge(0.9),
#             vjust = 0, hjust = 1.5, size = 4, color = "white")+
#   theme_minimal() +
#   scale_fill_manual(values = c("blue", "gray50"))+
#   scale_x_continuous(label = scales::percent_format(accuracy = 1))+
#   theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))+
#   theme(legend.title = element_blank(),
#         axis.text.x = element_text(size = 10),
#         axis.text.y = element_text(size = 14))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dfpps <- pps%>%
#   filter(PLAYER_NAME == "Joel Embiid")%>%
#   summarize(PLAYER_NAME,final_zone, pps_individual, pps_league)%>%
#   rename("Selected Player PPS" = pps_individual,
#          "League PPS" = pps_league)
# 
# 
# dfpps <- dfpps[!duplicated(dfpps), ]
# 
# 
# 
# court_heat <- pps%>%
#   group_by(final_zone)%>%
#   mutate(mloc_x = mean(LOC_X),
#          mloc_y = mean(LOC_Y))%>%
#   summarize(final_zone, mloc_x, mloc_y)
# 
# 
# court_ppp <- merge(x = dfpps, court_heat, by = "final_zone", all.x = TRUE)
# 
# 
# court_ppp <- court_ppp[!duplicated(court_ppp), ]
#   #mutate(ind_pps_round = round(court_ppp$"Selected Player PPS", 2))
# 
# 
# 
# court_ppp$ind_ppsround <- round(court_ppp$`Selected Player PPS`, digit = 2)
# 
# 
# 
# 
# 
# 
#   plot_court(court_themes$white, use_short_three = F)+
#     geom_point(data = court_ppp,
#                aes(x = mloc_x, y = mloc_y, fill = court_ppp$ind_pps_round), pch = 21, size = 14)+
#     geom_text(data = court_ppp,
#       aes(x = mloc_x, y = mloc_y, label = court_ppp$ind_pps_round), size = 10, vjust = -1.5)+
#     #scale_color_manual(values = c(palette), aesthetics = c("fill", "color"))+
#     scale_color_gradient2(midpoint = median(pps$pps_individual), low = "blue", mid = "white", high = "red")+
#     scale_x_continuous(limits = c(-27.5, 27.5)) +
#     scale_y_continuous(limits = c(0, 45))+
#     theme(
#       legend.position="none",
#       #legend.title = element_blank(),
#       plot.background = element_rect(fill="white", color = "white"),
#       panel.background = element_rect(fill="white", color = "white"),
#       plot.title = element_text(hjust = 0.5, size = 22, vjust = -13, face = "bold", colour = "black"),
#       plot.subtitle = element_text(hjust = 0.5, size = 10, vjust = -22, face = "bold", colour = "black"))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#   plot_court(court_themes$white, use_short_three = F)+
#     geom_point(data = court_ppp,
#                aes(x = mloc_x, y = mloc_y, fill = court_ppp$ind_ppsround), pch = 21, color = "black", size = 12)+
#     scale_fill_gradient2(midpoint = median(pps$pps_individual), low = "blue", mid = "white", high = "red")+
#     geom_text(data = court_ppp,
#               aes(x = mloc_x, y = mloc_y, label = court_ppp$ind_ppsround), size = 8, vjust = -1.5)+
#     geom_text_repel()+
#     #scale_color_manual(values = c(palette), aesthetics = c("fill", "color"))+
#     scale_color_gradient2(midpoint = median(pps$pps_individual), low = "blue", mid = "white", high = "red")+
#     scale_x_continuous(limits = c(-27.5, 27.5)) +
#     scale_y_continuous(limits = c(0, 45))+
#     theme(
#       legend.position="none",
#       #legend.title = element_blank(),
#       plot.background = element_rect(fill="white", color = "white"),
#       panel.background = element_rect(fill="white", color = "white"),
#       plot.title = element_text(hjust = 0.5, size = 22, vjust = -13, face = "bold", colour = "black"),
#       plot.subtitle = element_text(hjust = 0.5, size = 10, vjust = -22, face = "bold", colour = "black"))
# 
# 
# 
# 
# 
# 
# median(pps$pps_individual)
# 
#   dfpps <- melt(dfpps[,c("final_zone", "Selected Player PPS", "League PPS")],id.vars = 1)
# 
# 
#   three <- dfm%>%
#     filter(final_zone == "Left Corner 3" | final_zone == "Left Side Above the Break 3" |
#              final_zone == "Center Above the Break 3" | final_zone == "Right Side Above the Break 3" |
#              final_zone == "Right Corner 3")
# 
# 
# 
#   action_type <- compiled_shots%>%
#     group_by(PLAYER_NAME, ACTION_TYPE)%>%
#     mutate(total_shots_individual_action_type = n(),
#            sum_point_value_individual_action_type = sum(point_value),
#            pps_individual_action_type = sum_point_value_individual_action_type/total_shots_individual_action_type,
#            freq_individual_action_type = total_shots_individual_action_type/total_shots_individual)%>%
#     ungroup()%>%
#     group_by(ACTION_TYPE)%>%
#     mutate(freq_percentile = ntile(action_type$freq_individual_action_type, 100))
# 
# 
#   action_type$action_type_freq_percentile <- ntile(action_type$freq_individual_action_type, 100)
#   action_type$action_type_pps_percentile <- ntile(action_type$pps_individual_action_type, 100)
# 
# 
#   table_action_type <- action_type%>%
#     filter(PLAYER_NAME == "Devin Booker")%>%
#     #filter(PLAYER_NAME == input$playerchoose1)%>%
#     summarize(ACTION_TYPE, freq_individual_action_type,action_type_freq_percentile,pps_individual_action_type, action_type_pps_percentile)%>%
#     setorder(cols = - "freq_individual_action_type")%>%
#     filter(freq_individual_action_type > .02)
# 
#   table_action_type <- table_action_type[!duplicated(table_action_type), ]
#   table_action_type <- table_action_type[!duplicated(table_action_type$ACTION_TYPE), ]
# 
#   table_action_type%>%
#     gt() %>%
#     gtExtras::gt_theme_espn()%>%
#     cols_label(ACTION_TYPE = "Action Type",
#                freq_individual_action_type = "Frequency",
#                action_type_freq_percentile = "Percentile",
#                pps_individual_action_type = "Points per Shot",
#                action_type_pps_percentile = "Percentile")%>%
# 
#     fmt_percent(
#       columns = freq_individual_action_type,
#       decimals = 1)%>%
#     fmt_number(columns = pps_individual_action_type,
#                decimals = 2)%>%
#     data_color(columns = action_type_freq_percentile,
#                palette = c("blue","steelblue1","white", "indianred1", "red"),
#                domain = c(-0,100))%>%
#     data_color(columns = action_type_pps_percentile,
#                palette = c("blue","steelblue1","white", "indianred1", "red"),
#                domain = c(-0,100))
# 
# 
# 
# 
#   zone_range <- compiled_shots%>%
#     group_by(PLAYER_NAME, ZONE_RANGE)%>%
#     mutate(total_shots_individual_zone_range = n(),
#            sum_point_value_individual_zone_range = sum(point_value),
#            pps_individual_zone_range = sum_point_value_individual_zone_range/total_shots_individual_zone_range,
#            freq_individual_zone_range = total_shots_individual_zone_range/total_shots_individual)%>%
#     ungroup()
# 
#   zone_range$zone_range_freq_percentile <- ntile(zone_range$freq_individual_zone_range, 100)
#   zone_range$zone_range_pps_percentile <- ntile(zone_range$pps_individual_zone_range, 100)
# 
#   table_zone_range <- zone_range%>%
#     filter(PLAYER_NAME == "Joel Embiid")%>%
#     #filter(PLAYER_NAME == input$playerchoose1)%>%
#     summarize(ZONE_RANGE, freq_individual_zone_range,zone_range_freq_percentile,pps_individual_zone_range, zone_range_pps_percentile)%>%
#     setorder(cols = - "freq_individual_zone_range")
# 
#   table_zone_range <- table_zone_range[!duplicated(table_zone_range), ]
#   table_zone_range <- table_zone_range[!duplicated(table_zone_range$ZONE_RANGE), ]
# 
#   table_zone_range%>%
#     gt() %>%
#     gtExtras::gt_theme_espn()%>%
#     cols_label(ZONE_RANGE = "Shot Distance",
#                freq_individual_zone_range = "Frequency",
#                zone_range_freq_percentile = "Percentile",
#                pps_individual_zone_range = "Points per Shot",
#                zone_range_pps_percentile = "Percentile")%>%
# 
#     fmt_percent(
#       columns = freq_individual_zone_range,
#       decimals = 1)%>%
#     fmt_number(columns = pps_individual_zone_range,
#                decimals = 2)%>%
#     data_color(columns = zone_range_freq_percentile,
#                palette = c("blue","steelblue1","white", "indianred1", "red"),
#                domain = c(-0,100))%>%
#     data_color(columns = zone_range_pps_percentile,
#                palette = c("blue","steelblue1","white", "indianred1", "red"),
#                domain = c(-0,100))
# 
#  defense <- defense_data
# 
#  defense$DFG_perc<- ntile(defense$DFG., 100)
#  defense$Deflection_perc<- ntile(defense$DEFLECTIONS, 100)
#  defense$Contested2_perc<- ntile(defense$CONTESTED.2PT.SHOTS, 100)
#  defense$Contested3_perc<- ntile(defense$CONTESTED.3PT.SHOTS, 100)
#  defense$TotContested_perc<- ntile(defense$TOTAL.CONTESTED.SHOTS, 100)
# 
# 
#  defense$screen_assists_perc<- ntile(defense$SCREEN.ASSISTS.PTS, 100)
#  defense$loose_balls_perc<- ntile(defense$LOOSE.BALLS.RECOVERED, 100)
#  defense$charges_perc<- ntile(defense$CHARGES.DRAWN, 100)
#  defense$feet_per_min_perc<- ntile(defense$FEET.PER.MIN, 100)
#  defense$avg_off_speed_perc<- ntile(defense$AVG.SPEED.OFF, 100)
#  defense$avg_def_speed_perc<- ntile(defense$AVG.SPEED.DEF, 100)
#  defense$avg_speed_perc<- ntile(defense$AVG.SPEED, 100)
# 
#  defense <- defense%>%
#    rename(Total.Contested.Shots = TotContested_perc,
#           Contested.3PT = Contested3_perc,
#           Defensive.Field.Goal.Percentage = DFG_perc,
#           Deflections = Deflection_perc,
#           Contested.2PT = Contested2_perc,
#           Screen.Assist.Points = screen_assists_perc,
#           Loose.Balls.Recovered = loose_balls_perc,
#           Charges = CHARGES.DRAWN,
#           Charges.Drawn = charges_perc,
#           Feet.Per.Minute = feet_per_min_perc,
#           Average.Speed.Offense = avg_off_speed_perc,
#           Average.Speed.Defense = avg_def_speed_perc,
#           Average.Speed.Total = avg_speed_perc)
# 
# 
#  defense_melt <- reshape2::melt(defense[,c("PLAYER", "Deflections","Defensive.Field.Goal.Percentage","Total.Contested.Shots", "Contested.3PT",
#                                            "Contested.2PT", "Screen.Assist.Points", "Loose.Balls.Recovered","Charges.Drawn",
#                                  "Feet.Per.Minute","Average.Speed.Total","Average.Speed.Defense", "Average.Speed.Offense")],id.vars = 1)
# 
# 
#  defense_melt2 <- defense_melt%>%
#    filter(variable == "Deflections" | variable == "Total.Contested.Shots"| variable == "Defensive.Field.Goal.Percentage" | variable == "Total.Contested.Shots"|
#             variable == "Contested.3PT" | variable == "Contested.2PT")
# 
# 
# 
# defense_table2 <-defense_melt2%>%
# filter(PLAYER == "Devin Booker")
# 
# 
#  ggplot()+
#  geom_point(data = defense_table2,
#             aes(x = variable, y = value, fill = value), pch = 21, color = "black", size = 20)+
#    scale_fill_gradient2(midpoint = 50, low = "blue", mid = "gray", high = "red", space ="Lab" )+
#    geom_text(data = defense_table2,
#              aes(x = variable, y = value, label = value), color = "white", size = 7)+
#    geom_segment(data = defense_table2,
#                 aes(x = variable, xend = variable, y = 0, yend = value-2),
#                 color = "gray", lwd = 1) +
#    labs(y = "",
#         x = "")+
#    coord_flip() +
#    theme_minimal()+
#   theme(legend.position="none",
#         axis.text.y = element_text( size = 16),
#         axis.text.x = element_blank())+
#    scale_y_continuous(limits=c(0, 100))
# 
# 
#  hustle_melt <- defense_melt%>%
#    filter(variable == "Screen.Assist.Points" | variable == "Loose.Balls.Recovered"| variable == "Charges.Drawn" | variable == "Feet.Per.Minute"|
#             variable == "Average.Speed.Total" | variable == "Average.Speed.Defense" | variable == "Average.Speed.Offense")
# 
# 
# #
# # hustle_table <-hustle_melt%>%
# #    filter(PLAYER == "Devin Booker")
# #
# 
# ggplot()+
#   geom_point(data = hustle_table,
#              aes(x = variable, y = value, fill = value), pch = 21, color = "black", size = 20)+
#   scale_fill_gradient2(midpoint = 50, low = "blue", mid = "gray", high = "red", space ="Lab" )+
#   geom_text(data = hustle_table,
#             aes(x = variable, y = value, label = value), color = "white", size = 7)+
#   geom_segment(data = hustle_table,
#                aes(x = variable, xend = variable, y = 0, yend = value-2),
#                color = "gray", lwd = 1) +
#   labs(y = "",
#        x = "")+
#   coord_flip() +
#   theme_minimal()+
#   theme(legend.position="none",
#         axis.text.y = element_text( size = 16),
#         axis.text.x = element_blank())+
#   scale_y_continuous(limits=c(0, 100))
# 
# 
# 
# library(cowplot)
# 
# 
# shots <- shots_data %>%
#   filter(GAME_DATE == "04-04-2023") %>%
#   filter(PLAYER_NAME == "Joel Embiid" )
# 
# 
# # ------------------------- Create Plot
# 
# plot_court(court_themes$white, use_short_three = F) +
#   # ---- plot shot "points" with x & y locations
# 
#   ggplot()+
#   geom_point(data = shots, aes(x = LOC_X, y = LOC_Y, color = SHOT_MADE, fill = SHOT_MADE),
#              size =3, shape = 21, stroke = .5) +
#   # ---- plot player hedashot (remove these 2 lines if you're plotting multiple players!)
#   draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"),
#              x = -5, y = 10, width = 9, height = 9)
# 
#   draw_image(paste0("https://cdn.nba.com/headshots/nba/latest/1040x760/", unique(shots$PLAYER_ID), ".png"),
#              x = -19, y = -1.3, width = 9, height = 9)
# 
# 
# https://ryanmiele14.shinyapps.io/2022_nba_player_dashboard/
