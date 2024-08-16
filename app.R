library(dplyr)
library(ggplot2)
library(GGally)
library(mclust)
library(shiny)
library(plotly)
library(tidyr)
library(cluster)
library(factoextra)
library(formattable)
library(purrr)
library(RColorBrewer)
library(stringr)
library(knitr)
library(ClusterR)
library(stringi)
library(boot)
library(htmlwidgets)
library(gridExtra)
library(tsibble)
library(fable)
library(forecast)
library(bslib)
library(reshape)
library(viridis)
library(bsicons)
library(DT)
library(tibble)


statcast <- read.csv("statcast.csv")





## Data Cleaning ---------------------------------------------------------
# Column Names
statcast <- statcast %>%
  rename_with(~c("name", "avg", "obp", "slg", "ops"),
              c(last_name..first_name, batting_avg, 
                on_base_percent, slg_percent, on_base_plus_slg)) %>%
  mutate(hr_rate = home_run/pa) %>%
  relocate(avg, obp, slg, ops, woba, .after = pa)



# Converts stats to scale above/below Lg Avgs for that year
statcast_adj <- statcast %>%
  group_by(year) %>%
  mutate(across(c(k_percent, 
                  bb_percent, 
                  xba, 
                  xslg, 
                  xwoba, 
                  xwobacon,
                  barrel_batted_rate, 
                  hard_hit_percent,
                  z_swing_miss_percent, 
                  oz_swing_percent, 
                  oz_swing_miss_percent, 
                  hr_rate),
                ~ . / weighted.mean(., pa) * 100, 
                .names = "{.col}+")) %>%
  rename_with(~ gsub("percent", "pct", .), ends_with("+")) %>%
  rename_with(~ gsub("swing_miss", "whiff", .), ends_with("+")) %>%
  rename_with(~ gsub("batted_", "", .), ends_with("+")) %>%
  select(1:10, ends_with("+")) %>%
  filter(pa >= 200) %>%
  as.data.frame()




## Principal Component Analysis (PCA) ------------------------------------
set.seed(789)
pca <- prcomp(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], 
              scale. = TRUE)
pca_summary <- summary(pca)





# Get first 3 principal components
pc <- statcast_adj %>%
  cbind(pca$x[, 1:3] %>% as.data.frame()) %>%
  mutate(PC1 = as.numeric(100 + 15 * scale(PC1) * sd(PC1)),
         PC2 = as.numeric(100 + 15 * scale(PC2) * sd(PC2)),
         PC3 = as.numeric(100 + 15 * scale(PC3) * sd(PC3))) %>%
  ungroup() %>%
  select(PC1, PC2, PC3) %>%
  as.data.frame()





## Gaussian Mixture Model (GMM) ------------------------------------------
set.seed(123)
models <- c("EEI")
clusters <- 7
gmm_mlb <- Mclust(pc, G = clusters, modelNames = models)





## Exploring Model Results -----------------------------------------------
# Joins Cluster Probabilities with Player Data
archetypes <- statcast_adj %>%
  cbind(Cluster = gmm_mlb$classification, 
        round(predict(gmm_mlb, data = pc)$z, 3),
        as.data.frame(pc)) %>%
  rename_with(~ str_replace(., "^[0-9]+$", ~ paste0("probC", .))) %>%
  select(-Cluster, Cluster) %>%
  mutate(across(c(ends_with('+')), round, 2)) %>%
  mutate(maxProb = do.call(pmax, select(., starts_with("probC"))),
         PlayerYear = paste(str_split(name, ", ", simplify = T)[, 2],
                            str_split(name, ", ", simplify = T)[, 1],
                            year, sep = " "),
         iso = slg - avg) %>%
  relocate(iso, .after = ops) %>%
  arrange(PlayerYear)



# Aggregates Results to get Average Attributes of each Cluster
archetypes_agg <- archetypes %>%
  pivot_longer(cols = starts_with("probC"), 
               names_to = "ClusterPivot",
               names_prefix = "probC",
               values_to = "weight",
               names_repair = "unique") %>%
  group_by(Cluster) %>%
  summarize(across(ends_with("+") | c(avg, obp, slg, ops, woba, pa), 
                   ~ sum(. * weight, na.rm = TRUE) / 
                     sum(weight, na.rm = TRUE),
                   .names = "{col}")) %>%
  mutate(across(ends_with('+'), round, 2)) %>%
  mutate(iso = slg - avg) %>%
  relocate(iso, .before = woba) %>%
  mutate(across(c(avg, obp, slg, ops, iso, woba), round, 3)) %>%
  mutate(across(pa, round, 0)) %>%
  cbind(t(round(gmm_mlb$parameters$mean, 4))) %>% 
  as.data.frame()










## RShiny Application ---------------------------------------------------- 
# Define UI
cluster_names <- c("Walker", "Free-Swinging Power Hitter", 
                   "Patient Contact Hitter", "TTO Hitter", 
                   "Chaser", "Contact Seeker", "Superstar")


statcast1 <- statcast %>% mutate(
  PlayerYear = paste(str_split(name, ", ", simplify = T)[, 2],
                     str_split(name, ", ", simplify = T)[, 1],
                     year, sep = " "))


playerdata <- merge(archetypes, statcast1, by = "PlayerYear")



# List of Cards
cards <- list(
  card(full_screen = TRUE,
       card_header("Archetype Breakdown", class = "bg-dark"),
       plotOutput("bar_chart")),
  card(full_screen = TRUE,
       card_header("Comparable Players", class = "bg-dark"),
       uiOutput("comps")),
  card(full_screen = TRUE,
       card_header("PC Scores over Time", class = "bg-dark"),
       plotOutput("linechart")),
  card(full_screen = TRUE,
       card_header("Season Stats", class = "bg-dark"),
       uiOutput("stats"))
)



choose_players <- selectizeInput(inputId = "player1", 
                                 label = "Select Player:",
                                 choices = NULL,
                                 selected = 'Juan Soto 2021',
                                 options = list(maxOptions = 3000))



# Define UI
ui <- page_sidebar(
  title = "Player Profile",
  sidebar = choose_players,
  layout_columns(col_widths = c(3, 5, 4, 9, 3),
                 row_heights = c(3, 4),
                 cards[[2]], 
                 cards[[3]],
                 layout_columns(fill = TRUE,
                                min_height = "200px",
                                col_widths = c(12),
                                row_heights = c(1,1,1),
                                uiOutput("vbox1"),
                                uiOutput("vbox2"),
                                uiOutput("vbox3")),
                 cards[[1]],
                 cards[[4]])
)



# Define Server
server <- function(input, output, session) {
  
  
  updateSelectizeInput(session, 'player1', 
                       choices = archetypes$PlayerYear,
                       selected = 'Aaron Judge 2023',
                       server = TRUE)
  
  
  # Boxes for each PC Value
  output$vbox1 <- renderUI({
    value_box(
      title = "Contact Quality",
      value = archetypes %>%
        filter(PlayerYear == input$player1) %>%
        select(PC1) %>%
        round(0) %>%
        pull(),
      showcase = bs_icon("fire", size = "0.75em"),
      theme = "red"
    )
  })
  
  output$vbox2 <- renderUI({
    value_box(
      title = "Contact Frequency",
      value = archetypes %>%
        filter(PlayerYear == input$player1) %>%
        select(PC2) %>%
        round(0) %>%
        pull(),
      showcase = bs_icon("crosshair", size = "0.75em"),
      theme = "blue"
    )
  })
  
  output$vbox3 <- renderUI({
    value_box(
      title = "Swing Aggression",
      value = archetypes %>%
        filter(PlayerYear == input$player1) %>%
        select(PC3) %>%
        round(0) %>%
        pull(),
      showcase = bs_icon("eye", size = "0.75em"),
      theme = "green"
    )
  })
  
  
  
  # Comparable Players Table
  output$comps <- renderUI({
    player <- archetypes %>%
      filter(PlayerYear == input$player1) %>%
      select(PC1, PC2, PC3)
    
    calculate_match <- function(pc1, pc2, pc3) {
      100 - sqrt(pc1^2 + pc2^2 + pc3^2)
    }
    
    matches <- archetypes %>%
      filter(player_id != archetypes %>% 
               filter(PlayerYear == input$player1) %>% 
               select(player_id) %>%
               pull()) %>%
      rowwise() %>%
      mutate(Match = max(calculate_match(PC1 - player$PC1,
                                         PC2 - player$PC2,
                                         PC3 - player$PC3), 0)) %>%
      arrange(desc(Match)) %>%
      select(PlayerYear, Match) %>%
      head(n = 10) %>%
      mutate(Match = sprintf("%.1f%%", Match))
    
    datatable(matches, 
              fillContainer = FALSE, 
              style = "jqueryui",
              filter = "none",
              selection = "none",
              options = list(dom = 't'),
              rownames = FALSE)
  })
  
  
  
  # Basic Output Stats
  output$stats <- renderUI({
    
    
    playerdata <- playerdata %>%
      filter(PlayerYear == input$player1) %>%
      select(pa.x, avg.x, obp.x, slg.x, ops.x, 
             iso, woba.x, home_run.x, xba, xslg, 
             xwoba, xwobacon, barrel_batted_rate, 
             hard_hit_percent, k_percent, bb_percent,
             z_swing_miss_percent, oz_swing_percent, 
             oz_swing_miss_percent) %>%
      mutate(across(c(avg.x, obp.x, slg.x, ops.x, iso, woba.x, 
                      xba, xslg, xwoba, xwobacon), 
                    ~format(round(.x, 3), nsmall = 3))) %>%
      mutate(across(c(barrel_batted_rate, hard_hit_percent, 
                      k_percent, bb_percent, 
                      z_swing_miss_percent, oz_swing_percent, 
                      oz_swing_miss_percent), 
                    ~format(round(.x, 1), nsmall = 1))) %>%
      rename_with(~c("Plate Appearances", "AVG", "OBP", "SLG", 
                     "OPS", "ISO", "wOBA", "Home Runs",
                     "xBA", "xSLG", "xwOBA",
                     "xwOBAcon", "Barrel %", "Hard Hit %",
                     "K%", "BB%", "Zone Whiff %",
                     "O-Zone Swing %", "O-Zone Whiff %"),
                  c(pa.x, avg.x, obp.x, slg.x, ops.x, 
                    iso, woba.x, home_run.x, xba, xslg, 
                    xwoba, xwobacon, barrel_batted_rate, 
                    hard_hit_percent, k_percent, bb_percent,
                    z_swing_miss_percent, oz_swing_percent, 
                    oz_swing_miss_percent)) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column("Metric")
    
    
    datatable(playerdata, 
              fillContainer = FALSE, 
              style = "jqueryui", 
              filter = "none",
              selection = "none",
              options = list(dom = 't', pageLength = 1000),
              rownames = FALSE)
  })
  
  
  
  # PCs Over Time Line Chart
  output$linechart <- renderPlot({
    data <- archetypes %>%
      filter(player_id == archetypes %>% 
               filter(PlayerYear == input$player1) %>% 
               select(player_id) %>%
               pull())
    
    ggplot(data, aes(x = year)) +
      geom_line(aes(y = PC1, color = "PC1")) +
      geom_point(aes(y = PC1, color = "PC1")) +
      geom_line(aes(y = PC2, color = "PC2")) +
      geom_point(aes(y = PC2, color = "PC2")) +
      geom_line(aes(y = PC3, color = "PC3")) +
      geom_point(aes(y = PC3, color = "PC3")) +
      geom_hline(aes(yintercept = 100, , 
                     linetype = "League Average"),
                 color = "black") +
      scale_linetype_manual(values = 2) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), 
                                      by = 1), 
                         limits = c(min(data$year), max(data$year))) +
      scale_y_continuous(breaks = seq(0, 300, by = 20)) +
      scale_color_manual(values = c("PC1" = "darkred", 
                                    "PC2" = "cornflowerblue", 
                                    "PC3" = "darkgreen"),
                         labels = c("PC1" = "Contact Quality", 
                                    "PC2" = "Contact Frequency", 
                                    "PC3" = "Swing Aggression")) +
      labs(x = "Year", 
           y = "PC Value", 
           color = "Principal Component") + 
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  
        axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold"),  
        axis.text.y = element_text(size = 12, face = "bold")
      )
  })
  
  
  
  # Player Archetypes Bar Chart
  output$bar_chart <- renderPlot({
    player_year_data <- 
      archetypes[archetypes$PlayerYear == input$player1, ] %>% 
      mutate_at(vars(starts_with('probC')), ~ . * 100)
    
    
    long_data <- player_year_data %>%
      pivot_longer(cols = starts_with('probC'), 
                   names_to = 'ClusterPivot', 
                   values_to = 'Percentage',
                   names_repair = "unique") %>%
      mutate(Cluster = factor(Cluster, 
                              levels = names(player_year_data)
                              [grepl("probC", names(player_year_data))]))
    
    
    ggplot(long_data, aes(x = Percentage, 
                          y = ClusterPivot, 
                          fill = ClusterPivot)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(Percentage, 1), size = 12), 
                hjust = -0.1) +
      scale_fill_viridis(discrete = TRUE) +
      scale_y_discrete(labels = cluster_names) +
      scale_x_continuous(breaks = seq(0, 100, by = 10), 
                         limits = c(0, 100)) +
      labs(x = 'Percentage',
           y = 'Archetype') +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.text.x = element_text(size = 12, face = "bold"),  
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.position = 'none'
      )
  })
}

shinyApp(ui, server)
