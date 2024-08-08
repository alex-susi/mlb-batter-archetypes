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


statcast <- read.csv("statcast.csv")





## Data Cleaning ---------------------------------------------------------
# Column Names
statcast <- statcast %>%
  rename_with(~c("name", "avg", "obp", "slg", "ops"),
              c(last_name..first_name, batting_avg, 
                on_base_percent, slg_percent, on_base_plus_slg)) %>%
  mutate(hr_rate = home_run/pa) %>%
  relocate(avg, obp, slg, ops, woba, .after = pa)



# League Averages
lg_avgs <- statcast %>%
  group_by(year) %>%
  summarise_at(.vars = vars(c(4:ncol(statcast) - 1)), 
               .funs = c(mean = "mean")) %>%
  as.data.frame()

lg_avgs %>%
  select(1, 8:ncol(lg_avgs)) %>%
  kable()



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
  rename_with(~ gsub("swing", "sw", .), ends_with("+")) %>%
  rename_with(~ gsub("batted_", "", .), ends_with("+")) %>%
  select(1:10, ends_with("+")) %>%
  filter(pa >= 200) %>%
  as.data.frame()

write.csv(statcast_adj, "statcast_adj.csv")



# Correlation Matrix
ggpairs(statcast_adj, columns = grep("\\+$", names(statcast_adj)))



head(scale(statcast_adj[,c(grep("\\+$", names(statcast_adj)))]))
lapply(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], sd)
lapply(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], range)
colMeans(statcast_adj[,c(grep("\\+$", names(statcast_adj)))])

# Current best
# 200 PA's, 3 PC's, 7 clusters, No SS or Avg EV, EEI (0.1732) 





## Principal Component Analysis (PCA) ------------------------------------
set.seed(789)
pca <- prcomp(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], 
              scale. = TRUE)
pca_summary <- summary(pca)



(pca$sdev)^2 # Eigenvalues
round(pca$rotation[,1:3], 4) # Matrix of eigenvectors
write.csv(round(pca$rotation[,1:3], 4), "pc.csv")
pca$center # Mean of each unscaled predictor
pca$scale # St Dev of each unscaled predictor
pca$x # Input data transformed into PCA space



# Elbow Curve for PCA
screeplot(pca, type = "lines", col = "blue")

plot(c(1:ncol(pca_summary$importance)), 
     pca_summary$importance[3, ],
     xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance",
     main = "Principal Components vs Cumulative Variance",
     type = "b", 
     col = "blue")




# Get first 3 principal components
pc <- statcast_adj %>%
  cbind(pca$x[, 1:3] %>% as.data.frame()) %>%
  mutate(PC1 = as.numeric(100 + 15 * scale(PC1) * sd(PC1)),
         PC2 = as.numeric(100 + 15 * scale(PC2) * sd(PC2)),
         PC3 = as.numeric(100 + 15 * scale(PC3) * sd(PC3))) %>%
  ungroup() %>%
  select(PC1, PC2, PC3) %>%
  as.data.frame()



# Plot Clusters in 3D PCA Space
plot_ly(x = pc[, "PC1"], 
        y = pc[, "PC2"], 
        z = pc[, "PC3"], 
        type = "scatter3d", 
        mode = "markers",
        hovertemplate = paste(
          "<b>PC1:</b> %{x:.0f}<br>",
          "<b>PC2:</b> %{y:.0f}<br>",
          "<b>PC3:</b> %{z:.0f}<extra></extra>"),
        marker = list(size = 5, 
                      color = rgb(normalize(pc[, "PC1"]), 
                                  normalize(pc[, "PC2"]), 
                                  normalize(pc[, "PC3"])), 
                      colorscale = "Viridis", 
                      showscale = TRUE)) %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = "3D Scatter Plot of Principal Components")



# Silhouette Scores to determine # of Clusters
silhouette_scores <- data.frame("Model" = character(), 
                                "Clusters" = numeric(),
                                "SilhouetteScore" = numeric())

models <- c("EII", "VII", "EEI", "VEI", "EVI", 
            "VVI", "EEE", "VEE", "EVE", "VVE",
            "EEV", "VEV", "EVV", "VVV")

for (m in models) {
  for (c in c(4:12)) {
    
    set.seed(23)
    gmm_model <- Mclust(pc, G = c, modelNames = m)
    silhouette_score <- silhouette(gmm_model$classification, dist(pc))
    
    silhouette_scores[nrow(silhouette_scores) + 1, ] <- 
      c(m, c, round(mean(silhouette_score[, 3]), 4))
    
  }
}


# Plot Silhouette Scores
plot(silhouette_scores[,2], silhouette_scores[,3], type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Silhouette Score", 
     main = "Silhouette Score vs. Number of Clusters")

silhouette_scores %>%
  arrange(desc(SilhouetteScore)) %>%
  #filter(Clusters == 7) %>%
  filter(Model == "EEI") %>%
  head(n = 20)





## Gaussian Mixture Model (GMM) ------------------------------------------
set.seed(123)
models <- c("EEI")
clusters <- 7
gmm_mlb <- Mclust(pc, G = clusters, modelNames = models)

# Cluster Centers
round(gmm_mlb$parameters$mean, 4)





## Bootstrapping for Validation ------------------------------------------

# Consistent cluster labeling
get_consistent_labels <- function(gmm) {
  cluster_means <- apply(gmm$parameters$mean, 2, mean)
  order_clusters <- order(cluster_means)
  consistent_labels <- match(gmm$classification, order_clusters)
  return(consistent_labels)
}

original_labels <- get_consistent_labels(gmm_mlb)



# Bootstrapping function
bootstrap_gmm <- function(data, n_clusters) {
  sample_indices <- sample(1:nrow(data), 
                           replace = TRUE)
  bootstrap_sample <- data[sample_indices, ]
  bootstrap_model <- Mclust(bootstrap_sample, 
                            G = n_clusters,
                            modelNames = models)
  consistent_labels <- get_consistent_labels(bootstrap_model)
  return(adjustedRandIndex(original_labels[sample_indices], 
                           consistent_labels))
}


ari_scores <- replicate(500, bootstrap_gmm(pc, clusters), 
                        simplify = FALSE) %>% unlist()

# Summary statistics of ARI scores
summary(ari_scores)

# Histogram of ARI scores
hist(ari_scores, breaks = 20, 
     main = "Distribution of ARI Scores", 
     xlab = "Adjusted Rand Index")





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

#write.csv(archetypes, "archetypes.csv")



# Aggregates Results to get Average Attributes of each Cluster
archetypes_agg <- archetypes %>%
  pivot_longer(cols = starts_with("probC"), 
               names_to = "ClusterPivot",
               names_prefix = "probC",
               values_to = "weight",
               names_repair = "unique") %>%
  group_by(Cluster) %>%
  summarize(across(ends_with("+") | c(avg, obp, slg, ops, woba, pa), 
      ~ sum(. * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE),
      .names = "{col}")) %>%
  mutate(across(ends_with('+'), round, 2)) %>%
  mutate(iso = slg - avg) %>%
  relocate(iso, .before = woba) %>%
  mutate(across(c(avg, obp, slg, ops, iso, woba), round, 3)) %>%
  mutate(across(pa, round, 0)) %>%
  cbind(t(round(gmm_mlb$parameters$mean, 4))) %>% 
  as.data.frame()



# Plot Clusters in 3D PCA Space
cluster_plot <- plot_ly(archetypes,
                        x = ~ PC1, 
                        y = ~ PC2, 
                        z = ~ PC3,
                        color = ~ as.factor(Cluster),
                        colors = "viridis",
                        type = "scatter3d",
                        mode = "markers",
                        text = ~ PlayerYear,
                        hovertemplate = paste(
                          "<b>%{text}</b><br>",
                          "<b>PC1:</b> %{x:.0f}<br>",
                          "<b>PC2:</b> %{y:.0f}<br>",
                          "<b>PC3:</b> %{z:.0f}<extra></extra>")) %>%
  layout(title = "3D Plot of Batter Archetypes",
         scene = list(xaxis = list(title = 'PC1 - Contact Quality'),
                      yaxis = list(title = 'PC2 - Contact Frequency'),
                      zaxis = list(title = 'PC3 - Swing Aggression')))

#saveWidget(as_widget(cluster_plot), "cluster_plot.html")



# Batter-Specific Results
archetypes %>%
  #filter(name == "Gallo, Joey") %>%
  #filter(name == "Stanton, Giancarlo") %>%
  filter(name == "Bichette, Bo" | name == "Bregman, Alex") %>%
  #filter(name == "Rizzo, Anthony") %>%
  #filter(name == "Arraez, Luis") %>%
  #filter(name == "Báez, Javier") %>%
  relocate(PlayerYear, .before = pa) %>%
  filter(year == 2023) #%>%
  #select(-name, -year) %>% write.csv("BregmanBichette.csv")


# Proportion of Batters in Each Cluster
archetypes %>%
  group_by(Cluster) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n),3)) %>%
  as.data.frame()


archetypes %>%
  arrange(desc(PC1)) %>%
  #filter(year == 2023) %>%
  select(-c(player_id, name, year)) %>%
  distinct(PlayerYear, .keep_all = TRUE) %>%
  select(PlayerYear, everything()) %>%
  #filter(year %in%  c(2023, 2022, 2021)) %>%
  head(n = 10) 


archetypes %>%
  group_by(Cluster) %>%
  slice_max(order_by = maxProb, n = 1) %>%
  ungroup() %>%
  select(-c(player_id)) %>%
  #select(c(name, year, pa, starts_with('probC'), Cluster, maxProb)) %>%
  as.data.frame()# %>%
  #write.csv("top10.csv", row.names = FALSE, fileEncoding = "latin1")



# Principal Component Leaders
get_leaders <- function(df, col, n, y) {
  top <- df %>%
    filter(year == y) %>%
    arrange(desc(!!sym(col))) %>%
    slice_head(n = n) %>%
    mutate(rank = row_number(), type = paste(col))
  
  bottom <- df %>%
    filter(year == y) %>%
    arrange(!!sym(col)) %>%
    slice_head(n = n) %>%
    mutate(rank = row_number(), type = paste(col))
  
  bind_rows(top, bottom)
}


bind_rows(lapply(c("PC1", "PC2", "PC3"), function(col) 
  get_leaders(archetypes, col, 10, 2015))) %>%
  select(PlayerYear, PC1, PC2, PC3, Cluster, rank) #%>%
  #write.csv("pcleaders.csv", row.names = FALSE, fileEncoding = "latin1")

  




## Stability of Each PC and Cluster --------------------------------------
lagged <- archetypes %>% 
  select(name, year, player_id, 
         c(starts_with("PC")), 
         c(starts_with("probC"))) %>%
  group_by(player_id) %>%
  arrange(player_id, year) %>%
  mutate(across(starts_with("PC"), lag, 
                .names = "{.col}_lag"),
         across(starts_with("probC"), lag, 
                .names = "{.col}_lag")) %>%
  filter_at(vars(ends_with("_lag")), all_vars(!is.na(.))) %>%
  ungroup() %>%
  as.data.frame()


plot_stability <- function(pc, pc_lag) {
  model <- lm(as.formula(paste(pc, "~", pc_lag)), data = lagged)
  slope <- coef(model)[2]
  slope_text <- paste("Slope =", round(slope, 4))
  
  ggplot(lagged, aes_string(x = pc_lag, y = pc)) +
    geom_point() + 
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = paste0(pc, " (Year t)"), 
         y = paste0(pc, " (Year t + 1)"),
         subtitle = slope_text) +
    ggtitle(paste0("Stability of ", pc)) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}



# Plots Stability of Principal Components
grid.arrange(grobs = lapply(seq_along(
  grep("^PC\\d$", names(lagged), value = TRUE)), 
  function(i) plot_stability(
    grep("^PC\\d$", names(lagged), value = TRUE)[i],
    grep("^PC\\d_lag$", names(lagged), value = TRUE)[i])), nrow = 1)


# Plots Stability of Cluster Probabilities
grid.arrange(grobs = lapply(seq_along(
  grep("^probC\\d$", names(lagged), value = TRUE)),
  function(i) plot_stability(
    grep("^probC\\d$", names(lagged), value = TRUE)[i],
    grep("^probC\\d_lag$", names(lagged), value = TRUE)[i])), nrow = 2)





# Generate density plots for each PC and Cluster
data_long <- archetypes %>%
  pivot_longer(cols = starts_with("PC"), 
               names_to = "PC", 
               values_to = "Value")

ggplot(data_long, aes(x = Value, 
                      fill = PC, 
                      color = PC)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Cluster, scales = "free") +
  theme_minimal() +
  labs(title = "Density Plots by Cluster and Principal Component",
       x = "Value",
       y = "Density")






## RShiny Application ---------------------------------------------------- 
# Define UI
cluster_names <- c("Walker", "Free-Swinging Power Hitter", 
                   "Patient Contact Hitter", "TTO Hitter", 
                   "Chaser", "Contact Seeker", "Superstar")


statcast <- statcast %>% mutate(
  PlayerYear = paste(str_split(name, ", ", simplify = T)[, 2],
                     str_split(name, ", ", simplify = T)[, 1],
                     year, sep = " "))


playerdata <- merge(archetypes, statcast, by = "PlayerYear")



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
                                 choices = archetypes$PlayerYear,
                                 selected = 'Juan Soto 2021',
                                 options = list(maxOptions = 5000))



# Define UI
ui <- page_sidebar(
  title = "Player Profile",
  sidebar = choose_players,
  layout_columns(col_widths = c(4, 5, 3, 8, 4),
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
server <- function(input, output) {
  
  # Boxes for each PC Value
  output$vbox1 <- renderUI({
    value_box(
      title = "Contact Quality",
      value = archetypes %>%
        filter(PlayerYear == input$player1) %>%
        select(PC1) %>%
        round(0) %>%
        pull(),
      showcase = bsicons::bs_icon("fire", size = "1em"),
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
      showcase = bsicons::bs_icon("crosshair", size = "1em"),
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
      showcase = bsicons::bs_icon("eye", size = "1em"),
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
      t()
    
    
    datatable(playerdata, 
              fillContainer = FALSE, 
              style = "jqueryui", 
              filter = "none",
              selection = "none",
              options = list(dom = 't', pageLength = 1000),
              rownames = TRUE)
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

