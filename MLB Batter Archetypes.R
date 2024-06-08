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


statcast <- read.csv("statcast4.csv")





## Data Cleaning ---------------------------------------------------------
# Column Names
statcast <- statcast %>%
  rename("name" = last_name..first_name,
         "avg" = batting_avg,
         "obp" = on_base_percent,
         "slg" = slg_percent,
         "ops" = on_base_plus_slg) %>%
  mutate(hr_rate = home_run/pa) %>%
  relocate(avg, obp, slg, ops, woba, .after = pa)


# Cleans up Names and # of PAs
#Encoding(statcast$name) <- "latin1"


# League Averages
lg_avgs <- statcast %>%
  group_by(year) %>%
  summarise_at(.vars = vars(c(5:ncol(statcast) - 1)), 
               .funs = c(mean = "mean")) %>%
  as.data.frame()

lg_avgs %>%
  select(1, 8:ncol(lg_avgs)) %>%
  kable()



# Converts stats to scale above/below Lg Avgs for that year
statcast_adj <- statcast %>%
  group_by(year) %>%
  mutate(across(c(k_percent, bb_percent, xba, xslg, xwoba, xwobacon, 
                  barrel_batted_rate, hard_hit_percent, 
                  #sweet_spot_percent,
                  z_swing_miss_percent, oz_swing_percent, 
                  oz_swing_miss_percent, hr_rate), 
                ~ . / weighted.mean(., pa) * 100, 
                .names = "{.col}+")) %>%
  rename_with(~ gsub("percent", "pct", .), ends_with("+")) %>%
  rename_with(~ gsub("swing", "sw", .), ends_with("+")) %>%
  rename_with(~ gsub("batted_", "", .), ends_with("+")) %>%
  select(1:10, ends_with("+")) %>%
  filter(pa > 200) %>%
  as.data.frame()



# Correlation Matrix
ggpairs(statcast_adj, columns = grep("\\+$", names(statcast_adj)))



head(scale(statcast_adj[,c(grep("\\+$", names(statcast_adj)))]))
lapply(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], sd)
lapply(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], range)
colMeans(statcast_adj[,c(grep("\\+$", names(statcast_adj)))])


# Current best
# 200 PA's, TRUE Scaled, 4 PC's, 8 clusters, EII (0.2021) 



## Principal Component Analysis (PCA) ------------------------------------
set.seed(789)
pca <- prcomp(statcast_adj[,c(grep("\\+$", names(statcast_adj)))], 
              scale. = TRUE)
pca_summary <- summary(pca)

ggplot(as.data.frame(pca$x), aes(x = PC1, y = PC2)) + 
  geom_point() + coord_fixed()


(pca$sdev)^2 # Eigenvalues
round(pca$rotation[,1:4],4) # Matrix of eigenvectors
pca$center # Mean of each unscaled predictor
pca$scale # St Dev of each unscaled predictor
pca$x # Input data transformed into PCA space


# Elbow Curve for PCA
screeplot(pca, type = "lines", col = "blue") # 4 is best
plot(c(1:ncol(pca_summary$importance)), (pca$sdev)^2,
     xlab = "Number of Principal Components",
     ylab = "Variances",
     main = "Statcast PCA Scree Plot",
     type = "b", col = "blue")
plot(c(1:ncol(pca_summary$importance)), pca_summary$importance[3,],
     xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance",
     main = "Principal Components vs Cumulative Variance",
     type = "b", col = "blue")


# Get first 4 principal components
pc <- pca$x[,1:4]



# Silhouette Scores to determine # of Clusters
silhouette_scores <- data.frame("Model" = character(), 
                                "Clusters" = numeric(),
                                "SilhouetteScore" = numeric())
models <- c("EII")

for (m in models) {
  for (c in c(2:20)) {
    
    set.seed(23)
    gmm_model <- Mclust(pc, G = c, modelNames = m)
    silhouette_score <- silhouette(gmm_model$classification, dist(pc))
    
    silhouette_scores[nrow(silhouette_scores) + 1, ] <- 
      c(m, c, round(mean(silhouette_score[, 3]), 4))
    
  }
}


# Plot Silhouette scores
plot(silhouette_scores[4:11,2], silhouette_scores[4:11,3], type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Silhouette Score", 
     main = "Silhouette Score vs. Number of Clusters")

silhouette_scores %>%
  arrange(desc(Clusters)) %>%
  head(n = 10)





## Gaussian Mixture Modeling (GMM) ---------------------------------------
set.seed(123)
clusters <- 8
gmm_mlb <- Mclust(pc, G = clusters, modelNames = 'EII')

fviz_cluster(gmm_mlb, data = pc, geom = "point", alpha = 0.5,
             show.clust.cent = TRUE)
round(gmm_mlb$parameters$mean, 4)



# Joins Cluster Probabilities with Player Data
batter_archetypes <- statcast_adj %>%
  cbind(gmm_mlb$classification, 
        round(predict(gmm_mlb, data = pc)$z, 3)) %>%
  rename("Cluster" = `gmm_mlb$classification`) %>%
  rename_with(~ str_replace(., "^[0-9]+$", ~ paste0("probC", .))) %>%
  select(-Cluster, Cluster) %>%
  mutate(across(c(ends_with('+')), round, 2)) %>%
  mutate(maxProb = do.call(pmax, select(., starts_with("probC"))),
         PlayerYear = paste(str_split(name, ", ", simplify = T)[, 2],
                            str_split(name, ", ", simplify = T)[, 1],
                            year, sep = " ")) %>%
  arrange(PlayerYear)




# Aggregates Results to see Average Attributes of each Cluster
weighted_mean <- function(x, w) {
  sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
}

weight_cols <- batter_archetypes %>%
  select(starts_with("probC")) %>%
  names()

cols_to_summarize <- batter_archetypes %>%
  select(ends_with("+"), avg, obp, slg, ops, woba) %>%
  names()


batter_archetypes_agg <- batter_archetypes %>%
  pivot_longer(cols = all_of(weight_cols), names_to = "Cluster", 
               names_prefix = "probC", values_to = "weight", 
               names_repair = "unique") %>%
  group_by(Cluster...23) %>%
  summarize(across(all_of(cols_to_summarize), ~ weighted_mean(., weight), 
                   .names = "{col}")) %>%
  mutate(across(c(ends_with('+')), round, 2)) %>%
  mutate(across(c(avg, obp, slg, ops, woba), round, 3)) %>%
  mutate(iso = slg - avg) %>%
  relocate(iso, .after = ops) %>%
  rename("Cluster" = `Cluster...23`) %>%
  as.data.frame()



# Exploring Results
batter_archetypes %>%
  filter(Cluster == 2, year == 2016)

batter_archetypes %>%
  filter(name == "Lindor, Francisco") %>%
  relocate(PlayerYear, .before = pa) %>%
  select(-name, -player_id, -year)

batter_archetypes %>%
  arrange(maxProb) %>%
  head(n = 20)

batter_archetypes %>%
  group_by(Cluster) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n/sum(n),3)) %>%
  as.data.frame()

batter_archetypes %>%
  arrange(desc(probC3)) %>%
  #filter(year == 2023) %>%
  select(-c(player_id, PlayerYear)) %>%
  #filter(year %in%  c(2023, 2022, 2021)) %>%
  head(n = 20)






## RShiny Application ---------------------------------------------------- 
# Define UI
ui <- fluidPage(
  titlePanel("Player Archetype Comparisons"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "player1", label = "Select Player 1:", 
                  choices = head(batter_archetypes$PlayerYear),
                  selected = 'Juan Soto 2021',
                  options = list(maxOptions = 500)),
      selectizeInput(inputId = "player2", label = "Select Player 2:", 
                  choices = head(batter_archetypes$PlayerYear),
                  selected = 'Bryce Harper 2021',
                  options = list(maxOptions = 500))
    ),
    mainPanel(
      
      tabsetPanel(
        id = "tabs",
        tabPanel(title = uiOutput("title1"), plotlyOutput("pie1"), 
                 column(6,plotOutput(outputId="plotgraph", 
                                     width="500px", height="500px"))),
        tabPanel(title = uiOutput("title2"), plotlyOutput("pie2"), 
                 column(6,plotOutput(outputId="plotgraph", 
                                     width="500px", height="500px"))),
        tabPanel(title = "Player Comparison", 
                 fluidRow(plotlyOutput("pieComp1"),
                          plotlyOutput("pieComp2")), 
                 column(6,plotOutput(outputId="plotgraph", 
                                     width="500px", height="500px")))
      )
    )
  )
)





# Define server logic
server_pie <- function(input, output, session) {
  
  # Player Lists
  updateSelectizeInput(session, 'player1', 
                       choices = unique(batter_archetypes$PlayerYear), 
                       server = TRUE)
  updateSelectizeInput(session, 'player2', 
                       choices = unique(batter_archetypes$PlayerYear), 
                       server = TRUE)
  
  
  # Dynamic Tab Titles
  output$title1 = renderText({
    batter_archetypes[batter_archetypes$PlayerYear == input$player1,
                      ncol(batter_archetypes)] 
  })
  
  output$title2 = renderText({
    batter_archetypes[batter_archetypes$PlayerYear == input$player2,
                      ncol(batter_archetypes)]
  })
  
  
  
  # Player 1 Pie Chart
  output$pie1 <- renderPlotly({
    player_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == input$player1, 
                        ]%>% 
      mutate_at(vars((c(starts_with('probC')))), ~.*100)
    
    
    r <- percent(map_dbl(
      player_year_data[, grepl("probC", 
                               names(player_year_data))], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power", "TTO", "Patient", "Walker"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  # Player 2 Pie Chart
  output$pie2 <- renderPlotly({
    player_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == input$player2, 
                        ]%>% 
      mutate_at(vars(c(starts_with('probC'))), ~.*100)
    
    
    r <- percent(map_dbl(
      player_year_data[, grepl("probC", 
                               names(player_year_data))], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power","TTO", "Patient", "Walker"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  # Player Comparison Pie Chart
  output$pieComp1 <- renderPlotly({
    player_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == input$player1, 
                        ]%>% 
      mutate_at(vars(c(starts_with('probC'))), ~.*100)
    
    
    r <- percent(map_dbl(
      player_year_data[, grepl("probC", 
                               names(player_year_data))], ~.x))
    
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power", "TTO", "Patient", "Walker"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  
  
  output$pieComp2 <- renderPlotly({
    player_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == 
                              input$player2, ] %>% 
      mutate_at(vars(c(starts_with('probC'))), ~.*100)
    
    
    r <- percent(map_dbl(
      player_year_data[, grepl("probC", 
                               names(player_year_data))], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power", "TTO", "Patient", "Walker"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })

  
  
  output$pieComp <- renderPlotly({
    player1_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == 
                              input$player1, ] %>% 
      mutate_at(vars(c(starts_with('probC'))), ~.*100)
    
    player2_year_data <- 
      batter_archetypes[batter_archetypes$PlayerYear == 
                              input$player2, ] %>% 
      mutate_at(vars(c(starts_with('probC'))), ~.*100)
    
    
    
    r1 <- percent(map_dbl(
      player1_year_data[, grepl("probC", 
                                names(player_year_data))], ~.x))
    
    r2 <- percent(map_dbl(
      player2_year_data[, grepl("probC", 
                                names(player_year_data))], ~.x))
    
    
    p1 <- plot_ly(
      type = 'pie',
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power", "TTO", "Patient", "Walker"),
      values = r1,
      domain = list(x = c(0, 0.48)),
      marker = list(colors = brewer.pal(length(r1), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p2 <- plot_ly(
      type = 'pie',
      labels = c("Average", "Chaser", "Contact-focused",
                 "Superstar", "Low-Power", "TTO", "Patient", "Walker"),
      values = r2,
      domain = list(x = c(0.52, 1)),
      marker = list(colors = brewer.pal(length(r2), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    p <- subplot(p1, p2, nrows = 2)
    
    p
  })
}





# Run the Application 
shinyApp(ui = ui, server = server_pie)

