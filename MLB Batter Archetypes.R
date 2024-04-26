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

statcast <- read.csv("statcast3.csv")

ggpairs(statcast, columns = c(names(statcast[,5:dim(statcast)[2]])))


# Data Cleaning
statcast <- statcast %>%
  rename("name" = last_name..first_name,
         "avg" = batting_avg,
         "obp" = on_base_percent,
         "slg" = slg_percent,
         "ops" = on_base_plus_slg) %>%
  mutate(hr_rate = home_run/pa) %>%
  relocate(avg, obp, slg, ops, woba, .after = pa)


# Cleans up Names and # of PAs
Encoding(statcast$name) <- "latin1"
statcast <- statcast %>%
  mutate(include = ifelse(pa >= 400 | year == 2024, 1, 0)) %>%
  filter(include == 1) %>%
  select(-include)



lg_avgs <- statcast %>%
  pivot_longer(cols = c(5:ncol(statcast)), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(year, variable) %>%
  summarize(avg_value = weighted.mean(value, pa)) %>%
  pivot_wider(names_from = variable, values_from = avg_value) %>%
  as.data.frame()



# Converts stats to scale above/below Lg Avgs for that year
statcast_adj <- statcast %>%
  group_by(year) %>%
  mutate(adj_k_pct = k_percent/
           weighted.mean(k_percent, pa)*100,
         adj_bb_pct = bb_percent/
           weighted.mean(bb_percent, pa)*100,
         adj_xba = xba/weighted.mean(xba, pa)*100,
         adj_xslg = xslg/weighted.mean(xslg, pa)*100,
         adj_xwoba = xwoba/weighted.mean(xwoba, pa)*100,
         #adj_avg_ev = exit_velocity_avg/
         weighted.mean(exit_velocity_avg, pa)*100,
         #adj_ss_pct = sweet_spot_percent/
         weighted.mean(sweet_spot_percent, pa)*100,
         adj_barrel_pct = barrel_batted_rate/
           weighted.mean(barrel_batted_rate, pa)*100,
         adj_hh_pct = hard_hit_percent/
           weighted.mean(hard_hit_percent, pa)*100,
         adj_zswm_pct = z_swing_miss_percent/
           weighted.mean(z_swing_miss_percent, pa)*100,
         adj_ozsw_pct = oz_swing_percent/
           weighted.mean(oz_swing_percent, pa)*100,
         adj_ozswm_pct = oz_swing_miss_percent/
           weighted.mean(oz_swing_miss_percent, pa)*100,
         adj_hr_rate = hr_rate/weighted.mean(hr_rate, pa)*100) %>%
  select(c(1:10), c(24:34)) %>%
  as.data.frame()


head(scale(statcast_adj[,11:(length(names(statcast_adj)))]))
lapply(statcast_adj[,11:(length(names(statcast_adj)))], sd)
lapply(statcast_adj[,11:(length(names(statcast_adj)))], range)




# ####
## Using PCA to transform data for K-means -----------------------
pca <- prcomp(statcast_adj[,11:(length(names(statcast_adj)))], 
              scale. = TRUE)
summary(pca)

ggplot(as.data.frame(pca$x), aes(x = PC1, y = PC2)) + 
  geom_point() + coord_fixed()


# Eigenvalues
pca$sdev

# Matrix of eigenvectors
pca$rotation

# Mean of each UNSCALED predictor
pca$center

# St Dev of each UNSCALED predictor
pca$scale

# Input data transformed into PCA space
pca$x

# Elbow Curve for PCA
screeplot(pca, type = "lines", col = "blue") # 4 is best

# Get first 4 principal components
pc <- pca$x[,1:2]
pc




# Loops through varying number of cluster centers for k-means
wcss <- data.frame("k" = numeric(), "tot_withinss" = numeric())

for (x in c(1:20)) {
  
  cluster <- kmeans(pc, centers = x, iter.max = 10, nstart = 1000)
  wcss[nrow(wcss) + 1, ] <- c(x, cluster$tot.withinss)
  
} 

# Elbow Method
elbow <- ggplot(wcss, aes(x = wcss$k, y = wcss$tot_withinss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:20) # k = 3 is best


# Silhouette Score
silhouette_score <- function(k){
  km <- kmeans(pc, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(pc))
  mean(ss[, 3])
}

k <- 2:15
avg_sil <- sapply(k, silhouette_score)
plot(k, type = 'b', avg_sil, xlab = 'Number of clusters', 
     ylab = 'Average Silhouette Scores', frame = FALSE)




# K-means
kmeans_mlb <- kmeans(pc, centers = 5, nstart = 1000)
fviz_cluster(kmeans_mlb, data = pc, geom = "point")



batter_archetypes <- cbind(statcast_adj, kmeans_mlb$cluster)
batter_archetypes %>%
  group_by(`kmeans_mlb$cluster`) %>%
  pivot_longer(cols = c(5:17), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(`kmeans_mlb$cluster`, variable) %>%
  summarize(avg_value = mean(value)) %>%
  pivot_wider(names_from = variable, values_from = avg_value) %>%
  as.data.frame() %>%
  arrange(desc(woba))

batter_archetypes %>%
  filter(`kmeans_mlb$cluster` == 7, year == 2022)
# 4 = High Contact, low K%, moderate power
# 1 = Jack-of-All-Trades
# 3 = Frequent but weaker contact, moderate BB%, low K%
# 2 = TTO - high BB%, high K%, high Barrel% and HR%
# 7 = Superstars - good balance of contact, high power, and discipline
# 6 = Chasers - high K% and chase %
# 5 = good power, higher contact, low K%, low whiff %

batter_archetypes %>%
  filter(last_name..first_name == "LeMahieu, DJ")










## Using PCA for Gaussian Mixture Modeling (GMM) ---------------------

pca <- prcomp(statcast_adj[,11:(length(names(statcast_adj)))], 
              scale. = TRUE)
summary(pca)

ggplot(as.data.frame(pca$x), aes(x = PC1, y = PC2)) + 
  geom_point() + coord_fixed()


# Eigenvalues
pca$sdev

# Matrix of eigenvectors
pca$rotation

# Mean of each UNSCALED predictor
pca$center

# St Dev of each UNSCALED predictor
pca$scale

# Input data transformed into PCA space
pca$x

# Elbow Curve for PCA
screeplot(pca, type = "lines", col = "blue") # 4 is best

# Get first 4 principal components
pc <- pca$x[,1:2]
pc



# Loop to run through varying number of centers for kmeans
wcss <- data.frame("k" = numeric(), "tot_withinss" = numeric())

# Using 2 principal components
for (x in c(1:20)) {
  
  cluster <- Mclust(pc, G = x)
  wcss[nrow(wcss) + 1, ] <- c(x, cluster$tot.withinss)
  
} 


# Silhouette Score
silhouette_scores <- sapply(2:15, function(k) {
  gmm_model <- Mclust(pc, G = k)
  silhouette_score <- silhouette(gmm_model$classification, 
                                 dist(pc))
  #ss <- silhouette(km$cluster, dist(pc))
  mean(silhouette_score[, 3])
})


# Plot Silhouette scores
plot(2:15, silhouette_scores, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Silhouette Score", 
     main = "Silhouette Score vs. Number of Clusters")



# GMM Model
set.seed(123)
gmm_mlb <- Mclust(pc, G = 5)
fviz_cluster(gmm_mlb, data = pc, geom = "point", alpha = 0.5,
             show.clust.cent = TRUE)
gmm_mlb$parameters$mean



# Probability of Being in each Cluster
cluster_probs <- round(predict(gmm_mlb, data = pc)$z, 3)
head(cluster_probs)


batter_archetypes_gmm <- statcast_adj %>%
  cbind(gmm_mlb$classification, cluster_probs) %>%
  rename("Cluster" = `gmm_mlb$classification`,
         "probC1" = c('1'),
         "probC2" = c('2'),
         "probC3" = c('3'),
         "probC4" = c('4'),
         "probC5" = c('5')
#         "probC6" = c('6'),
) %>%
  select(-Cluster, Cluster) %>%
  mutate(PlayerYear = paste(str_split(name, ", ", simplify = T)[, 2],
                            str_split(name, ", ", simplify = T)[, 1],
                            year, sep = " ")) %>%
  arrange(PlayerYear)


batter_archetypes_gmm_agg <- batter_archetypes_gmm %>%
  group_by(Cluster) %>%
  pivot_longer(cols = c(11:ncol(batter_archetypes_gmm)-2), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(Cluster, variable) %>%
  summarize(avg_value = mean(value)) %>%
  pivot_wider(names_from = variable, values_from = avg_value) %>%
  mutate_at(vars(c('probC1', 'probC2', 'probC3', 
                   'probC4', 
                   'probC5'#, 'probC6'
                   )), ~ round(., 3)) %>%
  as.data.frame() %>%
  #select(-year, -pa) %>%
  arrange(desc(woba))




batter_archetypes_gmm %>%
  filter(Cluster == 2, year == 2016)

batter_archetypes_gmm %>%
  filter(name == "Gallo, Joey")


# Clusters
# 1 = TTO - high HR%, high BB%, high K%
# 2 = Superstar - balance of power, patience, and contact
# 3 = Chaser
# 4 = Contact-focused
# 5 = Well-rounded


batter_archetypes_gmm %>%
  group_by(Cluster) %>%
  summarise(n = n()) %>%
  as.data.frame()

batter_archetypes_gmm %>%
  arrange(desc(probC5)) %>%
  filter(year == 2023) %>%
  head()





## RShiny Application ----------------------------------------------------- 
# Define UI
ui <- fluidPage(
  titlePanel("Player Archetype Comparisons"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "player1", label = "Select Player 1:", 
                  choices = head(batter_archetypes_gmm$PlayerYear),
                  selected = 'Juan Soto 2021',
                  options = list(maxOptions = 50)),
      selectizeInput(inputId = "player2", label = "Select Player 2:", 
                  choices = head(batter_archetypes_gmm$PlayerYear),
                  selected = 'Bryce Harper 2021',
                  options = list(maxOptions = 50))
    ),
    mainPanel(
      #plotlyOutput("radar")
      
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
                       choices = unique(batter_archetypes_gmm$PlayerYear), 
                       server = TRUE)
  updateSelectizeInput(session, 'player2', 
                       choices = unique(batter_archetypes_gmm$PlayerYear), 
                       server = TRUE)
  
  
  # Dynamic Tab Titles
  output$title1 = renderText({
    batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == input$player1, 
                                ncol(batter_archetypes_gmm)] 
  })
  
  output$title2 = renderText({
    batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == input$player2, 
                                ncol(batter_archetypes_gmm)]
  })
  
  
  
  # Player 1 Pie Chart
  output$pie1 <- renderPlotly({
    player_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear ==
                              input$player1, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    
    r <- percent(map_dbl(player_year_data[, c("probC1", "probC2", 
                                              "probC3", "probC4", 
                                              "probC5")], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("TTO", "Superstar", "Contact-focused", 
                "All-around", "Chaser"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  # Player 2 Pie Chart
  output$pie2 <- renderPlotly({
    player_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == 
                              input$player2, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    
    r <- percent(map_dbl(player_year_data[, c("probC1", "probC2", 
                                              "probC3", "probC4", 
                                              "probC5")], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("TTO", "Superstar", "Chaser",
                 "Contact-focused", "All-around"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  # Player Comparison Pie Chart
  output$pieComp1 <- renderPlotly({
    player_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == 
                              input$player1, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    
    r <- percent(map_dbl(player_year_data[, c("probC1", "probC2", 
                                              "probC3", "probC4", 
                                              "probC5")], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("TTO", "Superstar", "Chaser",
                 "Contact-focused", "All-around"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })
  
  
  
  
  output$pieComp2 <- renderPlotly({
    player_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == 
                              input$player2, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    
    r <- percent(map_dbl(player_year_data[, c("probC1", "probC2", 
                                              "probC3", "probC4", 
                                              "probC5")], ~.x))
    
    p <- plot_ly(
      type = 'pie',
      values = r, 
      labels = c("TTO", "Superstar", "Chaser",
                 "Contact-focused", "All-around"), 
      marker = list(colors = brewer.pal(length(r), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p
  })

  
  
  output$pieComp <- renderPlotly({
    player1_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == 
                              input$player1, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    player2_year_data <- 
      batter_archetypes_gmm[batter_archetypes_gmm$PlayerYear == 
                              input$player2, ] %>% 
      mutate_at(vars("probC1", "probC2", "probC3", 
                     "probC4", "probC5"), ~.*100)
    
    
    
    r1 <- percent(map_dbl(player1_year_data[, c("probC1", "probC2", 
                                                "probC3", "probC4", 
                                                "probC5")], ~.x))
    
    r2 <- percent(map_dbl(player2_year_data[, c("probC1", "probC2", 
                                                "probC3", "probC4", 
                                                "probC5")], ~.x))
    
    
    p1 <- plot_ly(
      type = 'pie',
      labels = c("TTO", "Superstar", "Chaser",
                 "Contact-focused", "All-around"),
      values = r1,
      domain = list(x = c(0, 0.48)),
      marker = list(colors = brewer.pal(length(r1), "Set1")),
      textinfo = 'label+percent',
      showlegend = TRUE
    )
    
    
    p2 <- plot_ly(
      type = 'pie',
      labels = c("TTO", "Superstar", "Chaser",
                 "Contact-focused", "All-around"),
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





## DO NOT USE BELOW ##
# ####
## Using Raw Data for Gaussian Mixture Modeling (GMM) --------------
# This method is bad, Silhouette Scores are much lower
# Find Optimal # of Cluster Centers
# Elbow
for (x in c(1:20)) {
  
  cluster <- Mclust(statcast_adj[,6:ncol(statcast_adj)], G = x)
  wcss[nrow(wcss) + 1, ] <- c(x, cluster$tot.withinss)
  
} 


# Silhouette Score
silhouette_scores <- sapply(2:15, function(k) {
  gmm_model <- Mclust(statcast_adj[,6:ncol(statcast_adj)], G = k)
  silhouette_score <- silhouette(gmm_model$classification, 
                                 dist(
                                   statcast_adj[,6:ncol(statcast_adj)]))
  #ss <- silhouette(km$cluster, dist(pc))
  mean(silhouette_score[, 3])
})


# Plot Silhouette scores
plot(2:15, silhouette_scores, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Silhouette Score", 
     main = "Silhouette Score vs. Number of Clusters")



gmm_mlb <- Mclust(statcast_adj[,6:ncol(statcast_adj)], G = 6)
fviz_cluster(gmm_mlb, data = statcast_adj[,6:ncol(statcast_adj)], 
             geom = "point", alpha = 0.5,
             show.clust.cent = TRUE)
gmm_mlb$parameters$mean



# Probability of Being in each Cluster
cluster_probs <- round(predict(gmm_mlb, 
                               data = 
                                 statcast_adj[,6:ncol(statcast_adj)])$z, 
                       3)
head(cluster_probs)


batter_archetypes_gmm <- statcast_adj %>%
  cbind(gmm_mlb$classification, cluster_probs) %>%
  rename("Cluster" = `gmm_mlb$classification`,
         "name" = last_name..first_name,
         "probC1" = c('1'),
         "probC2" = c('2'),
         "probC3" = c('3'),
         "probC4" = c('4'),
         "probC5" = c('5'),
         "probC6" = c('6'),) %>%
  select(-Cluster, Cluster)
  

batter_archetypes_gmm_agg <- batter_archetypes_gmm %>%
  group_by(Cluster) %>%
  pivot_longer(cols = c(5:ncol(batter_archetypes_gmm)-1), 
               names_to = "variable", 
               values_to = "value") %>%
  group_by(Cluster, variable) %>%
  summarize(avg_value = mean(value)) %>%
  pivot_wider(names_from = variable, values_from = avg_value) %>%
  mutate_at(vars(c('probC1', 'probC2', 'probC3', 
                   'probC4', 'probC5', 'probC6')), ~ round(., 3)) %>%
  as.data.frame() %>%
  arrange(desc(woba))



# 1 = HR or Bust - high HR%, low BB%, high K%, below Avg Contact %
# 2 = Superstar - balance of power, patience, and contact
# 3 = TTO - high HR%, high BB%, high K%
# 4 = High Contact, Avg Power numbers, good discipline
# 5 = Frequent but weak contact, low power numbers
# 6 = Jack of all Trades

batter_archetypes_gmm %>%
  filter(Cluster == 2, year == 2015)

batter_archetypes_gmm %>%
  filter(name == "Trout, Mike")













## Junk --------------------------
batter_archetypes_gmm %>%
  mutate_at(vars(starts_with("probC")), 
            list(wavg_adj_hr_rate = ~ 
                   sum(adj_hr_rate * .) / sum(.))) %>%
  select((ncol(.) - 5):ncol(.)) %>%
  head(1) %>%
  t() %>%
  as.data.frame() %>%
  rename(wavg_adj_hr_rate = c('1'))


columns <- names(pca$rotation[,1])
wavg_function <- function(column) {
  
  use <- sym(paste("wavg_", column, sep = ""))
  
  batter_archetypes_gmm %>%
    mutate_at(vars(starts_with("probC")), 
              list(use = ~ sum(sym(column) * .) / sum(.))) %>%
    select((ncol(.) - 5):ncol(.)) %>%
    head(1) %>%
    t() %>%
    as.data.frame() %>%
    rename(use = c('1')) -> stored_result
  
  return(stored_result)
}

wavg_function <- function(column) {
  
  use <- sym(paste("wavg_", column, sep = ""))
  
  
  return(use)
}
class(sym(columns[1]))
wavg_function(columns[1])

archetypes_gmm_agg <- 
  data.frame(Cluster = c(1:6),
             probC1 = batter_archetypes_gmm_agg$probC1,
             probC2 = batter_archetypes_gmm_agg$probC2,
             probC3 = batter_archetypes_gmm_agg$probC3,
             probC4 = batter_archetypes_gmm_agg$probC4,
             probC5 = batter_archetypes_gmm_agg$probC5,
             probC6 = batter_archetypes_gmm_agg$probC6)









