olympic = read.csv("C:/_McGill MMA 2023/2. Fall 2023/MGSC 661 Multivariate Statistical Analysis/Final Project/Dataset 2 - Olympic events data.csv")

library(ggplot2)
library(reshape2)
#install.packages("rworldmap")
library(sp)
library(rworldmap)
library(MASS)
library(klaR)
require(psych)
library(ggfortify)
library(cluster)


# PRE-PROCESSING
##############################

# Replace NA values in the Medal column
olympic$Medal[is.na(olympic$Medal)] = "No medal"

# Remove the missing values of Height and Weight
olympic = na.omit(olympic, cols = c("Height", "Weight"))

# Change some values from the Team column
olympic$Team = ifelse(olympic$Team == "East Germany", "Germany", olympic$Team)
olympic$Team = ifelse(olympic$Team == "West Germany", "Germany", olympic$Team)
olympic$Team = ifelse(olympic$Team == "Chinese Taipei", "China", olympic$Team)
olympic$Team = ifelse(olympic$Team == "Soviet Union", "Russia", olympic$Team)

# Create a column for Body Mass Index (BMI)
olympic$BMI = (olympic$Weight) / (olympic$Height/100)^2

# Create a column that categorizes the years of the games
olympic$Year_category = ifelse(olympic$Year < 1920, "Early 20th Century",
                        ifelse(olympic$Year < 1948, "World War Period",
                        ifelse(olympic$Year < 1992, "Cold War Period",
                        ifelse(olympic$Year >= 1992, "Actual Period", NA))))

# Create a column that categorizes the sports
winter_sports = c("Alpine Skiing", "Biathlon", "Bobsleigh", "Cross Country Skiing", "Curling", "Figure Skating", "Freestyle Skiing", "Ice Hockey", "Luge", "Nordic Combined", "Short Track Speed Skating", "Skeleton", "Ski Jumping", "Snowboarding", "Speed Skating")
team_sports = c("Baseball", "Basketball", "Beach Volleyball", "Football", "Handball", "Hockey", "Ice Hockey", "Rugby", "Rugby Sevens", "Softball", "Volleyball", "Water Polo")
individual_sports = c("Archery", "Athletics", "Badminton", "Boxing", "Cycling", "Fencing", "Golf", "Gymnastics", "Judo", "Modern Pentathlon", "Shooting", "Table Tennis", "Taekwondo", "Tennis", "Trampolining", "Weightlifting", "Wrestling")
water_sports = c("Canoeing", "Diving", "Rowing", "Sailing", "Swimming", "Synchronized Swimming")
others = c("Aeronautics", "Alpinism", "Art Competitions", "Basque Pelota", "Cricket", "Croquet", "Equestrianism", "Jeu De Paume", "Lacrosse", "Military Ski Patrol", "Motorboating", "Polo", "Racquets", "Rhythmic Gymnastics", "Roque", "Tug-Of-War", "Triathlon")

olympic$Sport_category = ifelse(olympic$Sport %in% winter_sports, "Winter Sport",
                          ifelse(olympic$Sport %in% team_sports, "Team Sport",
                          ifelse(olympic$Sport %in% individual_sports, "Individual Sport",
                          ifelse(olympic$Sport %in% water_sports, "Water Sport",
                          ifelse(olympic$Sport %in% others, "Others", NA)))))

# Create a column that indicates the country where the game was held
olympic$Country = ifelse(olympic$Year == 1896, "Greece",
                  ifelse(olympic$Year == 1900, "France",
                  ifelse(olympic$Year == 1904, "United States",
                  ifelse(olympic$Year == 1908, "United Kingdom",
                  ifelse(olympic$Year == 1912, "Sweden",
                  ifelse(olympic$Year == 1920, "Belgium",
                  ifelse(olympic$Year == 1924, "France",
                  ifelse(olympic$Year == 1928, "Netherlands",
                  ifelse(olympic$Year == 1932, "United States",
                  ifelse(olympic$Year == 1936, "Germany",
                  ifelse(olympic$Year == 1948, "United Kingdom",
                  ifelse(olympic$Year == 1952, "Finland",
                  ifelse(olympic$Year == 1956, "Australia",
                  ifelse(olympic$Year == 1960, "Italy",
                  ifelse(olympic$Year == 1964, "Japan",
                  ifelse(olympic$Year == 1968, "Mexico",
                  ifelse(olympic$Year == 1972, "Germany",
                  ifelse(olympic$Year == 1976, "Canada",
                  ifelse(olympic$Year == 1980, "Russia",
                  ifelse(olympic$Year == 1984, "United States",
                  ifelse(olympic$Year == 1988, "South Korea",
                  ifelse(olympic$Year == 1992, "Spain",
                  ifelse(olympic$Year == 1996, "United States",
                  ifelse(olympic$Year == 2000, "Australia",
                  ifelse(olympic$Year == 2004, "Greece",
                  ifelse(olympic$Year == 2008, "China",
                  ifelse(olympic$Year == 2012, "United Kingdom",
                  ifelse(olympic$Year == 2016, "Brazil",
                  ifelse(olympic$Year == 1994, "Norway",
                  ifelse(olympic$Year == 1998, "Japan",
                  ifelse(olympic$Year == 2002, "United States",
                  ifelse(olympic$Year == 2006, "Italy",
                  ifelse(olympic$Year == 2010, "Canada",
                  ifelse(olympic$Year == 2014, "Russia",
                  ifelse(olympic$Year == 1906, "Greece", NA)))))))))))))))))))))))))))))))))))

# Create a new sex column that shows numerical values
olympic$Sex_num = ifelse(olympic$Sex == "F", 1, 0)

# Create a new season column that shows numerical values
olympic$Season_num = ifelse(olympic$Season == "Summer", 1, 0)

# Create a new medal column that shows numerical values
olympic$Medal_num = ifelse(olympic$Medal == "Gold", 3,
                    ifelse(olympic$Medal == "Silver", 2,
                    ifelse(olympic$Medal == "Bronze", 1, 0)))

# Create a performance score for each person
olympic$Total_Events_Participated = ave(rep(1, nrow(olympic)), olympic$ID, FUN = sum)
olympic$Performance_Score = olympic$Medal_num * (1 / olympic$Total_Events_Participated)

# Dummify the sport_category variables
sport_dummies = model.matrix(~olympic$Sport_category - 1, data=olympic)
colnames(sport_dummies) = paste("SportCat", colnames(sport_dummies), sep="_")
olympic = cbind(olympic, sport_dummies)
colnames(olympic)[(ncol(olympic)-4):ncol(olympic)] = c("D_Individual", "D_Others", "D_Team", "D_Water", "D_Winter")


attach(olympic)

# Count unique values
length(unique(Country))
length(unique(Team))
length(unique(Sport))
length(unique(Year))
length(unique(Name))
table(Sex)
table(Medal)

US = olympic[olympic$Team == "United States", ]
Germany = olympic[olympic$Team == "Germany", ]
Russia = olympic[olympic$Team == "Russia", ]
Canada = olympic[olympic$Team == "Canada", ]
France = olympic[olympic$Team == "France", ]
Italy = olympic[olympic$Team == "Italy", ]
UK = olympic[olympic$Team == "Great Britain", ]
Japan = olympic[olympic$Team == "Japan", ]
Australia = olympic[olympic$Team == "Australia", ]
China = olympic[olympic$Team == "China", ]



# EXPLORATORY DATA ANALYSIS
##############################

# Correlation matrix for numerical values
numerical_var = olympic[c("Age", "BMI", "Performance_Score", "Sex_num", 'Season_num', "D_Individual", "D_Others", "D_Team", "D_Water", "D_Winter")]
cor_matrix = round(cor(numerical_var), 1)

melted_cor_matrix = melt(cor_matrix)

ggplot(data = melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Correlation Matrix of Olympic Dataset Numerical Variables")

# Check how numerical values are distributed
hist(Age)
hist(BMI)
hist(Performance_Score) 

# Obtain the Top teams in terms of performance score
team_performance = aggregate(Performance_Score ~ Team, data = olympic, sum)
colnames(team_performance) = c("Team", "Performance_Score")
top_teams = head(team_performance[order(-team_performance$Performance_Score), ], 10)

ggplot(top_teams, aes(x = reorder(Team, Performance_Score), y = Performance_Score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Top 10 Teams by Performance Score", x = "Team", y = "Total Medals") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5))

map1 = joinCountryData2Map(team_performance, joinCode = "NAME", nameJoinColumn = "Team")
mapCountryData(map1, nameColumnToPlot="Performance_Score", catMethod="fixedWidth", mapTitle="Heatmap of Olympic Performance",
               colourPalette="heat", addLegend=TRUE)



# CLUSTERING MODEL (PCA)
##############################

# Create a dataframe for each period of time
columns_1 = c("ID", "Age", "BMI", "Medal_num", "D_Individual", "D_Others", "D_Team", "D_Water") # No woman in sports nor winter sports in the early 20th
early_20th = olympic[olympic$Year_category == "Early 20th Century", ] 
early_20th = early_20th[columns_1]


columns_2 = c("ID", "Age", "BMI", "Medal_num", "Sex_num", 'Season_num', "D_Individual", "D_Others", "D_Team", "D_Water", "D_Winter")
world_war = olympic[olympic$Year_category == "World War Period", ]
world_war = world_war[columns_2]

cold_war = olympic[olympic$Year_category == "Cold War Period", ]
cold_war = cold_war[columns_2]

actual_period = olympic[olympic$Year_category == "Actual Period", ]
actual_period = actual_period[columns_2]


#Recalculate the performance score for each period of time
early_20th$Total_Events_Participated = ave(rep(1, nrow(early_20th)), early_20th$ID, FUN = sum)
early_20th$Performance_Score = early_20th$Medal_num * (1 / early_20th$Total_Events_Participated)
early_20th = subset(early_20th, select = -c(ID, Medal_num, Total_Events_Participated))

world_war$Total_Events_Participated = ave(rep(1, nrow(world_war)), world_war$ID, FUN = sum)
world_war$Performance_Score = world_war$Medal_num * (1 / world_war$Total_Events_Participated)
world_war = subset(world_war, select = -c(ID, Medal_num, Total_Events_Participated))

cold_war$Total_Events_Participated = ave(rep(1, nrow(cold_war)), cold_war$ID, FUN = sum)
cold_war$Performance_Score = cold_war$Medal_num * (1 / cold_war$Total_Events_Participated)
cold_war = subset(cold_war, select = -c(ID, Medal_num, Total_Events_Participated))

actual_period$Total_Events_Participated = ave(rep(1, nrow(actual_period)), actual_period$ID, FUN = sum)
actual_period$Performance_Score = actual_period$Medal_num * (1 / actual_period$Total_Events_Participated)
actual_period = subset(actual_period, select = -c(ID, Medal_num, Total_Events_Participated))


# Run cluster for each period of time
pca_early_20th = prcomp(early_20th, scale. = TRUE)
autoplot(pca_early_20th, data = early_20th, loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA - Early 20th Century") + theme(plot.title = element_text(hjust = 0.5))

pca_world_war = prcomp(world_war, scale. = TRUE)
autoplot(pca_world_war, data = world_war, loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA - World War Period") + theme(plot.title = element_text(hjust = 0.5))

pca_cold_war = prcomp(cold_war, scale. = TRUE)
autoplot(pca_cold_war, data = cold_war, loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA - Cold War Period") + theme(plot.title = element_text(hjust = 0.5))

pca_actual_period = prcomp(actual_period, scale. = TRUE)
autoplot(pca_actual_period, data = actual_period, loadings = TRUE, loadings.label = TRUE) + ggtitle("PCA - Actual Period") + theme(plot.title = element_text(hjust = 0.5))



# CLUSTERING MODEL (k-means)
##############################

# Obtain the optimal number of k with the elbow method
numerical_var = olympic[c("Age", "BMI", "Performance_Score", "Sex_num", 'Season_num', "D_Individual", "D_Others", "D_Team", "D_Water", "D_Winter")]
wss = sapply(2:8, function(k){kmeans(numerical_var, centers=k)$tot.withinss})
elbow_df = data.frame(k = 2:8, wss = wss)

ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "blue", size = 1) +  
  geom_point(color = "darkred", size = 2) +  
  theme_minimal() +  # Minimal theme
  labs(
    title = "Elbow Method for Choosing Optimal K",
    x = "Number of Clusters (K)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    axis.title = element_text(size = 12, face = "bold"), 
    axis.text = element_text(size = 10))


# Run cluster for the actual period of time with optimal k=3
km_actual_period = kmeans(actual_period, 3)
km_actual_period
actual_period$Cluster = as.factor(km_actual_period$cluster)
ggplot(actual_period, aes(x = Age, y = Performance_Score, color = Cluster)) +
  geom_point() +  ggtitle("Cluster Age vs Performance - Actual Period") +
  theme(plot.title = element_text(hjust = 0.5))
ggplot(actual_period, aes(x = Age, y = BMI, color = Cluster)) +
  geom_point() +  ggtitle("Cluster Age vs BMI - Actual Period") +
  theme(plot.title = element_text(hjust = 0.5))



