library(dplyr)
whiskey <- read.csv('whiskey.csv', header = TRUE, stringsAsFactors = TRUE)
str(whiskey)
head(whiskey)

six_whiskeys <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Glenglassaugh","Tullibardine", "Ardbeg", "Bruichladdich", "Glenmorangie"),]
View(six_whiskeys)

w1 <- whiskey[,-70:-71]
w1
w2 <- w1[,-1]
w2
dist(w2, method="binary")

set.seed(1)
twelve_km <- kmeans(x = w2, centers = 12, nstart = 50)
twelve_km

twelve_km$cluster
whiskey$NAME
whiskey$NAME[20]
twelve_km$cluster[20]
table(twelve_km$cluster)

# View the cluster assignments of the 12-means model fit 
# Which whiskeys are in the same cluster as Bunnahabhain? (Bunnahabhain has index 20 in whiskey$NAME and is in cluster 10 from the cluster vector):
for (i in 1:length(twelve_km$cluster)){
  if (twelve_km$cluster[i] == 10) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Aberfeldy (Aberfeldy has index 1 in whiskey$NAME and is in cluster 11 from the cluster vector):
for (i in 1:length(twelve_km$cluster)){
  if (twelve_km$cluster[i] == 11) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Bruichladdich (Bruichladdich has index 19 in whiskey$NAME and is in cluster 2 from the cluster vector):
for (i in 1:length(twelve_km$cluster)){
  if (twelve_km$cluster[i] == 2) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

set.seed(2)
hc.whiskey <- hclust(dist(w2,method = "binary"), method = "complete")
plot(hc.whiskey, main = "Complete Linkage", xlab = "", sub = "")
cluster_assignments <- cutree(hc.whiskey, k = 12)
cluster_assignments

# Examine the cluster containing Bunnahabhain:
for (i in 1:length(cluster_assignments)){
  if (cluster_assignments[i] == 9) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Aberfeldy:
for (i in 1:length(cluster_assignments)){
  if (cluster_assignments[i] == 1) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Bruichladdich:
for (i in 1:length(cluster_assignments)){
  if (cluster_assignments[i] == 10) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

table(cluster_assignments)
