
whiskey <- read.csv("whiskey.csv", header = TRUE, stringsAsFactors = TRUE)
str(whiskey)
head(whiskey)

six_whiskeys <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Glenglassaugh","Tullibardine", "Ardbeg", "Bruichladdich", "Glenmorangie"),]
View(six_whiskeys)

BunGlen <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Glenglassaugh"),1:69]
J1 <- dist(BunGlen[,-1], method = "binary")

BunTull <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Tullibardine"),1:69]
J2 <- dist(BunTull[,-1], method = "binary")

BunnArd <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Ardbeg"),1:69]
J3 <- dist(BunnArd[,-1], method = "binary")

BunBrui <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Bruichladdich"),1:69]
J4 <- dist(BunBrui[,-1], method = "binary")

BunMor <- whiskey[whiskey$NAME %in% c("Bunnahabhain", "Glenmorangie"),1:69]
J5 <- dist(BunMor[,-1], method = "binary")

JaccardDistances <- data.frame(J1, J2, J3, J4, J5)
JaccardDistances
colnames(JaccardDistances) <- c("Glenglassaugh","Tullibardine", "Ardbeg", "Bruichladdich", "Glenmorangie")
rownames(JaccardDistances) <- "Bunnahabhain"
JaccardDistances

whiskey.without.reg.dist <- whiskey[,1:69]
whiskey.without.reg.dist
set.seed(1)
km.whiskey <- kmeans(whiskey.without.reg.dist[,-1], centers = 12, nstart = 50)
km.whiskey

km.whiskey$cluster
whiskey$NAME

# Which whiskeys are in the same cluster as Bunnahabhain?
for (i in 1:length(km.whiskey$cluster)){
  if(km.whiskey$cluster[i] == 10) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Aberfeldy:
for (i in 1:length(km.whiskey$cluster)){
  if(km.whiskey$cluster[i] == 11) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

# Examine the cluster containing Bruichladdich:
for (i in 1:length(km.whiskey$cluster)){
  if(km.whiskey$cluster[i] == 2) {
    print(whiskey$NAME[i], max.levels = 0)
  }
}

whistab <- data.frame(cbind(as.character(whiskey[,1]), as.factor(km.whiskey$cluster)))
colnames(whistab) <- c("Name", "Cluster")
kmeansMembers <- whistab[order(whistab$Cluster),]
kmeansMembers

set.seed(2)
hc.whiskey <- hclust(dist(whiskey.without.reg.dist[,-1],method = "binary"), method = "complete")
plot(hc.whiskey, main = "Complete Linkage", xlab = "", sub = "")
cluster_assignments <- cutree(hc.whiskey, k = 12)
cluster_assignments

whistab_hier <- data.frame(cbind(as.character(whiskey[,1]), as.factor(cluster_assignments)))
colnames(whistab_hier) <- c("Name", "Cluster")
hier.com.members <- whistab_hier[order(whistab_hier$Cluster),]
hier.com.members
