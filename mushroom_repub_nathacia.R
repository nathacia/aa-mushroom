#nathacia nathacia
#association rule mining, cluster analysis, kmeans, hierarchical, density-based clustering
#using datasets `mushroom` and `votes.repub`

library(arules)
library(arulesViz)

library(cluster)
library(factoextra)
library(car)
library(dbscan)

#
#ASSOCIATION RULE MINING -- mushroom dataset
#

#import data
data(package='arules')
data("Mushroom")

#investigating the data
class(Mushroom)
View(Mushroom)
inspect(Mushroom)
inspect(head(Mushroom))
inspect(head(Mushroom,2))
inspect(tail(Mushroom))
inspect(tail(Mushroom,3))

size(Mushroom)

LIST(head(Mushroom,1))
LIST(tail(Mushroom,1))

#frequent items (review & visualization)
itemFrequencyPlot(Mushroom)
itemFrequencyPlot(Mushroom, topN=10, type='absolute')

frequent_variable = eclat(Mushroom,
                          parameter = list(support=.5, maxlen=10))
frequent_variable
inspect(frequent_variable)

freq_variable_sorted <- sort(frequent_variable, by = 'support')
freq_variable_sorted
inspect(freq_variable_sorted)
inspect(head(freq_variable_sorted, 5))

#generate rules
mushrules <- apriori(Mushroom,
                     parameter = list(support=.85, conf=.7))
mushrules
inspect(mushrules)
mushrules_conf <- sort(mushrules, by='confidence', decreasing = T)
inspect(head(mushrules_conf))
mushrules_lift <- sort(mushrules, by='lift', decreasing = T)
inspect(head(mushrules_lift))
mushrules_lift_subset <- subset(mushrules, lift>1)
inspect(mushrules_lift_subset)

plot(mushrules_lift_subset)
plot(mushrules_lift_subset, interactive = T)
plot(mushrules_lift_subset,method='matrix')
plot(mushrules_lift_subset,method='graph')

#restrict lhs & rhs
mushrules <- apriori(Mushroom,
                     parameter = list(support=.85, conf=.7, minlen=2),
                     appearance = list(default='rhs', lhs='VeilType=partial'))
mushrules
inspect(mushrules)
mushrules <- apriori(Mushroom,
                     parameter = list(support=.85, conf=.7, minlen=2),
                     appearance = list(default='lhs', rhs='VeilType=partial'))
mushrules
inspect(mushrules)

#
#CLUSTER ANALYSIS -- votes.repub dataset
#

#import data
data(package='cluster')
data("votes.repub")

#investigating the data
View(votes.repub)
str(votes.repub)
summary(votes.repub)
pairs(votes.repub) #figure margins too large
class(votes.repub)
names(votes.repub)

#preprocessing
votesdset <- na.omit(votes.repub)

#visualize
dist <- get_dist(votesdset, method = 'euclidean')
dist
fviz_dist(dist1)
dist1 <- get_dist(votesdset, method = 'pearson')
dist1

#kmeans
fviz_nbclust(votesdset, kmeans, method = 'wss')
set.seed(123)
k1 <- kmeans(votesdset, center=5, nstart=25)
k1
fviz_cluster(k1, data=votesdset)

#hierarchical
hc1 <- agnes(votesdset, method = 'complete') 
plot(hc1)
hc2 <- agnes(votesdset, method = 'single')
plot(hc2)
hc3 <- agnes(votesdset, method = 'average')
plot(hc3)

hc1$ac
hc2$ac
hc3$ac

#density based clustering
kNNdistplot(votesdset, k=5)

db <- dbscan(votesdset, eps = 50, minPts = 5)
fviz_cluster(db, data = votesdset)