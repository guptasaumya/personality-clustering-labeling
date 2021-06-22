## AMI22T Home Exercise 3, Problem 2
## Personality Analysis
## Saumya Gupta, DS


# set path of all-data export import directory
setwd('C:/Users/gupta/OneDrive/Documents/MS-DS/AMI22T/HomeExercises/HomeExercise3/')


# load required packages
library(data.table)
library(mvtnorm)
library(ggplot2)
library(dplyr)


# load data and store in a data table
survey.results <- fread("five-personality.txt")

survey.results <- data.frame(survey.results)

# check structure if any coding required
str(survey.results)


# check missing data points
sum(is.na(survey.results))

# check number of rows with missing data
sum(apply(survey.results, 1, anyNA))

# calculate percentage of rows with missing data
mean(is.na(survey.results))

# remove missing data
survey.results <- na.omit(survey.results)

# # save column-wise distributions
# for (question in names(survey.results)) {
#   column.distribution.plot = ggplot(survey.results,
#                                     aes_string(question)) + geom_histogram()
#
#   ggsave(paste(question, "_distribution", ".png"),
#          plot = column.distribution.plot)
# }


# set seed
set.seed(9899)


# Sampling Appropriate Subset (Analysis with K-Means) ----
# get variance-covariance matrix
dispersion <- var(survey.results)

# get mean vector
centroid <- colMeans(survey.results)

# get density heights using the statistics and the survey results
density.heights <-
  dmvnorm(as.matrix(survey.results),
          mean = centroid,
          sigma = dispersion)

# check total within-cluster sum of squares for different k-values
# with sample size = 1000
# pass density heights calculated previously to get similarly distributed sample
sample.people <-
  sample(1:nrow(survey.results), 1000, prob = density.heights)

survey.results.sample <- survey.results[sample.people, ]

total.within.ss = c()

#check with different k values
k.values = 1:15

for (k in k.values) {
  km.output = kmeans(survey.results.sample, k, nstart = 50)
  
  total.within.ss = c(total.within.ss, km.output$tot.withinss)
}

# plot within-cluster sum of squares score with different k (elbow plot)
scores.for.k = data.frame(k.values = k.values,
                          total.within.ss = total.within.ss)

ggplot(scores.for.k, aes(k.values, total.within.ss)) +
  geom_point() +
  ggtitle("Total Within-Cluster Sum of Squares for K-Values
          Sample Size = 1000") +
  theme(plot.title = element_text(hjust = 0.5))


# do same with sample size = 2000
sample.people <-
  sample(1:nrow(survey.results), 2000, prob = density.heights)

survey.results.sample <- survey.results[sample.people, ]

total.within.ss = c()

k.values = 1:15

for (k in k.values) {
  km.output = kmeans(survey.results.sample, k, nstart = 50)
  
  total.within.ss = c(total.within.ss, km.output$tot.withinss)
}

scores.for.k = data.frame(k.values = k.values,
                          total.within.ss = total.within.ss)

ggplot(scores.for.k, aes(k.values, total.within.ss)) +
  geom_point() +
  ggtitle("Total Within-Cluster Sum of Squares for K-Values
          Sample Size = 2000") +
  theme(plot.title = element_text(hjust = 0.5))

# # check different sample sizes with K = 4 for K-Means
# total.within.ss = c()
#
# sample.sizes = seq(500, 2500, by = 50)
#
# for (sample_size in sample.sizes) {
#   sample.people <-
#     sample(1:nrow(survey.results), sample_size, prob = density.heights)
#
#   survey.results.sample <- survey.results[sample.people, ]
#
#   km.output = kmeans(survey.results.sample, 4, nstart = 50)
#
#   total.within.ss = c(total.within.ss, km.output$tot.withinss)
# }
#
# scores.for.sample.size = data.frame(sample.sizes = sample.sizes,
#                                     total.within.ss = total.within.ss)
#
# ggplot(scores.for.sample.size, aes(sample.sizes, total.within.ss)) +
#   geom_point() +
#   ggtitle("Total Within-Cluster Sum of Squares for Sample Sizes\nk = 4") +
#   theme(plot.title = element_text(hjust = 0.5))


# Clustering (Sample Size = 1000, K = 4) ----
## K-Means Clustering ----
sample.people <-
  sample(1:nrow(survey.results), 1000, prob = density.heights)

survey.results.sample <- survey.results[sample.people, ]

# perform K-Means
km.output = kmeans(survey.results.sample, 4, nstart = 50)

# bind cluster results with data
clustered = cbind(survey.results.sample, km.output$cluster)

names(clustered)[51] <- "k.means.cluster"


## Hierarchical Clustering ----
# perform hierarchical with complete linkage
hc.complete = hclust(dist(survey.results.sample), method = "complete")

# perform hierarchical with average linkage
hc.average = hclust(dist(survey.results.sample), method = "average")

# perform hierarchical with single linkage
hc.single = hclust(dist(survey.results.sample), method = "single")

# bind cluster results with data
clustered = cbind(clustered,
                  cutree(hc.complete, 4),
                  cutree(hc.average, 4),
                  cutree(hc.single, 4))

names(clustered)[52] <- "hierarchical.complete.cluster"

names(clustered)[53] <- "hierarchical.average.cluster"

names(clustered)[54] <- "hierarchical.single.cluster"


# Validations and Visualisations ----
# check for singleton clusters
table(clustered$k.means.cluster)

table(clustered$hierarchical.complete.cluster)

table(clustered$hierarchical.average.cluster)

table(clustered$hierarchical.single.cluster)

# remove cluster results from average and single linkage
clustered <- clustered %>% select(-c(53, 54))

# check agreement between results from hierarchical and K-Means clustering
table(clustered$k.means.cluster,
      clustered$hierarchical.complete.cluster)

# in proportions from hierarchical clusters perspective
prop.table(
  table(
    clustered$k.means.cluster,
    clustered$hierarchical.complete.cluster
  ),
  margin = 1
)

# use first two PCs to accommodate visualising clusters
# find principal components
principal.components <-
  prcomp(clustered %>% select(-c(51, 52)), scale = TRUE)

# next three code snippets are just for information
# calculate variances using standard deviation of the projected points
pc.variances <- (principal.components$sdev) ^ 2

# calculate proportion explained
pc.proportion.variances <- pc.variances / sum(pc.variances)

# get components collectively explaining 90% of input variance
number.of.components <-
  length(pc.proportion.variances[cumsum(pc.proportion.variances) < .90]) + 1

# get first two components to represent points
# (although will not represent properly)
first.2.PCs = principal.components$x[, 1:2]

# plot K-Means results
plot(
  first.2.PCs,
  col = (clustered$k.means.cluster + 1),
  main = "K-Means Clustering Results\n(K=4)",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 1
)

# plot hierarchical results
plot(
  first.2.PCs,
  col = (clustered$hierarchical.complete.cluster + 1),
  main = "Hierarchical Clustering Results\n(K=4)",
  xlab = "",
  ylab = "",
  pch = 20,
  cex = 1
)

# cluster labelling algorithm according to K-Means results
# define reversed question from survey
negatives = c(
  "mean_EXT2",
  "mean_EXT4",
  "mean_EXT6",
  "mean_EXT8",
  "mean_EXT10",
  "mean_EST1",
  "mean_EST3",
  "mean_EST4",
  "mean_EST5",
  "mean_EST6",
  "mean_EST7",
  "mean_EST8",
  "mean_EST9",
  "mean_EST10",
  "mean_AGR1",
  "mean_AGR3",
  "mean_AGR5",
  "mean_AGR7",
  "mean_CSN2",
  "mean_CSN4",
  "mean_CSN6",
  "mean_CSN8",
  "mean_OPN2",
  "mean_OPN4",
  "mean_OPN6"
)

# get cluster means
# multiply reversed statement with -1 for further grouping along row axis
k.means <- clustered %>%
  group_by(k.means.cluster) %>%
  summarise(across(1:50, list(mean = mean), .names = "mean_{.col}")) %>%
  mutate(across(all_of(negatives), ~ .x * (-1)))

# get personality wise row sums (this is considering reversed statements)
k.means$sum_EXT = rowSums(k.means %>% select(contains("EXT")))

k.means$sum_EST = rowSums(k.means %>% select(contains("EST")))

k.means$sum_ARG = rowSums(k.means %>% select(contains("AGR")))

k.means$sum_CSN = rowSums(k.means %>% select(contains("CSN")))

k.means$sum_OPN = rowSums(k.means %>% select(contains("OPN")))

# analyse sums column for labelling
k.means = k.means %>% select(contains("sum"))

seq(-20, 20, by = 10)

seq(-44, 4, by = 12)

seq(-14, 26, by = 10)

seq(-14, 26, by = 10)

seq(-8, 32, by = 10)

# Extrovert, Stable, Agreeable, Conscious, Open

# "33333" = 'Average'
# "23434" = 'ReservedCompassionateCurious'
# "34434" = 'StableCompassionateCurious'
# "33444" = 'CompassionateCuriousOrganized'

# replace cluster numbers with labels
clustered$k.means.cluster[clustered$k.means.cluster == 1] <-
  "Neutral"

clustered$k.means.cluster[clustered$k.means.cluster == 2] <-
  "ReservedCompassionateCurious"

clustered$k.means.cluster[clustered$k.means.cluster == 3] <-
  "StableCompassionateCurious"

clustered$k.means.cluster[clustered$k.means.cluster == 4] <-
  "CompassionateCuriousOrganized"

# plot cluster sizes with new cluster labels
ggplot(clustered,
       aes(k.means.cluster, fill = k.means.cluster)) +
  geom_bar() +
  ggtitle("Cluster Sizes") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "None")


# Prediction of Unseen Subset ----
# get sample of unseen people from population
all.people = 1:nrow(survey.results)

unseen.people = all.people[!all.people %in% sample.people]

# predictions on 100 people (randomly decided)
unseen.people <-
  sample(unseen.people, 100)

survey.results.unseen <- survey.results[unseen.people, ]

# use Euclidean distance with cluster centroids to assign nearest cluster
predicted.cluster <- rep(0, 100)

survey.results.unseen <-
  cbind(survey.results.unseen, predicted.cluster)

for (person in row.names(survey.results.unseen)) {
  distance = c(dist(rbind(
    survey.results.unseen[person,], km.output$centers[1,]
  )),
  dist(rbind(
    survey.results.unseen[person,], km.output$centers[2,]
  )),
  dist(rbind(
    survey.results.unseen[person,], km.output$centers[3,]
  )),
  dist(rbind(
    survey.results.unseen[person,], km.output$centers[4,]
  )))
  
  survey.results.unseen[person, "predicted.cluster"] = which.min(distance)
}

survey.results.unseen$predicted.cluster[survey.results.unseen$predicted.cluster == 1] <-
  "Neutral"

survey.results.unseen$predicted.cluster[survey.results.unseen$predicted.cluster == 2] <-
  "ReservedCompassionateCurious"

survey.results.unseen$predicted.cluster[survey.results.unseen$predicted.cluster == 3] <-
  "StableCompassionateCurious"

survey.results.unseen$predicted.cluster[survey.results.unseen$predicted.cluster == 4] <-
  "CompassionateCuriousOrganized"

# plot cluster assignments
ggplot(survey.results.unseen,
       aes(predicted.cluster, fill = predicted.cluster)) +
  geom_bar() +
  ggtitle("Label Assignments for Unseen Sample") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "None")
