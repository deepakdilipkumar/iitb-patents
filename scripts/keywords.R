library(RTextTools)
library(topicmodels)
library(ggplot2)

filed <- read.csv("data/filed.csv")
granted <- read.csv("data/granted.csv")

dtm <- create_matrix(granted$Title,
                     stemWords=TRUE,
                     removeStopwords=TRUE,
                     minWordLength=3,
                     removePunctuation= TRUE)

# assuming 20 topics LDA model
lda <- LDA(dtm, 10)


SEED = sample(1:1000000, 1)
k = 10

models <- list(
  CTM       = CTM(dtm, k = k, control = list(seed = SEED, var = list(tol = 10^-4), em = list(tol = 10^-3))),
  VEM       = LDA(dtm, k = k, control = list(seed = SEED)),
  VEM_Fixed = LDA(dtm, k = k, control = list(estimate.alpha = FALSE, seed = SEED)),
  Gibbs     = LDA(dtm, k = k, method = "Gibbs", control = list(seed = SEED, burnin = 1000,
                                                               thin = 100,    iter = 1000))
)

terms(models$CTM,10)
terms(models$VEM,10)
terms(models$VEM_Fixed,10)
terms(models$Gibbs,10)
