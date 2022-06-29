## Cross validation with 20-50 topics to find min (sweep) #
library(tidyverse)
library(topicmodels)
library(doParallel)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(wordcloud)
source("src/lda_topic_modeling_api.R")

document_directory = "data/docs/"
my_desired_number_of_topics = 7
my_desired_number_of_top_words_per_topic = 10

file_list = list.files(document_directory) 
# try and create a doc term matrix from a list of pdf, html, txt, powerpoint and word docs
all_docs_in_a_directory = paste0(document_directory, file_list)
my_docs_df = from.a.list.of.files.to.file.text.df(all_docs_in_a_directory)
my_docs_as_tidy_txt = from.file.text.df.to.tidytext(my_docs_df)

# this gets our very own doc term matrix
my_dtm = from.tidy.text.to.dtm(my_docs_as_tidy_txt)

burnin = 1000
iter = 1000
keep = 50
# define our "full data" - during development I pretend the full dataset is 
# just the first 500 AP articles, which is enough for a reasonable test while not taking
# forever to run.  When I ran the final model, I came back and removed the "1:500" from below
full_data  <- my_dtm
n <- nrow(full_data)
#-----------validation--------
k <- 5 # number of topics
splitter <- sample(1:n, round(n * 0.75))
train_set <- full_data[splitter, ]
valid_set <- full_data[-splitter, ]
fitted <- LDA(train_set, k = k, method = "Gibbs",
              control = list(burnin = burnin, iter = iter, keep = keep) )
perplexity(fitted, newdata = train_set)
perplexity(fitted, newdata = valid_set)

#---------------5-fold cross-validation---------------------
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)
clusterEvalQ(cluster, {
  library(topicmodels)
})
clusterExport(cluster, c("full_data", "k", "burnin", "iter", "keep", "splitfolds"))
results <- foreach(i = 1:folds) %dopar% {
  train_set <- full_data[splitfolds != i , ]
  valid_set <- full_data[splitfolds == i, ]
  
  fitted <- LDA(train_set, k = k, method = "Gibbs",
                control = list(burnin = burnin, iter = iter, keep = keep) )
  return(perplexity(fitted, newdata = valid_set))
}
stopCluster(cluster)

#----------------5-fold cross-validation, different numbers of topics----------------
cluster <- makeCluster(detectCores(logical = TRUE) - 1) # leave one CPU spare...
registerDoParallel(cluster)
clusterEvalQ(cluster, {
  library(topicmodels)
})
folds <- 5
splitfolds <- sample(1:folds, n, replace = TRUE)
candidate_k <- seq(20,50, 1) # sweep for topics based on initial validation
clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
# we parallelize by the different number of topics.  A processor is allocated a value
# of k, and does the cross-validation serially.  This is because it is assumed there
# are more candidate values of k than there are cross-validation folds, hence it
# will be more efficient to parallelise
system.time({
  results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
    k <- candidate_k[j]
    results_1k <- matrix(0, nrow = folds, ncol = 2)
    colnames(results_1k) <- c("k", "perplexity")
    for(i in 1:folds){
      train_set <- full_data[splitfolds != i , ]
      valid_set <- full_data[splitfolds == i, ]
      
      fitted <- LDA(train_set, k = k, method = "Gibbs",
                    control = list(burnin = burnin, iter = iter, keep = keep) )
      results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
    }
    return(results_1k)
  }
})
stopCluster(cluster)
results_df <- as.data.frame(results)
ggplot(results_df, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("5-fold cross-validation of topic modelling with narrowed topic number") +
  labs(x = "Candidate number of topics", y = "Perplexity when fitting the trained model to the hold-out set")
