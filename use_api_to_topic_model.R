library(tidyverse)
source("src/lda_topic_modeling_api.R")

document_directory = "data/docs/"
my_desired_number_of_topics = 41
my_desired_number_of_top_words_per_topic = 10

file_list = list.files(document_directory) 
# try and create a doc term matrix from a list of pdf, html, txt, powerpoint and word docs
all_docs_in_a_directory = paste0(document_directory, file_list)
my_docs_df = from.a.list.of.files.to.file.text.df(all_docs_in_a_directory)
my_docs_as_tidy_txt = from.file.text.df.to.tidytext(my_docs_df)

# this gets our very own doc term matrix
my_dtm = from.tidy.text.to.dtm(my_docs_as_tidy_txt)

# now we can topic model with it
my_lda = get.lda(my_dtm, my_desired_number_of_topics)
my_topics = get.tidy.topics.from.lda(my_lda)
my_top_terms = get.top.terms.from.topics(my_topics, my_desired_number_of_top_words_per_topic)

# my_topics_per_doc
my_topics_per_doc = get.tidy.topics.from.lda(my_lda, per.document = TRUE)

my_tidy_docs_classified_into_topics = get.tidy.document.classification.from.lda(my_topics_per_doc)
my_tidy_docs_classified_into_topics

# check out this link https://www.r-bloggers.com/2017/01/cross-validation-of-topic-modelling/
# and see if you can get that code running (it will take an overnight run to complete the example code in the link)

#extracting topic list from my_topics
topic_list <- unique(my_topics[[2]])

#Write to csv
topic_df <- as.data.frame(topic_list)
write_csv(topic_df, file = "topics.csv")

topics_df <- as.data.frame(my_topics)

#Get rid of numbers
topics_no_num <- topics_df %>% filter(term %>% str_detect("^[:alpha:]+$")) 
write_csv(topics_no_num, file = "topics_and_topic_number.csv")

# topics organized into categories
categorized_topics <- read.csv('topics_and_category.csv')
my_topics_with_categories = inner_join(my_topics_per_doc, categorized_topics)%>% 
  group_by(document, category) %>% summarise(cat_gamma = sum(gamma)) %>% 
  mutate(rank = rank(-cat_gamma, ties.method = "min")) %>%
  arrange(document, rank, cat_gamma) %>% ungroup()

my_tidy_docs_classified_into_topics = get.tidy.document.classification.from.lda(my_topics_per_doc)
my_tidy_docs_classified_into_topics

# Prep and organize data for spearman correlation

## import human topic modeling
human <- read.csv("categorized.csv")  # csv containing human rankings

## Combine two data frames
correlations_df = inner_join(my_topics_with_categories, human)

vector_of_corr_results = c()  #empty vector to append to
# loop to find correlation vals for each doc.
for (x in all_docs_in_a_directory){
  current_document_result = compute_correlation_btwn_topic_ranks(x, correlations_df)
  vector_of_corr_results <- c(vector_of_corr_results, current_document_result)
}

corr_results_df <- as.data.frame(vector_of_corr_results)

# summary statistics on vector_of_corr_results min, max, mean, median, std_dev
summary(corr_results_df)
sd(vector_of_corr_results)
