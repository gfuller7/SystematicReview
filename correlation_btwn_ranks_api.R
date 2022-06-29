library(tidyverse)

compute_correlation_btwn_topic_ranks = function(filename, topic_df, correlation_method="spearman")
{
  for_file_df = topic_df %>% filter(document == filename)
  correlation_result = cor(for_file_df$rank, for_file_df$rankh)
  return(correlation_result)
}