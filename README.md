# SystematicReview
R project to read and evaluate files using topic models.

Files: 
lda_topic_modeling_api.R : courtesty of Ross Gore. Used to define functions used in use_api_to_topic_model.R
use_api_to_toic_model.R : originally by Ross Gore, edited by me. Runs the topic model and assists with human validation of the model. Computes Spearman correlation
cross_validation_api_to_topic_model.R : Runs a five-fold cross validation across a range of topic numbers to determine range of optimal topic number.
cross_validation_smallertopics.R : Runs the same five-fold cross validation as above, but on a smaller, identified range of topic numbers to determine optimal topic number. 
correlations_btwn_ranks_api.R : Defines a function to calculate the Spearman correlation on a document level. Used in use_api_to_topic_model.R in a loop over all documents. 
