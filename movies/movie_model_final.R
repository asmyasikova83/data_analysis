#rm(list = ls())
library(recommenderlab)
library(tidyverse)

getwd()

#build, test and evaluate a prediction model for movie recommender

data(MovieLense)
MovieLense

#take a look at the data
MovieLense_df <- recommenderlab::getData.frame(MovieLense) %>%
  as_tibble()

#make a matrix
MovieLense_df_mt <- reshape2::dcast(MovieLense_df,
                               user ~ item,
                               value.var = "rating",
                               fill = 0)

#remove column with users, only ratings
MovieLense_mt <- as.matrix(MovieLense_df_mt[,-1])
colnames(MovieLense_mt) <- colnames(MovieLense_df_mt[,-1])
rownames(MovieLense_mt) <- MovieLense_df_mt$user

#we need a 2-step transformation of the data to make and evluate predictions
MovieLense_mt.CsparseMatrix <- as(MovieLense_mt, "CsparseMatrix")
class(MovieLense_mt.data)

MovieLense_new <- new("realRatingMatrix", data = MovieLense_mt.CsparseMatrix)
class(MovieLense_new)

#for reproducibility
set.seed(2022)

#Methods for realRatingMatrix
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

#prepare a scheme of learning

#k = 3 - number of folds/times to run the evaluation
#train = 0.9 - fraction of the data set used for training
#given = 1 - single number of items given for evaluation
#goodRating = 3 -  threshold at which ratings are good for evaluation
eval_scheme <- evaluationScheme(MovieLense_new,
                                 method = "cross-validation",
                                 k = 3,
                                 train  = 0.9,
                                 given = 1,
                                 goodRating = 3)


#let's look at the size of folds
sapply(eval_scheme@runsTrain, length)

# the list of the algo to test
algorithms <- list(
  "RANDOM"   = list(name = "RANDOM", param = NULL),
  "POPULAR"  = list(name = "POPULAR", param = NULL),
  "SVD" = list(name = "SVD", param = NULL),
  "IBCF" = list(name  = "IBCF", param = NULL))



#train the model, top 15 items for all the algorithms
results <- recommenderlab::evaluate(x      = eval_scheme,
                                    method = algorithms,
                                    type   = "topNList",
                                    n      = seq(1, 15, 1))
#unpack results, top 15 items
avg_conf_matr <- function(res) {
  tmp <- res %>%
    getConfusionMatrix()  %>%
    as.list()
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>%
    mutate(n = seq(1, 15, 1)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR')
}

#draw results
results_tbl <- results %>%
  map(avg_conf_matr) %>%
  enframe() %>%
  unnest(value)

results_tbl %>%
  ggplot(aes(recall, precision,
             color = fct_reorder2(as.factor(name), precision, recall))) +
  geom_line() +
  geom_point() +
  labs(title = "Precision-Recall curves",
       subtitle = "from 1 to 15 movies",
       color = "Model") +
  theme(legend.position = "bottom")


results_tbl %>%
  ggplot(aes(FPR, TPR, color = fct_reorder2(as.factor(name), FPR, TPR))) +
  geom_line() +
  geom_point() +
  labs(title = "ROC curves",
       subtitle = "from 1 to 15 movies",
       color = "Model") +
  theme(legend.position = "bottom")

##test the model
recc_model <- Recommender(data = getData(eval_scheme, "train"),
                          method = "POPULAR")

#make predictions using test data
# train: This is the training set
# known: This is the test set, with the item used to build the recommendations
# unknown: This is the test set, with the item used to test the recommendations

eval_prediction <- predict(object = recc_model,
                           newdata = getData(eval_scheme, "known"),
                           n = 4,
                           type = "topNList")

#evaluate the quality
#given = 4 - how many items were given to create the predictions
#goodRating = 3 - the threshold for determining what rating in data is considered a good rating
eval_accuracy <- calcPredictionAccuracy(x = eval_prediction,
                                        data = getData(eval_scheme, "unknown"),
                                        byUser = FALSE,
                                        given = 4,
                                        goodRating = 3)
eval_accuracy["precision"]
#0.4650794

eval_accuracy["recall"]
#0.03016257

#Conclusion: we will further use a model with method POPULAR, because it yuilds best results
