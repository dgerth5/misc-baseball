library(readr)
library(tidyverse)

statcast_19 <- read_csv("C:/Users/david/Downloads/statcast-19.csv")

swing_df <- statcast_19 %>%
  mutate(swing = if_else(type == "S" | type == "X", 1, 0),
         count = paste0(balls, "-", strikes),
         adj_score = bat_score - fld_score,
         std_pfx_x = if_else(p_throws == "R", pfx_x, pfx_x*-1),
         std_release_pos_x = if_else(release_pos_x == "R", release_pos_x, release_pos_x*-1))%>%
  dplyr::select(swing, count, release_speed, std_release_pos_x, release_pos_z, release_spin_rate, 
                release_extension, release_spin_rate, adj_score, effective_speed, plate_x, plate_z, pfx_z, std_pfx_x,
                p_throws, stand, zone) %>%
  drop_na()

# plot swing_df
ggplot(swing_df, aes(x = as.factor(swing))) +
  geom_bar(fill = "green", aes(y=..count../sum(..count..)))

# set factors

swing_df$count <- as.factor(swing_df$count)
swing_df$p_throws <- as.factor(swing_df$p_throws)
swing_df$stand <- as.factor(swing_df$stand)
swing_df$zone <- as.factor(swing_df$zone)

small_swing <- swing_df[sample(nrow(swing_df), 50000), ]

# split train/test
library(caTools)
set.seed(135)

sample <- sample.split(small_swing$swing, SplitRatio = .75)
train <- subset(small_swing, sample == TRUE)
test  <- subset(small_swing, sample == FALSE)

# probit

s_probit <- glm(swing ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x,
                data = train, 
                family = binomial(link = "probit"))

p_probit <- predict(s_probit, test, type = "response")

# lasso/ridge
library(glmnet)
# train
x_mat <- model.matrix(swing ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x, data = train)[,-1]
y <- train$swing
# test 
x_mat_test <- model.matrix(swing ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x, data = test)[,-1]
y_test <- test$swing

# ridge
cv.ridge.lambda <- cv.glmnet(x_mat, y, alpha = 0, family = "binomial")
plot(cv.ridge.lambda)
s_ridge <- glmnet(x_mat, y, alpha = 0, family = "binomial", lambda = cv.ridge.lambda$lambda.1se)
coef(s_ridge)

p_ridge <- predict(s_ridge, newx = x_mat_test, s = cv.ridge.lambda$lambda.1se,
                   type = "response")

# lasso
cv.lasso.lambda <- cv.glmnet(x_mat, y, alpha = 1, family = "binomial")
plot(cv.lasso.lambda)
s_lasso <- glmnet(x_mat, y, alpha = 1, family = "binomial", lambda = cv.lasso.lambda$lambda.1se)
coef(s_lasso)

p_lasso <- predict(s_lasso, newx = x_mat_test, s = cv.lasso.lambda$lambda.1se,
                   type = "response")

# catboost  - gradient boosting
library(catboost)

train_pool <- catboost.load_pool(x_mat, as.matrix(y))
s_catboost <- catboost.train(train_pool, params = list(loss_function = 'Logloss',
                                                       iterations = 1000, metric_period=1))
test_pool <- catboost.load_pool(x_mat_test, as.matrix(y_test))
p_catboost <- catboost.predict(s_catboost, test_pool, prediction_type = "Probability")

# naive bayes 
library(naivebayes)
# had issues with zero probabilities, changed by laplace smoothing 
s_nb <- naive_bayes(as.factor(swing) ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x,
                    data = train, usekernel = TRUE, laplace = TRUE)
test1 <- test[,-1]
p_nb <- predict(s_nb, test1, type = "prob")[,2]
# random forest - bagging
library(ranger)

s_rf <- ranger(as.factor(swing) ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x,
               data = train,
               num.trees = 500,
               probability = TRUE)

p_rf <- predict(s_rf, test1)$predictions

# xgboost - gradient boosting
library(xgboost)
library(caret)

xgb_train <- xgb.DMatrix(data = x_mat, label = y)
xgb_test <- xgb.DMatrix(data = x_mat_test, label = y_test)

watchlist = list(train=xgb_train, test=xgb_test)

# originally used nrounds = 100, but logloss started to increase after 56
xgb_train <- xgb.train(data = xgb_train, max.depth = 4, watchlist = watchlist, nrounds = 100, objective = "binary:logistic")
s_xgb <- xgboost(data = xgb_train, max.depth = 4, nrounds = 56, objective = "binary:logistic")

p_xgb <- predict(xgb_train, xgb_test)

# decision tree
library(rpart)

s_dt <- rpart(as.factor(swing) ~ count + p_throws + stand + release_speed + release_spin_rate + release_extension +  adj_score + zone + pfx_z + std_pfx_x,
              data = train)
p_dt <- predict(s_dt, test1, type = "prob")[,2]

# looking at correlations between predictions

df <- data.frame(pbt = p_probit, ridge = p_ridge, lasso = p_lasso, cat = p_catboost,
                 nb = p_nb, dt = p_dt, rf = p_rf[,2], xgb = p_xgb)
cor(df)
