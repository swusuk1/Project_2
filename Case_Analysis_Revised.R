library(rpart)
library(rpart.plot)
library(forecast)
library(tidyr)
library(ROSE)

cred <- read.csv('credit_8.csv')
names(cred)

cred <- drop_na(cred)

head(cred,10)
str(cred)

# Choosing to only keep columns that seem like they're good predictors
cred <- cred[ , c(3, 4, 6:17, 19, 22, 29)]
names(cred)


cred$cat_TARGET <- ifelse(cred$TARGET <= mean(cred$TARGET, na.rm = TRUE), 0, 1)

set.seed(1331)

train_index <-sample(1:nrow(cred), 0.6*nrow(cred))
valid_index <-setdiff(1:nrow(cred), train_index)

train_df <- cred[train_index, ]
valid_df <- cred[valid_index, ]

nrow(train_df)
nrow(valid_df)

train_df$TARGET <- as.factor(train_df$TARGET)
train_df_balanced <- ROSE(TARGET ~  CNT_CHILDREN 
                          + AMT_INCOME_TOTAL + AMT_CREDIT 
                          + AMT_ANNUITY 
                          + AMT_GOODS_PRICE 
                          + DAYS_EMPLOYED,
                          data = train_df, seed = 1331)$data

table(train_df_balanced$TARGET)

class_tr <- rpart(TARGET ~ CNT_CHILDREN 
                  + AMT_INCOME_TOTAL + AMT_CREDIT 
                  + AMT_ANNUITY 
                  + AMT_GOODS_PRICE 
                  + DAYS_EMPLOYED,
                  data = train_df_balanced, method = "class", maxdepth = 20)
prp(class_tr)

# Implementing new record

new_record_class <- data.frame(CNT_CHILDREN = 0,
                               AMT_INCOME_TOTAL = 180000,
                               AMT_CREDIT = 383760, 
                               AMT_ANNUITY = 40428, 
                               AMT_GOODS_PRICE = 360000, 
                               DAYS_EMPLOYED = -1304)
new_record_class2 <- data.frame(CNT_CHILDREN = 0,
                               AMT_INCOME_TOTAL = 292500,
                               AMT_CREDIT = 675000, 
                               AMT_ANNUITY = 24376.5, 
                               AMT_GOODS_PRICE = 675000, 
                               DAYS_EMPLOYED = -1548)

new_record_class3 <- data.frame(CNT_CHILDREN = 0,
                               AMT_INCOME_TOTAL = 157500,
                               AMT_CREDIT = 761067, 
                               AMT_ANNUITY = 33655.5, 
                               AMT_GOODS_PRICE = 657000, 
                               DAYS_EMPLOYED = -2124)

new_record_class4 <- data.frame(CNT_CHILDREN = 0,
                               AMT_INCOME_TOTAL = 90000,
                               AMT_CREDIT = 67500, 
                               AMT_ANNUITY = 7047, 
                               AMT_GOODS_PRICE = 67500, 
                               DAYS_EMPLOYED = 365243)

new_record_class5 <- data.frame(CNT_CHILDREN = 3,
                               AMT_INCOME_TOTAL = 135000,
                               AMT_CREDIT = 301464, 
                               AMT_ANNUITY = 20277, 
                               AMT_GOODS_PRICE = 238500, 
                               DAYS_EMPLOYED = -989)

class_tr <- predict(class_tr, newdata = new_record_class)
class_tr


