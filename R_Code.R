#Exploratory analysis
combined.data <- rbind(training_data, test_data)
View(combined.data)
dim(combined.data)
combined.data$num_of_symptoms <- sapply(gregexpr("[[:alpha:]]+", combined.data$symptoms), function(x) sum(x > 0))
combined.data$diagnosis_date <- paste("01-",combined.data$diagnosis_date,sep='')
combined.data$diagnosis_date <- strptime(combined.data$diagnosis_date, format = "%d-%b-%y")
combined.data$diagnosis_date <- format(combined.data$diagnosis_date, "%d/%m/%y")

col_num <- c(3:7, 9, 12:16, 23, 25:34)
combined.data[,col_num] <- lapply(combined.data[,col_num] , factor)

combined.data$age <- as.numeric(combined.data$age)
combined.data$height <- as.numeric(combined.data$height)
combined.data$weight <- as.numeric(combined.data$weight)
combined.data$tumor_diagnosis <- as.numeric(combined.data$tumor_diagnosis)
combined.data$psa_6_months <- as.numeric(combined.data$psa_6_months)
combined.data$psa_1_year <- as.numeric(combined.data$psa_1_year)
combined.data$tumor_1_year <- as.numeric(combined.data$tumor_1_year)
combined.data$tumor_6_months <- as.numeric(combined.data$tumor_6_months)

str(combined.data)

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}


for (var in 1:ncol(combined.data)) {
  if (class(combined.data[,var])=="numeric") {
    combined.data[is.na(combined.data[,var]),var] <- mean(combined.data[,var], na.rm = TRUE)
  } else if (class(combined.data[,var]) %in% c("character", "factor")) {
    combined.data[is.na(combined.data[,var]),var] <- Mode(combined.data[,var], na.rm = TRUE)
  }
}

summary(combined.data)
tail(combined.data)
training_data <- combined.data[1:15385,]
test_data <- combined.data[15386:26916,]

for (var in 1:ncol(training_data)) {
  if (class(training_data[,var])=="numeric") {
    training_data[is.na(training_data[,var]),var] <- mean(training_data[,var], na.rm = TRUE)
  } else if (class(training_data[,var]) %in% c("character", "factor")) {
    training_data[is.na(training_data[,var]),var] <- Mode(training_data[,var], na.rm = TRUE)
  }
}

test_data$survival_7_years <- 'NA'
View(test_data)
str(training_data)
str(test_data)
dim(training_data)
dim(test_data)
summary(training_data)
summary(test_data)
# View(test_data$survival_7_years)
# svm.model <- svm(survival_7_years ~., data = training_data, cost = 100, gamma = 1)
# svm.pred <- predict(svm.model, test_data[,-33])
# table(training_data$survival_7_years)
# table(pred = svm.pred, true = test_data[,33])

#randomforest approach
library(randomForest)
rf0 <- randomForest(survival_7_years ~ gleason_score + n_score + m_score + stage + 
                      age + race + weight + smoker + tumor_diagnosis + tumor_1_year + 
                      rd_thrpy + h_thrpy + brch_thrpy + rad_rem + multi_thrpy + 
                      survival_1_year + num_of_symptoms,
                      data = training_data, ntree = 500,importance = T,
                     mtry = 3)
print(rf0)

rf1 <- randomForest(survival_7_years ~ . -id -diagnosis_date -symptoms -previous_cancer 
                    -smoker -chm_thrpy -cry_thrpy -brch_thrpy -rad_rem -first_degree_history  
                    -h_thrpy -multi_thrpy 
                    , data = training_data, ntree = 500,importance = T,
                    mtry = 4)
print(rf1)
plot(rf1)
rf1$predicted
rf1$importance
# 0             1 MeanDecreaseAccuracy MeanDecreaseGini
# gleason_score    9.138138e-03  1.817929e-03         5.974649e-03        401.51255 log
# t_score         -3.941188e-03  8.337345e-03         1.370131e-03        437.90576 
# n_score          2.316340e-02  1.697969e-02         2.048834e-02        316.22239 log
# m_score          3.123868e-03  3.762977e-03         3.401583e-03         85.85739 log 
# stage            9.254201e-03  1.061691e-02         9.847664e-03        201.62709 log
# age              1.209215e-03 -5.114320e-04         4.623641e-04        520.14179 
# race             7.251368e-05 -2.900451e-04        -8.519755e-05        149.33240 log
# height           2.066831e-04  2.313583e-04         2.201647e-04        270.96809 
# weight           3.188983e-04  8.584986e-04         5.542140e-04        544.47133 log
# family_history   2.842609e-04 -1.091241e-04         1.140583e-04        121.67872 
 #log smoker
# side            -2.251415e-05 -3.976130e-04        -1.856060e-04        157.91314
# tumor_diagnosis  2.842117e-03  2.249595e-03         2.584042e-03        531.63913 log
# tumor_6_months   1.492846e-03  1.810594e-04         9.232810e-04        247.72807 
# tumor_1_year     3.090187e-02  4.203531e-03         1.936683e-02        650.57733 log
# psa_diagnosis    3.749571e-03  1.099026e-03         2.602239e-03        566.72076 
# psa_6_months     1.149469e-03  1.102774e-03         1.128480e-03        293.29616
# psa_1_year       2.490139e-02 -6.090669e-04         1.386203e-02        628.26245
# tea              3.346899e-05 -1.522166e-05         1.275932e-05        424.89957
# rd_thrpy         1.396067e-02  8.409583e-03         1.155956e-02        187.39775 log
# h_thrpy1 brch_thrpy1 rad_rem1 multi_thrpy1 
# survival_1_year  4.078937e-02  3.092886e-02         3.651314e-02        414.54736
# num_of_symptoms  6.595819e-04  4.736690e-04         5.813321e-04        366.39786 log

boxplot(training_data$tumor_1_year ~ training_data$survival_7_years)
plot(density(training_data$tumor_1_year))
summary(training_data$tumor_1_year)
training_data$tumor_1_year[training_data$tumor_1_year <= 37] <- "less than or eq 37"
training_data$tumor_1_year[training_data$tumor_1_year > 37] <- "greater than 37"
str(training_data)
training_data$tumor_1_year <- as.factor(training_data$tumor_1_year)
View(training_data)
boxplot(training_data$psa_1_year ~ training_data$survival_7_years)
plot(density(training_data$psa_1_year))
summary(training_data$psa_1_year)
summary(training_data$psa_diagnosis)
plot(density(training_data$psa_diagnosis))
summary(training_data$weight)
plot(density(training_data$weight))

x <- training_data[,c(-1,-2,-24,-33)]
y <- training_data[,33]
tuneRF(x, y, mtry = 3, ntreeTry = 500, stepFactor=0.5, improve=0.05)

Predict.survived <- predict(rf0, test_data)
table(Predict.survived, test_data$survival_7_years)
# 
# Predict.survived   NA
# 0 5562
# 1 5969

# Predict.survived   NA
# 0 5490
# 1 6041
test_data$survival_7_years <- Predict.survived
class(training_data$symptoms)
training_data$symptoms <- as.character(training_data$symptoms)
table(nchar(c(training_data$symptoms)))
tail(nchar(c(training_data$symptoms)))
nchar(training_data$symptoms)
?substr
training_data_copy <- training_data
training_data$num_of_symptoms <- sapply(gregexpr("[[:alpha:]]+", training_data$symptoms), function(x) sum(x > 0))
str(training_data)
training_data$num_of_symptoms <- as.factor(training_data$num_of_symptoms)


test_data$num_of_symptoms <- sapply(gregexpr("[[:alpha:]]+", test_data$symptoms), function(x) sum(x > 0))
test_data$num_of_symptoms <- as.factor(test_data$num_of_symptoms)
str(test_data)
#logistic regression 

fit1 <- glm(survival_7_years ~ gleason_score + n_score + m_score + stage + 
              age + race + weight + smoker + tumor_diagnosis + tumor_1_year + 
              rd_thrpy + h_thrpy + brch_thrpy + rad_rem + multi_thrpy + 
              survival_1_year + num_of_symptoms, data = training_data, family = binomial(logit))
summary(fit1)
library(MASS)
step <- stepAIC(fit1, direction="both")
step$anova
predict.fit1<- predict(fit1, test_data, type="response")

# gleason_score + t_score + n_score + m_score + 
#   stage + age + race + height + weight + family_history + first_degree_history + 
#   previous_cancer + smoker + side + tumor_diagnosis + tumor_6_months + 
#   tumor_1_year + psa_diagnosis + psa_6_months + psa_1_year + 
#   rd_thrpy + h_thrpy + chm_thrpy + cry_thrpy + brch_thrpy + 
#   rad_rem + multi_thrpy + survival_1_year + num_of_symptoms
getwd()
write.csv(test_data, file = "Enova_submission.csv", row.names = FALSE)

library(car)
vif(fit1) 
sqrt(vif(fit1)) > 2

boxplot(training_data$age ~ training_data$survival_1_year)
plot(density(training_data$weight))

table(training_data$survival_7_years, training_data$num_of_symptoms)
barplot(prop.table(table(training_data$num_of_symptoms, training_data$survival_7_years)))

# library(rpart)
# library(rpart.plot)
# tree.rpart1 <- rpart(survival_7_years~.,data = training_data, method = "class", control = rpart.control(minsplit = 500, minbucket = 100, cp = 0.001))
# rpart.plot(tree.rpart1)
# tree.pred1 <- predict(tree.rpart1, test_data, type = "class")

#Bivariate Analysis

library(ggplot2)
summary(training_data)
summary(test_data)
View(training_data)
library(corrplot)

ggplot(training_data, aes(x= race, fill=survival_7_years)) + 
  geom_bar(position="dodge") +
  labs(title="Survival by Race", x="Race", y="Count of Survivals") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Survival for 7 years")
#For Race 4, cases and deaths both are more

ggplot(training_data, aes(x= gleason_score, fill=survival_7_years)) + 
  geom_bar(position="dodge") +
  labs(title="Survival by Gleason_Score", x="gleason_score", y="Count of Survivals") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Survival for 7 years")
#For Gleason_score 7-10, deaths are more than survivals. This shows the importance and relation of gleason_score

ggplot(training_data, aes(x= stage, fill=survival_7_years)) + 
  geom_bar(position="dodge") +
  labs(title="Survival by Stage", x="stage", y="Count of Survivals") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Survival for 7 years")
#Very obvious one, it shows that for stage 4 the deaths are almost double than survivals

training_tab<- table(training_data$gleason_score, training_data$survival_7_years)
prop.table(training_tab)*100
chisq.test(training_tab)
#Chi_sq testing to prove the relation between variables

ggplot(data=training_data, aes(x=age, fill=survival_7_years))+geom_density(alpha=.8) +
  labs(title="age distribution by survival", x="age", y="survival") +  
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "survival_7_years")

training_data_cornum <- training_data[,c('age', 'tumor_1_year')]
cormat <- cor(training_data_cornum)
corrplot(cormat)
#We know that tumor_1_year has strong correlation with survival_7_year but, 
#age doesn't have any correlation withtumor_1_year. This shows that age is not an important factor.

summary(training_data$weight)
plot(density(training_data$weight))
length(training_data$weight[training_data$weight <176])
ggplot(data=training_data, aes(x=log(weight), fill=survival_7_years))+geom_density(alpha=.8) +
  labs(title="Price Distribution by Chicago Regions", x="Log(Price)", y="Density") +  
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "survival_7_years")
#doesn't give a very clear picture.

boxplot(tumor_1_year ~ survival_7_years, data=training_data, main="Performance Change After Training by Gender", 
        xlab="Gender", ylab="Performance",
        col=c("orange", "lightblue4")) 
summary(training_data$tumor_1_year)
age_temp_1 <- training_data$age[training_data$age > 75]
age_temp_1 <- as.data.frame(age_temp_1)
age_temp_1[age_temp_1 > 75] <- "75+"
age_temp_1[age_temp_1 <= 75] <- "75-"
table(age_temp_1)
View(age_temp_1)
age_temp_1$sur <- training_data$survival_1_year
tab <- table(age_temp_1$age_temp_1, age_temp_1$sur )
tab1 <- prop.table(tab,2)
barplot(tab1, xlab="survived", col=c("darkgrey", "darkgoldenrod1"), main="survived by age")

#No dominant correlation between age and survival as we checked for both "survival_1_year" and "survival_7_years"

tumor_temp <- training_data$tumor_1_year
tumor_temp <- as.data.frame(tumor_temp)

tumor_temp[tumor_temp > 37] <- "37+"
tumor_temp[tumor_temp <= 37] <- "37-"
tumor_temp[tumor_temp != "37+" & tumor_temp != "37-" ] <- "others"
tumor_temp$sur <- training_data$survival_7_years
tumor_temp$tumor_temp <- as.factor(tumor_temp$tumor_temp) 
tumor_temp$sur <- as.factor(tumor_temp$sur)
tab <- table(tumor_temp$tumor_temp, tumor_temp$sur)
tab1 <- prop.table(tab,2)
barplot(tab1, xlab="survived", col=c("darkgrey", "darkgoldenrod1", "black"), main="survived by tumor_1_year")

#Therefore tumor_1_year is an important factor as it clearly determines that if the size of the tumor 
# after 1 year of diagnosis is more than 37mm, chances of death are more.

# Checking for psa_1_year

boxplot(psa_1_year ~ survival_7_years, data = training_data)
summary(training_data$psa_1_year)
psa_temp <- training_data$psa_1_year
psa_temp <- as.data.frame(psa_temp)
psa_temp[psa_temp > 9.0000] <- "9+"
psa_temp[psa_temp <= 9.0000] <- "9-"
table(psa_temp)
psa_temp$sur <- training_data$survival_7_years
psa_temp

tab<- table(psa_temp$psa_temp, psa_temp$sur)
tab1 <- prop.table(tab,2)
barplot(tab1, xlab="survived", col=c("darkgrey", "darkgoldenrod1"), main="survived by psa_1_year")
# Clearly shows the relation that with psa_1_year more than 9, chances of surviving for 7 years is less.

table(training_data$num_of_symptoms, training_data$survival_7_years)

install.packages("caret")
install.packages("e1071")

library(caret) 
library(e1071)
order(str(training_data))
x = training_data[,-33]
y = training_data[,c(3:7,9,11,16,17,19,20,22,23,34)]
#cross validation and training model based on Naive Bayes
model <- naiveBayes(survival_7_years ~ ., data = training_data)
class(model)
summary(model)
print(model)
preds <- predict(model, newdata = test_data)
table(preds)
conf_matrix <- table(preds, test_data$survival_7_years)
prop.table(conf_matrix,2)
tbl_list <- sapply(training_data[-33], table, training_data[,33])
tbl_list <- lapply(tbl_list, t)



cond_probs <- sapply(tbl_list, function(x) { 
  apply(x, 1, function(x) { 
    x / sum(x) }) })

cond_probs <- lapply(cond_probs, t)

print(cond_probs)

ggplot(training_data, aes(x= num_of_symptoms, fill=survival_7_years)) + 
  geom_bar(position="dodge") +
  labs(title="Survival by num_of_symptoms", x="num_of_symptoms", y="Count of Survivals") + 
  theme(plot.title = element_text(hjust = 0.5))+ 
  scale_fill_discrete(name = "Survival for 7 years")
training_tab<- table(training_data$num_of_symptoms, training_data$survival_7_years)
prop.table(training_tab)*100
chisq.test(training_tab)
#Not strong but little correlation between number of symptoms and survival is there.

training_data_new <- training_data
summary(training_data_new)
attach(training_data_new)


#Combining and Recoding the variables for equal weight distribution
library(rockchalk)
levels(training_data_new$m_score)
training_data_new$n_score <-  combineLevels(training_data_new$n_score,levs = c("N1", "NX"), newLabel = c("Not_N0"))

training_data_new$m_score <-  combineLevels(training_data_new$m_score,levs = c("M1a", "M1b", "M1c"), newLabel = c("M1"))

training_data_new$age[training_data_new$age > 75] <- "75+"
training_data_new$age[training_data_new$age <= 75] <- "75-"
training_data_new$age <- as.factor(training_data_new$age)
table(training_data_new$age)

training_data_new$weight[training_data_new$weight > 176] <- "176+"
training_data_new$weight[training_data_new$weight <= 176] <- "176-"
training_data_new$weight <- as.factor(training_data_new$weight)
table(training_data_new$weight, training_data_new$survival_7_years)
tab<- prop.table(table(training_data_new$weight, training_data_new$survival_7_years))*100

levels(training_data_new$family_history)
training_data_new$family_history <-  combineLevels(training_data_new$family_history,levs = c("1", "2", "3", "4", "5"), newLabel = c("yes"))
training_data_new$family_history <-  combineLevels(training_data_new$family_history,levs = c("0"), newLabel = c("no"))

summary(training_data_new)
levels(training_data_new$first_degree_history)
training_data_new$first_degree_history <-  combineLevels(training_data_new$first_degree_history,levs = c("0"), newLabel = c("no"))

training_data_new$tumor_diagnosis[training_data_new$tumor_diagnosis > 47] <- "47+"
training_data_new$tumor_diagnosis[training_data_new$tumor_diagnosis <= 47] <- "47-"
training_data_new$tumor_diagnosis <- as.factor(training_data_new$tumor_diagnosis)
training_data_new$tumor_diagnosis[training_data_new$tumor_diagnosis == "8"] <- "47-"
training_data_new$tumor_diagnosis <- as.factor(training_data_new$tumor_diagnosis)

training_data_new$tumor_1_year[training_data_new$tumor_1_year > 37] <- "37+"
training_data_new$tumor_1_year[training_data_new$tumor_1_year <= 37] <- "37-"
training_data_new$tumor_1_year[training_data_new$tumor_1_year != "37+" & training_data_new$tumor_1_year != "37-" ] <- "others"
table(training_data_new$tumor_1_year)
training_data_new$tumor_1_year <- as.factor(training_data_new$tumor_1_year)

training_data_new$psa_1_year[training_data_new$psa_1_year > 9] <- "9+"
training_data_new$psa_1_year[training_data_new$psa_1_year <= 9] <- "9-"
training_data_new$psa_1_year[training_data_new$psa_1_year != "37+" & training_data_new$psa_1_year != "37-" ] <- "others"
training_data_new$psa_1_year <- as.factor(training_data_new$psa_1_year)
table(training_data_new$psa_1_year)

training_data_new$psa_diagnosis <- training_data$psa_diagnosis
training_data_new$psa_diagnosis[training_data_new$psa_diagnosis > 11] <- "11+"
training_data_new$psa_diagnosis[training_data_new$psa_diagnosis <= 11] <- "11-"
training_data_new$psa_diagnosis[training_data_new$psa_diagnosis != "11+" & training_data_new$psa_diagnosis != "11-" ] <- "others"
training_data_new$psa_diagnosis <- as.factor(training_data_new$psa_diagnosis)
table(training_data_new$psa_diagnosis)

summary(training_data_new)

rf3 <- randomForest(survival_7_years ~. -id -diagnosis_date -height -previous_cancer -smoker
                    -tumor_6_months -psa_6_months -tea -symptoms
                    ,data = training_data_new, ntree= 500, mtry = 5, importance = T)
print(rf3)

x <- training_data_new[,c(-1,-2,-24,-33)]
y <- training_data_new[,33]
tuneRF(x, y, mtry = 3, ntreeTry = 500, stepFactor=1, improve=0.05)

Predict.survived <- predict(rf3, test_data)


fit1 <- glm(survival_7_years ~ gleason_score + t_score + n_score + m_score + stage + 
              age + race + weight +family_history+ first_degree_history + side + tumor_diagnosis 
            + tumor_1_year + psa_diagnosis + psa_1_year + rd_thrpy + h_thrpy + brch_thrpy 
            + rad_rem + multi_thrpy + survival_1_year + num_of_symptoms, 
            data = training_data_new, family = binomial(logit))
summary(fit1)
