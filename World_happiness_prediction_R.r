library(performance)
library(randomForest)
library(caTools)
library(dplyr)
library(corrplot)
library(caret)
library(leaps)
library(ggplot2)
library(dplyr)
library(forcats)
library(plotly)

################ DATA CLEANING #########################

## Adding Year column 
data_2015 <- mutate(data_2015, Year = as.character("2015"))
data_2016 <- mutate(data_2016, Year = as.character("2016"))
data_2017 <- mutate(data_2017, Year = as.character("2017"))
data_2018 <- mutate(data_2018, Year = as.character("2018"))
data_2019 <- mutate(data_2019, Year = as.character("2019"))
data_2020 <- mutate(data_2020, Year = as.character("2020"))
data_2021 <- mutate(data_2021, Year = as.character("2021"))

## Adding Overall Rank column in 2 files as its missing
data_2020 <- data_2020 %>%  mutate(id = seq(from = 1, to = 153))
data_2021 <- data_2021 %>%  mutate(id = seq(from = 1, to = 149))

## Renaming all Columns of all files same, which will be used further and are of importance.
data_2021 <- rename(data_2021,
                    "Overall rank" = id,
                    "Country or region" = Country.name,
                    "Score" = Ladder.score ,
                    GDP_per_capita = Logged.GDP.per.capita,
                    Social_support = Social.support,
                    Healthy_life_expectancy = Healthy.life.expectancy,
                    Freedom_to_make_life_choices = Freedom.to.make.life.choices,
                    Generosity = Generosity,
                    Perceptions_of_corruption = Perceptions.of.corruption)

data_2020 <- rename(data_2020,
                    "Overall rank" = id,
                    "Country or region" = Country.name,
                    "Score" = Ladder.score ,
                    GDP_per_capita = Logged.GDP.per.capita,
                    Social_support = Social.support,
                    Healthy_life_expectancy = Healthy.life.expectancy,
                    Freedom_to_make_life_choices = Freedom.to.make.life.choices,
                    Generosity = Generosity,
                    Perceptions_of_corruption = Perceptions.of.corruption)


data_2019 <- rename(data_2019,
                    "Overall rank" = Overall.rank,
                    "Country or region" = Country.or.region,
                    Score = Score ,
                    GDP_per_capita = GDP.per.capita,
                    Social_support = Social.support,
                    Healthy_life_expectancy = Healthy.life.expectancy,
                    Freedom_to_make_life_choices = Freedom.to.make.life.choices,
                    Generosity = Generosity,
                    Perceptions_of_corruption = Perceptions.of.corruption)

data_2018 <- rename(data_2018,
                    "Overall rank" = Overall.rank,
                    "Country or region" = Country.or.region,
                    Score = Score ,
                    GDP_per_capita = GDP.per.capita,
                    Social_support = Social.support,
                    Healthy_life_expectancy = Healthy.life.expectancy,
                    Freedom_to_make_life_choices = Freedom.to.make.life.choices,
                    Generosity = Generosity,
                    Perceptions_of_corruption = Perceptions.of.corruption)


data_2017 <- rename(data_2017,
                    "Overall rank" = Happiness.Rank,
                    "Country or region" = Country,
                    Score = Happiness.Score,
                    GDP_per_capita = Economy..GDP.per.Capita.,
                    Social_support = Family,
                    Healthy_life_expectancy = Health..Life.Expectancy.,
                    Freedom_to_make_life_choices = Freedom,
                    Generosity = Generosity,
                    Perceptions_of_corruption = Trust..Government.Corruption.)

data_2016 <- rename(data_2016,
                    "Overall rank" = Happiness.Rank
                    , "Country or region" = Country
                    , Score = Happiness.Score
                    , GDP_per_capita = Economy..GDP.per.Capita.
                    , Social_support = Family
                    , Healthy_life_expectancy = Health..Life.Expectancy.
                    , Freedom_to_make_life_choices = Freedom
                    , Generosity = Generosity
                    , Perceptions_of_corruption = Trust..Government.Corruption.)


data_2015 <- rename(data_2015,
                    "Overall rank" = Happiness.Rank
                    , "Country or region" = Country
                    , Score = Happiness.Score
                    , GDP_per_capita = Economy..GDP.per.Capita.
                    , Social_support = Family
                    , Healthy_life_expectancy = Health..Life.Expectancy.
                    , Freedom_to_make_life_choices = Freedom
                    , Generosity = Generosity
                    , Perceptions_of_corruption = Trust..Government.Corruption.)

## Correcting Datatype for 2018 file column of Perceptions_of_corruption to match across all files.
data_2018$Perceptions_of_corruption <- as.double(data_2018$Perceptions_of_corruption)

## Combining all 7 years of data in a single dataframe.
data_2015_to_2021 <- bind_rows(data_2015, data_2016, data_2017, data_2018, data_2019, data_2020, data_2021)

head(data_2015_to_2021)

## Creating Continent column to do some visualisation and Exploratory Data Analysis.
data_2015_to_2021$Continent <- NA
data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                                   "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                                   "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                                   "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                                   "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                                   "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                                   "Cambodia", "Afghanistan", "Yemen", "Syria"))] <- "Asia"
data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                                   "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                                   "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                                   "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                                   "Russia", "Lithuania", "Latvia", "Moldova", "Romania",
                                                                   "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                                   "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                                   "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                                   "Bulgaria", "Albania", "Ukraine"))] <- "Europe"
data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("Canada", "Costa Rica", "United States", "Mexico",  
                                                                   "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                                   "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                                   "Haiti"))] <- "North America"
data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                                   "Colombia", "Ecuador", "Bolivia", "Peru",
                                                                   "Paraguay", "Venezuela"))] <- "South America"
data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("New Zealand", "Australia"))] <- "Australia"
data_2015_to_2021$Continent[which(is.na(data_2015_to_2021$Continent))] <- "Africa"

data_2015_to_2021$Continent[which(data_2015_to_2021$Country %in% c("Antarctica"))] <- "Antarctica"

## Moving the continent column's position in the dataset to the second column

data_2015_to_2021 <- data_2015_to_2021 %>% select("Country or region",Continent, everything())
attach(data_2015_to_2021)

## Selecting only 11 columns from the dataframe to do further analysis after we found there are missing values of other columns across multiple files and marking NAs to 0.
data_2015_to_2021 <- select(data_2015_to_2021, "Country or region", "Continent", "Overall rank" , Score,
                            GDP_per_capita , Social_support , Healthy_life_expectancy, Freedom_to_make_life_choices, Perceptions_of_corruption, Generosity, Year)

data_2015_to_2021[is.na(data_2015_to_2021)] <- 0

###### Checking for outliers
outliers<- function(column){
  lower.bound<-quantile(column,0.25) - 1.5*IQR(column)
  upper.bound<-quantile(column,0.75) + 1.5*IQR(column)
  return(column[column<lower.bound | column>upper.bound])
}
outliers(data_2015_to_2021$GDP_per_capita)
outliers(data_2015_to_2021$Social_support)
outliers(data_2015_to_2021$Healthy_life_expectancy)
outliers(data_2015_to_2021$Freedom_to_make_life_choices)
outliers(data_2015_to_2021$Perceptions_of_corruption)
outliers(data_2015_to_2021$Generosity)
outliers(data_2015_to_2021$'Overall rank')
outliers(data_2015_to_2021$Score)

datatable(data_2015_to_2021)


# Data Visualisation

# Compute descriptive statistics

plot_ly(data_2015_to_2021, x = ~Score, color = ~Continent, type = "box")%>%
  layout(title = 'Box plot for Score Continentwise on full Dataset',plot_bgcolor='#e5ecf6')

## 
data_2015_to_2021
df <- data_2015_to_2021
tail(df)
unique(df$Year)
unique(df$Continent)
df_all <- df
slope <- 5000
df_all$size <- sqrt(df_all$Freedom_to_make_life_choices  * slope)

## Animations for GDP per capita vs Score for year 2015 to 2021

df_all %>%
  plot_ly(
    x = ~Score , 
    y = ~GDP_per_capita, 
    size = ~size, 
    sizes = c(min(df_all$size), max(df_all$size)),
    color = ~Continent , 
    frame = ~Year, 
    text = ~'Country or region', 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers'
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    ))%>%
  layout(title = 'GDP per capita vs Score from year 2015 to 2021 for all continents ',plot_bgcolor='#e5ecf6')


# Data Sampling

########################## DATA SAMPLING ###############################
sample2015to2021 <- data_2015_to_2021[data_2015_to_2021$"Overall rank" <= 80, ] #### Sampling only top 80 countries

####### Happiness score for each continent

plot_ly(sample2015to2021, x = ~Score, color = ~Continent, type = "box")%>%
  layout(title = 'Box plot for Score based on Continents on full Dataset',plot_bgcolor='#e5ecf6')

# Compute descriptive statistics by groups
library(ggpubr)
stable <- desc_statby(sample2015to2021, measure.var = "Score",
                      grps = "Continent")
stable <- stable[, c("Continent","mean","median")]
names(stable) <- c("Continent", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))

### Continent wise Descriptive Satistics for Sampled Dataset
stable.p

## ANOVA TEST

### One way Anova Test

abc <- summary(sample2015to2021)
abc

is.factor(sample2015to2021$Continent)
sample2015to2021$Continent = factor(sample2015to2021$Continent)
is.factor(sample2015to2021$Continent)
table(sample2015to2021$Continent)
m<- aov(sample2015to2021$Score ~ sample2015to2021$Continent, data=sample2015to2021)
summary(m)



### Multiple pairwise-comparison between the means of groups

### Tukey multiple pairwise-comparisons

TukeyHSD(m)

# Feature Selection and Engineering

# Building First Linear Regression Model Model with all Parameters.
myModel <- lm(Score ~ GDP_per_capita + Social_support +  Healthy_life_expectancy + Freedom_to_make_life_choices + Generosity + Perceptions_of_corruption, data = sample2015to2021)
summary(myModel) 
anova(myModel) 

## Correlation Plot for all the attributes of sample data 

# Correlation Matrix to find which variables are highly correlated.
correlationMatrix <- cor(sample2015to2021[,4:10])
corrplot(correlationMatrix)

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
highlyCorrelated  # Healthy_life_expectancy , GDP , Perceptions_of_corruption
paste("R between GDP and Healthy_life_expectancy:" , round(cor(sample2015to2021$GDP_per_capita , sample2015to2021$Healthy_life_expectancy),3))
dataf = data.frame(Score,GDP_per_capita , Social_support , Healthy_life_expectancy , Freedom_to_make_life_choices, Perceptions_of_corruption,Generosity )

## Pairwise Corrplot
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

## Pairwise correlation matrix
pairs(dataf ,upper.panel = panel.cor)

# Creating the best fit model

newModel2 = step(lm(Score~1,data = sample2015to2021),direction= "forward", scope =
                   (~GDP_per_capita + Social_support +  Healthy_life_expectancy + Freedom_to_make_life_choices + Generosity + Perceptions_of_corruption))

# Assessment and Presentation

summary(newModel2)
anova(newModel2)
par(mfrow=c(2,2))
plot(newModel2)

## Histogram of Residuals

#histogram of residuals
ggplot(data = sample2015to2021, aes(x = newModel2$residuals)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')

## Regularisation of the best fit model

### Ridge regression

############### Regulariser #################
lm_subset = sample2015to2021[,c('Score', 'GDP_per_capita','Social_support', 'Freedom_to_make_life_choices' , 'Generosity', 'Healthy_life_expectancy')]
# expects predictor matrix and response matrix
x = model.matrix(lm_subset$Score ~.-1, data=lm_subset)
y = lm_subset$Score

library(glmnet) 
fit.ridge = glmnet(x, y, alpha=0)
plot(fit.ridge, xvar="lambda", label=TRUE)
cv.ridge = cv.glmnet(x, y, alpha=0) # cv.glmnet is the built in cross validation function - default CV K=10.
coef(cv.ridge)
plot(cv.ridge)

### Lasso regression

fit.lasso = glmnet(x,y)
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso = cv.glmnet(x, y) # glmnet built in cross validation. Default is 10 fold.
coef(cv.lasso)
plot(cv.lasso)

###################### TRAIN TEST PREDICT ####################

### Fitting Multiple Linear Regression to the dataset
split = sample.split(lm_subset$Score, SplitRatio = 0.8)
training_set = subset(lm_subset, split == TRUE)
test_set = subset(lm_subset, split == FALSE)
regressor_lm = lm(formula = Score ~ .,
                  data = training_set)

summary(regressor_lm)


##Predicting the Test set results
y_pred_lm = predict(regressor_lm, newdata = test_set)
Pred_Actual_lm <- as.data.frame(cbind(Prediction = y_pred_lm, Actual = test_set$Score))
## Evaluating Accuracy
MSE.lm <- sum((test_set$Score - y_pred_lm)^2)/nrow(test_set)
gg.lm <- ggplot(Pred_Actual_lm, aes(Actual, Prediction )) +
  geom_point(aes(color = "Blue"), show.legend = FALSE) + theme_bw() + geom_abline() +
  labs(title = "Multiple Linear Regression (Actual vs Predicted) ", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel
print(paste("Mean Squared Error (Multiple Linear Regression):", MSE.lm))
print(paste("RMSE (Multiple Linear Regression):",sqrt(mean((Pred_Actual_lm$Actual - Pred_Actual_lm$Prediction)^2))))
gg.lm


## Prediction of Happiness Scores using Random Forest

### Fitting Random Forest Regression to the dataset
set.seed(1234)
regressor_rf = randomForest(x = lm_subset[-1],
                            y = lm_subset$Score,
                            ntree = 50)
# Predicting a new result with Random Forest Regression
y_pred_rf = predict(regressor_rf, newdata = test_set)

Pred_Actual_rf <- as.data.frame(cbind(Prediction = y_pred_rf, Actual = test_set$Score))

gg.rf <- ggplot(Pred_Actual_rf, aes(Actual, Prediction )) +
  geom_point(aes(color = "Blue"), show.legend = FALSE) + theme_bw() + geom_abline() +
  labs(title = "Random Forest (Actual vs Predicted) ", x = "Actual happiness score",
       y = "Predicted happiness score") +
  theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
        axis.title = element_text(family = "Helvetica", size = (10)),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
      plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent')) #transparent legend panel

MSE.nn <- sum((test_set$Score - y_pred_rf)^2)/nrow(test_set)
### Findings
print(paste("Mean Squared Error (Random Forest):", MSE.nn))
print(paste("RMSE (Random Forest):",sqrt(mean((Pred_Actual_rf$Actual - Pred_Actual_rf$Prediction)^2))))
gg.rf

