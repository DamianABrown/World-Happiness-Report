remove(list = ls())

#Loading tidyverse for tidying data as well as using for ggplot
library(tidyverse)


#Importing data from csv file on local device
data.2015 <- read.csv('C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Happiness Data/2015.csv')
data.2016 <- read.csv('C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Happiness Data/2016.csv')
data.2017 <- read.csv('C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Happiness Data/2017.csv')
data.2018 <- read.csv('C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Happiness Data/2018.csv')
data.2019 <- read.csv('C:/Users/damia/Desktop/Random_Projects/Kaggle Datasets/Happiness Data/2019.csv')


#Creating new dataset with only varialbes desired without altering original data
new.data.2015 <- data.2015 %>% subset(select = c(Country, Region, Happiness.Rank, Happiness.Score, 
                                                 Economy..GDP.per.Capita., Health..Life.Expectancy., 
                                                 Trust..Government.Corruption., Generosity, Freedom))
new.data.2016 <- data.2016 %>% subset(select = c(Country, Region, Happiness.Rank, Happiness.Score, 
                                                 Economy..GDP.per.Capita., Health..Life.Expectancy., 
                                                 Trust..Government.Corruption., Generosity, Freedom))
new.data.2017 <- data.2017 %>% subset(select = c(Country, Happiness.Rank, Happiness.Score, Economy..GDP.per.Capita., 
                                                 Health..Life.Expectancy., Trust..Government.Corruption., 
                                                 Generosity, Freedom))
new.data.2018 <- data.2018 %>% subset(select = c(Country.or.region, Overall.rank, Score, GDP.per.capita, 
                                                 Healthy.life.expectancy, Perceptions.of.corruption, 
                                                 Generosity, Freedom.to.make.life.choices))
new.data.2019 <- data.2019 %>% subset(select = c(Country.or.region, Overall.rank, Score, GDP.per.capita,
                                                 Healthy.life.expectancy, Perceptions.of.corruption, 
                                                 Generosity, Freedom.to.make.life.choices))


#Adding new column to each dataset for year observations were obtained
new.data.2015 <- new.data.2015 %>% add_column(Year = 2015)
new.data.2016 <- new.data.2016 %>% add_column(Year = 2016)
new.data.2017 <- new.data.2017 %>% add_column(Year = 2017)
new.data.2018 <- new.data.2018 %>% add_column(Year = 2018)
new.data.2019 <- new.data.2019 %>% add_column(Year = 2019)


#Organizing columns in order desired
new.data.2015 <- new.data.2015 %>% subset(select = c(1, 2, 10, 3, 4, 6, 5, 8, 9, 7))
new.data.2016 <- new.data.2016 %>% subset(select = c(1, 2, 10, 3, 4, 6, 5, 8, 9, 7))
new.data.2017 <- new.data.2017 %>% subset(select = c(1, 9, 2, 3, 5, 4, 7, 8, 6))
new.data.2018 <- new.data.2018 %>% subset(select = c(1, 9, 2, 3, 5, 4, 7, 8, 6))
new.data.2019 <- new.data.2019 %>% subset(select = c(1, 9, 2, 3, 5, 4, 7, 8, 6))


#Creating uniform variable names for each column across datasets
colnames(new.data.2015) <- c('Country', 'Region', 'Year', 'Happiness Ranking', 'Happiness Score',
                             'Life Expectancy', 'GDP per Capita', 'Generosity',
                             'Freedom', 'Trust in Government')
colnames(new.data.2016) <- c('Country', 'Region', 'Year', 'Happiness Ranking', 'Happiness Score',
                             'Life Expectancy', 'GDP per Capita', 'Generosity',
                             'Freedom', 'Trust in Government')
colnames(new.data.2017) <- c('Country', 'Year', 'Happiness Ranking', 'Happiness Score',
                             'Life Expectancy', 'GDP per Capita', 'Generosity',
                             'Freedom', 'Trust in Government')
colnames(new.data.2018) <- c('Country', 'Year', 'Happiness Ranking', 'Happiness Score',
                             'Life Expectancy', 'GDP per Capita', 'Generosity',
                             'Freedom', 'Trust in Government')
colnames(new.data.2019) <- c('Country', 'Year', 'Happiness Ranking', 'Happiness Score',
                             'Life Expectancy', 'GDP per Capita', 'Generosity',
                             'Freedom', 'Trust in Government')


#Creating temperary list to use for assigning countries to region
temp.region.list <- new.data.2015[,c(1,2)]


#Inputting region into dataset that are missing value
new.data.2017 <- left_join(temp.region.list, new.data.2017)
new.data.2018 <- left_join(temp.region.list, new.data.2018)
new.data.2019 <- left_join(temp.region.list, new.data.2019)
remove(temp.region.list)


#Checking data values for each dataset before combining all sets together
summary(new.data.2015)
summary(new.data.2016)
summary(new.data.2017)
summary(new.data.2018) #Trust in government not numeric value, must change before combining
summary(new.data.2019)


#Converting new.data.2018 "Trust in Government" variable into a numeric value, instead of factor
trust.in.gov <- new.data.2018[,10]
trust.in.gov <- as.character(trust.in.gov)
trust.in.gov <- as.numeric(trust.in.gov)
new.data.2018[,10] <- trust.in.gov
summary(new.data.2018)
remove(trust.in.gov)


#Building final dataset with all vaiables and observations from each year
happiness.dataset <- bind_rows(new.data.2015, new.data.2016, new.data.2017, new.data.2018, new.data.2019)


#Clean data to remove NA's
summary(happiness.dataset)
happiness.dataset <- happiness.dataset %>% subset(!is.na(Year))
summary(happiness.dataset)
happiness.dataset <- happiness.dataset %>% subset(!is.na(`Trust in Government`))
summary(happiness.dataset)

###
#Data Visualization
###

#Plotting Happiness Score based on region for all years
ggplot(happiness.dataset) + geom_boxplot(aes(x = (happiness.dataset$Region),
                                             y = (happiness.dataset$`Happiness Score`))) + 
  xlab('Region') + ylab('Happiness Score') + coord_flip()


#Plotting Happiness Score based on region for each individual year
ggplot(happiness.dataset) + geom_boxplot(aes(x = (happiness.dataset$Region),
                                             y = (happiness.dataset$`Happiness Score`))) + 
  xlab('Region') + ylab('Happiness Score') + coord_flip() + facet_wrap(~Year)


#Plotting Change in average happiness score over the years by region
temp.data <- happiness.dataset %>% group_by(Region, Year) %>% summarise(Ave = mean(`Happiness Score`))

ggplot(temp.data, aes(x = Year, y = Ave)) + geom_point(aes(color = Region)) + geom_line(aes(color = Region)) + 
  xlab("Year") + ylab("Average Happiness Score By Region")

remove(temp.data)


###
#Data Exploration
###

#Correlation between happiness and life expectancy
cor(happiness.dataset$`Happiness Score`, happiness.dataset$`Life Expectancy`)


#Linear Regression happiness score and life expectancy
slr <- lm(`Happiness Score` ~ `Life Expectancy`, happiness.dataset)
summary(slr)
remove(slr)

#Correlation between Freedom and life Trust in Government
cor(happiness.dataset$Freedom, happiness.dataset$`Trust in Government`)


#Linear Regression Freedom and Trust in Government
slr <- lm(`Freedom` ~ `Trust in Government`, happiness.dataset)
summary(slr)
remove(slr)
