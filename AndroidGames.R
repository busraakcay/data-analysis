# Data Analysis of Google Play Store Data Sets' and Predicting its Ratings.
##########################################################################

# Importing useful packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, psych, ggthemes, igraph, CatEncoders, caTools, rpart, rattle,
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, randomForest, datetime, rpart.plot,
               stringr, tidyr, tidyverse, readr, reshape2, gridExtra, lubridate, ca, cluster, RColorBrewer)

# Reading the CSV file
data <-read.csv("android-games.csv", header=TRUE, sep=",")

# Looking at the first 6 of the data set
head(data)

# Looking at the dimension of the data set.
dim(data)

# Looking at the structure of the data set.
str(data)

# Looking at the structure of the data set deeply.
glimpse(data)

# Are there null variables?
sum(is.na(data))

# Are there too many?
mean(is.na(data))

# Cleaning up null values.
# Assign it to the clean(cl) -cldata-
cldata <- na.omit(data)
#mlData <- cldata

tail(cldata)

# Checking out for the null variables.
sum(is.na(cldata))

summary(cldata)

str(cldata)

dim(cldata)

describe(cldata)

### Creating factor variables for easier analysis

cldata$Category <- as.factor(cldata$Category)
cldata$Installs <- as.factor(cldata$Installs)
cldata$Genres <- as.factor(cldata$Genres)
cldata$Android.Ver <- as.factor(cldata$Android.Ver)

# Checking out for the factor variables.
glimpse(cldata)

###########################################################################

cldata = subset(cldata)
#plotting a bar graph for categories
ggplot(aes(x = Category), data = cldata)+
  geom_bar(fill = 'red')+
  coord_flip()+
  ggtitle("Categories")

############################################################################

#Cleaning the installs column
cldata = subset(cldata, df$Installs)

#plotting a bar graph for level of installs.
ggplot(aes(x = Installs), data = cldata )+
  geom_bar(fill = 'blue')+
  coord_flip()+
  ggtitle('Installs')

############################################################################

#A histogram for the distribution of ratings in the google play dataset
med = median(subset(cldata$Rating, cldata$Rating >= 0.01))

#Histogram
ggplot(aes(x = Rating), data = cldata )+
  geom_histogram(binwimlh = 0.1, fill = 'yellow')+
  xlim(1,5)+ 
  geom_vline(xintercept = med, col = 'blue')+
  ggtitle('Rating')

summary(cldata$Rating)

############################################################################

#The larger the size of the application the better rated it is.

### Rating vs. Category

#plot the average rating for each category
dfcat <- subset(cldata, !is.na(Rating))
dfcat <- dfcat%>%
  group_by(Category)%>%
  summarise(Rating = mean(as.numeric(Rating)))

#plot

ggplot(aes(x = Category, y = Rating), data = dfcat)+
  geom_bar(stat="identity", fill = 'violetred2')+
  coord_flip(ylim = c(3.8,4.5))

############################################################################

#the relationship between price and category when accounting for the type
ggplot(aes(y = Price, x = Category), data = cldata)+
  geom_point(alpha = 0.2, color = 'tomato')+
  coord_flip()+
  ggtitle('Price vs. Category')

############################################################################

### ---- Content Rating !!!####

cldata %>% group_by(Content.Rating) %>% summarize(count = n()) %>%
  mutate(Content.Rating = reorder(Content.Rating, count)) %>%
  ggplot(aes(Content.Rating, count, fill = Content.Rating)) +
  geom_col()  + coord_flip() + theme_classic()+ theme(legend.position = "none")

############################################################################

### Number of apps by category ####

str(cldata$Category)
ggplot(cldata, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 2.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Number of apps by category", y = "") +
  guides(fill = FALSE)

############################################################################

### Exploring the ratings given to the apps, categorically

cldata %>%  group_by(Category) %>%  
  filter(!is.na(Rating), Category!='1.9') %>% 
  summarise(meanRating = mean(Rating)) %>% 
  ggplot(mapping = aes(x = Category, y = meanRating)) + 
  geom_col(aes(fill = Category)) + 
  geom_line(group = 1) +  coord_flip() + 
  ggtitle("Average rating across categories") + ylab("Average rating") + guides(fill=FALSE)

############################################################################

# Check what category has the highest rating,review,installs #####

ggplot(cldata, aes(x=Rating, y=Category)) +
  geom_segment(aes(yend=Category), xend=0, colour="grey") +
  geom_point(size=1, aes(colour=Type)) +
  scale_colour_brewer(palette="Set1", limits=c("Free", "Paid"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(Type ~ ., scales="free_y", space="free_y")

############################################################################

###Distribution of Ratings Across Categories
cldata %>% filter(Category!='1.9') %>% 
  ggplot(mapping = aes(x = Rating, fill = Category)) + 
  geom_histogram(bins = 50, position = "identity") + xlim(0,5) + 
  facet_wrap(~Category) + guides(fill = FALSE) + 
  ggtitle("Distribution of ratings across categories") + ylab("Count")

###############################################################################################


#######################################################################################################
#Prediction of Rating

"""
ml <- mlData
ml$Last.Updated <- as.Date(parse_date_time(ml$Last.Updated, orders="mdy"))

ml$App <- transform(LabelEncoder.fit(ml$App),ml$App)
ml$Reviews <- transform(LabelEncoder.fit(ml$Reviews),ml$Reviews)
ml$Size <- transform(LabelEncoder.fit(ml$Size),ml$Size)
ml$Price = as.numeric(gsub("\\$", "", ml$Price))
ml$Genres <- transform(LabelEncoder.fit(ml$Genres),ml$Genres)
ml$Current.Ver <- transform(LabelEncoder.fit(ml$Current.Ver),ml$Current.Ver)

sample <- sample.split(ml$Rating, SplitRatio=0.75)
train <- subset(ml, sample==TRUE)
test <- subset(ml, sample==FALSE)
dim(train)
dim(test)

model <- randomForest(formula = Rating~., data = train, mtry = 3, importance = TRUE, na.action = na.omit)

plot(model)

print(model)

func <- function(actual, predicted){
  return (100*mean(abs(actual-predicted)))
}

mltrain <- select(train, -c(Rating))
pred_train <- predict(model, mltrain)

mlt <- select(test, -c(Rating))
pred_test <- predict(model, mlt)

func(test$Rating, pred_test)

importance(model)

varImpPlot(model)
"""
