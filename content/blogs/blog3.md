---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
description:A statistical analysis of the factors that impact the price of Airbnb listings in Cape Town.
draft: false
image: pic08.jpg
keywords: ""
slug: tempus
title: Tempus
---
```{r setup, include=FALSE}
# leave this chunk alone
options(knitr.table.format = "html") 
knitr::opts_chunk$set(warning = FALSE, message = FALSE, 
  comment = NA, dpi = 300)
```


```{r load-libraries, echo=FALSE}

library(tidyverse) # the usual stuff: dplyr, readr, and other goodies
library(lubridate) # to handle dates
library(GGally) # for correlation-scatter plot matrix
library(ggfortify) # to produce residual diagnostic plots
library(rsample) # to split dataframe in training- & testing sets
library(janitor) # clean_names()
library(broom) # use broom:augment() to get tidy table with regression output, residuals, etc
library(huxtable) # to get summary table of all models produced
#library(kableExtra) # for formatting tables
library(moderndive) # for getting regression tables
library(skimr) # for skim
library(mosaic)
library(leaflet) # for interactive HTML maps
library(tidytext)
library(viridis)
library(vroom) 
library(caret)
```

# **Executive Summary:** 

We selected Cape Town to conduct our Airbnb analysis.

**Data Wrangling:**

After we downloaded the raw data set, we encountered several problems.

-   To isolate the variables that are most relevant to explaining the
    price for 2 people to stay for 4 nights in Cape Town, we read the
    description for each variable and removed those that did not make
    any sense.

-   There were many redundnat columns that were not needed. For example,
    we had to 2 duplicate columns of 'minimum_nights'.

-   There were several columns with only NA entries and therefore were
    absolutely insignificant.

-   There were many numeric variables that had NA entries and removing
    those rows would have impacted our analysis as we would be left with
    too little variables. Therefore, we replaced those NA entries with
    the average of the entire variable.

-   Few variables were not given in the form suitable for our
    statistical analysis and we therefore had to convert them into the
    right format.

-   Via Data Wrangling, we reduced the raw data set from 75 variables to
    35 variables.

    **Exploratory Data Analysis:**

    After cleaning our data, we wished to explore and analyse the data
    to assess which variables impact price the most.

-   After having a look at the distribution of every variable, we learnt
    that 'price' was skewed due to extremely high outliers. Therefore,
    to conduct our analysis, we standardised price by using logarithm
    and multiplied it with 4 as the analysis is concerned with finding
    price for 4 nights.

-   We removed outliers for certain variables to to discard extreme
    values and prevent our analysis from getting distorted.

-   There were 84 different property types and this made the analysis
    incomprehensible. Therefore, we ranked property_type and
    neighborhood_cleansed by frequency of booking and condensed all
    entries beyong top 5 as 'Others'.

-   Since we were supposed to find the best variables for 2 people and 4
    nights, we filtered the data set further by choosing properties that
    could accommodate at least 2 people. As per the nights, we only kept
    the properties that had a minimum stay comprised between 0 and 4
    nights or a maximum stay superior or equal to 4 nights.

-   We created several correlation graphs where we assessed the
    correlation of different variables with price, constructed boxplots
    and density plots to further analyse the correlation of variables,
    and we further reduced our selection to 25 variables.

-   Some of the variables that explained price the most are:

    **Best Model:**

    The best model contained 13 variables which explain
    'price_4\_nights' most accurately.

-   'host_since': means since when is the property owner is on Airbnb.

-   'host_response_rate': means how quickly the owner responds to
    prospective clients.

-   'host_acceptance_rate': means how selective the owner is to accept
    the client's application

-   host_listings_count: means how many properties has the host listed
    on Airbnb

-   longitude: means the co-ordinate for every property.

-   room_type: means the type of rooms at every property.

-   bedrooms: means the number of bedrooms in every property.

-   availability_30: whether or not the property was available 30 days
    prior to the date of the reservation.

-   number_of_reviews: how many reviews the property had on Airbnb.

-   review_scores_location: the ratings of the reviewers on the
    property's location

-   prop_type_simplified: new variable created that categorizes type of
    properties

-   neighborhood_type_simplified: variable categorises type of
    neigbourhoods

    One of the variables that we found was most interesting in its
    explaining of the price of Airbnb properties was the
    'number_of_reviews'. Indeed, it was negatively correlated to the
    price of properties which could be explained by one of the two
    following interpretations: 1) more affordable accommodations receive
    more visitors and are thus prone to receiving more reviews; 2)
    customers are usually more eager to post reviews to share their
    negative experiences.

    We observed a strong relationship of longitude and price which gave
    us an insight into which areas of Cape Town were more expensive than
    the others. What we found most interesting was how the area with
    coordinate -18.4 had an unusual concentration of expensive
    properties. By researching what does longitude correspond to, we
    found out that the most expensive areas of Cape Town (that is to say
    Clifton and Camps Bay) were in that area.

In your final group assignment you have to analyse data about Airbnb listings and fit a model to predict the total cost for two people staying 4 nights in an AirBnB in a city. You can download AirBnB data from [insideairbnb.com](http://insideairbnb.com/get-the-data.html){target="_blank"}; it was originally scraped from airbnb.com. 

The following [Google sheet](https://docs.google.com/spreadsheets/d/1QrR-0PUGVWvDiVQL4LOk7w-xXwiDnM3dDtW6k15Hc7s/edit?usp=sharing) shows which cities you can use; please choose one of them and add your group name next to it, e.g., A7, B13. No city can have more than 2 groups per stream working on it; if this happens, I will allocate study groups to cities with the help of R's sampling.


All of the listings are a GZ file, namely they are archive files compressed by the standard GNU zip (gzip) compression algorithm. You can download, save and extract the file if you wanted, but `vroom::vroom()` or `readr::read_csv()` can immediately read and extract this kind of a file. You should prefer `vroom()` as it is faster, but if vroom() is limited by a firewall, please use `read_csv()` instead.


`vroom` will download the *.gz zipped file, unzip, and provide you with the dataframe. 


```{r load_data, echo=FALSE, message=FALSE, warning=FALSE, cache=TRUE}

# use cache=TRUE so you dont donwload the data everytime you knit

listings <- vroom("http://data.insideairbnb.com/south-africa/wc/cape-town/2021-09-29/data/listings.csv.gz") %>% 
       clean_names()

```


Even though there are many variables in the dataframe, here is a quick description of some of the variables collected, and you can find a [data dictionary here](https://docs.google.com/spreadsheets/d/1iWCNJcSutYqpULSQHlNyGInUvHg2BoUGoNRIGa6Szc4/edit#gid=982310896)

- `price` = cost per night 
- `property_type`: type of accommodation (House, Apartment, etc.)
- `room_type`:

  - Entire home/apt (guests have entire place to themselves)
  - Private room (Guests have private room to sleep, all other rooms shared)
  - Shared room (Guests sleep in room shared with others)

- `number_of_reviews`: Total number of reviews for the listing
- `review_scores_rating`: Average review score (0 - 100)
- `longitude` , `latitude`: geographical coordinates to help us locate the listing
- `neighbourhood*`: three variables on a few major neighbourhoods in each city 


# Exploratory Data Analysis (EDA)

In the [R4DS Exploratory Data Analysis chapter](http://r4ds.had.co.nz/exploratory-data-analysis.html){target="_blank"}, the authors state:

> "Your goal during EDA is to develop an understanding of your data. The easiest way to do this is to use questions as tools to guide your investigation... EDA is fundamentally a creative process. And like most creative processes, the key to asking quality questions is to generate a large quantity of questions."


Conduct a thorough EDA. Recall that an EDA involves three things:

* Looking at the raw values.
    * `dplyr::glimpse()`
* Computing summary statistics of the variables of interest, or finding NAs
    * `mosaic::favstats()`
    * `skimr::skim()`
* Creating informative visualizations.
    * `ggplot2::ggplot()`
        * `geom_histogram()` or `geom_density()` for numeric continuous variables
        * `geom_bar()` or `geom_col()` for categorical variables
    * `GGally::ggpairs()` for scaterrlot/correlation matrix
        * Note that you can add transparency to points/density plots in the `aes` call, for example: `aes(colour = gender, alpha = 0.4)`
        
        
You may wish to have a level 1 header (`#`) for your EDA, then use level 2 sub-headers (`##`) to make sure you cover all three EDA bases. **At a minimum** you should address these questions:
aa
- How many variables/columns? How many rows/observations?
- Which variables aare numbers?
- Which are categorical or *factor* variables (numeric or character variables with variables that have a fixed and known set of possible values?
- What are the correlations between variables? Does each scatterplot support a linear relationship between variables? Do any of the correlations appear to be conditional on the value of a categorical variable?

At this stage, you may also find you want to use `filter`, `mutate`, `arrange`, `select`, or `count`. Let your questions lead you! 

> In all cases, please think about the message your plot is conveying. Don’t just say "This is my X-axis, this is my Y-axis", but rather what’s the **so what** of the plot. Tell some sort of story and speculate about the differences in the patterns in no more than a paragraph.

```{r}
dplyr::glimpse(listings)
skimr::skim(listings)
#NOTES#
# two people staying 4 nights in an AirBnB in a city
#TA - Peter Vodicka comments:
#We are only going to work with numerical variables, the most important one is price. Price is extremely right skewed
#Y = 4*logarithm of price, mutate this variable and investigate it, you need to get rid of outliers (negative, zero or extreme prices)
#Understand the Data
#We need to predict 4 nights so check the variable minimum_nights and check if they are above 4 and check maximum_nights (if < 4 not useful)
#Similar to assignment one, rank the property types (select the top 5 frequencies by number of observations and call the rest "others")
#Do a similar thing with neighbourhood_cleansed

#2 people 4 nights for different times of the year, different property/room types, neighbourhoods, weekday/weekends, ...
#Is the Data before the pandemic/during pandemic? We need to consider that
#Don't be afraid to exclude 30% of the Data (treatment of outliers is important)

#Best model will probably have 40-55% adj. R^2

#Introduce Chaptering for HTML (EDA, Visualization, Variables, Training/Testing, Executive Summary without Technical Terms!)

#Check frequency of variables and order them by count using arrange
```

```{r}
airbnb_1<-listings%>%
  select(host_since,
host_response_rate,
host_is_superhost,
host_acceptance_rate,
host_listings_count,
host_identity_verified,
neighbourhood_cleansed,
latitude,
longitude,
property_type,
room_type,
accommodates,
bathrooms_text,
bedrooms,
beds,
price,
minimum_nights,
maximum_nights,
has_availability,
availability_30,
availability_60,
availability_90,
number_of_reviews,
number_of_reviews_ltm,
number_of_reviews_l30d,
review_scores_rating,                         
review_scores_accuracy,                       
review_scores_cleanliness,                  
review_scores_checkin,                        
review_scores_communication,                  
review_scores_location, 
review_scores_value,
instant_bookable,
calculated_host_listings_count,
reviews_per_month)

```


## Data wrangling

Once you load the data, it's always a good idea to use `glimpse` to see what kind of variables you have and what data type (`chr`, `num`, `logical`, `date`, etc) they are. 

Notice that some of the price data (`price`) is given as a character string, e.g., "$176.00"

Since `price` is a quantitative variable, we need to make sure it is stored as numeric data `num` in the dataframe. To do so, we will first use `readr::parse_number()` which drops any non-numeric characters before or after the first number

```{r}
#transfer character to numeric variable and create a processed log price variable
mosaic::favstats(airbnb_1$price)
airbnb_2 <- airbnb_1 %>% 
  mutate(price = parse_number(price))%>%
  mutate(host_response_rate= parse_number(host_response_rate)/100)%>%
  mutate(host_acceptance_rate= parse_number(host_acceptance_rate)/100)%>%
  mutate(price = 4*log(price))
mosaic::favstats(airbnb_2$price)
glimpse(airbnb_2)
```
  
Use `typeof(listing$price)` to confirm that `price` is now stored as a number.



## Propery types


Next, we look at the variable `property_type`. We can use the `count` function to determine how many categories there are their frequency. What are the top 4 most common property types? What proportion of the total listings do they make up? 

Since the vast majority of the observations in the data are one of the top four or five property types, we would like to create a simplified version of `property_type` variable that has 5 categories: the top four categories and `Other`. Fill in the code below to create `prop_type_simplified`.


```{r}
#simplify the propertype variable by catogerizing into several major groups
airbnb_3 <- airbnb_2 %>%
  mutate(prop_type_simplified = case_when(
    property_type %in% c("Entire rental unit","Entire residential home", "Private room in residential home","Entire guest suite") ~ property_type, 
    TRUE ~ "Other"))

airbnb_3 <- airbnb_3 %>%
  mutate(neighbourhood_type_simplified = case_when(
    neighbourhood_cleansed %in% c("Ward 115","Ward 54", "Ward 77","Ward 23") ~ neighbourhood_cleansed, 
    TRUE ~ "Other"))
```


Use the code below to check that `prop_type_simplified` was correctly made.

```{r}
#show the major property groups ranking by descending order of number
airbnb_3 %>%
  count(property_type, prop_type_simplified) %>%
  arrange(desc(n)) 

airbnb_3 %>%
  count(neighbourhood_cleansed) %>%
  arrange(desc(n)) 
```        

Airbnb is most commonly used for travel purposes, i.e., as an alternative to traditional hotels. We only want to include  listings in our regression analysis that are intended for travel purposes:

- What are the  most common values for the variable `minimum_nights`? 
- Is ther any value among the common values that stands out? 
- What is the likely intended purpose for Airbnb listings with this seemingly unusual value for `minimum_nights`?

Filter the airbnb data so that it only includes observations with `minimum_nights <= 4`
```{r}
#filter the observation that can accommodates more than 2 people and can be booked for 4 days
airbnb_4 <- airbnb_3 %>%
  filter(accommodates >= 2, 
         minimum_nights<= 4, 
         maximum_nights>=4)
```

```{r}
#replace or remove the NA 
airbnb_4$review_scores_rating[is.na(airbnb_4$review_scores_rating)] <- round(mean(airbnb_4$review_scores_rating, na.rm = TRUE))

airbnb_4$host_response_rate[is.na(airbnb_4$host_response_rate)] <- round(mean(airbnb_4$host_response_rate, na.rm = TRUE))

airbnb_4$host_acceptance_rate[is.na(airbnb_4$host_acceptance_rate)] <- round(mean(airbnb_4$host_acceptance_rate, na.rm = TRUE))

airbnb_4$ review_scores_value[is.na(airbnb_4$ review_scores_value)] <- round(mean(airbnb_4$ review_scores_value, na.rm = TRUE))

airbnb_4$ review_scores_location [is.na(airbnb_4$ review_scores_location )] <- round(mean(airbnb_4$ review_scores_location , na.rm = TRUE))

airbnb_4$ review_scores_communication[is.na(airbnb_4$ review_scores_communication)] <- round(mean(airbnb_4$ review_scores_communication, na.rm = TRUE))

airbnb_4$ review_scores_checkin[is.na(airbnb_4$ review_scores_checkin)] <- round(mean(airbnb_4$ review_scores_checkin, na.rm = TRUE))

airbnb_4$ review_scores_cleanliness [is.na(airbnb_4$ review_scores_cleanliness )] <- round(mean(airbnb_4$ review_scores_cleanliness , na.rm = TRUE))

airbnb_4$ review_scores_accuracy [is.na(airbnb_4$ review_scores_accuracy )] <- round(mean(airbnb_4$ review_scores_accuracy , na.rm = TRUE))

airbnb_4$reviews_per_month [is.na(airbnb_4$reviews_per_month )] <- round(mean(airbnb_4$reviews_per_month, na.rm = TRUE))

na.omit(airbnb_4)
```


## Removing the outliers
```{r}

airbnb_5 <- airbnb_4

# # #remove the outliers for beds, host_listings_count,accommodates

cols_to_clean <- c("beds","host_listings_count","accommodates")

for (col in cols_to_clean){
  x <- airbnb_5[[col]]
  Q <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  iqr <- IQR(x,na.rm = TRUE)
  up <-  Q[2]+3*iqr # Upper Range  
  low<- Q[1]-3*iqr # Lower Range
  airbnb_5<- subset(airbnb_5, (x >= low) & (x <= up))
}

glimpse(airbnb_5)
```
        
# Mapping 

Visualisations of feature distributions and their relations are key to understanding a data set, and they can open up new lines of exploration. While we do not have time to go into all the wonderful geospatial visualisations one can do with R, you can use the following code to start with a map of your city, and overlay all AirBnB coordinates to get an overview of the spatial distribution of AirBnB rentals. For this visualisation we use the `leaflet` package, which includes a variety of tools for interactive maps, so you can easily zoom in-out, click on a point to get the actual AirBnB listing for that specific point, etc.

The following code, having downloaded a dataframe `listings` with all AirbnB listings in Milan, will plot on the map all AirBnBs where `minimum_nights` is less than equal to four (4). You could learn more about `leaflet`, by following [the relevant Datacamp course on mapping with leaflet](https://www.datacamp.com/courses/interactive-maps-with-leaflet-in-r)


```{r, out.width = '80%'}

leaflet(data = filter(listings, minimum_nights <= 4)) %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircleMarkers(lng = ~longitude, 
                   lat = ~latitude, 
                   radius = 1, 
                   fillColor = "blue", 
                   fillOpacity = 0.4, 
                   popup = ~listing_url,
                   label = ~property_type)
```

```{r}
airbnb_5 %>% summary()
```

```{r}
#Show the distribution of dependent variable y, that the shape of the curve is very similar to a normal distribution
favstats(~price,data=airbnb_5)
```

```{r}
#Number of columns, which are the variables, that we are interested in: 
ncol(airbnb_5)
```


```{r}
# Number of observations:
nrow(airbnb_5)
```


```{r}
#List out all numeric variables
select_if(airbnb_5, is.numeric)
```

```{r}
#List out all categorical variables
select_if(airbnb_5, is.character)
```
```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,number_of_reviews, reviews_scores_rating,
#reviews_per_month
airbnb_5 %>% 
  select(price,number_of_reviews,number_of_reviews_l30d,reviews_per_month,number_of_reviews_ltm) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()

```

```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,review_scores_rating,review_scores_accuracy,reviews_scores_cleanliness
airbnb_5 %>% 
  select(price,review_scores_rating,review_scores_accuracy,review_scores_cleanliness) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()

```
```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,review_scores_checkin,review_scores_communication,review_scores_location,review_scores_value
airbnb_5 %>% 
  select(price,review_scores_checkin,review_scores_communication,review_scores_location,review_scores_value) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()
```

```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,availability_30,availability_60,availability_90
airbnb_5 %>% 
  select(price,availability_30,availability_60,availability_90) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()
# The correlations among these three variables are very high and therefore needs to choose one of the variable when doing the modeling.
```
```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,host_listings_count,latitude,longitude,accomodates
airbnb_5 %>% 
  select(price,host_listings_count,latitude,longitude,accommodates) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()

```
```{r}
#Draw a scatter plot matrix to show the correlation matrix among price,bedrooms,beds,minimum_nights,maximum_nights
airbnb_5 %>% 
  select(price,bedrooms,beds,minimum_nights,maximum_nights) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()

# The correlation between Bed and Bedrooms is relatively high and from the common sense, these two variables are related. We may only choose one of the two when building the model.
```

```{r}
airbnb_5 %>% 
  select(price,review_scores_rating, 
         review_scores_accuracy, review_scores_cleanliness,review_scores_checkin, review_scores_communication, review_scores_location, review_scores_value) %>% 
  ggpairs(aes(alpha=0.2))+
  theme_bw()
```

```{r}
#The distribution of price
ggplot(airbnb_5, aes(x = price)) +
  geom_histogram(binwidth = 2.5)+
  theme_bw()

ggplot(airbnb_5, aes(x = price)) +
  geom_density()+
  theme_bw()
```

```{r}
ggplot(airbnb_5,aes(x=as.factor(beds),y=bedrooms))+
  geom_boxplot()+
  theme_bw()+
  labs(title="scatter plot of beds and bedrooms",
        x = 'number of beds')
```


```{r}
#ggplot of accommodates and price
ggplot(airbnb_5, aes(x =accommodates , y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()


ggplot(airbnb_5, aes(x =as.factor(accommodates) , y = price)) +
  geom_boxplot() + 
  geom_smooth(method = "lm") +
  labs(x = "accommodates")+
  theme_bw()
```

```{r}
#ggplot of bedrooms and price
ggplot(airbnb_5, aes(x =bedrooms , y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(airbnb_5, aes(x =as.factor(bedrooms) , y = price)) +
  geom_boxplot() + 
  geom_smooth(method = "lm") +
  labs(x = "bedrooms")+
  theme_bw()
```
```{r}
#ggplot of number_of_reviews and price
ggplot(airbnb_5, aes(x =number_of_reviews , y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()
```
```{r}
#ggplot of number_of_reviews_ltm and price
ggplot(airbnb_5, aes(x =number_of_reviews_ltm , y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()
```
```{r}
#ggplot of review_scores_location and price
ggplot(airbnb_5, aes(x =review_scores_location , y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()
```
```{r}
#ggplot of longitude and price
ggplot(airbnb_5, aes(x =longitude, y = price)) +
  geom_point(alpha=0.3) +
  geom_smooth(method = "lm") +
  theme_bw()
```


```{r}
#show the relationship between instant_bookable and price
ggplot(data = airbnb_5, mapping = aes(x = instant_bookable, y = price)) +
  geom_boxplot()
```
```{r}
#To show the relationship between price and property type
ggplot(data = airbnb_5, mapping = aes(x = prop_type_simplified, y = price)) +
  geom_boxplot()
```
```{r}
#we need to drop N/A
#To show the relationship between price and host response time
#ggplot(data = airbnb_5, mapping = aes(x = host_response_time, y = price)) + 
#  geom_boxplot()
```



## Data cleaning after EDA
```{r}
airbnb_6 <- airbnb_5[,-c(7, 8, 10, 17, 18, 21, 22, 24 ,25, 35, 28, 29, 30, 32)]
glimpse(airbnb_6)
# From the EDA, we understand the variables better, including how they are related to the price (the dependent variable) and the relationship among themselves.
```

    
# Regression Analysis

For the target variable $Y$, we will use the cost for two people to stay at an Airbnb location for four (4) nights. 

Create a new variable called `price_4_nights` that uses `price`, and `accomodates` to calculate the total cost for two people to stay at the Airbnb property for 4 nights. This is the variable $Y$ we want to explain.
```{r}
# compute the dependent variable $Y$, which is price_4_nights
airbnb_7 <- airbnb_6 %>% 
  mutate(price_4_nights = price*4,
         ln_price_4_nights = log(price_4_nights))

```


Use histograms or density plots to examine the distributions of `price_4_nights` and `log(price_4_nights)`. Which variable should you use for the regression model? Why?
```{r}
ggplot(airbnb_7, aes(x=price_4_nights))+
  geom_density() +  
  theme_bw() +            
  labs (
    title = "Density Plot for price_4_nights",
    y     = "Density"        
  )
# The density plot of the price-for-4-nights without doing the log is right skewed
```

```{r}
ggplot(airbnb_7, aes(x=ln_price_4_nights))+
  geom_density() +  
  theme_bw() +            
  labs (
    title = "Density Plot for ln_price_4_nights",
    y     = "Density"        
  )
# We can find out that after doing the log of price-4-nights, the density plot has a shape which is more similar to a normal distribution. This will help us to do the linear regression modeling
```

## Spliting the data into traiing and testing sets
```{r}
# Splitting data to make 75% of the data in training set and 25% data in the testing sets
library(rsample)
set.seed(1234)

train_test_split <- initial_split(airbnb_7, prop = 0.75)
airbnb_train <- training(train_test_split)
airbnb_test <- testing(train_test_split)

```



Fit a regression model called `model1` with the following explanatory variables: `prop_type_simplified`, `number_of_reviews`, and `review_scores_rating`. 

- Interpret the coefficient `review_scores_rating` in terms of `price_4_nights`.
- Interpret the coefficient of `prop_type_simplified` in terms of `price_4_nights`.

We want to determine if `room_type` is a significant predictor of the cost for 4 nights, given everything else in the model. Fit a regression model called model2 that includes all of the explananatory variables in `model1` plus `room_type`. 


## Building Models

### Model 1
```{r}
# A good model need to have a relatively low RMSD and a relatively high R-square

model1 <- lm(ln_price_4_nights ~ 
               prop_type_simplified+
               number_of_reviews +
               review_scores_rating, 
             data=airbnb_train)
msummary(model1)

car::vif(model1)
```

### Model 2
```{r}
# A good model need to have a relatively low RMSD and a relatively high R-square

model2 <- lm(ln_price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               review_scores_rating + 
               room_type, 
             data=airbnb_train)
msummary(model2)

car::vif(model2)
```

### Model 3
```{r}
model3 <- lm(ln_price_4_nights ~ 
               prop_type_simplified + 
               number_of_reviews + 
               host_identity_verified+
               review_scores_rating + 
               longitude+
               instant_bookable +
               host_listings_count+
               room_type, 
             data=airbnb_train)
msummary(model3)

car::vif(model3)


predictions <- model3 %>% predict(airbnb_test)
data.frame(
  RMSE = RMSE(predictions,  airbnb_test$ln_price_4_nights),
  R2 = R2(predictions,  airbnb_test$ln_price_4_nights)
)
 
# instant_bookable is not an significant variable
```


### Model 4
```{r}
model4 <- lm(ln_price_4_nights ~ . -price -price_4_nights, 
             data=airbnb_train)
msummary(model4)

```

### Model 5
```{r}
model5 <- lm(ln_price_4_nights ~ .-instant_bookable -host_is_superhost -host_identity_verified -review_scores_accuracy -bathrooms_text -has_availability -price -price_4_nights, 
             data=airbnb_train)
msummary(model5)
car::vif(model5)
```



### Model X  - the best model
```{r}
modelx <- lm(formula = ln_price_4_nights ~ 
               host_since + 
               host_response_rate + 
               host_acceptance_rate + 
               host_listings_count + 
               longitude + 
               room_type +   
               bedrooms + 
               availability_30 + 
               number_of_reviews +  
               review_scores_location +   
               prop_type_simplified + 
               neighbourhood_type_simplified, 
             data = airbnb_train)

msummary((modelx))
car::vif(modelx)


predictions <- modelx %>% predict(airbnb_test)

data.frame(
  RMSE = RMSE(predictions,  airbnb_test$ln_price_4_nights, na.rm=TRUE),
  R2 = R2(predictions,  airbnb_test$ln_price_4_nights, na.rm=TRUE)
)

#bathrooms_text instant_bookable + calculated_host_listings_count + review_scores_rating +
```
## RMSE test
```{r}
RMSE1 = 100*sd((predict(model1, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)
RMSE2 = 100*sd((predict(model2, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)
RMSE3 = 100*sd((predict(model3, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)
# RMSE4 = 100*sd((predict(model4, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)
# RMSE5 = 100*sd((predict(model5, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)
RMSEx = 100*sd((predict(modelx, airbnb_test)-airbnb_test$ln_price_4_nights),na.rm=TRUE)


RMSE1
RMSE2
RMSE3
#RMSE4
#RMSE5
RMSEx
```


## Further variables/questions to explore on our own

Our dataset has many more variables, so here are some ideas on how you can extend your analysis

1. Are the number of `bathrooms`, `bedrooms`, `beds`, or size of the house (`accomodates`) significant predictors of `price_4_nights`? Or might these be co-linear variables?
1. Do superhosts `(host_is_superhost`) command a pricing premium, after controlling for other variables?
1. Some hosts allow you to immediately book their listing (`instant_bookable == TRUE`), while a non-trivial proportion don't. After controlling for other variables, is `instant_bookable` a significant predictor of `price_4_nights`?
1. For all cities, there are 3 variables that relate to neighbourhoods: `neighbourhood`, `neighbourhood_cleansed`, and `neighbourhood_group_cleansed`. There are typically more than 20 neighbourhoods in each city, and it wouldn't make sense to include them all in your model. Use your city knowledge, or ask someone with city knowledge, and see whether you can group neighbourhoods together so the majority of listings falls in fewer (5-6 max) geographical areas. You would thus need to create a new categorical variabale `neighbourhood_simplified` and determine whether location is a predictor of `price_4_nights`
1. What is the effect of `avalability_30` or `reviews_per_month` on `price_4_nights`, after we control for other variables?


### Using the backward selection method to test the model
```{r}
# build the model using the backward selection method
full.model <- lm(ln_price_4_nights~., data = airbnb_train)
step(full.model, direction = "backward", trace= FALSE )

msummary(full.model)

# By looking at the t values here
# Significant: host_since, host_response_rate, host_acceptance_rate host_listings_count, room_type, bathrooms_text, bed, availability_30, review_scores_location, prop_type_simplified, neighbourhood_type_simplified
# Not very significant:
# host_is_superhost,  host_identity_verified, review_scores_accuracy 
```



## Diagnostics, collinearity, summary tables

As you keep building your models, it makes sense to:

1. Check the residuals, using `autoplot(model_x)`
1. As you start building models with more explanatory variables, make sure you use `car::vif(model_x)`` to calculate the **Variance Inflation Factor (VIF)** for your predictors and determine whether you have colinear variables. A general guideline is that a VIF larger than 5 or 10 is large, and your model may suffer from collinearity. Remove the variable in question and run your model again without it.



1. Create a summary table, using `huxtable` (https://mfa2022.netlify.app/example/modelling_side_by_side_tables/) that shows which models you worked on, which predictors are significant, the adjusted $R^2$, and the Residual Standard Error.
1. Finally, you must use the best model you came up with for prediction. Suppose you are planning to visit the city you have been assigned to over reading week, and you want to stay in an Airbnb. Find Airbnb's in your destination city that are apartments with a private room, have at least 10 reviews, and an average rating of at least 90. Use your best model to predict the total cost to stay at this Airbnb for 4 nights. Include the appropriate 95% interval with your prediction. Report the point prediction and interval in terms of `price_4_nights`. 
  - if you used a log(price_4_nights) model, make sure you anti-log to convert the value in $. You can read more about [hot to interpret a regression model when some variables are log transformed here](https://stats.idre.ucla.edu/other/mult-pkg/faq/general/faqhow-do-i-interpret-a-regression-model-when-some-variables-are-log-transformed/)

```{r}
huxreg(list("Model 1" = model1, "Model 2" = model2, "Model 3" = model3, "Model 4" = model4, "Model 5" = model5, "Model X" = modelx), statistics = c('#observations' = 'nobs',
                                'R squared' = 'r.squared', 
                                'Adj. R Squared' = 'adj.r.squared', 
                                'Residual SE' = 'sigma'), bold_signif = 0.05, 
                 stars = NULL ) %>% 
  set_caption('Comparison of models')
```


```{r}
prediction_data<- apply(airbnb_7[,1:25], 1, mean,na.rm=T)
predict(modelx, newdata = prediction_data, interval = "confidence")
```


# Deliverables


- By midnight on Monday 18 Oct 2021, you must upload on Canvas a short presentation (max 4-5 slides) with your findings, as some groups will be asked to present in class. You should present your Exploratory Data Analysis, as well as your best model. In addition, you must upload on Canvas your final report, written  using R Markdown to introduce, frame, and describe your story and findings. You should include the following in the memo:

1. Executive Summary: Based on your best model, indicate the factors that influence `price_4_nights`.
This should be written for an intelligent but non-technical audience. All
other sections can include technical writing.
2. Data Exploration and Feature Selection: Present key elements of the data, including tables and
graphs that help the reader understand the important variables in the dataset. Describe how the
data was cleaned and prepared, including feature selection, transformations, interactions, and
other approaches you considered.
3. Model Selection and Validation: Describe the model fitting and validation process used. State
the model you selected and why they are preferable to other choices.
4. Findings and Recommendations: Interpret the results of the selected model and discuss
additional steps that might improve the analysis
  
  

Remember to follow R Markdown etiquette rules and style; don't have the Rmd output extraneous messages or warnings, include summary tables in nice tables (use `kableExtra`), and remove any placeholder texts from past Rmd templates; in other words, (i.e. I don't want to see stuff I wrote in your final report.)
  
  
# Rubric

Your work will be assessed on a rubric which you can find here


```{r rubric, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "rubric.png"), error = FALSE)
```


# Acknowledgements

- The data for this project is from [insideairbnb.com](insideairbnb.com)