rm(list=ls())

library(Hmisc)
library(ggplot2)
library(dplyr)
library(corrplot)
library(maps)
library(mapdata)

# Professional plot theme
theme_set(theme_minimal())

data <- read.csv("C:/Users/bacch/Documents/Projects/Covid/COVID19_line_list_data.csv")

describe(data)

# CREATE BINARY VARIABLES

data$death_dummy <- as.integer(data$death != 0)
data$recovered_dummy <- as.integer(data$recovered != 0)

# OVERALL DEATH RATE

death_rate <- sum(data$death_dummy) / nrow(data)
death_rate

# AGE ANALYSIS

dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.99)

# GENDER ANALYSIS

men = subset(data, gender == "male")
women = subset(data, gender == "female")

mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

t.test(men$death_dummy, women$death_dummy, alternative="two.sided", conf.level = 0.99)

# AGE DISTRIBUTION GRAPH

ggplot(data, aes(x=age, fill=factor(death_dummy))) +
  geom_histogram(binwidth=10, position="dodge") +
  labs(title="Age Distribution of COVID-19 Patients",
       x="Age",
       y="Number of Patients",
       fill="Death Status")

# DEATH RATE BY GENDER GRAPH

gender_death <- aggregate(death_dummy ~ gender, data=data, mean)

ggplot(gender_death, aes(x=gender, y=death_dummy, fill=gender)) +
  geom_bar(stat="identity") +
  labs(title="COVID-19 Death Rate by Gender",
       x="Gender",
       y="Death Rate")

# TOP COUNTRIES BY CASES

country_cases <- data %>%
  group_by(country) %>%
  summarise(cases=n()) %>%
  arrange(desc(cases)) %>%
  head(10)

ggplot(country_cases, aes(x=reorder(country,cases), y=cases)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  labs(title="Top 10 Countries with Highest COVID-19 Cases",
       x="Country",
       y="Total Cases")

# DEATH RATE BY COUNTRY

country_death <- data %>%
  group_by(country) %>%
  summarise(death_rate=mean(death_dummy, na.rm=TRUE)) %>%
  arrange(desc(death_rate)) %>%
  head(10)

ggplot(country_death, aes(x=reorder(country,death_rate), y=death_rate)) +
  geom_bar(stat="identity", fill="red") +
  coord_flip() +
  labs(title="Countries with Highest COVID-19 Death Rates",
       x="Country",
       y="Death Rate")

# AGE GROUP ANALYSIS

data$age_group <- cut(data$age,
                      breaks=c(0,20,40,60,80,100),
                      labels=c("0-20","21-40","41-60","61-80","80+"))

age_death <- aggregate(death_dummy ~ age_group, data=data, mean)

ggplot(age_death, aes(x=age_group, y=death_dummy, fill=age_group)) +
  geom_bar(stat="identity") +
  labs(title="COVID-19 Death Rate by Age Group",
       x="Age Group",
       y="Death Rate")

# RECOVERY VS DEATH COMPARISON

summary_data <- data.frame(
  Category=c("Deaths","Recovered"),
  Count=c(sum(data$death_dummy,na.rm=TRUE),
          sum(data$recovered_dummy,na.rm=TRUE))
)

ggplot(summary_data, aes(x=Category, y=Count, fill=Category)) +
  geom_bar(stat="identity") +
  labs(title="COVID-19 Outcomes: Deaths vs Recoveries",
       x="Outcome",
       y="Number of Patients")

# CORRELATION HEATMAP

num_data <- data[,c("age","death_dummy","recovered_dummy")]

cor_matrix <- cor(num_data, use="complete.obs")

corrplot(cor_matrix,
         method="color",
         type="upper",
         addCoef.col="black",
         tl.col="black",
         title="Correlation Between Age, Death, and Recovery",
         mar=c(0,0,2,0))

# LOGISTIC REGRESSION MODEL

model <- glm(death_dummy ~ age + gender,
             data=data,
             family=binomial)

summary(model)

# PROBABILITY OF DEATH VS AGE

ggplot(data, aes(x=age, y=death_dummy)) +
  geom_point(alpha=0.3) +
  geom_smooth(method="glm", method.args=list(family="binomial")) +
  labs(title="Probability of COVID-19 Death vs Age",
       x="Age",
       y="Probability of Death")

# AGE vs GENDER HEATMAP

age_gender <- data %>%
  group_by(age_group, gender) %>%
  summarise(death_rate = mean(death_dummy, na.rm=TRUE))

ggplot(age_gender, aes(x=gender, y=age_group, fill=death_rate)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="yellow", high="red") +
  labs(title="COVID-19 Death Rate Heatmap by Age and Gender",
       x="Gender",
       y="Age Group",
       fill="Death Rate")

# GLOBAL COVID MAP

country_cases2 <- data %>%
  group_by(country) %>%
  summarise(cases=n())

world_map <- map_data("world")

map_data_join <- left_join(world_map, country_cases2, by=c("region"="country"))

ggplot(map_data_join, aes(x=long, y=lat, group=group, fill=cases)) +
  geom_polygon(color="black", size=0.1) +
  scale_fill_continuous(low="lightyellow", high="red", na.value="grey90") +
  labs(title="Global Distribution of COVID-19 Cases",
       fill="Cases") +
  theme_void()

# CASES VS DEATHS SCATTER PLOT

country_summary <- data %>%
  group_by(country) %>%
  summarise(
    cases=n(),
    deaths=sum(death_dummy, na.rm=TRUE)
  )

ggplot(country_summary, aes(x=cases, y=deaths)) +
  geom_point(color="red", alpha=0.7, size=3) +
  geom_smooth(method="lm", color="blue") +
  labs(title="Relationship Between COVID-19 Cases and Deaths by Country",
       x="Total Cases",
       y="Total Deaths")

# DAILY CASE TREND

data$reporting_date <- as.Date(data$reporting.date, format="%m/%d/%Y")

daily_cases <- data %>%
  group_by(reporting_date) %>%
  summarise(cases=n()) %>%
  arrange(reporting_date)

ggplot(daily_cases, aes(x=reporting_date, y=cases)) +
  geom_line(color="darkblue", linewidth=1) +
  geom_point(color="red") +
  labs(title="Daily Trend of Reported COVID-19 Cases",
       x="Reporting Date",
       y="Number of Cases")