mainfolder <- "C:/Users/Admin/OneDrive/Documents/Gthub_Projects/R-Programming/data/"
setwd(mainfolder)
arrests <- read.table(file = "Arrests.csv", header = TRUE, sep = ",")
head(arrests)
summary(arrests)
str(arrests)

# I will start by loading a few packages that I will use for this analysis
library(tidyverse)
library(ggplot2)

# I will convert release, colour, sex, employed and citizen columns into factor objects
arrests$released <- as.factor(arrests$released)
arrests$colour <- as.factor(arrests$colour)
arrests$sex <- as.factor(arrests$sex)
arrests$citizen <- as.factor(arrests$citizen)
arrests$employed <- as.factor(arrests$employed)

# let's see arrests by sex
arrests %>%
  group_by(sex) %>%
  count()
# Between 1997 and 2002 there were 443 female arrests and 4783 male arrests

arrests %>%
  group_by(sex, year) %>%
  count() %>%
  arrange(desc(n))
# We can see that most male and female got arrested in 2000 with 1165 arrests 
# for male and 105 for female. 2002 had the least arrests for both categories

# Arrests grouped by sex visualization for female
female <- arrests %>%
  filter(sex == "Female") %>%
  group_by(released) %>%
  count()
f <- female$n
percentage <- round(100*f/sum(f), 0.5)
lb <- c("Female Not Released", "Female Released")
lb1 <- paste(lb, percentage, "%", sep = " ")
pie(percentage, labels = lb1, main = "Arrests for Female 1997-2002", col = rainbow(length(percentage)))
# Out of the total arrests for female year 1997-2002, 14.2% were not released.

# Arrests grouped by sex visualization for male
male <- arrests %>%
  filter(sex == "Male") %>%
  group_by(released) %>%
  count()
m <- male$n
percents <- round(100*m/sum(m), 0.5)
l <- c("Male Not Released", "Male Released")
l1 <- paste(l, percents, "%", sep = " ")
pie(percents, labels = l1, main = "Arrests for Male 1997-2002", col = rainbow(length(percents)))
# Out of the total arrests for male category year 1997-2002, 17.3% were not released.

#let's see arrests by year
yearly_arrests <- arrests%>%
  group_by(year, colour) %>%
  count()
ggplot(yearly_arrests, aes(x = year, y = n, fill = colour)) + 
  geom_bar(stat = "identity", position = "dodge")+ 
  labs(title = "Yearly Arrests", caption = "1997-2002", y = "Total Arrests")
# 2000 saw the most arrests for white, while the most arrests for black was in 2001

# Let's see total arrests grouped by colour
arrests %>%
  group_by(colour) %>%
  count()
# Total arrests from 1997 to 2002 for white were 3938 while black had 1288 arrests

# Let's see total arrests and releases grouped by colour
arrests %>%
  group_by(colour, released) %>%
  count()
# I will visualize this, I will start by visualizing black arrests
piex <- arrests %>%
  filter(colour == "Black") %>%
  group_by(released) %>%
  count()
x <- piex$n
perc <- round(100*x/sum(x), 0.5)
lbl <- c("Black Not Released", "Black Released")
lbl1 <- paste(lbl, perc, "%", sep = " ")
pie(perc, labels = lbl1, main = "Arrests for Black 1997-2002", col = rainbow(length(perc)))
# From the above piechart we can see that from the total arrests for black people
# 74.1% were released while 25.9% were not released

# White arrests
piey <- arrests %>%
  filter(colour == "White") %>%
  group_by(released) %>%
  count()
y <- piey$n
perct <- round(100*y/sum(y), 0.5)
lbl2 <- c("White Not Released", "White Released")
lbl3 <- paste(lbl2, perct, "%", sep = " ")
pie(perct, labels = lbl3, main = "Arrests for White 1997-2002", col = rainbow(length(perc)))
# From the above piechart we can see that out of the total arrest for white people
# 85.8% were released while 14.2% were not released.















