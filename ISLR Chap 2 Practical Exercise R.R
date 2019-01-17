# Environment setting-up --------------
library(tidyverse)
library(magrittr)
library(rgl)

# Exercise 8 -----------------
college <- ISLR::College

fix(college)

# mosaic plot
college %$% plot(Private ~ Outstate)
# boxplot
college %$% plot(Private, Outstate)

# Elite variable
college <- mutate(college, Elite = if_else(Top10perc > 50, 'Yes', 'No') %>% as.factor())
summary(college)
college %$% plot(Elite, Outstate)

# hist 

# Exercise 9 ----------------
Auto.dat


# Exercise 10 --------------
Boston.dat <- MASS::Boston

# (b)
plot(Boston.dat)

# (c)
# crime rate, tax rates, pupil-teacher
select(Boston.dat, crim, tax, ptratio) %>% plot
select(Boston.dat, crim, tax, ptratio) %>% summary

# (d) & (e) 
Boston.dat %$% plot3d(x = crim, y = tax, z = ptratio, 
                      col = if_else(chas == 0, 'red', 'blue'), 
                      size = 6, alpha = 0.3) 
# The charles bound is labelled as the red points.

# (f) 
median(Boston.dat$ptratio)

# (g)






