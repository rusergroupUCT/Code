# June 28 2019
# Broom package presentation

# remember to set your working directory
setwd("~/") # everything of mine will be saved in the documents folder

library(broom)
# tidies up R regression output (in short)
# broom has 3 important functions (1) tidy(), (2) augment() & (3) glance()
# we'll go through them one at a time:

#Let's' us the iris dataset 
data("iris")
head(iris)

# Tidy ------------------------------------------------------------------------
# suppose we fit a simple glm
outglm1 <- glm(Sepal.Length ~ Sepal.Width + Species, data = iris)
# The summary comand below gives you the model output but it may be taxing to have to
#    copy this into word or even play around with the output ...
summary(outglm1)

# the tidy command will put your coefficients in a nice table
t1 <- tidy(outglm1)
t1
# so you can do nice things like rounding or format for an article
# in the following, we just add the estimate (standard error) and save that in a file
t1$nice_est <- paste0(round(t1$estimate, 2), " (", round(t1$std.error, 2), ")")
t1
write.csv(t1, "really_nice_manuscript_table.csv")


# tidy is also useful when you want to compare models:
# this this new model has Petal length added to the previous one...
outglm2 <- glm(Sepal.Length ~ Sepal.Width + Petal.Length + Species, data = iris)

t2 <- tidy(outglm2)
t1 <- tidy(outglm1)

t1$model <- "Simple Happy"
t2$model <- "My supervisor made me do it"

t1
t2
tt <- rbind(t1, t2) #combining the two model summaries in one table
tt
data.frame(tt)

# Glance -------------------------------------------------------------------------
# glance puts all your model summary statistics in a table...
# you can use this if you have multiple models you can us this to compare summary stats

glance(outglm1)
glance(outglm2)
rbind(glance(outglm1),glance(outglm1))


# Augment -----------------------------------------------------------------------
# augment can be used to add model predictions/ residuals/ e.t.c to your data 
# useful when you want to make prettier model diagnostic plots using ggplot

data.frame(iris) # the dataset we're using notice it only has 5 columns
pred1 <- augment(outglm1)
head(pred1) # augment has added the fitted values, residuals, cook's d, e.t.c

par(mfrow=c(2,2))
plot(outglm1) # this is what you would have got by default
par(mfrow =c(1,1))
# but now that you have all the info in a tibble, you can use ggplot to make them nicer
library(ggplot2)
ggplot(pred1, aes(Sepal.Length, .fitted)) + geom_point() #observed v fitted
qqnorm(pred1$.resid)
ggplot(pred1, aes(.resid, .fitted)) + geom_point()


# works with glms, clustering algorithms, e.t.c