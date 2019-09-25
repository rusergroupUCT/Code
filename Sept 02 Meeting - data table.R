library("dplyr")
library("data.table")

# Load the data
iris_frame = as.data.frame(iris)
iris = as.data.table(iris)

# Let's look at the dataset
# base r
iris_frame
# data.table
iris


# Some simple subsetting with base R and data.table
# Base R
x = iris_frame[iris_frame$Species == "setosa", ]
iris_frame

# data.table
iris["Species" == "setosa", with = FALSE]
iris

# Find the mean Sepal.width of each species flowers
# Base R
# From stackoverflow
# ? res = by(flights, list(flights$month, flights$day), function(x)
#   if (nrow(x) > 1000) {
#     c(
#       month = unique(x$month),
#       day = unique(x$day),
#       count = nrow(x),
#       avg_delay = mean(x$dep_delay, na.rm = TRUE))
#   })
# 
# # Store in data.frame and order by month, day
# df = do.call(rbind, res);
# df = df[order(df[, 1], df[, 2]) ,];
# ?

# data.table
mean = iris[, mean(Sepal.Width), by = Species,with =FALSE]

# dplyr 
iris = iris %>% 
  group_by(Species) %>%
  summarise(mean = mean(Sepal.Width))
iris[Species == "setosa"]
# Adding a new column to the dataset
# base r
iris_frame$new_col = iris_frame$Sepal.Length * 10
iris_frame

# dplyr
iris_frame = iris_frame %>% 
  mutate(new_col = Sepal.Length * 100)
iris_frame

# data.table
iris[, new_col := Sepal.Length * 10]
iris

# Extra -----------------------------------------------------------------------------

# Dplyr vs data.table benchmark
library("microbenchmark")

microbenchmark::microbenchmark(iris[, mean(Sepal.Width), by = Species])
microbenchmark::microbenchmark({iris_frame %>% 
    group_by(Species) %>%
    summarise(mean = mean(Sepal.Width))}
    )


