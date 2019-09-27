## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(babynames)
library(stringr)


## ----1-------------------------------------------------------------------
fruits <- c("Apple", "Akee", "Apricot", "Avocado", "Banana", "Bilberry", "Blackberry", "Blackcurrant",
            "Black sapote", "Blueberry", "Boysenberry", "Crab apples", "Currant",
            "Cherry", "Chico fruit", "Cloudberry", "Coconut", "Cranberry", "Cucumber",
            "Damson", "Date","Dragonfruit", "Durian", "Elderberry", "Feijoa", "Fig", "Kumquat",
            "Lemon", "Lime", "Loquat", "Longan", "Lychee", "Mango", "Mangosteen", "Marionberry", "Melon",
            "Passionfruit", "Peach", "Pear", "Persimmon", "Plantain", "Plum", "Satsuma", "Soursop", "Star apple",
            "Eggplant", "Olive", "Pea", "Pumpkin", "Squash", "Tomato", "Zucchini")

ct <- c("Cape Town is a port city on South Africa’s southwest coast, on a peninsula beneath the imposing Table Mountain. Slowly rotating cable cars climb to the mountain’s flat top, from which there are sweeping views of the city, the busy harbor and boats heading for Robben Island, the notorious prison that once held Nelson Mandela, which is now a living museum.")

jb <- c("Johannesburg, S0uth Africa's biggest c1ty and capital of Gauteng province, began as a 19th-century gold-mining settlement. Its sprawling S0weto t0wnship was once home to Nelson Mandela and Desm0nd Tutu. Mandela’s former residence is n0w the Mandela House museum. Other Soweto museums that recount the struggle to end segregation include the somber Apartheid Museum and Constitution Hill, a former prison complex")

babynames <- babynames::babynames


## Mutate Strings


### Change case

## ----2-------------------------------------------------------------------
#Base R

fruits_upper_base <- toupper(fruits)
fruits_upper_base

fruits_lower_base <- tolower(fruits)
fruits_lower_base

babynames$name_1 <- toupper(babynames$name)
head(babynames,15)
#String R

fruits_upper_stringr <- str_to_upper(fruits)
fruits_upper_stringr

fruits_lower_stringr <- str_to_lower(fruits)
fruits_lower_stringr

babynames$name_2 <- str_to_lower(babynames$name)



## ----3-------------------------------------------------------------------
#String R
ct_title <- str_to_title(ct)
ct_title

ct_sent <- str_to_sentence(ct)
ct_sent

### Replace strings

## ----4-------------------------------------------------------------------
#Base R
jb_1 <- sub(" ","-",jb) #only works for the first match
jb_1

jb_2 <- gsub(" ","-",jb) #works for all matches (global)
jb_2

jb_3 <- gsub("o","0",jb) #works for all matches (global)
jb_3

#String R
jb_4 <- str_replace(jb,"0","o") #analogous to sub
jb_4

jb_3 <- str_replace_all(jb_3,"0","o") #analogous to gsub
jb_3



## ----5-------------------------------------------------------------------
#String R
jb_rem <- str_remove(jb,"-")  #only works for the first match
jb_rem

jb_rem_2 <- str_remove_all(jb,"-")  #works for all matches
jb_rem_2


#### Anchors

## ----6-------------------------------------------------------------------
#String R
str_view(fruits,"^A")

fr_rep <- str_replace(fruits,"^A","a")  
fr_rep

str_view(fruits,"rry$")

fr_rep_2 <- str_replace(fruits,"rry$","ry") 
fr_rep_2

### Trimming White Space

## ----7-------------------------------------------------------------------
#String R
apple <- c(" Apple ")

str_trim(apple, side = c("left"))

str_trim(apple, side = c("right"))

str_trim(apple, side = c("both"))


### String Length

## ----8-------------------------------------------------------------------
#String R
apple <- c(" Apple ")

str_length(apple)

str_length(str_trim(apple, side = c("both")))

str_length(jb)

str_length(fruits)

babynames$length <- str_length(babynames$name)
head(babynames,15)

###String Padding

## ----9-------------------------------------------------------------------
#String R
or <- c("R is Great")
#Default pad is space and left side padding
str_pad(or,width = 25, side = c("left"))
str_pad(or,width = 25, side = c("right"))
str_pad(or,width = 25, side = c("both"))
#different padding
str_pad(or,width = 25, side = c("right"), pad = "!")

str_pad("a", 10, pad = c("-", "_", " "))

## Substrings

#### Extracting matched groups from a string. 

## ----10------------------------------------------------------------------
#String R
str_match(fruits,"rr")

str_extract(fruits,"rr")



## ----11------------------------------------------------------------------

#Base R

substr(fruits,1,5)

#Stringr
str_sub(fruits,1,5)



## ----12------------------------------------------------------------------
#Base R

substr(fruits,2,5)

#Stringr
str_sub(fruits,2,5)



## ----13------------------------------------------------------------------
#Ask for the substring starting at the second character, up to the fifth character, 

#Base R

substr(fruits,-5,-1) #Doesnt work

str_sub(fruits,-5,-1)

#Stringr
str_sub(fruits,-5,-2)


## Detect Patterns

## ----14------------------------------------------------------------------

str_detect(fruits, pattern = "berry") 

str_detect(ct, pattern = "the") 

baby_om <- babynames %>% filter(str_detect(name,"om"))
head(baby_om, 15)

baby_am <- babynames %>% filter(str_detect(name,"^am")) #No results to this...no small first letter
head(baby_am, 15)

#Solution 1
baby_am_2 <- babynames %>% filter(str_detect(name,"^Am")) #No results to this either
head(baby_am, 15)

#Solution 2

baby_am <- babynames %>% filter(str_detect(name_2,"^am")) #having all characters in a column in upper/lower case is best
head(baby_am, 15)

baby_ot <- babynames %>% filter(str_detect(name,"ot$"))
head(baby_ot, 15)


###Complicated
baby_ea <- babynames %>% filter(str_detect(str_sub(name,2,3),"ea"))
head(baby_ea, 15)

baby_ton <- babynames %>% filter(str_detect(str_sub(name,3,5),"ton"))
head(baby_ton, 15)

baby_ea_1 <- babynames %>% filter(str_detect(str_sub(name,2,3),"ea") & str_detect(name,"n$"))
head(baby_ea_1, 15)




## ----15------------------------------------------------------------------
str_subset(fruits, pattern = "berry") 

str_subset(fruits, pattern = "n") 

baby_ot_1 <- str_subset(babynames$name,"ot$") # result is often a vector
baby_ot_1



## ----a-------------------------------------------------------------------

str_subset(fruits, pattern = "[bm]") 

str_subset(fruits, pattern = "ab|m") 

str_subset(fruits, pattern = "[ab|m]") 



## ----16------------------------------------------------------------------
#Base R

str_count(fruits, pattern = "berry") 

str_count(fruits, pattern = "n") 

babynames$length_2 <- str_count(babynames$name,"nn")

head(babynames, 15)

## Combining/Splitting strings


### Combining strings

## ----17------------------------------------------------------------------
#Base R
paste0("x", "y", "z") 


paste("x", "y", "z") # default separator is " "

paste("x", "y", "z" , sep = "-")  

paste(jb, ct , sep = ". ") 

paste(fruits,collapse = ' and ')


#string r

str_c("Hi", ",", "these", "are", "some", "strings")            # Basic application

str_c("Hi", ",", "these", "are", "some", "strings", sep = " ")

paste("x", 1, "z" ,NA, sep = "-")  

str_c("x", 1, "z" ,NA, sep = "-")  

babynames$name_sex <- str_c(babynames$name,babynames$sex, sep = "-")

head(babynames,15)
### Splitting strings

## ----18------------------------------------------------------------------
#Base R

strsplit("This is a cat", " ") 

unlist(strsplit("This is a cat", " "))

strsplit("This is a cat", "") 

strsplit("This is a cat", "a") 
unlist(strsplit("This is a cat", "a"))


#string r

date_ranges <- c("23.01.2017 - 29.01.2017", "30.01.2017 - 06.02.2017")

# Split dates using " - "
split_dates <- str_split(date_ranges, pattern = fixed(" - "))

# Print split_dates
split_dates


# Split dates with n and simplify specified
split_dates_n <- str_split(date_ranges, pattern = fixed(" - "), simplify = TRUE, n = 2)

split_dates_n


# Subset split_dates_n into start_dates and end_dates
start_dates <- split_dates_n[, 1]
end_dates <- split_dates_n[, 2]

# Split start_dates into day, month and year pieces
str_split(start_dates, pattern = fixed("."), simplify = TRUE)


## ----19------------------------------------------------------------------
dt <- c("may?","money$","and&") 
var1 <- c("Data Science Begi555nners 123 tips and tricks 232")
var2 <- c("Data Science & Beginners% tips and?@ tricks")

#To remove the special characters
gsub("[\\?-\\$-\\&]","",dt)

str_replace_all(var2,"[:punct:]","")
#OR
str_remove_all(var2,"[:punct:]")

#To remove the numbers characters
gsub("[0-9]","",var1)

str_replace_all(var1,"\\d","")
#OR
str_remove_all(var1,"[:digit:]")


