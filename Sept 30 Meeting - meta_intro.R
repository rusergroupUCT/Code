# introduction to "meta" package
# datasets and examples from Schwarzer, Carpenter, Rucker
# https://www.imbi.uni-freiburg.de/lehre/lehrbuecher/meta-analysis-with-r


# install.packages("meta")
library(meta)
help(meta)

setwd("C:/Users/Leon.alive/Dropbox/R/Meta-analysis with R/example_data")

data1 <- read.csv("dataset01.csv", as.is=TRUE)

# look at dataset structure (NB!)
View(data1)

# choose function to match dataset and type of MA
# specify colums, data, and labels
mc1 <- metacont(Ne, Me, Se, Nc, Mc, Sc,
                data=data1,
                studlab=paste(author, year))

# look at your meta-analysis
print(mc1, digits=1)

# can select one study at a time with subset = 
print(metacont(Ne, Me, Se, Nc, Mc, Sc,
               data=data1,
               subset=9),
      digits=2)

# sm = summary method; can specify difference sm; 
# MD default for metacont
print(metacont(Ne, Me, Se, Nc, Mc, Sc, 
               sm="SMD",
               data=data1, 
               subset=9), 
      digits=2)

# get forest plot output
# specify MA object 
forest(mc1)

# change look of forest plot
# try the same forest plots with these different settings
settings.meta("jama")
settings.meta("revman5")

# remove random effect from plot; set xlab and xlim
# this is just example; all elements of the plot can be specified
forest(mc1, comb.random=F, 
       xlab="Difference in mean response (intervention - control)
units: maximum % fall in FEV1",
       xlim=c(-50,10))

# look at MA object 
View(mc1)
# if we don't have raw data (M,SD,n)
# we can use summary data (TE,seTE) to create MA
# metagen = generic inverse variance meta-analysis
# TE = treatment effect, seTE = standard error of treatment effect
mc1.gen <- metagen(TE, seTE, data=mc1, sm="MD")
mc1.gen
# I did not specify the study label argument
# try with studlab
mc1.gen <- metagen(TE, seTE, data=mc1, sm="MD", 
                   studlab = studlab)
mc1.gen
# note that we get the same estimates whether we use metagen or metacont 
c(mc1$TE.fixed, mc1$TE.random)
c(mc1.gen$TE.fixed, mc1.gen$TE.random)

# We can specify the method used for estimating heterogeneity:
# methods for estimating between study variance are:
# DerSimonian-Laird estimator (default)
mg1.DL <- metagen(TE, seTE, data=mc1)
# Paule-Mandel estimator (recommended)
mg1.PM <- metagen(TE, seTE, data=mc1, method.tau="PM")
# Restricted maximum-likelihood estimator (also reommended)
mg1.RM <- metagen(TE, seTE, data=mc1, method.tau="REML")
# Maximum-likelihood estimator
mg1.ML <- metagen(TE, seTE, data=mc1, method.tau="ML")
# Hunter-Schmidt estimator
mg1.HS <- metagen(TE, seTE, data=mc1, method.tau="HS")
# Sidik-Jonkman estimator
mg1.SJ <- metagen(TE, seTE, data=mc1, method.tau="SJ")
# Hedges estimator
mg1.HE <- metagen(TE, seTE, data=mc1, method.tau="HE")
# Empirical Bayes estimator
mg1.EB <- metagen(TE, seTE, data=mc1, method.tau="EB")
# Extract between-study variance tau-squared
tau2 <- data.frame(tau2=round(c(0,
                                mg1.DL$tau^2, mg1.PM$tau^2,
                                mg1.RM$tau^2, mg1.ML$tau^2,
                                mg1.HS$tau^2, mg1.SJ$tau^2,
                                mg1.HE$tau^2, mg1.EB$tau^2), 2),
                   row.names=c("FE", "DL", "PM", "REML", "ML",
                               "HS", "SJ", "HE", "EB"))
# Print tau-squared values
t(tau2)

# Now work through example for binary outcome
# load new dataset (also from Schwarzer, Carpenter, Rucker)
data7 <- read.csv("dataset07.csv")
View(data7)

# new function: metabin for binary data
# same recipe: specify columns, summary method, 
# new argument: method, for specifying pooling method 
# (was only inverse variance method for "MD")
# now we can specify "Inverse", "MH", "Peto", or "GLMM"
# also note demonstration of a different method for subsetting
metabin(Ee, Ne, Ec, Nc, sm="OR", data=data7, 
        method="I", 
        subset=study=="Milpied")

metabin(Ee, Ne, Ec, Nc, sm="OR", data=data7, 
        method="P",
        subset=study=="Milpied")

# see different summary methods for metabin:
metabin(Ee, Ne, Ec, Nc, sm="RR", data=data7,
        method="I",subset=study=="Milpied")

metabin(Ee, Ne, Ec, Nc, sm="RD", data=data7,
        method="I",subset=study=="Milpied")


mb1.mh <- metabin(Ee, Ne, Ec, Nc, sm="OR",
                  method="MH", method.tau="REML", 
                  data=data7, studlab=study)

forest(mb1.mh)
# bad example, but we can switch elements off in the forest plot
# remove random effects estimate and remove heterogeneity statistics
# also, we can specify the summary text, instead of default:
forest(mb1.mh, comb.random=F, hetstat=F,
       text.fixed="MH estimate")
# lets look at summary output
print(summary(mb1.mh), digits=2)

# we often want to look at subgroups 
# need column for subgroup in our dataset
data9 <- read.csv("dataset09.csv")
# in this dataset there is a column for study quality (blind)
View(data9)

mb9 <- metabin(Ee, Ne, Ec, Nc, sm="RR", method="I",
               data=data9, studlab=study)
print(summary(mb9), digits=2)

# we can update the existing meta object to make subgroups
mb9s <- update(mb9, byvar=blind, print.byvar=T)
print(summary(mb9s), digits=2)
forest(mb9s, comb.fixed=F, resid.hetstat=F)

# instead of subsetting we may want to measure effect for 3rd variable 
# meta-regression
data3 <- read.csv("dataset03.csv")
View(data3)

mb3 <- metacont(Ne,Me,Se,Nc,Mc,Sc,
                data=data3, sm="MD")
mb3
mb3r <- metareg(mb3, formula=duration, method.tau="REML") 
print(mb3r, digit=1)

plot(mb3r) # regression diagnostics
bubble(mb3r) 

# We can also adjust for a continuous variable 
data(dat.colditz1994, package="metafor")
data10 <- dat.colditz1994
View(data10)

mb10 <- metabin(event.e=tpos,
                n.e=tpos+tneg,
                event.c=cpos,
                n.c=cpos+cneg,
                data=data10, 
                studlab=paste(author, year))
mb10
mb10r <- metareg(mb10, ablat, method.tau="REML")
print(mb10r, digits=2)

# plots for regression
plot(mb10r)
bubble(mb10r)

# look at funnel plots
funnel(mb10)
# trimfill highlights "missing" information
tf10 <- trimfill(mb10)
funnel(tf10)
# now compare estimates from orignal metabin object and trimfill MA
print(tf10, digits=2, comb.fixed=T)
print(mb10, digits=2, comb.fixed=T)

funnel(mb1, studlab=T)
labbe(mb3s,studlab=F,sm="RR")

# test funnel plot asymmetry
metabias(mb10, method = "rank")
metabias(mb10, method = "linreg")
metabias(mb10, method = "mm")
metabias(mb10, method = "score")
metabias(mb10, method = "peters")
metabias(mb10, method = "count")

# main meta function we skipped are 
# metaprop()
# metamean()
# metarate()
# for MAs of single proportions, means or rates. 

# also visit https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/






