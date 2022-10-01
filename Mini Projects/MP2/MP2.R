# Solution for Problem 1
# Part a
# Load data
roadrace = read.csv("/Users/sahuankit010/Desktop/Repo/CS-6313-Stats/Mini Projects/MP2/roadrace.csv")

# Read Data
print(summary(roadrace))
print(colnames(roadrace))
attach(roadrace)

# Bar Graph
barplot(table(Maine), main = "Bar graph Maine")
# summary of Maine
t <- table(Maine)
m <- prop.table(m)
print(t)
print(m)

# Part b
maine <- subset(roadrace, Maine == "Maine")$Time..minutes.
away <- subset(roadrace, Maine == "Away")$Time..minutes.

# Summary for both "maine" and "away"

summary(maine)
summary(away)
IQR(maine)
IQR(away)

# Histograms
hist(maine, xlim = c(min(away), max(maine)), ylim = c(0, 2000), xlab = "Time", main = "Histogram of Maine")
hist(away, xlim = c(min(away), max(maine)), ylim = c(0, 2000), xlab = "Time", main = "Histogram of Away")

# Part c Side by Side Plot
boxplot(Time..minutes.~Maine)

# Part d Male and Female Runnner Part

ml <- Age[Sex == "M"]
fl <- Age[Sex == "F"]
ml = strtoi(ml)
fl = strtoi(fl)

boxplot(ml, fl, names = c("M", "F"))

summary(ml)
summary(fl)
IQR(ml)
IQR(fl)

# Solution for Problem 2

# Read the data
mc <- read.csv("/Users/sahuankit010/Desktop/Repo/CS-6313-Stats/Mini Projects/MP2/motorcycle.csv")
attach(mc)
mc

#boxplot
boxplot(Fatal.Motorcycle.Accidents)

#outliers
box <-boxplot(Fatal.Motorcycle.Accidents)
box$out
tail(mc[order(Fatal.Motorcycle.Accidents), ], 2)

#summary statistics
summary(Fatal.Motorcycle.Accidents)
IQR(Fatal.Motorcycle.Accidents)







