
setwd("/Users/sahuankit010/Desktop/Repo/CS-6313-Stats/Mini Projects/MP4")
getwd()

####################    Question 1      ########################################

data = read.csv("gpa.csv")

print(data)

# getting the gpa scores of the student in variable gpa
gpa = as.numeric(data$gpa)

# getting the act scores of the student in variable act 
act = as.numeric(data$act)


plot(gpa, act, main = "Scatter Plot GPA vs ACT", xlab = "GPA", ylab="ACT");

#Correlation
#abline() can be used to add vertical, horizontal or regression lines to a graph.
#lm() function -> is used to fit linear models -> regression 
abline(lm(act~gpa))

cor(gpa, act)

#Estimates::

library(boot)

cov.npar = function(ank, iters){
  gpa = ank$gpa[iters]
  act = ank$act[iters]
  result = cor(gpa, act)
  return (result)
}

cov.npar.boot = boot(data, cov.npar, R=999, sim="ordinary", stype="i")

print(cov.npar.boot)

plot(cov.npar.boot)

# Now doing the Point Estimation of bootstrap

#mean
print(mean(cov.npar.boot$t))

#Confidence Interval(with boot.ci)
print(boot.ci(cov.npar.boot))

#Percentile CI

print(sort(cov.npar.boot$t)[c(25,975)])

####################    Question 2      ########################################

voltages = read.csv("VOLTAGE.csv")
print(voltages)

#for remote location 0 and local location 1

remote = voltages$voltage[voltages$location==0]
local = voltages$voltage[voltages$location==1]

print(remote)
print(local)

boxplot(remote, local)

qqnorm(remote, main="QQ Norm for remote voltage location")
qqline(remote)

qqnorm(local, main="QQ Norm for local voltage location")
qqline(local)

local_variance = var(local)
remote_variance = var(remote)

print(local_variance)
print(remote_variance)

t.test(remote,local, alternative = "two.sided", conf.level = 0.95,var.equal = FALSE)

mean_remote = mean(remote)
mean_local = mean(local)

print(mean_local)
print(mean_remote)



ci = (mean_remote - mean_local) +c(-1,1)*qt(0.025,58)*sqrt((var(local) + var(remote))/30)
print(ci)

####################    Question 3      ########################################

vapor = read.csv("VAPOR.csv")
print(vapor)

theoretical = vapor$theoretical
experimental = vapor$experimental

diff = theoretical-experimental

print(diff)

hist(diff)

ci = mean(diff) + c(1,-1)*qt(0.975, 15)* (sd(diff)/sqrt(16))
print(ci)








