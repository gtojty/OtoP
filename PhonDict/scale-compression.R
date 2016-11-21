library(car)
library(ggplot2)

## example
# d <- data.frame()
# x <- seq(1,10001,100)
# ## lambda 0 == natural log; 1/2 == square root; 1 == no transform;
# lambda <- c(0,1/2,2/3,3/4,.8, .85, .9, .95,1)
# for(ll in lambda) {
#     d <- rbind(d, data.frame(y=bcPower(x, ll), x=x, lambda=ll))
# }
# ggplot(d, aes(x=x, y=y, color=as.factor(lambda))) + geom_point()

fdata <- read.csv('./extwords4_Harm1998.csv')
lambda <- seq(0.0,1.0,0.1)
d <- data.frame(fdata$wordform)
for(ll in lambda) {
  d <- cbind(d, data.frame(y=bcPower(fdata$sum_freq, ll)/sum(bcPower(fdata$sum_freq, ll)), x=fdata$wordform, lambda=ll))
}
newd <- d[,c(1,2,5,8,11,14,17,20,23,26,29,32)]
names(newd) <- c('wordform', paste('bcLambda', lambda, sep='_'))
fdata <- merge(fdata, newd, by=c("wordform"))
fdata <- fdata[,c(1:6,10:20,7:9)]

write.csv(fdata, './extwords5_Harm1998.csv', row.names=FALSE)
# hand change column names: bcLambda_0 -> bcLambda_0.0 and bcLambda_1 -> bcLambda_1.0
