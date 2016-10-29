require(ggplot2)
require(plyr)
require(reshape2)
require(sjPlot)
require(Hmisc)
require(GGally)
require(stats)
require(xlsx)
#require(moments)
require(forecast)
require(lme4)
require(tm)
require(MASS)
require(e1071)
require(car)
require(tidyr)
require(dplyr)

# prepare all the data in different runs
resLoc <- "./"
HidSizeVec <- c(20,100,200,400)
LearnRateVec <- c("1e-2","1e-3","1e-4","1e-5")
numRun <- 20
outFile <- "output.txt"
separator = "/"

# collect results of all runs in each condition
Dall <- data.frame()
for(hidsize in HidSizeVec){
  for(learnrate in LearnRateVec){
    folder <- paste("H", hidsize, "L", learnrate, sep="")
    File <- paste(resLoc, separator, folder, "/1/", outFile, sep="")
    Dout <- read.table(File, header = TRUE, sep = "\t", quote = "\"'", dec = ".")
    Dout$runID <- 1
    for(i in 2:numRun){
      File <- paste(resLoc, separator, folder, separator, i, separator, outFile, sep="")
      Dtemp <- read.table(File, header = TRUE, sep = "\t", quote = "\"'", dec = ".")
      Dtemp$runID <- i
      Dout <- rbind(Dout, Dtemp)
    }
    Davg <- Dout %>% group_by(ITER) %>% 
      summarize(Mean_Err = mean(Err, na.rm = TRUE), Std_Err = sd(Err, na.rm = TRUE),
                Mean_AcuTr = mean(AcuTr, na.rm = TRUE), Std_AcuTr = sd(AcuTr, na.rm = TRUE),
                Mean_AcuTe = mean(AcuTe, na.rm = TRUE), Std_AcuTe = sd(AcuTe, na.rm = TRUE)) %>% 
      ungroup()
    Davg$HidSize <- hidsize; Davg$LearnRate <- as.numeric(learnrate)
    # store results
    File <- paste(resLoc, separator, folder, "_avg.csv", sep="")
    write.csv(Davg, File, row.names = FALSE)
    Dall <- rbind(Dall, Davg)
  }
}
File <- paste(resLoc, separator, "All.csv", sep="")
write.csv(Dall, File, row.names = FALSE)

# draw results: training accuracy and error 
Dall <- read.csv(File)
Dall$HidSize <- factor(Dall$HidSize); Dall$LearnRate <- factor(Dall$LearnRate)
# accuracy
FigFile1 <- paste(resLoc, separator, "All_AcuTr.png", sep="")
ggplot(Dall, aes(x=ITER, y=Mean_AcuTr, color=LearnRate)) + 
  geom_ribbon(aes(ymin=Mean_AcuTr-Std_AcuTr, ymax=Mean_AcuTr+Std_AcuTr), alpha=0.2) + geom_line(size=0.1) +  
  scale_x_log10() + 
  facet_wrap(~HidSize) + 
  xlab("No. Training") + ylab("Accuracy") + ggtitle("Accuracy") + theme_bw()
ggsave(FigFile1, dpi = 300)
# error
FigFile2 <- paste(resLoc, separator, "All_Err.png", sep="")
ggplot(Dall, aes(x=ITER, y=Mean_Err, color=LearnRate)) + 
  geom_ribbon(aes(ymin=Mean_Err-Std_Err, ymax=Mean_Err+Std_Err), alpha=0.2) + geom_line(size=0.1) +  
  scale_x_log10() + 
  facet_wrap(~HidSize) + 
  xlab("No. Training") + ylab("Error") + ggtitle("Error") + theme_bw()
ggsave(FigFile2, dpi = 300)


# for itemacu_tr
acuTRFile <- "itemacu_tr.txt"
Dall_acu_tr <- data.frame()
numItem <- 304; itemVec <- paste("Acu", 1:numItem, sep=""); sdVec <- paste("Std", 1:numItem, sep="")
# collect results of all runs in each condition
for(hidsize in HidSizeVec){
  for(learnrate in LearnRateVec){
    folder <- paste("H", hidsize, "L", learnrate, sep="")
    File <- paste(resLoc, separator, folder, "/1/", acuTRFile, sep="")
    Dout <- read.table(File, header = TRUE, sep = "\t", quote = "\"'", dec = ".")
    Dout$runID <- 1
    for(i in 2:numRun){
      File <- paste(resLoc, separator, folder, separator, i, separator, acuTRFile, sep="")
      Dtemp <- read.table(File, header = TRUE, sep = "\t", quote = "\"'", dec = ".")
      Dtemp$runID <- i
      Dout <- rbind(Dout, Dtemp)
    }
    Dout$HidSize <- hidsize; Dout$LearnRate <- as.numeric(learnrate)
    Dout_avg <- data.frame(ITER=unique(Dout$ITER))
    for(item in itemVec){
      Davg_item <- Dout[,names(Dout) %in% c("ITER", item)]
      names(Davg_item) <- c("ITER", "Acu")
      avgv <- Davg_item %>% group_by(ITER) %>% 
        summarize(avg = mean(Acu, na.rm = TRUE), std = sd(Acu, na.rm = TRUE)) %>% 
        ungroup()
      Dout_avg <- merge(Dout_avg, avgv, by=c("ITER"))
    }
    newname <- c("ITER")
    for(i in 1:numItem){
      newname <- c(newname, itemVec[i], sdVec[i])
    }
    names(Dout_avg) <- newname
    Dout_avg$HidSize <- hidsize; Dout_avg$LearnRate <- as.numeric(learnrate)
    # store results
    File <- paste(resLoc, separator, folder, "_acu_tr.csv", sep="")
    write.csv(Dout_avg, File, row.names = FALSE)
    Dall_acu_tr <- rbind(Dall_acu_tr, Dout_avg)
  }
}
File <- paste(resLoc, separator, "All_acu_tr.csv", sep="")
write.csv(Dall_acu_tr, File, row.names = FALSE)

# draw item accuracy
Dall_acu_tr <- read.csv(File)
Dall_acu_tr$HidSize <- factor(Dall_acu_tr$HidSize); Dall_acu_tr$LearnRate <- factor(Dall_acu_tr$LearnRate)
# accuracy
FigFile <- paste(resLoc, separator, "Item_AcuTr.png", sep="")
ggplot(Dall_acu_tr, aes(x=ITER, y=Acu1, color=LearnRate)) + 
  geom_ribbon(aes(ymin=Acu1-Std1, ymax=Acu1+Std1), alpha=0.2) + geom_line(size=0.1) +  
  scale_x_log10() + 
  facet_wrap(~HidSize) + 
  xlab("No. Training") + ylab("Accuracy") + ggtitle("Accuracy") + theme_bw()
ggsave(FigFile1, dpi = 300)
