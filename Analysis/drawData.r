library(stringr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(hexbin)
library(plyr)
library(Rmisc)

options(max.print=10000)
drawrange <- c(1e4,1e7); errrange_ptop <- c(10, 1e7); errrange_otop <- c(10, 1e7)
scinot <- function(x){
  if(is.numeric(x)){ format(x, scientific=TRUE)
  }else{ error("x must be numeric") }
}
figDir <- './figure/'

pd <- position_dodge(0.1)
drawpoints <- c(1e6, 3e6, 5e6, 7e6, 9e6, 1e7)

# calculate and store aov results;
aovSub <- function(hlsize, lrnrate, drawpoint, data, txtName, aovtype){
  # get subdata
  subdata <- subset(data, hlsize == hlsize & lrnrate==lrnrate & iter==drawpoint)
  # anova test
  if(aovtype=="freq*reg"){ aovfit <- aov(err ~ freq*reg, data=subdata)
  }else{ 
    if(aovtype=="freq*const"){ aovfit <- aov(err ~ freq*const, data=subdata)
    }
  }
  s <- summary(aovfit); capture.output(s, file=paste(figDir, txtName, sep=""))
}

# calculate and draw figures
drawSub <- function(hlsize, lrnrate, drawpoint, data, picName, limits, aovtype){
  # get subdata
  subdata <- subset(data, hlsize == hlsize & lrnrate==lrnrate & iter==drawpoint)
  # anova test
  if(aovtype=="freq*reg"){ subdata_SE <- summarySE(subdata, measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
  }else{ 
    if(aovtype=="freq*const"){ subdata_SE <- summarySE(subdata, measurevar="err", groupvars=c("freq", "const", "hlsize", "lrnrate"), na.rm=TRUE)
    }
  }
  # draw figure
  ggplot(subdata_SE, aes(x=freq, y=err, linetype=reg, group=reg)) + 
    geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
    geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=limits) + 
    xlab("Frequency") + ylab("SSE") + ggtitle("Freq x Reg: Strain etal 1995 \n Hid Layer & Learn Rate") +
    facet_grid(lrnrate~hlsize)
  ggsave(paste(figDir, picName, sep=""), dpi = 300, height = 6, width = 12, units = 'in')
}


##################################################
## read data from csv files
DFtr <- read.csv('./trainingexp.csv'); DFtr_merge <- DFtr[,c('word', 'prob')]; names(DFtr_merge) <- c("O", "log_freq")
DFte <- read.csv('./testingexp.csv'); DFte_merge <- DFte[,c('word', 'prob')]; names(DFte_merge) <- c("O", "log_freq")
tr <- read.csv('./tr_allres_OtoP.csv'); te <- read.csv('./te_allres_OtoP.csv')

# Strain et al. 1995A:
Str1995A <- read.csv("Strain-etal-1995-Appendix-A.csv")
trStr1995A <- subset(tr, tr$O %in% Str1995A$O); trStr1995A <- merge(trStr1995A, Str1995A, by = c("O"), all.x = TRUE, all.y = FALSE)
frStr1995A <- subset(trStr1995A, freq == "H" | freq == "L")
cat(length(unique(frStr1995A$O)), '\n')
# 56

# draw accuracy
ggplot(frStr1995A, aes(x=iter, y=accuracy, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Acc") + ggtitle("Acc x Trials: Strain etal 1995 \n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'Str1995A_acu.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')
# draw sum squared error
ggplot(frStr1995A, aes(x=iter, y=err, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Err") + ggtitle("Err x Trials: Strain etal 1995 \n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'Str1995A_sse.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# anova test (for results under a single setting!)
aovfit <- aov(err ~ freq*reg + hlsize*lrnrate, data=frStr1995A)
summary(aovfit)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# freq            1   1499    1499  32.056 1.53e-08 ***
#   reg             1  12571   12571 268.804  < 2e-16 ***
#   freq:reg        1    261     261   5.577   0.0182 *  
#   Residuals   13884 649293      47                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
frStr1995A_avg <- summarySE(frStr1995A, measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(frStr1995A_avg, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 12.0)) + 
  xlab("Frequency") + ylab("SSE") + ggtitle("Freq x Reg: Strain etal 1995 \n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'Str1995A_sseavg.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# drawpoints
hlsize <- 100; lrnrate <- 5e-2; limits <- c(0.0, 2.0)
for(drawpoint in drawpoints){
  txtName <- paste('Str1995A_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.txt', sep="")
  aovSub(hlsize, lrnrate, drawpoint, frStr1995A, txtName, "freq*reg")
  picName <- paste('Str1995A_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.png', sep="")
  drawSub(hlsize, lrnrate, drawpoint, frStr1995A, picName, limits, "freq*reg")
}


# Taraban & McClelland 1987A1:
TM1987A1 <- read.csv("Taraban-McClelland-1987-Appendix-A1.csv", na.strings='na')
trTM1987A1 <- subset(tr, tr$O %in% TM1987A1$O); trTM1987A1 <- merge(trTM1987A1, TM1987A1, by = c("O"), all.x = TRUE, all.y = FALSE)
frTM1987A1 <- subset(trTM1987A1, freq == "H" | freq == "L")
cat(length(unique(frTM1987A1$O)), '\n')
# 94

# draw accuracy
ggplot(frTM1987A1, aes(x=iter, y=accuracy, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Acc") + ggtitle("Acc x Trials: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A1_acu.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')
# draw sum squared error
ggplot(frTM1987A1, aes(x=iter, y=err, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Err") + ggtitle("Err x Trials: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A1_sse.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# anova test (for results under a single setting!)
fit <- aov(err ~ freq*reg + hlsize*lrnrate, data=frTM1987A1)
summary(fit)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# freq            1    1738    1738  40.510 1.99e-10 ***
#   reg             1    8744    8744 203.810  < 2e-16 ***
#   freq:reg        1     339     339   7.895  0.00496 ** 
#   Residuals   23804 1021270      43                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
frTM1987A1_avg <- summarySE(frTM1987A1, measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(frTM1987A1_avg, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 10.0)) + 
  xlab("Frequency") + ylab("SSE") + ggtitle("Freq x Reg: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A1_sseavg.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# drawpoints
hlsize <- 100; lrnrate <- 5e-2; limits <- c(0.0, 2.0)
for(drawpoint in drawpoints){
  txtName <- paste('TM1987A1_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.txt', sep="")
  aovSub(hlsize, lrnrate, drawpoint, frTM1987A1, txtName, "freq*reg")
  picName <- paste('TM1987A1_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.png', sep="")
  drawSub(hlsize, lrnrate, drawpoint, frTM1987A1, picName, limits, "freq*reg")
}


# Taraban-McClelland-1987-Appendix-A2:
TM1987A2 <- read.csv("Taraban-McClelland-1987-Appendix-A2.csv", na.strings='na')
trTM1987A2 <- subset(tr, tr$O %in% TM1987A2$O); trTM1987A2 <- merge(tr1987A2, TM1987A2, by = c("O"), all.x = TRUE, all.y = FALSE)
fcTM1987A2 <- subset(trTM1987A2, freq == "H" | freq == "L")
cat(length(unique(fcTM1987A2$O)), '\n')
# 95

# draw accuracy
ggplot(fcTM1987A2, aes(x=iter, y=accuracy, color=interaction(freq, const))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Acc") + ggtitle("Acc x Trials: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, const))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A2_acu.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')
# draw summed square error
ggplot(fcTM1987A2, aes(x=iter, y=err, color=interaction(freq, const))) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Err") + ggtitle("Err x Trials: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, const))) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A2_sse.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# anova test (for results under a single setting!)
fit <- aov(err ~ freq*const + hlsize*lrnrate, data=fcTM1987A2)
summary(fit)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# freq            1       3     2.6   0.055 0.813906    
# const           1     565   565.2  12.243 0.000468 ***
#   freq:const      1     398   397.7   8.613 0.003341 ** 
#   Residuals   23556 1087579    46.2                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
fcTM1987A2_avg <- summarySE(fcTM1987A2, measurevar="err", groupvars=c("freq", "const", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(fcTM1987A2_avg, aes(x=freq, y=err, linetype=const, group=const)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 9.0)) + 
  xlab("Frequency") + ylab("SSE") + ggtitle("Freq x Const: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'TM1987A2_sseavg.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')

# drawpoints
hlsize <- 100; lrnrate <- 5e-2
for(drawpoint in drawpoints){
  txtName <- paste('TM1987A2_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.txt', sep="")
  aovSub(hlsize, lrnrate, drawpoint, TM1987A2, txtName, "freq*const")
  picName <- paste('TM1987A2_sseavg_H', hlsize, 'L', lrnrate, '_', drawpoint, '.png', sep="")
  drawSub(hlsize, lrnrate, drawpoint, fcTM1987A2, picName, limits, "freq*const")
}


# nonwords: Treiman et al. 1990 case:
Tr1990A <- read.csv("Treiman-etal-1990-Appendix.csv")
wordTr1990A <- merge(DFte_merge, Tr1990A, by = c("O"), all.x=TRUE, all.y=FALSE); wordTr1990A <- subset(wordTr1990A, freq == "H" | freq == "L")
xtabs(~freq, data=wordTr1990A)
# freq
# H  L 
# 24 24
teTr1990A <- subset(te, te$O %in% trTr1990A$O)
teTr1990A <- merge(teTr1990A, trTr1990A, by = c("O"), all.x = TRUE, all.y = FALSE)
fTr1990A <- subset(teTr1990A, freq == "H" | freq == "L")
cat(length(unique(fTr1990A$O)), '\n')
# 48

# draw accuracy
ggplot(fTr1990A, aes(x=iter, y=accuracy, color=freq)) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Acc") + ggtitle("Acc x Trials: Treiman etal 1990\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=freq)) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'Tr1990A_acu.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')
# draw sum squared error
ggplot(fTr1990A, aes(x=iter, y=err, color=freq)) + scale_x_log10(labels=scinot) + coord_cartesian(xlim=drawrange) + 
  xlab("Training Trials (log10)") + ylab("Avg Err") + ggtitle("Err x Trials: Treiman etal 1990\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=freq)) + facet_grid(lrnrate~hlsize)
ggsave(paste(figDir, 'Tr1990A_sse.png', sep=""), dpi = 300, height = 6, width = 12, units = 'in')



##################################################
## see exact words in different testing sets
# Strain et al. 1995A:
wordStr1995A <- merge(DFtr_merge, Str1995A, by = c("O"), all.x=TRUE, all.y=FALSE)
wordStr1995A <- subset(wordStr1995A, freq == "H" | freq == "L")
xtabs(~freq+reg, data=wordStr1995A)
# reg
# freq  E  R
# H 15 14
# L 13 14
unique(wordStr1995A$O[wordStr1995A$freq=="H" & wordStr1995A$reg=="R"])
# [1] best  bill  black bring dark  deal  saw   sense space stay  west  wife  write wrong
unique(wordStr1995A$O[wordStr1995A$freq=="H" & wordStr1995A$reg=="E"])
# [1] blood  break  dead   death  does   done   flow   foot   steak  sure   toward want   war watch worth 
unique(wordStr1995A$O[wordStr1995A$freq=="L" & wordStr1995A$reg=="R"])
# [1] blade blunt deed  ditch dodge dump  sack  sane  scorn scout weed  wick  wisp  yore 
unique(wordStr1995A$O[wordStr1995A$freq=="L" & wordStr1995A$reg=="E"])
# [1] blown  breast debt   dough  dove   dread  scarce suave  swamp  sword  wealth worm   wrath
unique(Str1995A$O[!(Str1995A$O %in% wordStr1995A$O)])
# [1] blister   blunder   boulder   broader   doctor    district  building  greatest  mirror    mercy     monarch   mischief  market    manner    money     measure  
# [17] mustard   mister    monkey    nowhere   morning   method    mother    nothing  pepper    parry     treasure  twofold   picture   training  people    pickle   
# [33] pious     croquet   toughness teacher   trying    police    trouble   wont


# Taraban & McClelland 1987A1:
wordTM1987A1 <- merge(DFtr_merge, TM1987A1, by = c("O"), all.x=TRUE, all.y=FALSE)
wordTM1987A1 <- subset(wordTM1987A1, freq == "H" | freq == "L")
xtabs(~freq+reg, data=wordTM1987A1)
# reg
# freq  E  R
# H 24 24
# L 24 24
unique(wordTM1987A1$O[wordTM1987A1$freq=="H" & wordTM1987A1$reg=="R"])
# [1] best  big   came  class dark  did   fact  got   group him   main  out   page  place see   soon  stop  tell  week  when  which will  with  write
unique(wordTM1987A1$O[wordTM1987A1$freq=="H" & wordTM1987A1$reg=="E"])
# [1] are    both   break  choose come   do     does   done   foot   give   great  have  move   pull   put    says   shall  want   watch  were   what   word   work  
unique(wordTM1987A1$O[wordTM1987A1$freq=="L" & wordTM1987A1$reg=="R"])
# [1] beam  broke bus   deed  dots  float grape lunch peel  pitch pump  ripe  sank  slam  slip  stunt swore trunk wake  wax   weld  wing  with  word 
unique(wordTM1987A1$O[wordTM1987A1$freq=="L" & wordTM1987A1$reg=="E"])
# [1] bowl  broad bush  deaf  doll  flood gross lose  pear  phase pint  plow  rouse sew   shoe  spook swamp swarm touch wad   wand  wash  wool  worm
unique(TM1987A1$O[!(TM1987A1$O %in% wordTM1987A1$O)])


# Taraban-McClelland-1987-Appendix-A2:
wordTM1987A2 <- merge(DFtr_merge, TM1987A2, by = c("O"), all.x=TRUE, all.y=FALSE)
wordTM1987A2 <- subset(wordTM1987A2, freq == "H" | freq == "L")
xtabs(~freq+const, data=wordTM1987A2)
# const
# freq  C  I
# H 24 24
# L 24 23
unique(wordTM1987A2$O[wordTM1987A2$freq=="H" & wordTM1987A2$const=="C"])
# [1] bag   bird  by    clean corn  draw  dust  fast  feet  fine  fish  get   girl  gold  help  high  mile  piece plate rice  rod   sent  skin  such
unique(wordTM1987A2$O[wordTM1987A2$freq=="H" & wordTM1987A2$const=="I"])
# [1] base  bone  but   catch cool  days  dear  five  flat  flew  form  go    goes  grow  here  home  meat  paid  plant roll  root  sand  small speak
unique(wordTM1987A2$O[wordTM1987A2$freq=="L" & wordTM1987A2$const=="C"])
# [1] brisk cane  clang code  cope  dime  fawn  gong  hide  hike  leg   loom  luck  math  mist  mix   moist mole  pail  peach peep  reef  taps  tend 
unique(wordTM1987A2$O[wordTM1987A2$freq=="L" & wordTM1987A2$const=="I"])
# [1] brood cook  cord  cove  cramp dare  fowl  gull  harm  hoe   lash  leaf  loss  mad   moth  mouse mush  pork  pose  pouch rave  tint  toad 
unique(TM1987A2$O[!(TM1987A2$O %in% wordTM1987A2$O)])
# [1] moose