library(stringr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(hexbin)
library(plyr)
library(Rmisc)

options(max.print=10000)

drawrange <- c(1e4,1e7)
errrange_ptop <- c(10, 1e7)
errrange_otop <- c(10, 1e7)
drawpoint <- 1e7
pd <- position_dodge(0.1)
##################################################
## Get training data (O to P mappings).

# # read from extwords.csv
# fileNam <- 'extwords3_Harm1998.csv'
# words <- read.csv(fileName)
# nms <- str_replace(names(words), "_", ""); nms <- str_to_lower(nms)
# names(words) <- nms
# P <- str_replace_all(words$repp, "(_|/)", "")
# OP <- paste(words$wordform, P, sep=".")

## Get O,P pairs and frequency from mikenet training set.
# getDict <- function(fname) {
#   val <- readLines(fname, n=-1)
#   val <- val[stringr::str_detect(val, "^(TAG|PROB) ")]
#   idx <- rep(1:(length(val)/2), each=2)
#   dat <- data.frame(val,idx)
#   cleanD <- function(d) {
#     O <- stringr::str_extract(d[1,]$val, "Rep_O: [^ ]+")
#     O <- stringr::str_replace_all(O, "(Rep_O:|[ /_])", "")
#     P <- stringr::str_extract(d[1,]$val, "Rep_P: [^ ,]+")
#     P <- stringr::str_replace_all(P, "(Rep_P:|[ /_])", "")
#     prob <- stringr::str_extract(d[2,]$val, "[.0-9]+")
#     data.frame(O, P, prob)
#   }
#   plyr::ddply(dat, .(idx), cleanD)
# }
# ff <- "./data/H100L1e-2/trainexp_full3.txt"
# OP <- getDict(ff)
# head(OP)

# read from TrEm.txt
getList <- function(te, exp1, exp2){
  raw <- str_extract(te, exp1); raw <- str_replace(raw, exp2, "")
  rawLoc <- which(!is.na(raw))
  rawList <- c()
  for(ll in rawLoc){
    rawList <- c(rawList, raw[ll])
  }
  return(rawList)
}
crtDF <- function(t){
  wordList <- getList(t, "Word: ([a-z\"]+)", "Word: ") # get words
  probList <- getList(t, "PROB ([0-9.]+)", "PROB ") # get probs
  phonoList <- getList(t, "Rep_P: ([_/a-zA-Z@&^]+)", "Rep_P: ") # get phonological representations
  orthoList <- getList(t, "Rep_O: ([_/a-zA-Z]+)", "Rep_O: ") # get orthographical representations
  P <- str_replace_all(phonoList, "(_|/)", "")
  OPList <- paste(wordList, P, sep=".") # create OP list
  cat('noPh: ', length(unique(wordList)), '; noUniqueP: ', length(unique(P)),'; noOP: ', length(unique(OPList)), '\n')
  DF <- data.frame(word=wordList, prob=as.numeric(probList), phono=phonoList, ortho=orthoList, op=OPList)
  return(list(first=DF, second=OPList))
}
TrfileNam <- 'TrEm_Harm1998_0.0.txt'; trf <- readLines(TrfileNam); head(trf, 20)
TefileNam <- 'Te_sqrt.txt'; tef <- readLines(TefileNam); head(tef, 20)

r <- crtDF(trf)
# noPh: 4017; noUniqueP: 3636; noOP: 4017 
DFtr <- r$first; OPList_tr <- r$second
write.csv(DFtr, './trainingexp.csv', row.names=FALSE)
r <- crtDF(tef)
# noPh: 48; noUniqueP: 48; noOP: 48 
DFte <- r$first; OPList_te <- r$second
write.csv(DFte, './testingexp.csv', row.names=FALSE)


##################################################
## get overall (average) performance data from a set of models. Model
## outputs ("output.txt" files) are assumed to be in subdirectories 
## relative to the current working directory.
readOutput <- function(f){
  print(f)
  prs <- str_split(f, "/", simplify=TRUE) # prs[1] is condition; prs[2] is run id; prs[3] is filename (output.txt)
  hlsize <- as.integer(str_extract(str_split(prs[1], "L", simplify=TRUE)[1], "[0-9]+"))
  lrnrate <- as.numeric(str_split(prs[1], "L", simplify=TRUE)[2])
  run <- as.integer(prs[2])
  retval <- read.delim(f)
  retval <- data.frame(hlsize, lrnrate, run, retval)
}
## format numbers in scientific notation
scinot <- function(x){
  if(is.numeric(x)){ format(x, scientific=TRUE)
  }else{ error("x must be numeric") }
}

# get OtoP accuracy data
f <- dir(".", pattern="^output.txt$", recursive=TRUE)
avgaccu <- ldply(f, readOutput); names(avgaccu) <- str_to_lower(names(avgaccu))
write.csv(avgaccu, './AvgAcu_OtoP.csv', row.names=FALSE)
# draw OtoP training error
ggplot(avgaccu, aes(x=iter, y=err_o2p)) + scale_x_log10(labels=scinot) +  
  coord_cartesian(xlim=errrange_otop) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Training Error x Trials (OtoP) \n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
ggsave('Error_OtoP.png', dpi = 300, height = 6, width = 12, units = 'in')
# for runmode=3 or 4, draw PtoP training error
ggplot(avgaccu, aes(x=iter, y=err_p2p)) + scale_x_log10(labels=scinot) +  
  coord_cartesian(xlim=errrange_otop) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Training Error x Trials (OtoP) \n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
ggsave('Error_PtoP_int.png', dpi = 300, height = 6, width = 12, units = 'in')

# draw training accuracy
ggplot(avgaccu, aes(x=iter, y=acutr)) + scale_x_log10(labels=scinot) +  
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Training Acc x Trials \n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
ggsave('AvgAcc_Tr_OtoP.png', dpi = 300, height = 6, width = 12, units = 'in')
# draw testing accuracy
ggplot(avgaccu, aes(x=iter, y=acute)) + scale_x_log10(labels=scinot) +  
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Testing Acc x Trials\n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
ggsave('AvgAcc_Te_OtoP.png', dpi = 300, height = 6, width = 12, units = 'in')

# get PtoP accuracy data (only for runmode = 1 and 4)
f <- dir(".", pattern="^output_ptop.txt$", recursive=TRUE)
avgaccu_ptop <- ldply(f, readOutput); names(avgaccu_ptop) <- str_to_lower(names(avgaccu_ptop))
write.csv(avgaccu_ptop, './AvgAcu_PtoP.csv', row.names=FALSE)
# draw PtoP training error (only for runmode = 1 and 4)
ggplot(avgaccu_ptop, aes(x=iter, y=err)) + scale_x_log10(labels=scinot) +  
  coord_cartesian(xlim=errrange_ptop) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Training Error x Trials (OtoP) \n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
ggsave('Error_PtoP.png', dpi = 300, height = 6, width = 12, units = 'in')


# # calculate distribution of words in the OtoP traing example
# f <- dir(".", pattern="^trainfreq.txt$", recursive=TRUE)
# trainfreq <- ldply(f, readOutput); names(trainfreq) <- str_to_lower(names(trainfreq))
# # draw distribution of occurrence of training examples
# timepoint <- 100000; runID <- 1
# trainfreq_sub <- trainfreq[trainfreq$iter==timepoint & trainfreq$run==runID,]
# # timepoint <- 10000
# # trainfreq_sub <- trainfreq[trainfreq$iter==timepoint,]
# wrd <- which(str_detect(names(trainfreq_sub), "^f[0-9]+$")); names(wrd) <- 1:4008
# freqdist <- tidyr::gather(trainfreq_sub, wrd, key="item", value="occur")
# 
# ggplot(freqdist, aes(x=item, y=occur, color=run)) + geom_bar(stat="identity", width=0.1, color=freqdist$run) +  
#   xlab("Training Examples") + ylab("Occurrence") + 
#   ggtitle(paste("Occurrence of Training Examples\nat ", timepoint, " training; Run ", runID, sep="")) +
#   facet_grid(lrnrate~hlsize)
# ggsave(paste('FreqDist_bar_OtoP_', timepoint, '.png'), dpi = 300, height = 6, width = 18, units = 'in')
# ggplot(freqdist, aes(occur, color=run)) + geom_histogram(bins=50) + facet_grid(hlsize~lrnrate) +
#   xlab("Occurrence") + ylab("Count") + 
#   ggtitle(paste("Histogram of Occurrence of Training Examples\n at ", timepoint, " Run ", runID, sep=""))
# ggsave(paste('FreqDist_hist_OtoP_', timepoint, '.png'), dpi = 300, height = 6, width = 18, units = 'in')
# 
# # calculate distribution of words in the PtoP traing example
# f <- dir(".", pattern="^trainfreq_ptop.txt$", recursive=TRUE)
# trainfreq <- ldply(f, readOutput); names(trainfreq) <- str_to_lower(names(trainfreq))
# # draw distribution of occurrence of training examples
# timepoint <- 100000; runID <- 1
# trainfreq_sub <- trainfreq[trainfreq$iter==timepoint & trainfreq$run==runID,]
# # timepoint <- 10000
# # trainfreq_sub <- trainfreq[trainfreq$iter==timepoint,]
# wrd <- which(str_detect(names(trainfreq_sub), "^f[0-9]+$")); names(wrd) <- 1:4008
# freqdist <- tidyr::gather(trainfreq_sub, wrd, key="item", value="occur")
# 
# ggplot(freqdist, aes(x=item, y=occur, color=run)) + geom_bar(stat="identity", width=0.1, color=freqdist$run) +  
#   xlab("Training Examples") + ylab("Occurrence") + 
#   ggtitle(paste("Occurrence of Training Examples\nat ", timepoint, " training; Run ", runID, sep="")) +
#   facet_grid(lrnrate~hlsize)
# ggsave(paste('FreqDist_bar_PtoP_', timepoint, '.png'), dpi = 300, height = 6, width = 18, units = 'in')
# ggplot(freqdist, aes(occur, color=run)) + geom_histogram(bins=50) + facet_grid(hlsize~lrnrate) +
#   xlab("Occurrence") + ylab("Count") + 
#   ggtitle(paste("Histogram of Occurrence of Training Examples\n at ", timepoint, " Run ", runID, sep=""))
# ggsave(paste('FreqDist_hist_PtoP_', timepoint, '.png'), dpi = 300, height = 6, width = 18, units = 'in')


##################################################
## get word-level performance data from a set of models & tidy it. 
## Data files ("itemacu_tr.txt") are assumed to be in subdirectories 
## relative to the current working directory.
## "itemacu_tr.txt" for item accuracy. 
## "outphonTr.txt" for item activated phonological representations
## "outphonErrTr.txt" for item squared root errors
getItemAcuActPhonErr <- function(f1, f2, f3, OPList){
  ## read item accuracy data
  t <- ldply(f1, readOutput); names(t) <- str_to_lower(names(t))
  t <- t[-which(names(t) == "noitem")] # tidyr way to do this is ??
  ## re-label item columns, with wordforms (O.P) they represent
  wrd <- which(str_detect(names(t), "^acu[0-9]+$")) # find the right columns
  ## should probably double check to ensure length(wrd) == length(OP)
  names(t)[wrd] <- OPList # NB: wordforms (O-rep) are unique
  ## convert from wide to long format
  t <- tidyr::gather(t, wrd, key="OP", value="accuracy")
  t <- tidyr::separate(t, OP, into=c("O", "P"), sep="[.]")
  
  ## read activated phoneme data
  actphon <- ldply(f2, readOutput); names(actphon) <- str_to_lower(names(actphon))
  actphon <- actphon[-which(names(actphon) == "noitem")] # tidyr way to do this is ??
  ## re-label item columns, with wordforms (O.P) they represent
  actp_pos <- which(str_detect(names(actphon), "^phon[0-9]+$")) # find the right columns
  names(actphon)[actp_pos] <- OPList # NB: wordforms (O-rep) are unique
  
  ## convert from wide to long format
  tractp <- tidyr::gather(actphon, actp_pos, key="OP", value="actphon")
  tractp <- tidyr::separate(tractp, OP, into=c("O", "P"), sep="[.]")
  tractp$actphon <- gsub("_", "", tractp$actphon)
  
  # merge tr with tractp
  t$actphon <- tractp$actphon
  
  if(!is.null(f3)){
    ## read item squared root errors data
    err <- ldply(f3, readOutput); names(err) <- str_to_lower(names(err))
    err <- err[-which(names(err) == "noitem")] # tidyr way to do this is ??
    ## re-label item columns, with wordforms (O.P) they represent
    err_pos <- which(str_detect(names(err), "^err[0-9]+$")) # find the right columns
    names(err)[err_pos] <- OPList # NB: wordforms (O-rep) are unique
  
    ## convert from wide to long format
    trerr <- tidyr::gather(err, err_pos, key="OP", value="err")
    trerr <- tidyr::separate(trerr, OP, into=c("O", "P"), sep="[.]")
    trerr$err <- gsub("_", "", trerr$err)
  
    # merge tr with trerr
    t$err <- trerr$err
  }
  
  return(t)
}
# for OtoP training items
f1 <- dir(".", pattern="^itemacu_tr.txt$", recursive=TRUE)
f2 <- dir(".", pattern="^outphonTr.txt$", recursive=TRUE)
f3 <- dir(".", pattern="^outphonErrTr.txt$", recursive=TRUE)
tr <- getItemAcuActPhonErr(f1, f2, f3, OPList_tr)
write.csv(tr, './tr_allres_OtoP.csv', row.names=FALSE)
# for OtoP testing items
f1 <- dir(".", pattern="^itemacu_te.txt$", recursive=TRUE)
f2 <- dir(".", pattern="^outphonTe.txt$", recursive=TRUE)
f3 <- dir(".", pattern="^outphonErrTe.txt$", recursive=TRUE)
te <- getItemAcuActPhonErr(f1, f2, f3, OPList_te)
write.csv(te, './te_allres_OtoP.csv', row.names=FALSE)

# for PtoP training items
f1 <- dir(".", pattern="^itemacu_tr_ptop.txt$", recursive=TRUE)
f2 <- dir(".", pattern="^outphonTr_ptop.txt$", recursive=TRUE)
tr_ptop <- getItemAcuActPhonErr(f1, f2, NULL, OPList_tr)
write.csv(tr_ptop, './tr_allres_PtoP.csv', row.names=FALSE)
# for PtoP testing items
f1 <- dir(".", pattern="^itemacu_te_ptop.txt$", recursive=TRUE)
f2 <- dir(".", pattern="^outphonTe_ptop.txt$", recursive=TRUE)
te_ptop <- getItemAcuActPhonErr(f1, f2, NULL, OPList_te)
write.csv(te_ptop, './te_allres_PtoP.csv', row.names=FALSE)

# subsample accuracy outputs ?
# if(FALSE){
#   samps <- sort(unique(tr$iter))
#   period <- 4 ## keep every nth sample
#   keep <- c(TRUE, rep(FALSE, length.out=period-1))
#   keep <- rep(keep, length.out=length(samps))
#   trs <- subset(tr, subset=keep)
# }else{
#   trs <- tr
# }

##### Plot average accuracy as output by model
tmp <- tr[,c("hlsize", "lrnrate", "iter", "avg")]; tmp <- unique(tmp)
ggplot(tmp, aes(x=iter, y=avg)) + scale_x_log10(labels=scinot) +
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
  ggtitle("Training Acc x Trials\n Hid Layer & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)
tmp <- te[,c("hlsize", "lrnrate", "iter", "avg")]; tmp <- unique(tmp)
ggplot(tmp, aes(x=iter, y=avg)) + scale_x_log10(labels=scinot) +
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
  ggtitle("Testing Acc x Trials\n Hid Layer Size & Learn Rate") +
  geom_point(alpha=.2, color="blue") + geom_smooth(span=.2, color="darkorange") + facet_grid(lrnrate~hlsize)


##### Diffs based on phonological structure
getCVCdata <- function(type, t){
  consp <- c("p", "b", "t", "d", "k", "g", "f", "v", "T", "D", "s", "z", "h", "S", "B", "C", "J", "m", "n", "G", "r", "l", "w", "j")
  consp.re <- paste0("[", paste(consp, collapse=""), "]")
  vowo <- c("a", "e", "i", "o", "u")
  if(type=='Harm&Seidenberg1999'){
    vowp <- c("i", "I", "E", "e", "@", "a", "x", "o", "U", "u", "^"); vowp.re <- paste0("[", paste(vowp, collapse="|"), "]")
  }else if(type=='Harm1998'){
    vowp <- c("i", "I", "E", "e", "@", "a", "o", "U", "u", "^", "W", "Y", "A", "O"); vowp.re <- paste0("[", paste(vowp, collapse=""), "]")
  }
  cvc.re <- paste0("^", consp.re, "{1}", vowp.re, "{1}", consp.re, "{1}", "$")
  ccvc.re <- paste0("^", consp.re, "{2}", vowp.re, "{1}", consp.re, "{1}", "$")
  cvcc.re <- paste0("^", consp.re, "{1}", vowp.re, "{1}", consp.re, "{2}", "$")
  ccvcc.re <- paste0("^", consp.re, "{2}", vowp.re, "{1}", consp.re, "{2}", "$")
  
  Pcvc <- str_detect(t$P, cvc.re); Pccvc <- str_detect(t$P, ccvc.re)
  Pcvcc <- str_detect(t$P, cvcc.re); Pccvcc <- str_detect(t$P, ccvcc.re)
  syl <- rep(NA, length.out=length(Pcvc))
  syl[Pcvc] <- "cvc"; syl[Pccvc] <- "ccvc"; syl[Pcvcc] <- "cvcc"; syl[Pccvcc] <- "ccvcc"
  
  ts <- data.frame(t, syl); tsub <- subset(ts, !is.na(syl))
  cat('noCVC: ', length(unique(tsub$O[tsub$syl=="cvc"])), '; noCVCC: ', length(unique(tsub$O[tsub$syl=="cvcc"])), 
      '; noCCVC: ', length(unique(tsub$O[tsub$syl=="ccvc"])), '; noCCVCC: ', length(unique(tsub$O[tsub$syl=="ccvcc"])), '\n')
  return(tsub)
}
## Based on Harm & Seidenberg 1999
# trsub <- getCVCdata("Harm&Seidenberg1999", tr)
# tesub <- getCVCdata("Harm&Seidenberg1999", te)

## Based on Harm 1998
trsub <- getCVCdata("Harm1998", tr) # noCVC:  1515 ; noCVCC:  814 ; noCCVC:  785 ; noCCVCC:  284
tesub <- getCVCdata("Harm1998", te) # noCVC:  48 ; noCVCC:  0 ; noCCVC:  0 ; noCCVCC:  0 

## find phon forms that are CVC with simple vowels. This def will also include CV, where V is a diphthong (e.g., pay, buy).
ggplot(trsub, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
  ggtitle("Training Phon CVC vs. CCVC vs. CVCC vs. CCVCC") +
  geom_smooth(aes(color=as.factor(syl)), span=.2) + facet_grid(lrnrate~hlsize)
ggsave('CVCAcc_tr.png', dpi = 300, height = 6, width = 12, units = 'in')
ggplot(tesub, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
  ggtitle("Testing Phon CVC vs. CCVC vs. CVCC vs. CCVCC") +
  geom_smooth(aes(color=as.factor(syl)), span=.2) + facet_grid(lrnrate~hlsize)
ggsave('CVCAcc_te.png', dpi = 300, height = 6, width = 12, units = 'in')


# ##### split items randomly into 4 groups
# doGroups <- function(d){
#   group <- sample(LETTERS[1:4], 1)
#   data.frame(d, group)
# }
# trsr <- ddply(trsub, "O", doGroups) ## will create a different random
# ## grouping each time this is run
# windows()
# ggplot(trsr, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
#   geom_smooth(aes(color=group), span=.2) + 
#   ggtitle("Randomly Grouped") + facet_grid(lrnrate~hlsize)
# ## compare a few of these to calibrate the eye
# 
# doGroupsN <- function(run, df){
#   data.frame(run, ddply(df, "O", doGroups))
# }
# runs <- 1:4
# trsr4 <- ldply(runs, doGroupsN, trsub)
# windows()
# ggplot(trsr4, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +
#   geom_smooth(aes(color=group), span=.2) + 
#   facet_wrap(~run, nrow=2) + ggtitle("Randomly Grouped, 4 ways") + facet_grid(lrnrate~hlsize)
# ##### break out sensible word groups to look at performance in more
# ##### detail
# 
# ## length based categories
# Olen <- str_length(tr$O) ## letter length
# Plen <- str_length(tr$P) ## phoneme length
# trsa <- data.frame(tr, Olen, Plen)
# trsa <- data.frame(trsa, matchlen=Olen==Plen)
# 
# ggplot(trsa, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + geom_smooth(aes(color=matchlen), span=.2) +
#   ggtitle("O length == P length vs. O length != P length") + facet_grid(lrnrate~hlsize)
#  
# trsa2 <- data.frame(trsa, difflen=Olen-Plen)
# ggplot(trsa2, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + geom_smooth(aes(color=as.factor(difflen)), span=.2) +
#   ggtitle("O length minus P length") + facet_grid(lrnrate~hlsize)
# ggsave('Acu_OminusP.png', dpi = 300, height = 6, width = 12, units = 'in')
# 
# ## Words with fewer letters than phonemes are the easiest cases to
# ## learn (e.g., ax, fix, next, fry, sky). Words that match in O and P ength are the next easiest, and those with 1 fewer phones than
# ## letters are next. Some bins are small; see the confidence intervals.
# 
# ggplot(trsa2, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + geom_smooth(aes(color=as.factor(Plen)), span=.2) +
#   ggtitle("Length in Phonemes") + facet_grid(lrnrate~hlsize)
# ggsave('PlengAcu.png', dpi = 300, height = 6, width = 12, units = 'in')
# ## Relationship between word length (in phonemes) and accuracy. 
# ## Shorter words are learned less well. That's just plain weird.
# 
# ## Words with C that are often written as digraphs (e.g., ship, chip, judge, the, this) vs. those without such C.
# digr.re <- "[TSDCG]"
# Pdigr <- str_detect(trsa2$P, digr.re)
# trsa2 <- data.frame(trsa2, Pdigr)
# 
# ggplot(trsa2, aes(x=iter, y=accuracy)) + scale_x_log10(labels=scinot) +
#   coord_cartesian(xlim=drawrange) + geom_smooth(aes(color=as.factor(Pdigr)), span=.2) +
#   ggtitle("Probable Digraph C vs. Other") + facet_grid(lrnrate~hlsize)
# ## Digraphs are harder to learn. Not surprising.


# # get words' frequencies
# mean_log_freq <- mean(DFtr$prob) # 0.2017773
# DFtr$mean[DFtr$prob>=mean_log_freq] <- "H"; DFtr$mean[DFtr$prob<mean_log_freq] <- "L"
# DFmerge <- DFtr[,c("word", "prob", "mean")]; names(DFmerge) <- c("O", "log_freq", "freq_mean")
# tr2 <- merge(tr, DFmerge, by = c("O"), all.x = TRUE, all.y = TRUE)
# write.csv(tr2, "AllRes_tr.csv", row.names=FALSE)
# 
# mean_log_freq <- mean(DFte$prob) # 0.05
# DFte$mean[DFte$prob>=mean_log_freq] <- "H"; DFte$mean[DFte$prob<mean_log_freq] <- "L"
# DFmerge <- DFte[,c("word", "prob", "mean")]; names(DFmerge) <- c("O", "log_freq", "freq_mean")
# te2 <- merge(te, DFmerge, by = c("O"), all.x = TRUE, all.y = TRUE)
# write.csv(te2, "AllRes_te.csv", row.names=FALSE)


#################### accuracy based on different types of words
DFtr <- read.csv('./trainingexp.csv'); DFtr_merge <- DFtr[,c('word', 'prob')]; names(DFtr_merge) <- c("O", "log_freq")
DFte <- read.csv('./testingexp.csv'); DFte_merge <- DFte[,c('word', 'prob')]; names(DFte_merge) <- c("O", "log_freq")
tr <- read.csv('./tr_allres_OtoP.csv'); te <- read.csv('./te_allres_OtoP.csv')

# strain et al. 1995 case:
strain1995A <- read.csv("Strain-etal-1995-Appendix-A.csv")
word1995A <- merge(DFtr_merge, strain1995A, by = c("O"), all.x=TRUE, all.y=FALSE)
word1995A <- subset(word1995A, freq == "H" | freq == "L")
xtabs(~freq+reg, data=word1995A)
# reg
# freq  E  R
# H 15 14
# L 13 14
unique(word1995A$O[word1995A$freq=="H" & word1995A$reg=="R"])
# [1] best  bill  black bring dark  deal  saw   sense space stay  west  wife  write wrong
unique(word1995A$O[word1995A$freq=="H" & word1995A$reg=="E"])
# [1] blood  break  dead   death  does   done   flow   foot   steak  sure   toward want   war watch worth 
unique(word1995A$O[word1995A$freq=="L" & word1995A$reg=="R"])
# [1] blade blunt deed  ditch dodge dump  sack  sane  scorn scout weed  wick  wisp  yore 
unique(word1995A$O[word1995A$freq=="L" & word1995A$reg=="E"])
# [1] blown  breast debt   dough  dove   dread  scarce suave  swamp  sword  wealth worm   wrath
unique(strain1995A$O[!(strain1995A$O %in% word1995A$O)])
# [1] blister   blunder   boulder   broader   doctor    district  building  greatest 
# [9] mirror    mercy     monarch   mischief  market    manner    money     measure  
# [17] mustard   mister    monkey    nowhere   morning   method    mother    nothing  
# [25] pepper    parry     treasure  twofold   picture   training  people    pickle   
# [33] pious     croquet   toughness teacher   trying    police    trouble   wont
tr_1995A <- subset(tr, tr$O %in% strain1995A$O)
tr_1995A <- merge(tr_1995A, strain1995A, by = c("O"), all.x = TRUE, all.y = FALSE)
tr_FR <- subset(tr_1995A, freq == "H" | freq == "L")
cat(length(unique(tr_FR$O)), '\n')
# 56
# draw accuracy
ggplot(tr_FR, aes(x=iter, y=accuracy, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Acc x Trials: Strain etal 1995 \n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave('AcuFR_Strainetal1995A.png', dpi = 300, height = 6, width = 12, units = 'in')
# draw squared root error
ggplot(tr_FR, aes(x=iter, y=err, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Err x Trials: Strain etal 1995 \n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave('ErrFR_Strainetal1995A.png', dpi = 300, height = 6, width = 12, units = 'in')
# anova test (for results under a single setting!)
fit <- aov(err ~ freq*reg, data=tr_FR)
summary(fit)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# freq            1   1499    1499  32.056 1.53e-08 ***
#   reg             1  12571   12571 268.804  < 2e-16 ***
#   freq:reg        1    261     261   5.577   0.0182 *  
#   Residuals   13884 649293      47                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FR_avg <- summarySE(tr_FR, measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FR_avg, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 12.0)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Reg: Strain etal 1995 \n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFRavg_Strainetal1995A.png', dpi = 300, height = 6, width = 12, units = 'in')
# drawpoint
fit2 <- aov(err ~ freq*reg, data=tr_FR[tr_FR$iter==drawpoint, ])
summary(fit2)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# freq          1    1.0   0.979   0.952     0.33    
# reg           1   19.1  19.142  18.609 1.98e-05 ***
#   freq:reg      1    0.7   0.700   0.681     0.41    
# Residuals   444  456.7   1.029                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FR_last <- summarySE(tr_FR[tr_FR$iter==drawpoint,], measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FR_last, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(0.0, 1.0)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Reg: Strain etal 1995 \n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFRavg_Strainetal1995A_1e7.png', dpi = 300, height = 6, width = 12, units = 'in')

# Taraban & McClelland 1987 case:
TM1987A1 <- read.csv("Taraban-McClelland-1987-Appendix-A1.csv", na.strings='na')
word1987A1 <- merge(DFtr_merge, TM1987A1, by = c("O"), all.x=TRUE, all.y=FALSE)
word1987A1 <- subset(word1987A1, freq == "H" | freq == "L")
xtabs(~freq+reg, data=word1987A1)
# reg
# freq  E  R
# H 24 24
# L 24 24
unique(word1987A1$O[word1987A1$freq=="H" & word1987A1$reg=="R"])
# [1] best  big   came  class dark  did   fact  got   group him   main  out   page  place
# [15] see   soon  stop  tell  week  when  which will  with  write
unique(word1987A1$O[word1987A1$freq=="H" & word1987A1$reg=="E"])
# [1] are    both   break  choose come   do     does   done   foot   give   great  have  
# [13] move   pull   put    says   shall  want   watch  were   what   word   work  
unique(word1987A1$O[word1987A1$freq=="L" & word1987A1$reg=="R"])
# [1] beam  broke bus   deed  dots  float grape lunch peel  pitch pump  ripe  sank  slam 
# [15] slip  stunt swore trunk wake  wax   weld  wing  with  word 
unique(word1987A1$O[word1987A1$freq=="L" & word1987A1$reg=="E"])
# [1] bowl  broad bush  deaf  doll  flood gross lose  pear  phase pint  plow  rouse sew  
# [15] shoe  spook swamp swarm touch wad   wand  wash  wool  worm
unique(TM1987A1$O[!(TM1987A1$O %in% word1987A1$O)])

tr_1987A1 <- subset(tr, tr$O %in% TM1987A1$O)
tr_1987A1 <- merge(tr_1987A1, TM1987A1, by = c("O"), all.x = TRUE, all.y = FALSE)
tr_FR <- subset(tr_1987A1, freq == "H" | freq == "L")
cat(length(unique(tr_FR$O)), '\n')
# 94
# draw accuracy
ggplot(tr_FR, aes(x=iter, y=accuracy, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Acc x Trials: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave('AcuFR_TarabanMcClelland1987A1.png', dpi = 300, height = 6, width = 12, units = 'in')
# draw squared root error
ggplot(tr_FR, aes(x=iter, y=err, color=interaction(freq, reg))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Err x Trials: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, reg))) + facet_grid(lrnrate~hlsize)
ggsave('ErrFR_TarabanMcClelland1987A1.png', dpi = 300, height = 6, width = 12, units = 'in')
# anova test (for results under a single setting!)
fit <- aov(err ~ freq*reg, data=tr_FR)
summary(fit)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# freq            1    1738    1738  40.510 1.99e-10 ***
#   reg             1    8744    8744 203.810  < 2e-16 ***
#   freq:reg        1     339     339   7.895  0.00496 ** 
#   Residuals   23804 1021270      43                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FR_avg <- summarySE(tr_FR, measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FR_avg, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 10.0)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Reg: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFRavg_TarabanMcClelland1987A1.png', dpi = 300, height = 6, width = 12, units = 'in')
# drawpoint
fit2 <- aov(err ~ freq*reg, data=tr_FR[tr_FR$iter==drawpoint,])
summary(fit2)
# Df Sum Sq Mean Sq F value   Pr(>F)    
# freq          1    0.9   0.913   0.804 0.370126    
# reg           1   17.3  17.291  15.226 0.000104 ***
#   freq:reg      1    0.9   0.927   0.817 0.366453    
# Residuals   764  867.6   1.136                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FR_last <- summarySE(tr_FR[tr_FR$iter==drawpoint,], measurevar="err", groupvars=c("freq", "reg", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FR_last, aes(x=freq, y=err, linetype=reg, group=reg)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(0.0, 1.0)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Reg: Taraban & McClelland 1987 A1\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFRavg_TarabanMcClelland1987A1_1e7.png', dpi = 300, height = 6, width = 12, units = 'in')

TM1987A2 <- read.csv("Taraban-McClelland-1987-Appendix-A2.csv", na.strings='na')
word1987A2 <- merge(DFtr_merge, TM1987A2, by = c("O"), all.x=TRUE, all.y=FALSE)
word1987A2 <- subset(word1987A2, freq == "H" | freq == "L")
xtabs(~freq+const, data=word1987A2)
# const
# freq  C  I
# H 24 24
# L 24 23
unique(word1987A2$O[word1987A2$freq=="H" & word1987A2$const=="C"])
# [1] bag   bird  by    clean corn  draw  dust  fast  feet  fine  fish  get   girl  gold 
# [15] help  high  mile  piece plate rice  rod   sent  skin  such
unique(word1987A2$O[word1987A2$freq=="H" & word1987A2$const=="I"])
# [1] base  bone  but   catch cool  days  dear  five  flat  flew  form  go    goes  grow 
# [15] here  home  meat  paid  plant roll  root  sand  small speak
unique(word1987A2$O[word1987A2$freq=="L" & word1987A2$const=="C"])
# [1] brisk cane  clang code  cope  dime  fawn  gong  hide  hike  leg   loom  luck  math 
# [15] mist  mix   moist mole  pail  peach peep  reef  taps  tend 
unique(word1987A2$O[word1987A2$freq=="L" & word1987A2$const=="I"])
# [1] brood cook  cord  cove  cramp dare  fowl  gull  harm  hoe   lash  leaf  loss  mad  
# [15] moth  mouse mush  pork  pose  pouch rave  tint  toad 
unique(TM1987A2$O[!(TM1987A2$O %in% word1987A2$O)])
# [1] moose
tr_1987A2 <- subset(tr, tr$O %in% TM1987A2$O)
tr_1987A2 <- merge(tr_1987A2, TM1987A2, by = c("O"), all.x = TRUE, all.y = FALSE)
tr_FC <- subset(tr_1987A2, freq == "H" | freq == "L")
cat(length(unique(tr_FC$O)), '\n')
# 95
# draw accuracy
ggplot(tr_FC, aes(x=iter, y=accuracy, color=interaction(freq, const))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Acc x Trials: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, const))) + facet_grid(lrnrate~hlsize)
ggsave('AcuFC_TarabanMcClelland1987A2.png', dpi = 300, height = 6, width = 12, units = 'in')
# draw squared root error
ggplot(tr_FC, aes(x=iter, y=err, color=interaction(freq, const))) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Err x Trials: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=interaction(freq, const))) + facet_grid(lrnrate~hlsize)
ggsave('ErrFC_TarabanMcClelland1987A2.png', dpi = 300, height = 6, width = 12, units = 'in')
# anova test (for results under a single setting!)
fit <- aov(err ~ freq*const, data=tr_FC)
summary(fit)
# Df  Sum Sq Mean Sq F value   Pr(>F)    
# freq            1       3     2.6   0.055 0.813906    
# const           1     565   565.2  12.243 0.000468 ***
#   freq:const      1     398   397.7   8.613 0.003341 ** 
#   Residuals   23556 1087579    46.2                     
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FC_avg <- summarySE(tr_FC, measurevar="err", groupvars=c("freq", "const", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FC_avg, aes(x=freq, y=err, linetype=const, group=const)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(6.0, 9.0)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Const: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFCavg_TarabanMcClelland1987A2.png', dpi = 300, height = 6, width = 12, units = 'in')
# drawpoint
fit2 <- aov(err ~ freq*const, data=tr_FC[tr_FC$iter==drawpoint,])
summary(fit2)
# Df Sum Sq Mean Sq F value Pr(>F)  
# freq          1   0.29 0.28764   3.121 0.0777 .
# const         1   0.30 0.30206   3.278 0.0706 .
# freq:const    1   0.30 0.30309   3.289 0.0701 .
# Residuals   756  69.67 0.09215                 
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
tr_FC_last <- summarySE(tr_FC[tr_FC$iter==drawpoint,], measurevar="err", groupvars=c("freq", "const", "hlsize", "lrnrate"), na.rm=TRUE)
ggplot(tr_FC_last, aes(x=freq, y=err, linetype=const, group=const)) + 
  geom_errorbar(aes(ymin=err-se, ymax=err+se), size=1.5, width=.1, position=pd) +
  geom_line(position=pd, size=1.5) + geom_point(position=pd) + scale_y_continuous(limits=c(0.0, 0.2)) + 
  xlab("Frequency") + ylab("Sum Squared Err") +
  ggtitle("Freq x Const: Taraban & McClelland 1987 A2\n Hid Layer & Learn Rate") +
  facet_grid(lrnrate~hlsize)
ggsave('ErrFCavg_TarabanMcClelland1987A2_1e7.png', dpi = 300, height = 6, width = 12, units = 'in')


# nonwords: Treiman et al. 1990 case:
treiman1990A <- read.csv("Treiman-etal-1990-Appendix.csv")
word1990A <- merge(DFte_merge, treiman1990A, by = c("O"), all.x=TRUE, all.y=FALSE)
word1990A <- subset(word1990A, freq == "H" | freq == "L")
xtabs(~freq, data=word1990A)
# freq
# H  L 
# 24 24
te_1990A <- subset(te, te$O %in% treiman1990A$O)
te_1990A <- merge(te_1990A, treiman1990A, by = c("O"), all.x = TRUE, all.y = FALSE)
te_F <- subset(te_1990A, freq == "H" | freq == "L")
cat(length(unique(te_F$O)), '\n')
# 48
# draw accuracy
ggplot(te_F, aes(x=iter, y=accuracy, color=freq)) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Acc") +  
  ggtitle("Acc x Trials: Treiman etal 1990\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=freq)) + facet_grid(lrnrate~hlsize)
ggsave('AcuF_Treimanetal1990A.png', dpi = 300, height = 6, width = 12, units = 'in')
# draw squared root error
ggplot(te_F, aes(x=iter, y=err, color=freq)) + scale_x_log10(labels=scinot) + 
  coord_cartesian(xlim=drawrange) + xlab("Training Trials (log10)") + ylab("Avg Err") +  
  ggtitle("Err x Trials: Treiman etal 1990\n Hid Layer & Learn Rate") +
  geom_smooth(span=.2, aes(color=freq)) + facet_grid(lrnrate~hlsize)
ggsave('ErrF_Treimanetal1990A.png', dpi = 300, height = 6, width = 12, units = 'in')
