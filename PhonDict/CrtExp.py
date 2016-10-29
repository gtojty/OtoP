# -*- coding: utf-8 -*-
"""
Created on Mon Aug 8 10:36:17 2016

@author: tg422

Select particular words from Coca and Moby dictionaries
"""

import pandas as pd
import numpy as np
import csv

# for Dictionary in which values are lists
class Dictlist(dict):
    def __setitem__(self, key, value):
        try:
            self[key]
        except KeyError:
            super(Dictlist, self).__setitem__(key, [])
        self[key].append(value)

def saveDict(fn,dict_rap):
    f = open(fn,'wb')
    w = csv.writer(f)
    for key, val in dict_rap.items():
        w.writerow([key, val])
    f.close()
     
def readDict(fn):
    f = open(fn,'rb')
    dict_rap = Dictlist()
    for key, val in csv.reader(f):
        dict_rap[key] = eval(val)
    f.close()
    return(dict_rap)

def clnDict(cond, Dict):
    # cond = 0, clean original dictionary
    # cond = 1, clean replacement dictionary
    resDict = Dictlist()
    if cond == 0:
        for pron in Dict.keys():
            resDict[pron] = list(set(Dict[pron][0]))
    elif cond == 1:    
        for pron in Dict.keys():
            resDict[pron] = list(set(Dict[pron]))
    return(resDict)    

# calculate occurrance    
def noOccur(pron, phonList):
    # calculate how many phonemes in phonList occur in pron    
    occur = 0
    for phon in phonList:
        occur += pron.count(phon)
    return(occur)

# create dictionary for phonemes
def findAll(con, st):
    # find all occurrence positions of con in st, return a list
    resList = list()
    index = 0    
    while index < len(st):
        index = st.find(con, index)
        if index == -1: break
        resList.append(index)
        index += len(con)
    return(resList)
    
def getPhonDict(pron, phonList):
    # get consonant and vowel phonemes in pron, create a dictionary: key: 1-5, value: cons/vows
    # and return the dictionary and the number of phonemes in it
    proncopy = pron[:]
    tempDic = dict()    
    for con in phonList:
        posList = findAll(con, proncopy)
        # posList = [m.start() for m in sre.finditer(con, proncopy)] 
        # note: in this way, you have to use '\' before '(' or '[' to disable its meaning!
        if len(posList) != 0:
            for pos in posList:
                tempDic[pos] = con
            # mark up searched items to avoid confusions 't' and '/tS/'
            if con[0] == '/':
                newcon = '/' + len(con[1:-1])*'_' + '/'
                proncopy = proncopy.replace(con, newcon)
    candpos = tempDic.keys()
    candpos.sort()  # sort according to position of found con/vow in pron
    # create new dictionary
    resDic = dict()
    newseq = 1
    for pos in candpos:
        resDic[newseq] = tempDic[pos]
        newseq += 1    
    return(resDic, len(candpos))    

# replace keys
def repKey(cond, Dict, ori, rep, phonList, keepList):
    # replace Dict's keys containing rep, use bound to determine the core to keep
    # and return the new dictionary
    # cond = 0, need to consider additional separator
    # cond = 1, no need to consider separator, directly replace!
    sep = '/'    
    newDict = Dictlist()
    for pron in Dict.keys():
        # get pron's composition        
        (pronDic, phonLen) = getPhonDict(pron, phonList)        
        # determine newkey
        if ori not in pronDic.values(): newkey = pron # pron does not contain rep
        else: # pron contains rep 
            # find rep in which location            
            pos = -1        
            for key, value in pronDic.items():
                if value == ori:
                    pos = key
                    break
            if pos == -1: raise ValueError('Pron is supposed to contain' + ori)    
            # create newkey according to pos
            if cond == 0: # need to consider additional separator          
                if pos == 1: # first phoneme is ori
                    if pronDic[pos+1] in keepList: newkey = pron.replace(ori, rep+sep) # the next phoneme requires separator
                elif pos == phonLen: # last phoneme is ori
                    if pronDic[pos-1] in keepList: newkey = pron.replace(ori, sep+rep) # the previous phoneme requires separator                    
                else: # check both previous and next position            
                    if (pronDic[pos+1] in keepList) and (pronDic[pos-1] in keepList): 
                        if rep != '': newkey = pron.replace(ori, sep+rep+sep)  
                        else: newkey = pron.replace(ori, sep)
                    elif (pronDic[pos+1] in keepList) and (pronDic[pos-1] not in keepList): newkey = pron.replace(ori, rep+sep) 
                    elif (pronDic[pos+1] not in keepList) and (pronDic[pos-1] in keepList): newkey = pron.replace(ori, sep+rep)
                    elif (pronDic[pos+1] not in keepList) and (pronDic[pos-1] not in keepList): newkey = pron.replace(ori, sep)  
            else: newkey = pron.replace(ori, rep)                
        # add newkey, value pair
        if len(Dict[pron][0]) == 1: newDict[newkey] = Dict[pron][0][0]
        else: 
            for i in range(len(Dict[pron][0])):
                newDict[newkey] = Dict[pron][0][i]        
    return(newDict)

# replace and split phonemes
def rep_split_merge_Phon(pron, phonList, repDict, splitDict, mergeDict):
    # replace phonemes in pron according to repDict
    # split vowels according to splitDict
    # merge vowels according to mergeDict
    # return the list of phonemes and its length
    # get original phonemes    
    proncopy = pron[:]
    oriDict = dict()
    for con in phonList:
        posList = findAll(con, proncopy)
        # posList = [m.start() for m in sre.finditer(con, proncopy)] 
        # note: in this way, you have to use '\' before '(' or '[' to disable its meaning!
        if len(posList) != 0:
            for pos in posList:
                oriDict[pos] = con
            # mark up searched items to avoid confusions 't' and '/tS/'    
            if con[0] == '/':
                newcon = '/' + len(con[1:-1])*'_' + '/'
                proncopy = proncopy.replace(con, newcon)
    posList = oriDict.keys()
    posList.sort()
    oriList = list()
    for pos in posList:
        oriList.append(oriDict[pos])
    # replace phonemens        
    repList = list()        
    for con in oriList:
        if con in repDict.keys(): repList.append(repDict[con])
        else: repList.append(con)
    # split compound vowels
    if len(splitDict) != 0:
        splitList = list()
        for con in repList:
            if con in splitDict.keys():
                splitList.append(splitDict[con].split("/")[0])
                splitList.append(splitDict[con].split("/")[1])
            else: splitList.append(con)
    else: splitList = repList        
    # merge phonemes
    if len(mergeDict) != 0:
        mergeList = list()
        cur = 0
        while(cur < len(splitList)):
            if cur <= len(splitList)-2:
                curcon = splitList[cur] + splitList[cur+1]
                if curcon in mergeDict:
                    mergeList.append(mergeDict[curcon])
                    cur = cur+1
                else: mergeList.append(splitList[cur])
            else: mergeList.append(splitList[cur])
            cur = cur+1
    else: mergeList = splitList
    
    return(mergeList, len(mergeList))

def getVowLoc(word_pron, vowlett_vowphon):
    # find the smallest position in word_pron where there is a vowel letter or vowel phoneme    
    posList = list()    
    for vow in vowlett_vowphon:    
        if vow != 'y' and word_pron.find(vow) != -1: # find the first occurrence of vowel letter or vowel phoneme
            posList.append(word_pron.find(vow))
        if vow == 'y' and word_pron.find(vow) != -1 and word_pron.find(vow) != 0:
            posList.append(word_pron.find(vow))
    if len(posList) != 0: 
        posList.sort()
        return(posList[0])
    else: return(posList)         

def getNewPron(phonList, lenList, newvows, phonMax):
    # create new pron form based on pron
    vowLoc = getVowLoc(''.join(phonList), newvows)
    newvowLoc = int((phonMax-1)/2.0)
    
    newpronList = list()
    for i in range(phonMax):
        newpronList.append('_'); newpronList.append('/')
    newpron, succ = ''.join(newpronList), False
    if vowLoc != []:
        succ = True
        for i in range(len(phonList)):
            if 2*(newvowLoc + i - vowLoc) < len(newpronList): newpronList[2*(newvowLoc + i - vowLoc)] = phonList[i]
            else: 
                succ = False
                break    
        newpron = ''.join(newpronList)
    else: succ = False        
    
    return(newpron, succ)        
    
def getNewWord(word, vowlett, lettMax):
    # create new word form based on vowLoc
    vowLoc = getVowLoc(word, vowlett)
    newvowLoc = int((lettMax-1)/2.0)
    
    wordList = [m for m in word]    
    newwordList = list()
    for i in range(lettMax):
        newwordList.append('_'); newwordList.append('/')
    newword, succ = ''.join(newwordList), False
    if vowLoc != []:
        succ = True
        for i in range(len(wordList)):
            if 2*(newvowLoc + i - vowLoc) < len(newwordList): newwordList[2*(newvowLoc + i - vowLoc)] = wordList[i]
            else: 
                succ = False
                break    
        newword = ''.join(newwordList)        
    else: succ = False
    
    return(newword, succ)     
                    
def sumfreq(lemitem):
    freq = 0.0    
    lenlem = len(lemitem)
    for i in np.arange(0,lenlem,2):
        freq += np.float64(lemitem[i+1])
    return(freq)                   

def writeExp_OtoP(wordDF, expFileName, probcol, epoch, st1, end1, st2, end2, PhonDic, LettDic):
    with open(expFileName, "w+") as exp_file:
        cur = 0    
        for i in range(len(wordDF)):
            cur += 1; print "Cur: ", cur        
            # TAG line        
            line = "TAG Word: " + str(wordDF.wordform[i]) + " Rep_O: " + wordDF.Rep_O[i] + " Rep_P: " + wordDF.Rep_P[i] + ","
            exp_file.write(line + "\n")
            # PROB line        
            line = "PROB " + str(wordDF.loc[i, probcol])
            exp_file.write(line + "\n")
            # CLAMP line
            if st1 == end1: line = "CLAMP Ortho " + str(st1) + " EXPANDED"
            else: line = "CLAMP Ortho " + str(st1) + "-" + str(end1) + " EXPANDED"
            exp_file.write(line + "\n")
            # Orthography encoding
            for j in range(0, len(wordDF.Rep_O[i]), 2):
                encodList = LettDic[wordDF.Rep_O[i][j]]
                line = ""
                for k in range(len(encodList)):
                    line = line + str(int(encodList[k])) + " "
                exp_file.write(line + "\n")
            exp_file.write("\n")
            # TARGET line
            if st2 == end2: line = "CLAMP Phono " + str(st2) + " EXPANDED"
            else: line = "TARGET Phono " + str(st2) + "-" + str(end2) + " EXPANDED"
            exp_file.write(line + "\n")
            # Phonology encoding
            for j in range(0, len(wordDF.Rep_P[i]), 2):
                encodList = PhonDic[wordDF.Rep_P[i][j]]
                line = ""
                for k in range(len(encodList)):
                    line = line + str(encodList[k]) + " "
                exp_file.write(line + "\n")
            exp_file.write(";\n")    

def writeExp_PtoP(wordDF, expFileName, probcol, epoch, st1, end1, st2, end2, PhonDic):
    with open(expFileName, "w+") as exp_file:
        cur = 0    
        for i in range(len(wordDF)):
            cur += 1; print "Cur: ", cur        
            # TAG line        
            line = "TAG Word: " + str(wordDF.wordform[i]) + " Rep_O: " + wordDF.Rep_O[i] + " Rep_P: " + wordDF.Rep_P[i] + ","
            exp_file.write(line + "\n")
            # PROB line        
            line = "PROB " + str(wordDF.loc[i,probcol])
            exp_file.write(line + "\n")
            # CLAMP line        
            if st1 == end1: line = "CLAMP Phono " + str(st1) + " EXPANDED"
            else: line = "CLAMP Phono "+ str(st1) + "-" + str(end1) + " EXPANDED" 
            exp_file.write(line + "\n")
            # Phonology encoding
            for j in range(0, len(wordDF.Rep_P[i]), 2):
                encodList = PhonDic[wordDF.Rep_P[i][j]]
                line = ""
                for k in range(len(encodList)):
                    line = line + str(encodList[k]) + " "
                exp_file.write(line + "\n")
            exp_file.write("\n")
            # TARGET line
            if st2 == end2: line = "TARGET Phono " + str(st2) + " EXPANDED"
            else: line = "TARGET Phono " + str(st2) + "-" + str(end2) + " EXPANDED"
            exp_file.write(line + "\n")
            # Phonology encoding
            for j in range(0, len(wordDF.Rep_P[i]), 2):
                encodList = PhonDic[wordDF.Rep_P[i][j]]
                line = ""
                for k in range(len(encodList)):
                    line = line + str(encodList[k]) + " "
                exp_file.write(line + "\n")
            exp_file.write(";\n")    


# ---------------------------------------------- #
# for creating example files: ortho: 8-letter length (3(con) + 2(vow) + 3(con))
#                             phono: 5-phoneme length (2(con) + 1(vow) + 2(con))              
# ---------------------------------------------- #
# Step 1: Read Moby and make some replacement in phonemes
cons = ['b', '/tS/', 'd', 'f', 'g', 'h', '/hw/', '/dZ/', 'k', 'l', 'm', '/N/', 'n', 'p', 'R', 'r', '/S/', 's', '/T/', '/D/', 't', 'v', 'w', 'j', '/x/', '/Z/', 'z']
vows = ['/&/', '/(@)/', '/A/', '/eI/', '/@/', '/-/', '/E/', '/i/', '/I/', '/aI/', '/Oi/', '/AU/', '/O/', '/oU/', '/u/', '/U/', '/[@]/', '/y/', 'Y']
# comb_convow has the sequence that combined ones are ahead of shorter ones!
comb_convow = ['/tS/', '/dZ/', '/hw/', 'b', 'd', 'f', 'g', 'h', 'k', 'l', 'm', '/N/', 'n', 'p', 'R', 'r', '/S/', 's', '/T/', '/D/', 't', 'v', 'w', 'j', '/x/', '/Z/', 'z',
               '/(@)/', '/[@]/', '/aI/', '/Oi/', '/AU/', '/A/', '/eI/', '/oU/', '/@/', '/&/', '/-/', '/E/', '/i/', '/I/', '/O/', '/u/', '/U/', '/y/', 'Y']
# list of phonemes with '//'
keepList = ['/tS/', '/hw/', '/dZ/', '/N/', '/S/', '/T/', '/D/', '/Z/', '/x/'] + vows
keepList.remove('Y')
mobyDic = readDict('./mobypron_dict.csv') # get Moby database
# 1) delete '/-/' after '/aI/'
newDic = Dictlist()
for key in mobyDic.keys():
    if key.find('/aI/-/') != -1: newkey = key.replace('/aI/-/', '/aI/')
    else: newkey = key
    if len(mobyDic[key][0]) == 1: newDic[newkey] = mobyDic[key][0][0]
    else:
        for i in range(len(mobyDic[key][0])):
            newDic[newkey] = mobyDic[key][0][i]
mobyDic = newDic; mobyDic = clnDict(1, mobyDic)
# 2) replace '/-/' with '/@/'
ori, rep = '/-/', '/@/'; mobyDic = repKey(1, mobyDic, ori, rep, comb_convow, keepList); mobyDic = clnDict(1, mobyDic)
# 3) replace '/hw/' with 'w'
ori, rep = '/hw/', 'w'; mobyDic = repKey(0, mobyDic, ori, rep, comb_convow, keepList); mobyDic = clnDict(1, mobyDic) 
# 4) replace 'R' with 'r'
ori, rep = 'R', 'r'; mobyDic = repKey(1, mobyDic, ori, rep, comb_convow, keepList); mobyDic = clnDict(1, mobyDic) 
# 5) replace '/[@]/' with '/@/'
ori, rep = '/[@]/', '/@/'; mobyDic = repKey(1, mobyDic, ori, rep, comb_convow, keepList); mobyDic = clnDict(1, mobyDic) 
# 6) replace '/(@)/' with '/@/'
ori, rep = '/(@)/', '/@/'; mobyDic = repKey(1, mobyDic, ori, rep, comb_convow, keepList); mobyDic = clnDict(1, mobyDic) 
# 7) remove keys with '/x/'
resDic = Dictlist()
for key in mobyDic.keys():
    if key.find('/x/') == -1:
        if len(mobyDic[key][0]) == 1: resDic[key] = mobyDic[key][0][0]
        else:
            for i in range(len(mobyDic[key][0])):
                resDic[key] = mobyDic[key][0][i]
saveDict('./mobypron_dictRep.csv', resDic)


# Step 2: get Moby and select pronunciations having maximum vowMax vowels
mobyDic = readDict('./mobypron_dictRep.csv') # get Moby database
vowMax = 1  # maximum number of vowels in one word;
subDic = Dictlist()
cur = 0 # current pron index in Moby
for pron in mobyDic.keys():
    cur += 1; print "Cur: ", cur
    if noOccur(pron, vows) <= vowMax:
        subDic[pron] = mobyDic[pron][0]
resDic = Dictlist()
for key in subDic.keys():
    if len(subDic[key][0]) == 1: resDic[key] = subDic[key][0][0]
    else:
        for i in range(len(subDic[key][0])):
            resDic[key] = subDic[key][0][i]
saveDict('./subMobypron_dict.csv', resDic)            


# Step 3: get Coca and select words having maximum number of letters
# ver 1: using coca_lem_sum_dict.csv
cocaDic = readDict('./coca_lem_sum_dict.csv') # get Coca database
lettMax = 8 # maximum number of letters in a word;
subDic = Dictlist()
cur = 0 # current word index in Coca
for word in cocaDic.keys():
    cur += 1; print "Cur: ", cur 
    if len(word) <= lettMax and word.find("_") == -1 and word.find("-") == -1 and word.find("'") == -1:
        # do not consider words with "_", "-", or "'"
        subDic[word] = cocaDic[word][0]
resDic = Dictlist()
for key in subDic.keys():
    if len(subDic[key][0]) == 1: resDic[key] = subDic[key][0][0]
    else:
        for i in range(len(subDic[key][0])):
            resDic[key] = subDic[key][0][i]
saveDict("./subCoca_dict1.csv", resDic)
# ver 2: using coca_lem_dict.csv
cocaDic = readDict('./coca_lem_dict.csv') # get Coca database
lettMax = 8 # maximum number of letters in a word;
subDic = Dictlist()
cur = 0 # current word index in Coca
for word in cocaDic.keys():
    cur += 1; print "Cur: ", cur 
    if len(word) <= lettMax and word.find("_") == -1 and word.find("-") == -1 and word.find("'") == -1:
        # do not consider words with "_", "-", or "'"
        wordList = cocaDic[word][0]
        newwordList = list()
        for i in range(0,len(wordList),3):
            if len(wordList[i]) <= lettMax and wordList[i].find("_") == -1 and wordList[i].find("'") == -1:
                newwordList.append(wordList[i]) # get word form
                newwordList.append(wordList[i+1])   # get word class
                newwordList.append(wordList[i+2])   # get word frequency
        subDic[word] = newwordList
resDic = Dictlist()
for key in subDic.keys():
    if len(subDic[key][0]) == 1: resDic[key] = subDic[key][0][0]
    else:
        for i in range(len(subDic[key][0])):
            resDic[key] = subDic[key][0][i]
saveDict("./subCoca_dict2.csv", resDic)


# Step 4: replace pron
# Based on Benchmark (Harm & Seidenberg 1999)
repDict = {'/tS/': 'C', '/dZ/': 'J', '/S/': 'S', '/T/': 'T', '/D/': 'D', '/Z/': 'B', '/N/': 'G', # for consonants
           '/i/': 'i', '/I/': 'I', '/E/': 'E', '/&/': '@', '/A/': 'a', '/O/': 'a', '/(@)/': 'E', '/oU/': 'o', '/U/': 'U', '/u/': 'u', '/@/': '^'} # for vowels
# note that for '/@/', if it is followed by a 'r', it will be replaced by 'a'
splitDict = { '/eI/': 'e/j', '/aI/': 'a/j', '/Oi/': 'o/j', '/AU/': 'a/w'} 
mergeDict = {}
newcons = ['p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 'T', 'D', 's', 'z', 'h', 'S', 
           'B', 'C', 'J', 'm', 'n', 'G', 'r', 'l', 'w', 'j'] # 24
newvows = ['i', 'I', 'E', '@', '^', 'o', 'U', 'u', 'e', 'a'] # 10
vowlett = ['a', 'e', 'i', 'o', 'u', 'y']

phonMax = 7 # maximum number of phonemes in a word;
lettMax = 8 # maximum number of letters in a word;

# ver 1: using subCoca_dict1.csv
# get Moby and subCoca for further checking the pronunciation
mobyDic = readDict('./submobypron_dict.csv') # get Moby database
cocaDic = readDict('./subCoca_dict1.csv') # get Coca database
# replace pron of extracted words

resDF = pd.DataFrame()
cur, findNo = 0, 0 # current word in cocaDic
# check whether there is pronunciation for this word in Moby
for word in cocaDic.keys():
    cur += 1; print "Cur: ", cur
    for pron in mobyDic.keys():
        (phonList, lenList) = rep_split_merge_Phon(pron, comb_convow, repDict, splitDict, mergeDict)
        if lenList <= phonMax:
            (newpron, succpron) = getNewPron(phonList, lenList, newvows, phonMax)            
            if succpron:
                wordList = mobyDic[pron][0]
                # remove words with '/' in it                    
                newwordList = list()
                for candword in wordList:
                    newwordList.append(candword.split("/")[0])
                if word in newwordList: # add to extrated word list
                    (newform, succform) = getNewWord(word, vowlett, lettMax)
                    if succform:
                        SumFreq = sumfreq(cocaDic[word][0]) # get summed frequency
                        resDF = resDF.append({'sum_freq': SumFreq, 'wordform': word, 'Moby_pron': pron, 
                                              'Rep_P': newpron, 'Rep_O': newform}, ignore_index=True)
                        findNo += 1; print "No. found: ", findNo                
                        break

resDF['norm_freq'] = resDF['sum_freq']/450.0 # frequency per million
resDF['log_freq'] = np.log(resDF['norm_freq']+1)/np.log(max(resDF['norm_freq']))
resDF.loc[(resDF['log_freq'] < 0.05), 'log_freq'] = 0.05
resDF = resDF[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
resDF.to_csv('./extwords1_HarmSeidenberg1999.csv', index=False)

# ver 2: using subCoca_dict2.csv
# get Moby and subCoca for further checking the pronunciation
mobyDic = readDict('./submobypron_dict.csv') # get Moby database
cocaDic = readDict('./subCoca_dict2.csv') # get Coca database

resDF = pd.DataFrame()
cur, findNo = 0, 0 # current word in cocaDic
# check whether there is pronunciation for this word in Moby
for rootword in cocaDic.keys():
    cur += 1; print "Cur: ", cur
    curvalue = cocaDic[rootword][0]
    for i in range(0, len(curvalue), 3):
        word = curvalue[i]
        for pron in mobyDic.keys():
            (phonList, lenList) = rep_split_merge_Phon(pron, comb_convow, repDict, splitDict, mergeDict)
            if lenList <= phonMax:
                (newpron, succpron) = getNewPron(phonList, lenList, newvows, phonMax)            
                if succpron:
                    wordList = mobyDic[pron][0]
                    # remove words with '/' in it                    
                    newwordList = list()
                    for candword in wordList:
                        newwordList.append(candword.split("/")[0])
                    if word in newwordList: # add to extrated word list
                        (newform, succform) = getNewWord(word, vowlett, lettMax)
                        if succform:
                            resDF = resDF.append({'word_class': curvalue[i+1], 'sum_freq': np.float64(curvalue[i+2]), 
                                                  'wordform': word, 'Moby_pron': pron, 
                                                  'Rep_P': newpron, 'Rep_O': newform}, ignore_index=True)
                            findNo += 1; print "No. found: ", findNo                
                            break

# merge resDF having the same Moby_pron, Rep_O and Rep_P by adding frequency values
# 'ap' : appge; 'at': at, at1; 'jj': jj, jjr, jjt; 'nn': nn1, nn2; 'rr': rr, rrr, rrt; 
# 'vb': vb0, vbdr, vbdz, vbg, vbm, vbn, vbr, vbz; 'vd': vd0, vdd, vdg, vdn, vdz;
# 'vh': vh0, vhd, vhg, vhn, vhz; 'vm': vm, vmk; 'vv': vv, vv0, vvd, vvg, vvgk, vvn, vvz
classlist = ['ap','at','cc','cs','da','db','dd','ex','ge','ii','jj','mc','md',
             'mf','nn','pn','pp','rr','to','uh','vb','vd','vh','vm','vv','xx']
# classlist will not be used, but we only use the first two characters of a word class marker to distinguish and merge similar classes!

newresDF = pd.DataFrame()
findNo = 0 # number of merged wordforms
for i in range(len(resDF)):
    if resDF.wordform[i] != '_': # not masked, it means this is a new item
        print "Cur: ", i+1
        curwordform = resDF.wordform[i]; curpron = resDF.Moby_pron[i]
        curclassID = resDF.word_class[i][0:2]
        curP = resDF.Rep_P[i]; curO = resDF.Rep_O[i]
        sumFreq = resDF.sum_freq[i]
        for j in range(len(resDF)):
            if j != i and curwordform == resDF.wordform[j] and curpron == resDF.Moby_pron[j] and curclassID == resDF.word_class[j][0:2]:
                sumFreq += resDF.sum_freq[j] # sum up the frequency
                resDF.loc[j,'wordform'] = '_' # mask the item in case it is chosen again!
        # after searching all data frame for items in the similar classes, add it
        newresDF = newresDF.append({'word_class': curclassID, 'sum_freq': np.float64(sumFreq), 
                                    'wordform': curwordform, 'Moby_pron': curpron, 
                                    'Rep_P': curP, 'Rep_O': curO}, ignore_index=True)
        findNo += 1; print "No. found: ", findNo  

newresDF['norm_freq'] = newresDF['sum_freq']/450.0 # frequency per million
newresDF['log_freq'] = np.log(newresDF['norm_freq']+1)/np.log(max(newresDF['norm_freq']))
newresDF.loc[(newresDF['log_freq'] < 0.05), 'log_freq'] = 0.05
newresDF = newresDF[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
newresDF.to_csv('./extwords2_HarmSeidenberg1999.csv', index=False)

newresDF2 = pd.DataFrame()
findNo = 0
for i in range(len(newresDF)):
    if newresDF.wordform[i] != '_': # not masked, it means this is a new item
        print "Cur: ", i+1
        curwordform = newresDF.wordform[i]; curpron = newresDF.Moby_pron[i]
        curP = newresDF.Rep_P[i]; curO = newresDF.Rep_O[i]
        sumFreq = newresDF.sum_freq[i]
        for j in range(len(newresDF)):
            if j != i and curwordform == newresDF.wordform[j] and curpron == newresDF.Moby_pron[j]:
                sumFreq += newresDF.sum_freq[j] # sum up the frequency
                newresDF.loc[j,'wordform'] = '_' # mask the item in case it is chosen again!
        # after searching all data frame for items in the similar classes, add it
        newresDF2 = newresDF2.append({'sum_freq': np.float64(sumFreq), 
                                    'wordform': curwordform, 'Moby_pron': curpron, 
                                    'Rep_P': curP, 'Rep_O': curO}, ignore_index=True)
        findNo += 1; print "No. found: ", findNo  

newresDF2['norm_freq'] = newresDF2['sum_freq']/450.0 # frequency per million
newresDF2['log_freq'] = np.log(newresDF2['norm_freq']+1)/np.log(max(newresDF2['norm_freq']))
newresDF2.loc[(newresDF2['log_freq'] < 0.05), 'log_freq'] = 0.05
newresDF2 = newresDF2[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
newresDF2.to_csv('./extwords3_HarmSeidenberg1999.csv', index=False)


# Based on Harm (1998)
# get Moby and subCoca for further checking the pronunciation
repDict = {'/tS/': 'C', '/dZ/': 'J', '/S/': 'S', '/T/': 'T', '/D/': 'D', '/Z/': 'B', '/N/': 'G', # for consonants
           '/i/': 'i', '/I/': 'I', '/E/': 'E', '/&/': '@', '/A/': 'a', '/O/': 'a', '/(@)/': 'E', '/oU/': 'o', '/U/': 'U', '/u/': 'u', '/@/': '^', # for vowles
           '/eI/': 'e', '/aI/': 'Y', '/AU/': 'A', '/Oi/': 'O'} # for vowels
# note that for '/@/', if it is followed by a 'r', it will be replaced by 'a'
splitDict = {}
mergeDict = {'ju': 'W'} 
newcons = ['p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 'T', 'D', 's', 'z', 'h', 'S', 
           'B', 'C', 'J', 'm', 'n', 'G', 'r', 'l', 'w', 'j'] # 24
newvows = ['i', 'I', 'E', '@', '^', 'o', 'U', 'u', 'e', 'a', 'W', 'Y', 'A', 'O'] # 14
vowlett = ['a', 'e', 'i', 'o', 'u', 'y']

phonMax = 7 # maximum number of phonemes in a word;
lettMax = 8 # maximum number of letters in a word;

# ver 1: use subCoca_dict1.csv
# get Moby and subCoca for further checking the pronunciation
mobyDic = readDict('./submobypron_dict.csv') # get Moby database
cocaDic = readDict('./subCoca_dict1.csv') # get Coca database
# replace pron of extracted words

resDF = pd.DataFrame()
cur, findNo = 0, 0 # current word in cocaDic
# check whether there is pronunciation for this word in Moby
for word in cocaDic.keys():
    cur += 1; print "Cur: ", cur
    for pron in mobyDic.keys():
        (phonList, lenList) = rep_split_merge_Phon(pron, comb_convow, repDict, splitDict, mergeDict)
        if lenList <= phonMax:
            (newpron, succpron) = getNewPron(phonList, lenList, newvows, phonMax)            
            if succpron:
                wordList = mobyDic[pron][0]
                # remove words with '/' in it                    
                newwordList = list()
                for candword in wordList:
                    newwordList.append(candword.split("/")[0])
                if word in newwordList: # add to extrated word list
                    (newform, succform) = getNewWord(word, vowlett, lettMax)
                    if succform:
                        SumFreq = sumfreq(cocaDic[word][0]) # get summed frequency
                        resDF = resDF.append({'sum_freq': SumFreq, 'wordform': word, 'Moby_pron': pron, 
                                              'Rep_P': newpron, 'Rep_O': newform}, ignore_index=True)
                        findNo += 1; print "No. found: ", findNo                
                        break

resDF['norm_freq'] = resDF['sum_freq']/450.0 # frequency per million
resDF['log_freq'] = np.log(resDF['norm_freq']+1)/np.log(max(resDF['norm_freq']))
resDF.loc[(resDF['log_freq'] < 0.05), 'log_freq'] = 0.05
resDF = resDF[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
resDF.to_csv('./extwords1_Harm1998.csv', index=False)

# ver 2: use subCoca_dict2.csv
# get Moby and subCoca for further checking the pronunciation
mobyDic = readDict('./submobypron_dict.csv') # get Moby database
cocaDic = readDict('./subCoca_dict2.csv') # get Coca database

resDF = pd.DataFrame()
cur, findNo = 0, 0 # current word in cocaDic
# check whether there is pronunciation for this word in Moby
for rootword in cocaDic.keys():
    cur += 1; print "Cur: ", cur
    curvalue = cocaDic[rootword][0]
    for i in range(0, len(curvalue), 3):
        word = curvalue[i]
        for pron in mobyDic.keys():
            (phonList, lenList) = rep_split_merge_Phon(pron, comb_convow, repDict, splitDict, mergeDict)
            if lenList <= phonMax:
                (newpron, succpron) = getNewPron(phonList, lenList, newvows, phonMax)            
                if succpron:
                    wordList = mobyDic[pron][0]
                    # remove words with '/' in it                    
                    newwordList = list()
                    for candword in wordList:
                        newwordList.append(candword.split("/")[0])
                    if word in newwordList: # add to extrated word list
                        (newform, succform) = getNewWord(word, vowlett, lettMax)
                        if succform:
                            resDF = resDF.append({'word_class': curvalue[i+1], 'sum_freq': np.float64(curvalue[i+2]), 
                                                  'wordform': word, 'Moby_pron': pron, 
                                                  'Rep_P': newpron, 'Rep_O': newform}, ignore_index=True)
                            findNo += 1; print "No. found: ", findNo                
                            break

# merge resDF having the same Moby_pron, Rep_O and Rep_P by adding frequency values
# 'ap' : appge; 'at': at, at1; 'jj': jj, jjr, jjt; 'nn': nn1, nn2; 'rr': rr, rrr, rrt; 
# 'vb': vb0, vbdr, vbdz, vbg, vbm, vbn, vbr, vbz; 'vd': vd0, vdd, vdg, vdn, vdz;
# 'vh': vh0, vhd, vhg, vhn, vhz; 'vm': vm, vmk; 'vv': vv, vv0, vvd, vvg, vvgk, vvn, vvz
classlist = ['ap','at','cc','cs','da','db','dd','ex','ge','ii','jj','mc','md',
             'mf','nn','pn','pp','rr','to','uh','vb','vd','vh','vm','vv','xx']
# classlist will not be used, but we only use the first two characters of a word class marker to distinguish and merge similar classes!

newresDF = pd.DataFrame()
findNo = 0 # number of merged wordforms
for i in range(len(resDF)):
    if resDF.wordform[i] != '_': # not masked, it means this is a new item
        print "Cur: ", i+1
        curwordform = resDF.wordform[i]; curpron = resDF.Moby_pron[i]
        curclassID = resDF.word_class[i][0:2]
        curP = resDF.Rep_P[i]; curO = resDF.Rep_O[i]
        sumFreq = resDF.sum_freq[i]
        for j in range(len(resDF)):
            if j != i and curwordform == resDF.wordform[j] and curpron == resDF.Moby_pron[j] and curclassID == resDF.word_class[j][0:2]:
                sumFreq += resDF.sum_freq[j] # sum up the frequency
                resDF.loc[j,'wordform'] = '_' # mask the item in case it is chosen again!
        # after searching all data frame for items in the similar classes, add it
        newresDF = newresDF.append({'word_class': curclassID, 'sum_freq': np.float64(sumFreq), 
                                    'wordform': curwordform, 'Moby_pron': curpron, 
                                    'Rep_P': curP, 'Rep_O': curO}, ignore_index=True)
        findNo += 1; print "No. found: ", findNo  

newresDF['norm_freq'] = newresDF['sum_freq']/450.0 # frequency per million
newresDF['log_freq'] = np.log(newresDF['norm_freq']+1)/np.log(max(newresDF['norm_freq']))
newresDF.loc[(newresDF['log_freq'] < 0.05), 'log_freq'] = 0.05
newresDF = newresDF[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
newresDF.to_csv('./extwords2_Harm1998.csv', index=False)

newresDF2 = pd.DataFrame()
findNo = 0
for i in range(len(newresDF)):
    if newresDF.wordform[i] != '_': # not masked, it means this is a new item
        print "Cur: ", i+1
        curwordform = newresDF.wordform[i]; curpron = newresDF.Moby_pron[i]
        curP = newresDF.Rep_P[i]; curO = newresDF.Rep_O[i]
        sumFreq = newresDF.sum_freq[i]
        for j in range(len(newresDF)):
            if j != i and curwordform == newresDF.wordform[j] and curpron == newresDF.Moby_pron[j]:
                sumFreq += newresDF.sum_freq[j] # sum up the frequency
                newresDF.loc[j,'wordform'] = '_' # mask the item in case it is chosen again!
        # after searching all data frame for items in the similar classes, add it
        newresDF2 = newresDF2.append({'sum_freq': np.float64(sumFreq), 
                                    'wordform': curwordform, 'Moby_pron': curpron, 
                                    'Rep_P': curP, 'Rep_O': curO}, ignore_index=True)
        findNo += 1; print "No. found: ", findNo  

newresDF2['norm_freq'] = newresDF2['sum_freq']/450.0 # frequency per million
newresDF2['log_freq'] = np.log(newresDF2['norm_freq']+1)/np.log(max(newresDF2['norm_freq']))
newresDF2.loc[(newresDF2['log_freq'] < 0.05), 'log_freq'] = 0.05
newresDF2 = newresDF2[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
newresDF2.to_csv('./extwords3_Harm1998.csv', index=False)



# Step 5: create benchmark training examples using extracted words
# Based on Harm & Seidenberg (1999)
# encoding of phonemes:
phonDF = pd.read_csv('./phon_HarmSeidenberg1999.txt', sep=' ', header=None)
phonDF.columns = ['Symbol', 'Sonorant', 'Consonantal', 'Voice', 'Nasal', 'Degree', 'Labial', 'Palatal', 'Pharyngeal', 'Round', 'Tongue','Radical']
PhonDic = dict()
for i in range(len(phonDF)):
    PhonDic[phonDF.loc[i, 'Symbol']] = list(phonDF.loc[i,phonDF.columns[1:]])
#PhonDic = {
#'p': [-1.0, 1.0, -1.0, -1.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0],  # as p in pat
#'b': [-1.0, 1.0, 0.0, -1.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0],	# as b in bat 
#'t': [-1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0],	# as t in top
#'d': [-1.0, 1.0, 0.0, -1.0, 1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0],	# as d in dog
#'k': [-1.0, 1.0, -1.0, -1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 0.0], # as k in kite
#'g': [-1.0, 1.0, 0.0, -1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 0.0], # as g in give
#'f': [-0.5, 1.0, -1.0, -1.0, 0.0, -1.0, 1.0, -1.0, 1.0, 0.0, 0.0],  # as f in fit
#'v': [-0.5, 1.0, 0.0, -1.0, 0.0, -1.0, 1.0, -1.0, 1.0, 0.0, 0.0],   #	as v in vine
#'T': [-0.5, 1.0, -1.0, -1.0, 0.0, -1.0, 1.0, -1.0, -1.0, 0.0, 0.0], # as th in with
#'D': [-0.5, 1.0, 0.0, -1.0, 0.0, -1.0, 1.0, -1.0, -1.0, 0.0, 0.0],	# as th in the
#'s': [-0.5, 1.0, -1.0, -1.0, 0.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0], #	as s in sit
#'z': [-0.5, 1.0, 0.0, -1.0, 0.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0],	# as z in jazz
#'h': [-0.5, 1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 1.0, -1.0, -1.0, -1.0], # as h in hat
#'S': [-0.5, 1.0, -1.0, -1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, 0.0], #	as sh in shoe
#'B': [-0.5, 1.0, 0.0, -1.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, 0.0], #	as ge in beige
#'C': [-0.8, 1.0, -1.0, -1.0, 1.0, -1.0, 0.0, -1.0, -1.0, 0.0, 0.0], # as tch in catch
#'J': [-0.8, 1.0, 0.0, -1.0, 1.0, -1.0, 0.0, -1.0, -1.0, 0.0, 0.0], # as g in gin
#'m': [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0, 0.0], # as m in mop
#'n': [0.0, 0.0, 1.0, 1.0, 1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0], # as n in nice
#'G': [0.0, 0.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0, -1.0, 0.0], # as ing in sing
#'r': [0.5, 0.0, 1.0, 0.0, -1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0],	# as r in rat
#'l': [0.5, 0.0, 1.0, 0.0, -1.0, -1.0, 1.0, -1.0, -1.0, 1.0, 0.0], # as l in loop
#'w': [0.8, 0.0, 1.0, 0.0, 0.0, 1.0, -1.0, -1.0, 1.0, -1.0, 0.0], #	as w in win
#'j': [0.8, 0.0, 1.0, 0.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, 1.0], #	as y in yes
#
#'i': [1.0, -1.0, 1.0, 0.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, 1.0], # as ee in heed
#'I': [1.0, -1.0, 1.0, 0.0, 0.0, -1.0, 0.0, -1.0, -1.0, 0.0, -1.0], # as i in hid
#'E': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, 0.0, -1.0, -1.0, -1.0, 1.0], # as ai in paid
#'e': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, 0.0, -1.0, -1.0, -1.0, -1.0], # as ea in head
#'@': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, 0.0, 1.0, -1.0, -1.0, 1.0], # as a in hat
#'a': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, -1.0, 1.0, -1.0, -1.0, -1.0], # as o in hot
#'x': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, -1.0, -1.0, 1.0, -1.0, -1.0], # as a in paw (this one is not used!)
#'o': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, -1.0, -1.0, 1.0, -1.0, 1.0], # as oa in toad
#'U': [1.0, -1.0, 1.0, 0.0, 0.0, -1.0, -1.0, -1.0, 1.0, 0.0, -1.0], # as oo in took
#'u': [1.0, -1.0, 1.0, 0.0, 0.0, -1.0, -1.0, -1.0, 1.0, 0.0, 1.0], # as oo in boot
#'^': [1.0, -1.0, 1.0, 0.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0, -1.0], # as u in hut
#
#'_': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]    # filler
#}

# create LettDic
import string
cand = string.ascii_lowercase
lettList = len(cand)*[0]
LettDic = dict()
for i in range(len(cand)):
    lettListCopy = lettList[:]
    lettListCopy[i] = 1     
    LettDic[cand[i]] = lettListCopy
LettDic['_'] = lettList # filler

# write to example file
# ver 1: using extwords1_HarmSeidenberg1999.csv
wordDF = pd.read_csv('./extwords1_HarmSeidenberg1999.csv')
writeExp_OtoP(wordDF, './TrEm1_HarmSeidenberg1999.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm1_PtoP_HarmSeidenberg1999.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)
# ver 2: using extwords2_HarmSeidenberg1999.csv
wordDF = pd.read_csv('./extwords2_HarmSeidenberg1999.csv')
writeExp_OtoP(wordDF, './TrEm2_HarmSeidenberg1999.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm2_PtoP_HarmSeidenberg1999.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)
# ver 3: using extwords3_HarmSeidenberg1999.csv
wordDF = pd.read_csv('./extwords3_HarmSeidenberg1999.csv')
writeExp_OtoP(wordDF, './TrEm3_HarmSeidenberg1999.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm3_PtoP_HarmSeidenberg1999.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)
    
# Based on Harm (1998)
# encoding of phonemes:
phonDF = pd.read_csv('./phon_Harm1998.txt', sep=' ', header=None)
phonDF.columns = ['Symbol', 'Labial', 'Dental', 'Alveolar', 'Palatal', 'Velar', 
                  'Glottal' ,'Stop', 'Fricative', 'Affricate', 'Nasal', 'Liquid', 
                  'Glide', 'Voice', 'Front', 'Center', 'Back', 'High', 'Mid', 'Low', 
                  'Tense', 'Retroflex', 'Round', 'Pre y', 'Post y', 'Post w']
PhonDic = dict()
for i in range(len(phonDF)):
    PhonDic[phonDF.loc[i, 'Symbol']] = list(phonDF.loc[i,phonDF.columns[1:]])
#PhonDic = {
#'p': [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],  # as p in pat
#'b': [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],	# as b in bat 
#'t': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],	# as t in top
#'d': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],	# as d in dog
#'k': [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as k in kite
#'g': [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as g in give
#'f': [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],  # as f in fit
#'v': [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],   #	as v in vine
#'T': [0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as th in with
#'D': [0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],	# as th in the
#'s': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], #	as s in sit
#'z': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],	# as z in jazz
#'h': [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as h in hat
#'S': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], #	as sh in shoe
#'B': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], #	as ge in beige
#'C': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as tch in catch
#'J': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as g in gin
#'m': [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as m in mop
#'n': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as n in nice
#'G': [0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as ing in sing
#'r': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0],	# as r in rat
#'l': [0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as l in loop
#'w': [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], #	as w in win
#'j': [0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], #	as y in yes
#
#'i': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as ee in heed
#'I': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as i in hid
#'E': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as ai in paid
#'e': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as ea in head
#'@': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as a in hat
#'a': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as o in hot
#'o': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0], # as oa in toad
#'U': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0], # as oo in took
#'u': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0], # as oo in boot
#'^': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0], # as u in hut
#
#'W': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 1.0, 0.0, 0.0], # as ew in hew
#'M': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0], # as ie in die
#'P': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0], # as ow in how
#'Q': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0], # as oy in boy
#
#'_': [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]    # filler
#}
# create LettDic
import string
cand = string.ascii_lowercase
lettList = len(cand)*[0]
LettDic = dict()
for i in range(len(cand)):
    lettListCopy = lettList[:]
    lettListCopy[i] = 1     
    LettDic[cand[i]] = lettListCopy
LettDic['_'] = lettList # filler

# write to example file
# ver 1: using extwords1_Harm1998.csv
wordDF = pd.read_csv('./extwords1_Harm1998.csv')
writeExp_OtoP(wordDF, './TrEm1_Harm1998.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm1_PtoP_Harm1998.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)
# ver 2: using extwords2_Harm1998.csv
wordDF = pd.read_csv('./extwords2_Harm1998.csv')
writeExp_OtoP(wordDF, './TrEm2_Harm1998.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm2_PtoP_Harm1998.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)
# ver 3: using extwords3_Harm1998.csv
wordDF = pd.read_csv('./extwords3_Harm1998.csv')
writeExp_OtoP(wordDF, './TrEm3_Harm1998.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm3_PtoP_Harm1998.txt', 'log_freq', 5, 0, 0, 2, 4, PhonDic)


# create test word examples, based on Harm (1998):
newcons = ['p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 'T', 'D', 's', 'z', 'h', 'S', 
           'B', 'C', 'J', 'm', 'n', 'G', 'r', 'l', 'w', 'j'] # 24
newvows = ['i', 'I', 'E', '@', '^', 'o', 'U', 'u', 'e', 'a', 'W', 'Y', 'A', 'O'] # 14
vowlett = ['a', 'e', 'i', 'o', 'u', 'y']
phonMax = 7 # maximum number of phonemes in a word;
lettMax = 8 # maximum number of letters in a word;

phonDF = pd.read_csv('./phon_Harm1998.txt', sep=' ', header=None)
phonDF.columns = ['Symbol', 'Labial', 'Dental', 'Alveolar', 'Palatal', 'Velar', 
                  'Glottal' ,'Stop', 'Fricative', 'Affricate', 'Nasal', 'Liquid', 
                  'Glide', 'Voice', 'Front', 'Center', 'Back', 'High', 'Mid', 'Low', 
                  'Tense', 'Retroflex', 'Round', 'Pre y', 'Post y', 'Post w']
PhonDic = dict()
for i in range(len(phonDF)):
    PhonDic[phonDF.loc[i, 'Symbol']] = list(phonDF.loc[i,phonDF.columns[1:]])

import string
cand = string.ascii_lowercase
lettList = len(cand)*[0]
LettDic = dict()
for i in range(len(cand)):
    lettListCopy = lettList[:]
    lettListCopy[i] = 1     
    LettDic[cand[i]] = lettListCopy
LettDic['_'] = lettList # filler

# using nonwords from Treiman-etal-1999 Appendix
wordDF = pd.read_csv('./Treiman-etal-1990-Appendix.csv')
# first, create phonological and orthographical representations for words in wordDF
wordDF['log_freq'] = 0.05; wordDF['Rep_P'] = ''; wordDF['Rep_O'] = ''
for i in range(len(wordDF)):
    word = wordDF.wordform[i]; pron = wordDF.pron[i]
    (phonList, lenList) = rep_split_merge_Phon(pron, newcons+newvows, {}, {}, {})
    (newpron, succ) = getNewPron(phonList, len(word), newvows, phonMax)
    (newform, succform) = getNewWord(word, vowlett, lettMax)
    if succ == True and succform == True: 
        wordDF.loc[i,'Rep_P'] = newpron; wordDF.loc[i,'Rep_O'] = newform
# store modified examples
wordDF.to_csv('./Treiman-etal-1990-Appendix.csv', index=False)
# second, write up example files
writeExp_OtoP(wordDF, './Te.txt', 'log_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_OtoP(wordDF, './Te_PtoP.txt', 'log_freq', 7, 0, 0, 2, 4, PhonDic)



# hand-corrected extwords3 to form extwords3_correct
wordDF = pd.read_csv('./extwords3_Harm1998_correct.csv')
wordDF['norm_freq'] = wordDF['sum_freq']/450.0 # frequency per million
wordDF['log_freq'] = np.log(wordDF['norm_freq']+1)/np.log(max(wordDF['norm_freq']))
wordDF.loc[(wordDF['log_freq'] < 0.05), 'log_freq'] = 0.05
wordDF['sqrt_freq'] = wordDF['sum_freq']
wordDF.loc[(wordDF['sqrt_freq'] > 1000000), 'sqrt_freq'] = 1000000
wordDF['sqrt_freq'] = np.sqrt(wordDF['sqrt_freq'])
wordDF = wordDF[['wordform', 'sum_freq', 'norm_freq', 'log_freq', 'sqrt_freq', 'Moby_pron', 'Rep_P', 'Rep_O']]
wordDF.to_csv('./extwords4_Harm1998.csv', index=False)
# hand-correct the words: "true" and "false", and "nan"

# create new Harm1998 training and testing examples
newcons = ['p', 'b', 't', 'd', 'k', 'g', 'f', 'v', 'T', 'D', 's', 'z', 'h', 'S', 
           'B', 'C', 'J', 'm', 'n', 'G', 'r', 'l', 'w', 'j'] # 24
newvows = ['i', 'I', 'E', '@', '^', 'o', 'U', 'u', 'e', 'a', 'W', 'Y', 'A', 'O'] # 14
vowlett = ['a', 'e', 'i', 'o', 'u', 'y']
phonMax = 7 # maximum number of phonemes in a word;
lettMax = 8 # maximum number of letters in a word;

phonDF = pd.read_csv('./phon_Harm1998.txt', sep=' ', header=None)
phonDF.columns = ['Symbol', 'Labial', 'Dental', 'Alveolar', 'Palatal', 'Velar', 
                  'Glottal' ,'Stop', 'Fricative', 'Affricate', 'Nasal', 'Liquid', 
                  'Glide', 'Voice', 'Front', 'Center', 'Back', 'High', 'Mid', 'Low', 
                  'Tense', 'Retroflex', 'Round', 'Pre y', 'Post y', 'Post w']
PhonDic = dict()
for i in range(len(phonDF)):
    PhonDic[phonDF.loc[i, 'Symbol']] = list(phonDF.loc[i,phonDF.columns[1:]])

import string
cand = string.ascii_lowercase
lettList = len(cand)*[0]
LettDic = dict()
for i in range(len(cand)):
    lettListCopy = lettList[:]
    lettListCopy[i] = 1     
    LettDic[cand[i]] = lettListCopy
LettDic['_'] = lettList # filler

# create realwords training and testing data
wordDF = pd.read_csv('./extwords4_Harm1998.csv')
writeExp_OtoP(wordDF, './TrEm4_Harm1998.txt', 'sqrt_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(wordDF, './TrEm4_PtoP_Harm1998.txt', 'sqrt_freq', 7, 0, 0, 2, 4, PhonDic)

# create nonwords testing data
nonwordDF = pd.read_csv('./Treiman-etal-1990-Appendix.csv')
nonwordDF['sqrt_freq'] = 100.0
nonwordDF.to_csv('./Treiman-etal-1990-Appendix.csv', index=False)
writeExp_OtoP(nonwordDF, './Te2.txt', 'sqrt_freq', 7, 0, 6, 4, 6, PhonDic, LettDic)
writeExp_PtoP(nonwordDF, './Te2_PtoP.txt', 'sqrt_freq', 7, 0, 0, 2, 4, PhonDic)
