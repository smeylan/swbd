
#Switchboard extraction
rm(list=ls())
#setwd() to script location
library('reshape')
library('lattice')
library('matlab')
library('lme4')
library('lmerTest')
library('koRpus')
library('ggplot2')
library('gridExtra')
library('Hmisc')
library('Rmisc')
Sys.setenv(PATH=paste(Sys.getenv("PATH"),"/usr/texbin",sep=":"))

source('~/Documents/R/sfunctions.R')
source('swbdLexDivHelper.R')

corpusDirectory = '~/Documents/corpora/Switchboard/'
auxCorpusDirectory = "~/Documents/corpora/Switchboard_aux_data/"

#Word table
# find . -name '*word.text' -exec cat {} \; > switchboardWords.txt
swbd = read.table(paste(corpusDirectory,'switchboardWords.txt', sep=''), stringsAsFactors=F)
names(swbd) <- c("File", "Start", "End", "Word")
swbd$ConvNumber <- as.numeric(substring(swbd$File, 3,6))
swbd$Role <- substring(swbd$File, 7,7) # Caller or callee, i.e. A or B
swbd$UttrNo <- as.numeric(substring(swbd$File, 16,20))

#Conversation Table
conv <- read.csv(paste(auxCorpusDirectory, 'conv.tab', sep=''), F, stringsAsFactors=F)[,1:6]
colnames(conv) <- c("ConvNumber", "Active", "SpeakerA", "SpeakerB", "TopicNo", "TalkDay")
conv = melt(conv, id.vars=c('ConvNumber','Active','TopicNo','TalkDay'))
names(conv)[5:6] = c('Speaker','SubjectNo')
conv$Speaker = gsub('Speaker','',as.character(conv$Speaker))
length(unique(conv$ConvNumber))

#Caller table
caller <- read.csv(paste(auxCorpusDirectory,"caller.tab", sep=''), F, stringsAsFactors=F)[,1:7]
colnames(caller) <- c("Caller", "Unknown1", "Unknown2", "Sex", "BirthYear", "Dialect", "Education")

#Remove annotations and silences from the word table, make lower case
d = merge(swbd, conv, by.x=c('ConvNumber','Role'), by.y=c('ConvNumber','Speaker'))
originalTokens = nrow(d)
d$Word = as.character(d$Word)
d = d[-grep('\\[',d$Word),]
silencesRemoved = nrow(d)
d$Word = tolower(d$Word)
d$Word  = gsub('[{}]','',d$Word)

#which words are the most commonly shared across everyone
d.agg = aggregate(TopicNo ~ ConvNumber + SubjectNo + Word ,d,  length)
d.conv.sub = aggregate(TopicNo ~ Word, d.agg, length)
names(d.conv.sub)[2] = 'count'
mostCommonlyShared = d.conv.sub[order(d.conv.sub$count, decreasing=T),][1:100,]

#remove function words at this point
fd= subset(d, Word %in% functionWords)
d = subset(d, !(Word %in% functionWords))
fwRemoved= nrow(d)
numFw = silencesRemoved - fwRemoved
dLength = aggregate(ConvNumber ~ Word, d, length)
names(dLength)[2] = 'Count'
dLength = dLength[order(dLength$Count, decreasing=T),]
length(unique(paste(d$ConvNumber, d$SubjectNo)))

#calculate the Uber index. Do not split(d) -- resulting data frames take too much memory
swbdU = aggregate(Word ~ ConvNumber + SubjectNo, d, uberIndex)
names(swbdU)[3] = 'lexDiv'
swbdU = merge(swbdU, caller, by.x='SubjectNo', by.y='Caller')
d.paste = aggregate(Word ~ ConvNumber + SubjectNo, d, function(x){paste(x, collapse=' ')})
lexdivs = do.call('rbind',mclapply(d.paste$Word, getLexDiv))
d.paste$MTLD = lexdivs$MTLD
d.paste$K = lexdivs$K
d.paste$I = 1/d.paste$K
swbdU = merge(swbdU, d.paste[,c('ConvNumber','SubjectNo','MTLD','K','I')])

swbdU$Age = 1988 - swbdU$BirthYear
wordCounts = aggregate(Word ~ ConvNumber + SubjectNo + TopicNo, d, length)
swbdU = merge(swbdU, wordCounts, by=c('ConvNumber','SubjectNo'))
names(swbdU)[length(names(swbdU))] = 'wordCount'

#merge speaker-level lexical diversity data back with conversation metadata
convU = merge(conv, swbdU, by=c('ConvNumber','SubjectNo','TopicNo'))

#split into speakers and merge to create dyads
speakersA = subset(convU, Speaker =='A')[,c('ConvNumber','SubjectNo','TopicNo','lexDiv','Sex','BirthYear','Dialect','Education','Age','wordCount')]
names(speakersA) = c('ConvNumber','SubjectNoA','TopicNo','lexDivA','SexA','BirthYearA','DialectA','EducationA','AgeA','wordCountA')
speakersB = subset(convU, Speaker =='B')[,c('ConvNumber','SubjectNo','TopicNo','lexDiv','Sex','BirthYear','Dialect','Education','Age','wordCount')]
names(speakersB) = c('ConvNumber','SubjectNoB','TopicNo','lexDivB','SexB','BirthYearB','DialectB','EducationB','AgeB','wordCountB' )
bothSpeakers = merge(speakersA, speakersB, by=c('ConvNumber','TopicNo')) 

#Pull out a vector of types per speaker per conversation
vsc = aggregate(Word ~ ConvNumber + SubjectNo, d, function(x){unique(setdiff(x, functionWords))})
vsc$ConvNumber = as.numeric(as.character(vsc$ConvNumber))
vsc$SubjectNo = as.numeric(as.character(vsc$SubjectNo))

##### Down-sampling to get rid of sample size asymmetry effects
#get the proportion of conversation-level shared types (Jaccard index)
d.content = d[!(d$Word %in% functionWords),c('ConvNumber','SubjectNo','Word')]

#statistics over d.content
contentCount = aggregate(Word ~ ConvNumber + SubjectNo, d.content, length)
sum(contentCount$Word)
yield = sapply(1:max(contentCount$Word), function(x){x * (length(which(contentCount$Word >= x)))})
numTokens = max(yield)
numTypesToSample = which.max(yield)

numConversations = nrow(subset(bothSpeakers, wordCountA >numTypesToSample & wordCountB >numTypesToSample))

tic()
speakerOverlapRaw =  mclapply(subset(bothSpeakers, wordCountA > numTypesToSample & wordCountB >numTypesToSample)$ConvNumber, function(z){getPropSharedTypesDS(z,100, numTypesToSample)})
toc()

shapiroTest = sapply(speakerOverlapRaw, function(x){shapiro.test(x$pShared)$p.value})
length(which(shapiroTest < .1))

speakerOverlap = do.call('rbind',speakerOverlapRaw)
bso = merge(bothSpeakers, speakerOverlap, by=c('ConvNumber'))
bso.agg = aggregate(cbind(pShared,pAfromB, pBfromA, numAtypes, numBtypes, numTotalTypes) ~ ConvNumber + SubjectNoA + lexDivA + BirthYearA + DialectA + EducationA  + AgeA + wordCountA + SubjectNoB + lexDivB + BirthYearB + DialectB + EducationB   + AgeB + wordCountB + TopicNo +SexA + SexB, bso, mean)


bso.agg$SexA = as.factor(tolower(gsub(' ','',bso.agg$SexA)))
bso.agg$SexB = as.factor(tolower(gsub(' ','',bso.agg$SexB)))
bso.agg$SexAB = as.factor(paste(bso.agg$SexA, bso.agg$SexB, sep='-'))
levels(bso.agg$SexAB)[3] = levels(bso.agg$SexAB)[2]
bso.agg$DialectA = as.factor(bso.agg$DialectA)
bso.agg$DialectB = as.factor(bso.agg$DialectB)

#Turn dyads into longform, bso.s
bso.A = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoA, lexDiv = bso.agg$lexDivA, BirthYear = bso.agg$BirthYearA, Dialect = bso.agg$DialectA,  Education = bso.agg$EducationA, Age= bso.agg$AgeA, Sex= bso.agg$SexA, wordCount = bso.agg$wordCountA, Speaker = 'A', propFromInterlocutor = bso.agg$pAfromB, pShared = bso.agg$pShared, numTypes = bso.agg$numAtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.B = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoB, lexDiv = bso.agg$lexDivB, BirthYear = bso.agg$BirthYearB, Dialect = bso.agg$DialectB, Education = bso.agg$EducationB, Age= bso.agg$AgeB, Sex= bso.agg$SexB, wordCount = bso.agg$wordCountB, Speaker = 'B', propFromInterlocutor = bso.agg$pBfromA, pShared = bso.agg$pShared, numTypes = bso.agg$numBtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.s = rbind(bso.A, bso.B)


As = subset(bso.s, Speaker == 'A')
Bs = subset(bso.s, Speaker == 'B')
names(As)[2:(length(names(As))-1)] = paste('spk', names(As)[2:(length(names(As))-1)], sep='_') 
names(Bs)[2:(length(names(Bs))-1)] = paste('int', names(Bs)[2:(length(names(Bs))-1)], sep='_')
AsWi = merge(As,Bs)

As = subset(bso.s, Speaker == 'A')
Bs = subset(bso.s, Speaker == 'B')
names(Bs)[2:(length(names(Bs))-1)] = paste('spk', names(Bs)[2:(length(names(Bs))-1)], sep='_') 
names(As)[2:(length(names(As))-1)] = paste('int', names(As)[2:(length(names(As))-1)], sep='_')
BsWi = merge(Bs,As)
bso.i = rbind(AsWi, BsWi)

nrow(bso.i) 

if(nrow(bso.i) != nrow(bso.s)){stop('problem with getting interlocutor data')}

if(any(aggregate(spk_speaker ~ ConvNumber, bso.i, function(x){length(unique(x))})$spk_speaker !=2)){stop('all conversations should have two participants')}