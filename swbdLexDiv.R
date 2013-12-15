#Switchboard extraction
rm(list=ls())
#setwd() to script location
library('reshape')
library('stargazer')
library('lattice')
library('matlab')
library('rgl')
library('RColorBrewer')
library('hexbin')
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

#Remove annotations and silences from the word table
d = merge(swbd, conv, by.x=c('ConvNumber','Role'), by.y=c('ConvNumber','Speaker'))
d$Word = as.character(d$Word)
d = d[-grep('\\[',d$Word),]
d$Word = tolower(d$Word)

#calculate the Uber index. Do not split(d) -- resulting data frames take too much memory
swbdU = aggregate(Word ~ ConvNumber + SubjectNo, d, uberIndex)
names(swbdU)[3] = 'lexDiv'
swbdU = merge(swbdU, caller, by.x='SubjectNo', by.y='Caller')

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
bothSpeakers = merge(speakersA, speakersB, by=c('ConvNumber')) 

#Pull out a vector of types per speaker per conversation
vsc = aggregate(Word ~ ConvNumber + SubjectNo, d, function(x){unique(setdiff(x, functionWords))})
vsc$ConvNumber = as.numeric(as.character(vsc$ConvNumber))
vsc$SubjectNo = as.numeric(as.character(vsc$SubjectNo))

#get the proportion of conversation-level shared types (Jaccard index)
propSharedTypes = lapply(1:length(bothSpeakers$ConvNumber), function(x){     
	cnum = as.numeric(as.character(bothSpeakers$ConvNumber[x]))
	SpeakerA = bothSpeakers$SubjectNoA[x]
	SpeakerB = bothSpeakers$SubjectNoB[x]
	
	SpeakerAtypeList = unlist(subset(vsc, ConvNumber == cnum & SubjectNo == SpeakerA)$Word)
	SpeakerBtypeList = unlist(subset(vsc, ConvNumber == cnum & SubjectNo == SpeakerB)$Word)
	numTotalTypes = length(union(SpeakerAtypeList, SpeakerBtypeList))	
	pShared = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / numTotalTypes
	numAtypes= length(SpeakerAtypeList)
	pAfromB = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / numAtypes
	numBtypes= length(SpeakerBtypeList)
	pBfromA = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / length(SpeakerBtypeList)
		
	return(data.frame(ConvNumber=cnum, pShared=pShared, pAfromB=pAfromB,pBfromA=pBfromA, numTotalTypes=numTotalTypes, numAtypes=numAtypes, numBtypes = numBtypes))	
})
propSharedTypes = do.call('rbind', propSharedTypes)
#and rejoin with metadata in bothSpeakers
bothSpeakers = merge(bothSpeakers, propSharedTypes)


##### Down-sampling to get rid of sample size asymmetry effects
d.content = d[!(d$Word %in% functionWords),c('ConvNumber','SubjectNo','Word')]

#statistics over d.content
contentCount = aggregate(Word ~ ConvNumber + SubjectNo, d.content, length)
sum(contentCount$Word)

tic()
speakerOverlap =  mclapply(bothSpeakers$ConvNumber, function(z){getPropSharedTypesDS(z,100)})
toc()
speakerOverlap = do.call('rbind',speakerOverlap)

bso = merge(bothSpeakers, speakerOverlap, by=c('ConvNumber'))
bso$TopicNo.x = NULL
names(bso)[names(bso) == 'TopicNo.y'] = 'TopicNo'
bso.agg = aggregate(cbind(pShared,pAfromB, pBfromA, numAtypes, numBtypes, numTotalTypes) ~ ConvNumber + SubjectNoA + lexDivA + BirthYearA + DialectA + EducationA  + AgeA + wordCountA + SubjectNoB + lexDivB + BirthYearB + DialectB + EducationB   + AgeB + wordCountB + TopicNo, bso, mean)

#merge back in the gender data. SexB throws an error if used as a factor in above aggregate function
bso.agg.Ag = merge(bso.agg, caller[,c('Caller','Sex')], by.x='SubjectNoA', by.y='Caller')
names(bso.agg.Ag)[length(names(bso.agg.Ag))] = 'SexA'
bso.agg.Bg = merge(bso.agg.Ag, caller[,c('Caller','Sex')], by.x='SubjectNoB', by.y='Caller')
names(bso.agg.Bg)[length(names(bso.agg.Bg))] = 'SexB'
bso.agg= bso.agg.Bg
bso.agg$SexA = as.factor(tolower(gsub(' ','',bso.agg$SexA)))
bso.agg$SexB = as.factor(tolower(gsub(' ','',bso.agg$SexB)))
bso.agg$SexAB = as.factor(paste(bso.agg$SexA, bso.agg$SexB, sep='-'))
levels(bso.agg$SexAB)[3] = levels(bso.agg$SexAB)[2]
bso.agg$DialectA = as.factor(bso.agg$DialectA)
bso.agg$DialectB = as.factor(bso.agg$DialectB)
bso.agg$EducationA = as.factor(bso.agg$EducationA)
bso.agg$EducationB = as.factor(bso.agg$EducationB)

#Turn dyads into longform, bso.s
bso.A = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoA, lexDiv = bso.agg$lexDivA, BirthYear = bso.agg$BirthYearA, Dialect = bso.agg$DialectA,  Education = bso.agg$EducationA, Age= bso.agg$AgeA, Sex= bso.agg$SexA, wordCount = bso.agg$wordCountA, Speaker = 'A', propFromInterlocutor = bso.agg$pAfromB, pShared = bso.agg$pShared, numTypes = bso.agg$numAtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.B = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoB, lexDiv = bso.agg$lexDivB, BirthYear = bso.agg$BirthYearB, Dialect = bso.agg$DialectB, Education = bso.agg$EducationB, Age= bso.agg$AgeB, Sex= bso.agg$SexB, wordCount = bso.agg$wordCountB, Speaker = 'B', propFromInterlocutor = bso.agg$pBfromA, pShared = bso.agg$pShared, numTypes = bso.agg$numBtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.s = rbind(bso.A, bso.B)