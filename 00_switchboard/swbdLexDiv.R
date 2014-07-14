rm(list=ls())
source('../shared/sfunctions.R')
source('~/.projconf/lexicalDivergence.R')
ipak(c('reshape','lattice','matlab','lme4','lmerTest','koRpus','ggplot2','gridExtra','Hmisc','Rmisc','matlab','hexbin','multicore'))
isrc(c('swbdLexDivHelper.R','getTablesForSwitchboard.R','plotSwbdLexDiv.R', 'individualRegressions.R','dyadRegressions.R','bootstrapRegressions.R'))

analysisDir = ensureTrailingSlash('output')
currentAnalysisDir = ensureTrailingSlash(paste(analysisDir, gsub('[ :]','\\_',Sys.time()), sep=''))
dir.create(currentAnalysisDir, showWarnings=F)
dir.create(paste(ensureTrailingSlash(currentAnalysisDir), 'graphs', sep=''), showWarnings = F)
dir.create(paste(ensureTrailingSlash(currentAnalysisDir), 'regressions', sep=''), showWarnings = F)
dir.create(paste(ensureTrailingSlash(currentAnalysisDir), 'tables', sep=''), showWarnings = F)

#paths to corpus data and metadata are in config.R
returnList = getTablesForSwitchboard(corpusDir, auxCorpusDir)
d = returnList[[1]]
conv = returnList[[2]]
callers = returnList[[3]]
returnList = NULL

#which words are the most commonly shared across everyone
d.agg = aggregate(TopicNo ~ ConvNumber + SubjectNo + Word ,d,  length)
d.conv.sub = aggregate(TopicNo ~ Word, d.agg, length)
names(d.conv.sub)[2] = 'count'
mostCommonlyShared = d.conv.sub[order(d.conv.sub$count, decreasing=T),][1:100,]

#remove function words at this point; maintain counts
fd= subset(d, Word %in% functionWords)
d = subset(d, !(Word %in% functionWords))
fwRemoved= nrow(fd)
dLength = aggregate(ConvNumber ~ Word, d, length)
names(dLength)[2] = 'Count'
dLength = dLength[order(dLength$Count, decreasing=T),]
numUniqueSubjectConversations = length(unique(paste(d$ConvNumber, d$SubjectNo)))

#get word counts
wordCounts = aggregate(Word ~ ConvNumber + SubjectNo + TopicNo, d, length)

#calculate the Uber index. Do not split(d) -- resulting data frames take too much memory
swbdU = getUberIndex(d, callers)
metricCorrelations = getMetricCorrelation(swbdU)

#merge speaker-level lexical diversity data back with conversation metadata
bothSpeakers = getDyads(conv, swbdU)

##### Down-sampling to get rid of sample size asymmetry effects
#get the proportion of conversation-level shared types (Jaccard index)
d.content = d[!(d$Word %in% functionWords),c('ConvNumber','SubjectNo','Word')]

#statistics over d.content
contentWords = aggregate(Word ~ ConvNumber + SubjectNo, d.content, length)
contentWordCount = sum(contentWords$Word)
yield = sapply(1:max(contentWords$Word), function(x){x * (length(which(contentWords$Word >= x)))})
numTokens = max(yield)
numTokensToSample = which.max(yield)
#Token count per conversation + speaker histogram
pdf(paste(currentAnalysisDir, 'graphs/tokenCountHistogram.pdf', sep=''), width=5, height=5)       
hist(contentWords$Word, breaks=100, col='cyan', xlab='Token Count, Excluding Function Words',main='Tokens Per Speaker Per Conversation')
abline(v=numTokensToSample, col='red',lwd=3)
dev.off()

numConversations = nrow(subset(bothSpeakers, wordCountA >numTokensToSample & wordCountB >numTokensToSample))

tic()
speakerOverlapRaw =  mclapply(subset(bothSpeakers, wordCountA > numTokensToSample & wordCountB >numTokensToSample)$ConvNumber, function(z){getPropSharedTypesDS(z,10, numTokensToSample)})
toc()

shapiroTest = sapply(speakerOverlapRaw, function(x){shapiro.test(x$pShared)$p.value})
proportionNonNormal = length(which(shapiroTest < .1)) / length(shapiroTest)

speakerOverlap = do.call('rbind',speakerOverlapRaw)
bso = merge(bothSpeakers, speakerOverlap, by=c('ConvNumber'))

bso.agg = aggregate(cbind(pShared,pAfromB, pBfromA, numAtypes, numBtypes, numTotalTypes) ~ ConvNumber + SubjectNoA + lexDivA + BirthYearA + DialectA + EducationA  + AgeA + wordCountA + SubjectNoB + lexDivB + BirthYearB + DialectB + EducationB   + AgeB + wordCountB + TopicNo +SexA + SexB, bso, mean)
bso.agg$SexA = as.factor(tolower(gsub(' ','',bso.agg$SexA)))
bso.agg$SexB = as.factor(tolower(gsub(' ','',bso.agg$SexB)))
bso.agg$SexAB = as.factor(paste(bso.agg$SexA, bso.agg$SexB, sep='-'))
levels(bso.agg$SexAB)[3] = levels(bso.agg$SexAB)[2]
bso.agg$DialectA = as.factor(bso.agg$DialectA)
bso.agg$DialectB = as.factor(bso.agg$DialectB)
bso.agg$cumulativeAge = bso.agg$AgeA + bso.agg$AgeB
bso.agg$ageDifference = abs(bso.agg$AgeA - bso.agg$AgeB)
bso.agg$cumulativeEducation = as.numeric(as.character(bso.agg$EducationA)) + as.numeric(as.character(bso.agg$EducationB))
bso.agg$educationDifference = abs(as.numeric(as.character(bso.agg$EducationA)) - as.numeric(as.character(bso.agg$EducationB)))
bso.agg$sameDialect = as.factor(ifelse(bso.agg$DialectA == bso.agg$DialectB,'Same dialect','Different dialect'))
bso.agg$TopicNo = as.factor(bso.agg$TopicNo)
bso.agg$ConvNumber = as.factor(bso.agg$ConvNumber)

#Turn dyads into longform, bso.s, giving the same field names to both the A and the B speaker
bso.A = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoA, lexDiv = bso.agg$lexDivA, BirthYear = bso.agg$BirthYearA, Dialect = bso.agg$DialectA,  Education = bso.agg$EducationA, Age= bso.agg$AgeA, Sex= bso.agg$SexA, wordCount = bso.agg$wordCountA, Speaker = 'A', propFromInterlocutor = bso.agg$pAfromB, pShared = bso.agg$pShared, numTypes = bso.agg$numAtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.B = data.frame(ConvNumber = bso.agg$ConvNumber, speaker = bso.agg$SubjectNoB, lexDiv = bso.agg$lexDivB, BirthYear = bso.agg$BirthYearB, Dialect = bso.agg$DialectB, Education = bso.agg$EducationB, Age= bso.agg$AgeB, Sex= bso.agg$SexB, wordCount = bso.agg$wordCountB, Speaker = 'B', propFromInterlocutor = bso.agg$pBfromA, pShared = bso.agg$pShared, numTypes = bso.agg$numBtypes, numTotalTypes = bso.agg$numTotalTypes, TopicNo = bso.agg$TopicNo)
bso.s = rbind(bso.A, bso.B)

#get conversation table
bso.i = getConversationTable(bso.s)

#get the number of conversations that each individual participates in
convPerPerson = aggregate(ConvNumber ~ spk_speaker, bso.i, length)
names(convPerPerson)[2] = 'count'
convPerPerson = convPerPerson[order(convPerPerson$count, decreasing=T),]
histogram(convPerPerson$count, breaks=20, xlab='Conversations Per Person')

#plot analyses
plotSwbdLexDiv(swbdU, bso.agg, currentAnalysisDir)

#regressions
individualModels = individualRegressions(bso.i, currentAnalysisDir)
dyadModels = dyadRegressions(bso.agg, currentAnalysisDir)
bootstrappedModels = bootstrappedRegressions(bso, currentAnalysisDir)

#write all desriptive statistics out to a single ASCII file in the experiment structure
sink(paste(currentAnalysisDir,'experimentProperties.txt'))
print(paste('Content Word Count:', contentWordCount))
print(paste('Function Words Removed:', fwRemoved))      
print(paste('Number of Conversations:', numConversations))
print(paste('Total number of tokens:', numTokens))
print(paste('Number of tokens used in the bootstrap:', numTokensToSample))
print(paste('Number of unique speaker-conversations:', numUniqueSubjectConversations))
print(paste('Proportion of non-normally distributed bootstrapped samples:', proportionNonNormal))
sink()

#clear out large intermediate data objects and save a version of the data
rm('fd','d','d.content','d.agg')
save.image(paste(currentAnalysisDir,'swbdLexDiv.RData', sep=''))
