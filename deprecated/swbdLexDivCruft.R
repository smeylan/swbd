#hist(swbdU$lexDiv)
#xyplot(lexDiv ~ BirthYear, groups=Dialect, swbdU, auto.key=T)
summary(lm(lexDiv ~ Sex + Age + Dialect + wordCount + TopicNo, swbdU)) #capture none of it
#age is significant; older equqls higher uber index

xyplot(lexDiv ~ log(wordCount), swbdU)
bwplot(lexDiv ~ Dialect, swbdU) #no variability in lexical diversity
bwplot(lexDiv ~ as.factor(ifelse(Age < 60, 'Young', 'Old')), swbdU) #a small difference in lexical diversity if we split as a function of age

xyplot(lexDivA ~ lexDivB, bothSpeakers) 
xyplot(wordCountA ~ wordCountB, bothSpeakers) 
bothSpeakers$verbosityMultiple = bothSpeakers$wordCountA / bothSpeakers$wordCountB
hist(bothSpeakers$verbosityMultiple, breaks = 100, xlim=c(0,2))
nrow(subset(bothSpeakers, verbosityMultiple < 1.3 & verbosityMultiple > .7)) / nrow(bothSpeakers)

xyplot(abs(lexDivA - lexDivB) ~ abs(AgeA - AgeB), bothSpeakers) 


names(bothSpeakers)
hist(bothSpeakers$pShared)

xyplot(c(bothSpeakers$pAfromB,bothSpeakers$pBfromA)  ~ c(bothSpeakers$AgeA,bothSpeakers$AgeB), bothSpeakers, ylab='Proportion of Types Shared With Interlocutor', xlab='Age in Years') 
summary(lm(c(bothSpeakers$pAfromB,bothSpeakers$pBfromA)  ~ c(bothSpeakers$AgeA,bothSpeakers$AgeB) + c(bothSpeakers$lexDivA, bothSpeakers$lexDivB)  )) #number of shared types does decrease over time

#all predictive power comes from the word counts
+ c(bothSpeakers$wordCountA, bothSpeakers$wordCountB)

bwplot(pShared ~ as.factor(ifelse(DialectA == DialectB,'Shared Dialect','Different Dialect')), bothSpeakers) 

bwplot(pShared ~ as.factor(TopicNo.x), bothSpeakers) 


xyplot(pShared  ~ (AgeA + AgeB), bothSpeakers, ylab='Proportion of Types Shared Between Speakers', xlab='Age in Years') 
	
xyplot(numAtypes  ~ numBtypes, bothSpeakers, ylab='A Types', xlab='B Types') 

xyplot(c(bso.agg$pAfromB,bso.agg$pBfromA)  ~ c(bso.agg$AgeA,bso.agg$AgeB), bothSpeakers, ylab='Proportion of Types Shared With Interlocutor', xlab='Age in Years') 

xyplot(c(bso.agg$pAfromB,bso.agg$pBfromA) ~ c(bso.agg$numAtypes,bso.agg$bso$numBtypes))#lexical diversity determines overlap

hist(c(bso.agg$pAfromB,bso.agg$pBfromA))
#three different groups= non-convergent, convergent, and pre-converged

xyplot(pAfromB ~pBfromA, bso.agg) #few asymmetries
plot3d(bso.agg$pAfromB, bso.agg$pBfromA, bso.agg$numTotalTypes)
xyplot(pShared ~ numTotalTypes, bso.agg)

info(bso.s)
hist(bso.s$wordCount, breaks=100)


bwplot(propFromInterlocutor ~ ageGroup, bso.s)
bwplot(lexDiv ~ ageGroup, bso.s)
anova(lm(lexDiv ~ ageGroup, bso.s))
anova(lm(propFromInterlocutor ~ ageGroup, bso.s))

xyplot(propFromInterlocutor ~ Age, bso.s)
xyplot(lexDiv ~ Age, bso.s)
summary(lm(lexDiv ~ Age, bso.s))

summary(lm(propFromInterlocutor ~ Age, bso.s))
summary(lm(numTypes ~ Age, bso.s)) #numTypes determines

xyplot(numTypes ~ Age, bso.s)
summary(lm(numTypes ~ log(Age), bso.s))

summary(lm(numTypes ~ log(Age), bso.s))
xyplot(numTypes ~ Age , bso.s)
summary(lm(numTypes ~ Age + Dialect + Education + wordCount, bso.s))
#word count is still predicting the number of types

summary(lm(propFromInterlocutor ~ Age, bso.s))

summary(lm(lexDiv ~ Age + Dialect +  Education, bso.s))


#lexical diversity

summary(lm(propFromInterlocutor ~ Age * numTypes, bso.s)) #numTypes determines
plot3d(bso.s$propFromInterlocutor, bso.s$Age, bso.s$numTypes)
#overlap increases with the number of types (yup, significant in interaction model)
#overlap decreases with age (ns in interaction model)
#number of types increases with age
summary(lm(numTypes ~ Age, bso.s))
#high age and high number of types, even lower overlap
	

summary(lm(c(bothSpeakers$pAfromB,bothSpeakers$pBfromA)  ~ c(bothSpeakers$AgeA,bothSpeakers$AgeB) + c(bothSpeakers$lexDivA, bothSpeakers$lexDivB)  ))
	

#is this switch symmetric?

 #number of shared types does decrease over time

#all predictive power comes from the word counts
+ c(bothSpeakers$wordCountA, bothSpeakers$wordCountB)

bwplot(pShared ~ as.factor(ifelse(DialectA == DialectB,'Shared Dialect','Different Dialect')), bso.agg) 

bwplot(pShared ~ as.factor(TopicNo.x), bothSpeakers) 


xyplot(pShared  ~ abs(AgeA + AgeB), bso.agg, ylab='Proportion of Types Shared Between Speakers', xlab='Age Difference') #the accom


summary(lm(pShared  ~ abs(AgeA + AgeB), bso.agg)) 
#cumulative age

#age-related decrease in the baseline
	
xyplot(numAtypes  ~ numBtypes, bothSpeakers, ylab='A Types', xlab='B Types') 

info(bso.agg)
bso.agg$AgeGroupA = as.factor(ifelse(bso.agg$AgeA <= median(bso.agg$AgeA), 'Young', 'Old'))
bso.agg$AgeGroupB = as.factor(ifelse(bso.agg$AgeB <= median(bso.agg$AgeB), 'Young', 'Old'))
bso.agg$AgeDyad = as.factor(paste(as.character(bso.agg$AgeGroupA), as.character(bso.agg$AgeGroupB), sep='-'))
levels(bso.agg$AgeDyad)[3] =levels(bso.agg$AgeDyad)[2]


bwplot(pShared ~ AgeDyad, bso.agg)
summary(lm(pShared ~ AgeDyad, bso.agg))
anova(lm(pShared ~ AgeDyad, bso.agg))

#jaccard index is something high



info(bso.s)

#shared vocabulary as a function of time
bwplot(pShared ~ as.factor(TopicNo), bso.agg)