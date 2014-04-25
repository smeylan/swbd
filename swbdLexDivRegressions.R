#Conversation-level Regression
#libraries loaded in SwbdLexDiv.R
cor(swbdU$lexDiv, swbdU$I, method='pearson')
cor(swbdU$lexDiv, swbdU$MTLD, method='pearson')
cor(swbdU$I, swbdU$MTLD, method='pearson')


#add additional predictors to conversation-level dataset, bso.agg
bso.agg$cumulativeAge = bso.agg$AgeA + bso.agg$AgeB
bso.agg$ageDifference = abs(bso.agg$AgeA - bso.agg$AgeB)
bso.agg$cumulativeEducation = as.numeric(as.character(bso.agg$EducationA)) + as.numeric(as.character(bso.agg$EducationB))
bso.agg$educationDifference = abs(as.numeric(as.character(bso.agg$EducationA)) - as.numeric(as.character(bso.agg$EducationB)))
bso.agg$sameDialect = as.factor(ifelse(bso.agg$DialectA == bso.agg$DialectB,'Same dialect','Different dialect'))
bso.agg$TopicNo = as.numeric(bso.agg$TopicNo)


dyadRegression = lm(pShared ~ TopicNo + lexDivA * lexDivB, bso.agg)
summary(dyadRegression)

dyadRegression = lm(pShared ~ TopicNo + cumulativeAge, bso.agg)
summary(dyadRegression)

#topic number should be treated as a factor; subject should be treated as a random effect

bso.agg = subset(bso.agg, !(EducationA == 9 | EducationB == 9))
bso.agg$pShared = bso.agg$pShared - mean(bso.agg$pShared) 
bso.agg$cumulativeAge = bso.agg$cumulativeAge - mean(bso.agg$cumulativeAge) 
bso.agg$ageDifference = bso.agg$ageDifference - mean(bso.agg$ageDifference) 
bso.agg$cumulativeEducation = bso.agg$cumulativeEducation - mean(bso.agg$cumulativeEducation)
bso.agg$educationDifference = bso.agg$educationDifference - mean(bso.agg$educationDifference)

dr.0 = lmer(pShared ~
	(1|TopicNo)
	+ AgeA * AgeB
	+ SexA * SexB
	+ EducationA + EducationB
	+ DialectA + DialectB
	, bso.agg, REML=F)
summary(dr.0)

dr.0b = lmer(pShared ~
	(1|TopicNo)
	+ AgeA
	+ SexA
	+ EducationA
	+ DialectA
	, bso.agg, REML=F)
summary(dr.0b)
anova(dr.0b, dr.0)


dr.1 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ ageDifference
	+ SexAB
	+ cumulativeEducation
	+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
summary(dr.1)
anova(dr.1, dr.0)
dr.2 = lmer(pShared ~
	(1|TopicNo)
	#+ cumulativeAge
	+ ageDifference
	+ SexAB
	+ cumulativeEducation
	+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.2) #keep age

dr.3 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	#+ ageDifference
	+ SexAB
	+ cumulativeEducation
	+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.3) #drop age difference

dr.4 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ ageDifference
	#+ SexAB
	+ cumulativeEducation
	+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.4) #keeo sex of the dyad

dr.5 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ ageDifference
	+ SexAB
	#+ cumulativeEducation
	+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.5) #keep cumulative education

dr.6 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ ageDifference
	+ SexAB
	+ cumulativeEducation
	#+ educationDifference
	+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.6) #remove education difference

dr.7 = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ ageDifference
	+ SexAB
	+ cumulativeEducation
	+ educationDifference
	#+ sameDialect
	, bso.agg, REML=F)
anova(dr.1, dr.7) #remove dialect  difference

dr.fn = lmer(pShared ~
	(1|TopicNo)
	+ cumulativeAge
	+ SexAB
	+ cumulativeEducation
	, bso.agg, REML=F)
anova(dr.1, dr.fn)
summary(dr.fn)


########## with Sattherwaite's approximation
coefs = as.data.frame(summary(dr.fn)$coefficients)
coefs[,1] = round(coefs[,1],digits=4)
coefs[,2] = round(coefs[,2],digits=5)
coefs[,3] = round(coefs[,3],digits=4) 
coefs[,4] = round(coefs[,4],digits=4) 
coefs[,5] = ifelse(coefs[,5] > .05, paste('>',round(coefs[,5],digits=1),sep=""), 
ifelse(coefs[,4] < .0001, "\\textbf{<.0001}", ifelse(coefs[,5] < .001,"\\textbf{<.001}", ifelse(coefs[,5] < .01, "\\textbf{<.01}", "\\textbf{<.05}"))))
colnames(coefs) = c("Coef $\\beta$","SE($\\beta$)", "Approx. \\textit{df}","$t$",'$Pr(>|t|)$')
 
prednames = data.frame(PName=row.names(coefs),NewNames=c('Intercept','Cumulative Age', 'Female - Male Dyad', 'Male - Male Dyad', 'Cumulative Education'))
 
row.names(coefs) = prednames$NewNames[prednames$PName == row.names(coefs)]
latex(coefs,file='/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/regressions/pSharedMixedModel_sig.tex',title="",table.env=FALSE,booktabs=TRUE)


