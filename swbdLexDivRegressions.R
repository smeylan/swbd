#Conversation-level Regression
#libraries loaded in SwbdLexDiv.R

#add additional predictors to conversation-level dataset, bso.agg
bso.agg$cumulativeAge = bso.agg$AgeA + bso.agg$AgeB
bso.agg$ageDifference = abs(bso.agg$AgeA - bso.agg$AgeB)
bso.agg$cumulativeEducation = as.numeric(as.character(bso.agg$EducationA)) + as.numeric(as.character(bso.agg$EducationB))
bso.agg$educationDifference = abs(as.numeric(as.character(bso.agg$EducationA)) - as.numeric(as.character(bso.agg$EducationB)))
bso.agg$sameDialect = as.factor(ifelse(bso.agg$DialectA == bso.agg$DialectB,'Same dialect','Different dialect'))
#bso.agg$SexAB already present

dyadRegression =lm(pShared ~ cumulativeAge + ageDifference + SexAB + sameDialect + cumulativeEducation + educationDifference + TopicNo, bso.agg)
summary(dyadRegression)
sgOutput = stargazer(dyadRegression, float=T, dep.var.labels='Prop. Shared Types In Conv.', single.row=T,title="Conversation-Level Predictors of Lexical Overlap", covariate.labels = c('Cumulative Age', 'Age Difference', 'Female - Male Dyad', 'Male - Male Dyad', 'Same Dialect Dyad', 'Cumulative Education', 'Education Difference' ,'Intercept'),omit='TopicNo' , label = 'dyadRegression')
cat( sgOutput, file = '~/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/regressions/dyadRegression.tex', sep='\n')

#Speaker-level Regression
#use speaker-level dataset bso.s
monadRegression = lm(propFromInterlocutor ~ Age + Education + Sex + as.factor(TopicNo), bso.s)
cat(stargazer(monadRegression, float=T, dep.var.labels='Prop. Shared Types With Interlocutor', single.row=T, no.space=F, title="Speaker-Level Predictors of Lexical Overlap", covariate.labels = c('Age','Education','Gender: Male', 'Intercept'), omit='TopicNo', label = 'monadRegression'), file = '~/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/regressions/monadRegression.tex', sep='\n')
