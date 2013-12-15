#Plots for LexDiv. Libraries and data derivation in SwbdLexDiv.R

outputFolder ='/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/'

#uber index as a function of age
pdf(paste(outputFolder, 'uberRegression.pdf', sep=''))
 xyplot(lexDiv ~ jitter(Age, factor=2) , bso.s, ylab='Uber Index of Lexical Diversity', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
})
dev.off() 

#because the number of tokens is held constant, we only really need to look at the number of types for the samplex speech
pdf(paste(outputFolder, 'typeCountRegression.pdf', sep=''))
xyplot(numTypes ~ jitter(Age, factor=2) , bso.s, ylab='Number of Types in Matched-Size Sample (200 tokens)', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
})
dev.off()  

#proportion of types from interlocutor as a function of age
pdf(paste(outputFolder, 'propFromInterlocutorRegression.pdf', sep=''))
xyplot(propFromInterlocutor ~ jitter(Age, factor=2), bso.s, ylab='Proportion of Types Shared With Interlocutor', xlab='Age', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
}) 
dev.off()

#few type asymmetries
pdf(paste(outputFolder, 'sharedSymmetry.pdf', sep=''))
xyplot(pAfromB ~ pBfromA, bso.agg, ylab='Proportion of A Types from B', xlab='Proportion of B Types from A', xlim = c(.2,.5), ylim = c(.2,.5), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p','g'))
	panel.abline(b=1,a=0, col='red')
	panel.abline(lm(y~x), col='black')
})
dev.off()

#shared types as a function of dyad age
pdf(paste(outputFolder, 'jaccardsAgeAge.pdf', sep=''))
resolution=3
levelplot(pShared  ~ (resolution*round(AgeA/resolution)) * (resolution*round(AgeB/resolution)), triangularTransform(bso.agg,'AgeA','AgeB'), xlab = 'Age of Speaker 1', ylab= 'Age of Speaker 2', panel = function(...) {
	     panel.fill(col = "gray") 	
         panel.levelplot(...)
         panel.abline(b=1,a=0, col='red')
       }, main="Speaker Age")
dev.off()        
#no bluer on the identity-- not the case that similar ages predict more shared types. closer to the origin is bluer: younger speakers share more


#shared types as a function of dyad dialect
pdf(paste(outputFolder, 'jaccardsDialectDialect.pdf', sep=''))
levelplot(pShared  ~ DialectA * DialectB, triangularTransform(bso.agg, 'DialectA', 'DialectB'), xlab = 'Diaelect of Speaker 1', ylab= 'Dialect of Speaker 2', panel = function(...) {
        panel.fill(col = "gray") 	
         panel.levelplot(...)         
         panel.abline(b=1,a=0, col='red')
       }, main="Shared Types Per Speaker Dialect",scales=list(x=list(rot=90)))
dev.off()

#shared types as a function of dyad education
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/jaccardsEducationEducation.pdf')
levelplot(pShared  ~ EducationA * EducationB, triangularTransform(bso.agg, 'EducationA', 'EducationB'),  xlab = 'Education Level of Speaker 1', ylab= 'Education Level of Speaker 2', panel = function(...) {
        panel.fill(col = "gray") 	
         panel.levelplot(...)
         panel.abline(b=1,a=1, col='red')
       }, main="Shared Types Per Speaker Education",scales=list(x=list(rot=90)))       
dev.off()

#shared types as a function of Uber index of lexical diversity
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/jaccardsUberUber.pdf')       
levelplot(pShared  ~ round(lexDivA) * round(lexDivB), triangularTransform(bso.agg,'lexDivA','lexDivB'), xlab = 'Uber Index of Speaker 1', ylab= 'Uber Index of Speaker 2', panel = function(...) {
	     panel.fill(col = "gray") 	
         panel.levelplot(...)
         panel.abline(b=1,a=0, col='red')         
       }, main="Shared Types Per Lexical Diversity of Dyad")
dev.off()       

#shared types as a function unique types (determines uber index if sample size fixed)
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/jaccardsTypesTypes.pdf')       
resolution=5
levelplot(pShared  ~ (resolution*round(numAtypes/resolution)) * (resolution*round(numBtypes/resolution)), triangularTransform(bso.agg,'numAtypes','numBtypes'), xlab = 'Number of Types per 200 Tokens, Speaker 1', ylab= 'Number of Types per 200 Tokens, Speaker 2', panel = function(...) {
         panel.fill(col = "gray") 	
         panel.levelplot(...)
         panel.abline(b=1,a=0, col='red')         
       }, main="Speaker Lexical Diversity")
dev.off()

#Dyad age histogram
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/ageHistogram.pdf')       
bsot = triangularTransform(bso.agg,'AgeA','AgeB')
hbins = hexbin(bsot$AgeA, bsot$AgeB, xbins=	25)
plot(hbins, xlab = 'Age of Speaker 1', ylab= 'Age of Speaker 2', main='Speaker Ages')
dev.off()

#Token count per conversation + speaker histogram
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/tokenCountHistogram.pdf')       
hist(contentCount$Word, breaks=100, col='cyan', xlab='Token Count, Excluding Function Words',main='Tokens Per Speaker Per Conversation')
abline(v=200, col='red',lwd=3)
dev.off()