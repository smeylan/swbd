#Plots for LexDiv. Libraries and data derivation in SwbdLexDiv.R

outputFolder ='/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/'

#uber index as a function of age
pdf(paste(outputFolder, 'uberRegression.pdf', sep=''), width=5, height=5)
 xyplot(lexDiv ~ jitter(Age, factor=2) , bso.s, ylab='Uber Index of Lexical Diversity', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
})
dev.off() 
summary(lm(lexDiv ~Age, bso.s))


#because the number of tokens is held constant, we only really need to look at the number of types for the samplex speech
pdf(paste(outputFolder, 'typeCountRegression.pdf', sep=''), width=5, height=5)
xyplot(numTypes ~ jitter(Age, factor=2) , bso.s, ylab='No. of Types in Matched-Size Sample', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
})
dev.off()  

#proportion of types from interlocutor as a function of age
pdf(paste(outputFolder, 'propFromInterlocutorRegression.pdf', sep=''), width=5, height=5)
xyplot(pShared ~ jitter(Age, factor=2), bso.s, ylab='Prop. of Speaker Types Shared With Interlocutor', xlab='Age', type=c('p'), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
	panel.abline(lm(y~x), col='black')
}) 
dev.off()
summary(lm(pShared ~Age, bso.s))

#few type asymmetries
pdf(paste(outputFolder, 'sharedSymmetry.pdf', sep=''))
xyplot(pAfromB ~ pBfromA, bso.agg, ylab='Proportion of A Types from B', xlab='Proportion of B Types from A', xlim = c(.2,.5), ylim = c(.2,.5), panel = function(x, y) {
	panel.xyplot(x, y, type=c('p','g'))
	panel.abline(b=1,a=0, col='red')
	panel.abline(lm(y~x), col='black')
})
dev.off()

#shared types as a function of dyad age
pdf(paste(outputFolder, 'jaccardsAgeAge.pdf', sep=''), height=5, width=5.5)
resolution=3
levelplot(pShared  ~ (resolution*round(AgeA/resolution)) * (resolution *round(AgeB/resolution)), triangularTransform(bso.agg,'AgeA','AgeB'), xlab = 'Age of Speaker 1', ylab= 'Age of Speaker 2', panel = function(...) {
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
resolution=3
levelplot(pShared  ~ (resolution*round(lexDivA/resolution)) * (resolution*round(lexDivB/resolution)), triangularTransform(bso.agg,'lexDivA','lexDivB'), xlab = 'Uber Index of Speaker 1', ylab= 'Uber Index of Speaker 2', panel = function(...) {
	     panel.fill(col = "gray") 	
         panel.levelplot(...)
         panel.abline(b=1,a=0, col='red')         
       }, main="Shared Types Per Lexical Diversity of Dyad")
dev.off()       

#shared types as a function unique types (determines uber index if sample size fixed)
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/jaccardsTypesTypes.pdf', height=5, width=5.5)       
resolution=2
levelplot(pShared  ~ (resolution*round(numAtypes/resolution)) * (resolution*round(numBtypes/resolution)), triangularTransform(bso.agg,'numAtypes','numBtypes'), xlab = 'No. of Types per Sample, Speaker 1', ylab= 'No. of Types per Sample, Speaker 2', panel = function(...) {
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

p1 = ggplot(data=bsot,aes(x=AgeA,y=AgeB))+stat_density2d(aes(fill=..density.., alpha=cut(..density..,breaks=c(0,.0005,Inf))),geom="tile", contour=F) + scale_fill_gradientn(colours = terrain.colors(10), values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", limits=c(.0005,.003)) + scale_alpha_manual(values=c(0,1),guide="none")+ geom_abline(intercept = 0, slope = 1) + opts(axis.line = theme_segment(colour = "black"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.border = theme_blank(),
    panel.background = theme_blank(),
    strip.background = theme_blank(),
    axis.text.x = theme_text(colour = "black"),
    axis.text.y = theme_text(colour = "black")
    ) +xlab('Age of Older Speaker') + ylab('Age of Younger Speaker') + theme(legend.position="none") + coord_cartesian(ylim=c(10,63), xlim=c(10,65))

p1.build = ggplot_build(p1)

hist_top <- ggplot()+geom_histogram(data=bsot, aes(x=AgeA), binwidth=3, colour="gray", fill="gray")+ opts(axis.line = theme_segment(colour = "black"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.border = theme_blank(),
    panel.background = theme_blank(),
    strip.background = theme_blank(),
    axis.text.x = theme_text(colour = "black"),
    axis.text.y = theme_text(colour = "black")
    ) +xlab('') + ylab('Count') + theme(legend.position="none") + coord_cartesian(ylim=c(0,125), xlim=p1.build$panel$ranges[[1]]$x.range)

hist_right <- ggplot()+geom_histogram(data=bsot, aes(x=AgeB), binwidth=3, colour="gray", fill="gray")+ opts(axis.line = theme_segment(colour = "black"),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.border = theme_blank(),
    panel.background = theme_blank(),
    strip.background = theme_blank(),
    axis.text.x = theme_text(colour = "black"),
    axis.text.y = theme_text(colour = "black")
    ) +xlab('') + ylab('Count') + theme(legend.position="none") +coord_flip()

empty <- ggplot()+geom_point(aes(1,1), colour="white")+
         opts(axis.ticks=theme_blank(), 
              panel.background=theme_blank(), 
              axis.text.x=theme_blank(), axis.text.y=theme_blank(),           
              axis.title.x=theme_blank(), axis.title.y=theme_blank(),
              panel.grid.major = theme_blank(),
		    panel.grid.minor = theme_blank()
              )
grid.arrange(hist_top, empty, p1, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4)) 


pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/ageHistogram.pdf')   
grid.arrange(hist_top, empty, p1, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4)) 
dev.off()

#Token count per conversation + speaker histogram
pdf('/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/figures/tokenCountHistogram.pdf', width=5, height=5)       
hist(contentCount$Word, breaks=100, col='cyan', xlab='Token Count, Excluding Function Words',main='Tokens Per Speaker Per Conversation')
abline(v=157, col='red',lwd=3)
dev.off()