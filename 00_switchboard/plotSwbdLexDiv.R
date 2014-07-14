#Plots for LexDiv. Libraries and data derivation in SwbdLexDiv.R

plotSwbdLexDiv = function(swbdU, bso.agg, currentAnalysisDir){

  
  #uber index as a function of age
  pdf(paste(currentAnalysisDir, 'graphs/uberRegression.pdf', sep=''), width=5, height=5)
    print(xyplot(lexDiv ~ jitter(Age, factor=2) , bso.s, ylab='Uber Index of Lexical Diversity', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
      panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
      panel.abline(lm(y~x), col='black')
    }))
  dev.off() 
  #summary(lm(lexDiv ~Age, bso.s))
  
  
  #because the number of tokens is held constant, we only really need to look at the number of types for the samplex speech
  pdf(paste(currentAnalysisDir, 'graphs/typeCountRegression.pdf', sep=''), width=5, height=5)
    print(xyplot(numTypes ~ jitter(Age, factor=2) , bso.s, ylab='No. of Types in Matched-Size Sample', xlab='Age of Speaker', type=c('p'), panel = function(x, y) {
      panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
      panel.abline(lm(y~x), col='black')
    }))
  dev.off()  
  #summary(lm(numTypes ~ Age, bso.s))
  
  
  #proportion of types from interlocutor as a function of age
  pdf(paste(currentAnalysisDir, 'graphs/propFromInterlocutorRegression.pdf', sep=''), width=5, height=5)
    print(xyplot(pShared ~ jitter(Age, factor=2), bso.s, ylab='Prop. of Speaker Types Shared With Interlocutor', xlab='Age', type=c('p'), panel = function(x, y) {
      panel.xyplot(x, y, type=c('p'),  pch=20, cex=.5)
      panel.abline(lm(y~x), col='black')
    })) 
  dev.off()
  #summary(lm(pShared ~Age, bso.s))
  
  #how many types does each speaker use? Are some conversations asymmetric?
  pdf(paste(currentAnalysisDir, 'graphs/sharedSymmetry.pdf', sep=''))
    print(xyplot(pAfromB ~ pBfromA, bso.agg, ylab='Proportion of A Types from B', xlab='Proportion of B Types from A', xlim = c(.2,.5), ylim = c(.2,.5), panel = function(x, y) {
    panel.xyplot(x, y, type=c('p','g'))
    panel.abline(b=1,a=0, col='red')
    panel.abline(lm(y~x), col='black')
  }))
  dev.off()
  
  #shared types as a function of dyad age
  pdf(paste(currentAnalysisDir, 'graphs/jaccardsAgeAge.pdf', sep=''), height=5, width=5.5)
  resolution=3
    print(levelplot(pShared  ~ (resolution*round(AgeA/resolution)) * (resolution *round(AgeB/resolution)), triangularTransform(bso.agg,'AgeA','AgeB'), xlab = 'Age of Speaker 1', ylab= 'Age of Speaker 2', panel = function(...) {
      panel.fill(col = "gray") 	
      panel.levelplot(...)
      panel.abline(b=1,a=0, col='red')
    }, main="Proportion of Shared Types per Speaker Age"))
  dev.off()        
  
  #shared types as a function of dyad dialect
  pdf(paste(currentAnalysisDir, 'graphs/jaccardsDialectDialect.pdf', sep=''))
    print(levelplot(pShared  ~ DialectA * DialectB, triangularTransform(bso.agg, 'DialectA', 'DialectB'), xlab = 'Diaelect of Speaker 1', ylab= 'Dialect of Speaker 2', panel = function(...) {
    panel.fill(col = "gray") 	
    panel.levelplot(...)         
    panel.abline(b=1,a=0, col='red')
  }, main="Shared Types Per Speaker Dialect",scales=list(x=list(rot=90))))
  dev.off()
  
  #shared types as a function of dyad education
  pdf(paste(currentAnalysisDir,'graphs/jaccardsEducationEducation.pdf', sep=''))
    print(levelplot(pShared  ~ as.factor(EducationA) * as.factor(EducationB), triangularTransform(bso.agg, 'EducationA', 'EducationB'),  xlab = 'Education Level of Speaker 1', ylab= 'Education Level of Speaker 2', panel = function(...) {
      panel.fill(col = "gray") 	
      panel.levelplot(...)
      panel.abline(b=1,a=1, col='red')
    }, main="Shared Types Per Speaker Education",scales=list(x=list(rot=90))))       
  dev.off()
  
  #shared types as a function of Uber index of lexical diversity
  pdf(paste(currentAnalysisDir,'graphs/jaccardsUberUber.pdf', sep='') )      
    resolution=3
    print(levelplot(pShared  ~ (resolution*round(lexDivA/resolution)) * (resolution*round(lexDivB/resolution)), triangularTransform(bso.agg,'lexDivA','lexDivB'), xlab = 'Uber Index of Speaker 1', ylab= 'Uber Index of Speaker 2', panel = function(...) {
      panel.fill(col = "gray") 	
      panel.levelplot(...)
      panel.abline(b=1,a=0, col='red')         
    }, main="Shared Types Per Lexical Diversity of Dyad"))
  dev.off()       
  
  #shared types as a function unique types (determines uber index if sample size fixed)
  pdf(paste(currentAnalysisDir,'graphs/jaccardsTypesTypes.pdf', sep=''), height=5, width=5.5)       
    resolution=2
    print(levelplot(pShared  ~ (resolution*round(numAtypes/resolution)) * (resolution*round(numBtypes/resolution)), triangularTransform(bso.agg,'numAtypes','numBtypes'), xlab = 'No. of Types per Sample, Speaker 1', ylab= 'No. of Types per Sample, Speaker 2', panel = function(...) {
      panel.fill(col = "gray") 	
      panel.levelplot(...)
      panel.abline(b=1,a=0, col='red')         
    }, main="Speaker Lexical Diversity"))
  dev.off()
  
  #Dyad age histogram
  bsot = triangularTransform(bso.agg,'AgeA','AgeB')
  hbins = hexbin(bsot$AgeA, bsot$AgeB, xbins=	25)
  
  #first-pass: hexbin
  pdf(paste(currentAnalysisDir,'graphs/ageHistogram.pdf', sep=''))       
    plot(hbins, xlab = 'Age of Speaker 1', ylab= 'Age of Speaker 2', main='Speaker Ages')
  dev.off()
  
  #publication qualilty: ggplot version of dyad age histogram
  p1 = ggplot(data=bsot,aes(x=AgeA,y=AgeB))+stat_density2d(aes(fill=..density.., alpha=cut(..density..,breaks=c(0,.0005,Inf))),geom="tile", contour=F) + scale_fill_gradientn(colours = terrain.colors(10), values = NULL, space = "Lab", na.value = "grey50", guide = "colourbar", limits=c(.0005,.003)) + scale_alpha_manual(values=c(0,1),guide="none")+ geom_abline(intercept = 0, slope = 1)  +xlab('Age of Older Speaker') + ylab('Age of Younger Speaker') + theme(legend.position="none") + coord_cartesian(ylim=c(10,63), xlim=c(10,65)) + theme_bw() + theme(axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  axis.text.x = element_text(colour = "black"),
  axis.text.y = element_text(colour = "black"))

  p1.build = ggplot_build(p1)
  
  hist_top <- ggplot()+geom_histogram(data=bsot, aes(x=AgeA), binwidth=3, colour="gray", fill="gray") +xlab('') + ylab('Count') + theme(legend.position="none") + coord_cartesian(ylim=c(0,125), xlim=p1.build$panel$ranges[[1]]$x.range) + theme_bw() + theme(axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  axis.text.x = element_text(colour = "black"),
  axis.text.y = element_text(colour = "black"))
   
  hist_right <- ggplot()+geom_histogram(data=bsot, aes(x=AgeB), binwidth=3, colour="gray", fill="gray") +xlab('') + ylab('Count') + theme(legend.position="none") +coord_flip()  + theme_bw() + theme(axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  strip.background = element_blank(),
  axis.text.x = element_text(colour = "black"),
  axis.text.y = element_text(colour = "black"))
  
  empty <- ggplot()+geom_point(aes(1,1), colour="white") + theme_bw() + theme(axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank())
  
  pdf(paste(currentAnalysisDir, 'graphs/ageHistogram.pdf', sep='')) 
  print(grid.arrange(hist_top, empty, p1, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4)))
  dev.off()
  
}