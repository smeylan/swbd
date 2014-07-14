bootstrappedRegressions = function(bso, currentAnalysisDir){
  
  #Why just take the mean from the  nonparametric bootstrap? Fit a model to each sample, and construct CIs for parameters. (model constructed over all conversations)*100. See Figure 3
  #Removes the needs for Sattherwaite’s approximation
  
  bso$SexA = as.factor(tolower(gsub(' ','',bso$SexA)))
bso$SexB = as.factor(tolower(gsub(' ','',bso$SexB)))
bso$SexAB = as.factor(paste(bso$SexA, bso$SexB, sep='-'))
levels(bso$SexAB)[3] = levels(bso$SexAB)[2]
bso$DialectA = as.factor(bso$DialectA)
bso$DialectB = as.factor(bso$DialectB)
bso$ConvNumber = as.factor(bso$ConvNumber)
bso$cumulativeAge = bso$AgeA + bso$AgeB
bso$ageDifference = abs(bso$AgeA - bso$AgeB)
bso$cumulativeEducation = as.numeric(as.character(bso$EducationA)) + as.numeric(as.character(bso$EducationB))
bso$educationDifference = abs(as.numeric(as.character(bso$EducationA)) - as.numeric(as.character(bso$EducationB)))
bso$sameDialect = as.factor(ifelse(bso$DialectA == bso$DialectB,'Same dialect','Different dialect'))
bso$TopicNo = as.factor(bso$TopicNo)
bso$AggregateLexDiv = bso$lexDivA + bso$lexDivB
bso$centeredPShared = bso$pShare - mean(bso$pShare)

bso=subset(bso, EducationA !=9 & EducationB !=9)

#now use bso as the data source for the bootstrap

bso.split = split(bso, bso$repetition)

#model with lexical diversity
bso.allRuns = lapply(bso.split, function(singleRun){
  dr0 = lmer(pShared ~
               (1|TopicNo)+ ageDifference + cumulativeAge
             + SexAB
             + cumulativeEducation + educationDifference
             +sameDialect
             + AggregateLexDiv
             , data=singleRun, REML=F)
  return(summary(dr0))	
})

getParameter = function(regressionDF, parameter){
  rdf = data.frame(t(regressionDF	$coefficients[row.names(regressionDF$coefficients) == parameter]))
  names(rdf) = colnames(regressionDF$coefficients)
  return(rdf)	
}

coefNames = rownames(bso.allRuns[[1]]$coefficients)
allCoefEstimates = do.call('rbind',mclapply(coefNames, function(coefName){
  rdf = do.call('rbind', lapply(bso.allRuns, function(x){getParameter(x,coefName)}))
  rdf$variable = coefName
  return(rdf)	
}))

ci95 = as.data.frame(do.call('rbind', lapply(split(allCoefEstimates, allCoefEstimates$variable), function(x){
  qtl = quantile(x$Estimate, c(.025,.975))
  return(data.frame(lower = qtl[1], upper = qtl[2]))
  #return(subset(x, Estimate  <= confInt[1] &  Estimate  >= confInt[3]))			
})))
ci95$variable = rownames(ci95)


ci99 = as.data.frame(do.call('rbind', lapply(split(allCoefEstimates, allCoefEstimates$variable), function(x){
  qtl = quantile(x$Estimate, c(.005,.995))
  return(data.frame(lower = qtl[1], upper = qtl[2]))
  #return(subset(x, Estimate  <= confInt[1] &  Estimate  >= confInt[3]))			
})))
ci99$variable = rownames(ci99)

pdf(paste(currentAnalysisDir,'graphs/bootstrappedOverlapRegressionWithLexDiv.pdf', sep=''), height=7, width=7)
print(ggplot(data=allCoefEstimates, aes(x=Estimate))+ stat_bin(fill="cornflowerblue") + facet_wrap(~variable, scales='free') +geom_vline(xintercept=0, colour='red') + geom_vline(aes(xintercept=upper), data=ci99, colour='blue')+ geom_vline(aes(xintercept=lower), data=ci99, colour='blue') + theme_bw(base_size=9) + ylab('Count') + xlab('Regression Coefficient Estimate'))
dev.off()

#model without lexical diversity as a predictor
bso.nld = mclapply(bso.split, function(singleRun){
  dr0 = lmer(centeredPShared ~
               (1|TopicNo)+ ageDifference + cumulativeAge
             + SexAB
             + cumulativeEducation + educationDifference
             +sameDialect
             , data=singleRun, REML=F)
  return(summary(dr0))	
})

getParameter = function(regressionDF, parameter){
  rdf = data.frame(t(regressionDF	$coefficients[row.names(regressionDF$coefficients) == parameter]))
  names(rdf) = colnames(regressionDF$coefficients)
  return(rdf)	
}

coefNames = rownames(bso.nld[[1]]$coefficients)
nldCoefEstimates = do.call('rbind',mclapply(coefNames, function(coefName){
  rdf = do.call('rbind', lapply(bso.nld, function(x){getParameter(x,coefName)}))
  rdf$variable = coefName
  return(rdf)	
}))

ci95nld = as.data.frame(do.call('rbind', lapply(split(nldCoefEstimates, nldCoefEstimates$variable), function(x){
  qtl = quantile(x$Estimate, c(.025,.975))
  return(data.frame(lower = qtl[1], upper = qtl[2]))
  #return(subset(x, Estimate  <= confInt[1] &  Estimate  >= confInt[3]))			
})))
ci95nld$variable = rownames(ci95nld)


ci99nld = as.data.frame(do.call('rbind', lapply(split(nldCoefEstimates, nldCoefEstimates$variable), function(x){
  qtl = quantile(x$Estimate, c(.005,.995))
  return(data.frame(lower = qtl[1], upper = qtl[2]))
  #return(subset(x, Estimate  <= confInt[1] &  Estimate  >= confInt[3]))			
})))
ci99nld$variable = rownames(ci99nld)
ci99nld$crosses = !((ci99nld$lower > 0) == (ci99nld$upper > 0))

pdf(paste(currentAnalysisDir, 'graphs/bootstrappedOverlapRegressionWithoutLexDiv.pdf', sep=''), height=7, width=7)
print(ggplot(data=nldCoefEstimates, aes(x=Estimate))+ stat_bin(fill="cornflowerblue") + facet_wrap(~variable, scales='free') +geom_vline(xintercept=0, colour='red') + geom_vline(aes(xintercept=upper), data=ci99nld, colour='blue')+ geom_vline(aes(xintercept=lower), data=ci99nld, colour='blue') + theme_bw(base_size=9) + ylab('Count') + xlab('Regression Coefficient Estimate'))
dev.off()

rlist = list() 
rlist[['bso_allRuns']] = bso.allRuns
rlist[['bso_nld']] = bso.nld
return(rlist)
}