dyadRegressions = function(bso.agg, currentAnalysisDir){
  
  #regression-specific predictors
  bso.agg = subset(bso.agg, !(EducationA == 9 | EducationB == 9))
  bso.agg$centeredpShared = bso.agg$pShared - mean(bso.agg$pShared) 
  bso.agg$cumulativeAge = bso.agg$cumulativeAge - mean(bso.agg$cumulativeAge) 
  bso.agg$ageDifference = bso.agg$ageDifference - mean(bso.agg$ageDifference) 
  bso.agg$cumulativeEducation = bso.agg$cumulativeEducation - mean(bso.agg$cumulativeEducation)
  bso.agg$educationDifference = bso.agg$educationDifference - mean(bso.agg$educationDifference)
  
  bso.agg$EducationA_f = as.factor(bso.agg$EducationA)
  bso.agg$EducationB_f = as.factor(bso.agg$EducationB)
  bso.agg$jointEd = paste(bso.agg$EducationA,bso.agg$EducationB, sep='-')
  
  #levels is non-deterministic
  bso.agg$jointEd = as.factor(sapply(strsplit(bso.agg$jointEd, '-'), function(x){
    if (x[1]  < x[2]){
      paste(x[2], x[1], sep='-')
    } else {
      paste(x[1],x[2], sep='-')
    }
  }))
  
  #need a consistent way to map symmetrical factor levels together
  
  dr.0 = lmer(pShared ~
                (1|TopicNo) 
              + AgeA * AgeB
              + SexA * SexB
              + EducationA + EducationB
              + DialectA + DialectB
              , bso.agg, REML=F)
  sink(paste(currentAnalysisDir,'regressions/dyad_dr0.txt', sep=''))
  print(summary(dr.0))
  sink()
  
  dr.0b = lmer(pShared ~
                 (1|TopicNo)
               + AgeA
               + SexA
               + EducationA
               + DialectA
               , bso.agg, REML=F)
  sink(paste(currentAnalysisDir,'regressions/dyad_dr0b.txt', sep=''))
  print(summary(dr.0b))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr1.txt', sep=''))
  print(summary(dr.1))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr2.txt', sep=''))
  print(summary(dr.2))
  sink()
  
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr3.txt', sep=''))
  print(summary(dr.3))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr4.txt', sep=''))
  print(summary(dr.4))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr5.txt', sep=''))
  print(summary(dr.5))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr6.txt', sep=''))
  print(summary(dr.6))
  sink()
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
  sink(paste(currentAnalysisDir,'regressions/dyad_dr7.txt', sep=''))
  print(summary(dr.7))
  sink() 
  anova(dr.1, dr.7) #remove dialect  difference
  
  dr.fn = lmer(pShared ~
                 (1|TopicNo) 
               + cumulativeAge
               + SexAB
               + cumulativeEducation
               , bso.agg, REML=F)
  sink(paste(currentAnalysisDir,'regressions/dyad_dr_fn.txt', sep=''))
  print(summary(dr.0))
  sink()
  anova(dr.1, dr.fn)
  
  ########## output final linear model, with Sattherwaite's approximation, to a .tex file
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
  rd = latex(coefs,file=paste(ensureTrailingSlash(currentAnalysisDir),'tables/pSharedMixedModel_sig.tex', sep=''),title="",table.env=FALSE,booktabs=TRUE)
  sink()
  #return the fit linear models so that we can save them
  dyadModels = list()
  dyadModels[['dr0']] = dr.0
  dyadModels[['dr1']] = dr.1
  dyadModels[['dr2']] = dr.2
  dyadModels[['dr3']] = dr.3
  dyadModels[['dr4']] = dr.4
  dyadModels[['dr5']] = dr.5
  dyadModels[['dr6']] = dr.6
  dyadModels[['dr7']] = dr.7
  dyadModels[['drfn']] = dr.fn
  
  return(dyadModels)
}