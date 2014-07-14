individualRegressions = function(bso.i, currentAnalysisDir){
  
  mr.me1 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)   + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me1.txt',sep=''))
  print(summary(mr.me1))
  sink()
  
  mr.me2 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                #+ spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me2.txt',sep=''))
  print(summary(mr.me2))
  anova(mr.me1, mr.me2) #lower information criteria; better log likelihood; keep spk_Age

  
  mr.me3 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                #+ spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me3.txt',sep=''))
  print(summary(mr.me3))
  sink()
  anova(mr.me1, mr.me3) #lower information criteria; better log likelihood, keep spk_Sex
  
  mr.me4 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                #+ spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me4.txt',sep=''))
  print(summary(mr.me4))
  sink()
  anova(mr.me1, mr.me4) #Can remove education
  
  mr.me5 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                #+ spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me5.txt',sep=''))
  summary(mr.me5)
  sink()
  anova(mr.me1, mr.me5) ##lower information criteria; better log likelihood, keep spk_Dialect
  
  mr.me6 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                #+ int_Sex 
                + int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me6.txt',sep=''))
  print(summary(mr.me6))
  sink()
  anova(mr.me1, mr.me6) ##lower information criteria; better log likelihood, keep int_sex.. this is interesting
  
  mr.me7 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                #+ int_Age 
                + int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me7.txt',sep=''))
  print(summary(mr.me7))
  sink()
  anova(mr.me1, mr.me7) #without int age has lower BIC
  
  mr.me8 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                #+ int_Education
                + int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me8.txt',sep=''))
  print(summary(mr.me8))
  sink()
  anova(mr.me1, mr.me8) #lower information criteria; better log likelihood, can remove interlocutor education
  
  mr.me9 = lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex 
                + int_Age 
                + int_Education
                #+ int_Dialect 
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me9.txt',sep=''))
  print(summary(mr.me9))
  sink()
  anova(mr.me1, mr.me9) #int_dialect does not matter;    
  
  mr.me.fn=lmer(c_spk_lexDiv ~ 
                  (1|TopicNo)  + (1|spk_speaker)
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex
                + int_Age  
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_me.fn.txt',sep=''))
  print(summary(mr.me.fn))
  sink()
  
  anova(mr.me.fn, mr.me1)  #new model has lower information criteria
  
  
  #add interaction terms
  mr.ie1=lmer(c_spk_lexDiv ~ 
                 (1|TopicNo)  
               + spk_Age 
               + spk_Sex
               + spk_Education
               + spk_Dialect
               + int_Sex  
               + int_Age 
               + int_Education
               + spk_Age * int_Age
               #+ spk_Sex * int_Sex
               #+ spk_Education * int_Education
               #+ spk_Dialect * int_Dialect
               , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie1.txt',sep=''))
  print(summary(mr.ie1))
  sink()
  anova(mr.me.fn, mr.ie1)  #age-age interaction not needed
  
  mr.ie2=lmer(c_spk_lexDiv ~ 
                 (1|TopicNo)  
               + spk_Age 
               + spk_Sex
               + spk_Education
               + spk_Dialect
               + int_Sex
               + int_Age   
               + int_Education
               #+ spk_Age * int_Age
               + spk_Sex * int_Sex
               #+ spk_Education * int_Education
               #+ spk_Dialect * int_Dialect
               , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie2.txt',sep=''))
  print(summary(mr.me2))
  sink()
  anova(mr.me.fn, mr.ie2)  #sex-sex interaction not needed
  
  mr.ie3=lmer(c_spk_lexDiv ~ 
                 (1|TopicNo)  
               + spk_Age 
               + spk_Sex
               + spk_Education
               + spk_Dialect
               + int_Sex  
               + int_Education
               #+ spk_Age * int_Age
               #+ spk_Sex * int_Sex
               + spk_Education * int_Education
               #+ spk_Dialect * int_Dialect
               , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie3.txt',sep=''))
  summary(mr.ie3)
  sink()
  anova(mr.me.fn, mr.ie3)  #rank problem
  
  mr.ie4=lmer(c_spk_lexDiv ~ 
                 (1|TopicNo)  
               + spk_Age 
               + spk_Sex
               + spk_Education
               + spk_Dialect
               + int_Sex  
               + int_Education
               + int_Age
               #+ spk_Age * int_Age
               #+ spk_Sex * int_Sex
               #+ spk_Education * int_Education
               + spk_Dialect * int_Dialect
               , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie4.txt',sep=''))
  print(summary(mr.me4))
  sink()
  anova(mr.me.fn, mr.ie4)  #BIC is lower for model without interaction
  
  #no interaction terms contribute, keep mr.me.fn
  
  bso.i$agediff = abs(bso.i$spk_Age -bso.i$int_Age)
  mr.ie.ad=lmer(c_spk_lexDiv ~
                  (1|TopicNo)  
                + spk_Age 
                + spk_Sex
                + spk_Education
                + spk_Dialect
                + int_Sex
                + int_Age   
                + int_Education
                + agediff
                , bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie_ad.txt',sep=''))
  print(summary(mr.ie.ad))
  sink()
  anova(mr.me.fn, mr.ie.ad)  #higher information criteria
  

  mr.ie.dmatch=lmer(c_spk_lexDiv ~ 
                      (1|TopicNo)  
                    + spk_Age 
                    + spk_Sex
                    + spk_Education
                    + spk_Dialect
                    + int_Age	
                    + int_Sex  
                    + int_Education
                    + dmatch,
                    bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie_dmatch.txt',sep=''))
  print(summary(mr.ie.dmatch))
  sink()
  anova(mr.me.fn, mr.ie.dmatch) #no improvement
  
  mr.ie.dmatchd=lmer(c_spk_lexDiv ~ 
                       (1|TopicNo)  
                     + spk_Age 
                     + spk_Sex
                     + spk_Education
                     + spk_Dialect
                     + int_Age	
                     + int_Sex  
                     + int_Education
                     + dmatch * spk_Dialect,
                     bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie_dmatchd.txt',sep=''))
  print(summary(mr.ie.dmatchd))
  sink()
  anova(mr.me.fn, mr.ie.dmatchd) # no improvement
  #no improvement
  
  #compare mr.me.fn to models where dialect is removed
  mr.ie.dmatch_drop=lmer(c_spk_lexDiv ~ 
                           (1|TopicNo)  
                         + spk_Age 
                         + spk_Sex
                         + spk_Education
                         + int_Age	
                         + int_Sex  
                         + int_Education
                         + dmatch,
                         bso.i, REML=F)
  sink(paste(currentAnalysisDir, 'regressions/indiv_mr_ie_dmatch_drop.txt',sep=''))
  print(summary(mr.ie.dmatch_drop))
  sink()
  anova(mr.me.fn, mr.ie.dmatch_drop) #keep dialect treatment
  
 
  #write the best model out to a latec table
  coefs = as.data.frame(summary(mr.me.fn)$coefficients)
  coefs[,1] = round(coefs[,1],digits=3)
  coefs[,2] = round(coefs[,2],digits=3)
  coefs[,3] = round(coefs[,3],digits=3) 
  coefs[,4] = round(coefs[,4],digits=3)
  coefs[,5] = ifelse(coefs[,5] > .05, paste('>',round(coefs[,5],digits=1),sep=""), 
 ifelse(coefs[,5] < .0001, "\\textbf{<.0001}", ifelse(coefs[,5] < .001,"\\textbf{<.001}", ifelse(coefs[,5] < .01, "\\textbf{<.01}", "\\textbf{<.05}"))))
  
  colnames(coefs) = c("Coef $\\beta$","SE($\\beta$)", "Approx. \\textit{df}","$t$",'$Pr(>|t|)$')
  prednames = data.frame(PName=row.names(coefs),NewNames=c(
    "Intercept","Speaker Age", "Speaker Gender: Male","Speaker Education: Less than College","Speaker Education: College", "Speaker Education: Some College","Speaker Education: Unknown", "Speaker Dialect: New England", "Speaker Dialect: North Midland", "Speaker Dialect: Northern", "Speaker Dialect: NYC", "Speaker Dialect: South Midland", "Speaker Dialect: Southern", "Speaker Dialect: Western", "Interlocutor Gender: Male","Interlocutor Age"))
  row.names(coefs) = prednames$NewNames[prednames$PName == row.names(coefs)]
  rd = latex(coefs,file=paste(currentAnalysisDir, 'tables/lexDivMixedModel.tex', sep=''),title="",table.env=FALSE,booktabs=TRUE)
 sink()
 rlist = list()
 rlist[['mr_me1']] = mr.me1
 rlist[['mr_me2']] = mr.me2
 rlist[['mr_me3']] = mr.me3
 rlist[['mr_me4']] = mr.me4
 rlist[['mr_me5']] = mr.me5
 rlist[['mr_me6']] = mr.me6
 rlist[['mr_me7']] = mr.me7
 rlist[['mr_me8']] = mr.me8
 rlist[['mr_me9']] = mr.me9
 rlist[['mr_me_fn']] = mr.me.fn
 rlist[['mr_ie1']] = mr.ie1
 rlist[['mr_ie2']] = mr.ie2
 rlist[['mr_ie3']] = mr.ie3
 rlist[['mr_ie4']] = mr.ie4
 rlist[['mr_ie_ad']] = mr.ie.ad
 rlist[['mr_ie_dmatch']] = mr.ie.dmatch
 rlist[['mr_ie_dmatchd']] = mr.ie.dmatchd
 rlist[['mr_ie_dmatch_drop']] = mr.ie.dmatch_drop
 return(rlist)
}