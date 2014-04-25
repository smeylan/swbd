#bso.i now has interlocutor properties for assessing in a linear model
info(bso.i)
bso.i$c_spk_lexDiv = bso.i$spk_lexDiv - mean(bso.i$spk_lexDiv)
bso.i$TopicNo = as.factor(bso.i$TopicNo)
bso.i$dmatch = bso.i$spk_Dialect == bso.i$int_Dialect
levels(bso.i$spk_Dialect) = gsub(' ','', levels(bso.i$spk_Dialect))
bso.i$spk_Education = as.factor(bso.i$spk_Education)

mr.me1 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
summary(mr.me1)

mr.me2 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	#+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me2) #lower information criteria; better log likelihood; keep spk_Age

mr.me3 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	#+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me3) #lower information criteria; better log likelihood, keep spk_Sex

mr.me4 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	#+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me4) #lower information criteria; better log likelihood, keep  spk_Education

mr.me5 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	#+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me5) ##lower information criteria; better log likelihood, keep spk_Dialect

mr.me6 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	#+ int_Sex 
	+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me6) ##lower information criteria; better log likelihood, keep int_sex.. this is interesting

mr.me7 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	#+ int_Age 
	+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me7) #without int age has lower BIC

mr.me8 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	#+ int_Education
	+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me8) #lower information criteria; better log likelihood, keep int_Education

mr.me9 = lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex 
	+ int_Age 
	+ int_Education
	#+ int_Dialect 
, bso.i, REML=F)
anova(mr.me1, mr.me9) #int_dialect does not matter;  fewer terms  

mr.me.fn=lmer(c_spk_lexDiv ~ 
	(1|TopicNo)  
	+ spk_Age 
	+ spk_Sex
	+ spk_Education
	+ spk_Dialect
	+ int_Sex
	+ int_Age  
, bso.i, REML=F)
anova(mr.me.fn, mr.me1)  #new model has lower information criteria
summary(mr.me.fn)

#add interaction terms
mr.ie.1=lmer(c_spk_lexDiv ~ 
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
anova(mr.me.fn, mr.ie.1)  #age-age interaction not needed

mr.ie.2=lmer(c_spk_lexDiv ~ 
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
anova(mr.me.fn, mr.ie.2)  #sex-sex interaction not needed

mr.ie.3=lmer(c_spk_lexDiv ~ 
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
anova(mr.me.fn, mr.ie.3)  #rank problem

mr.ie.4=lmer(c_spk_lexDiv ~ 
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
anova(mr.me.fn, mr.ie.4)  #BIC is lower for model without interaction

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
anova(mr.me.fn, mr.ie.ad)  #higher information criteria

#this is good, just need a concise way to report it

mr.me.fn

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
summary(mr.ie.dmatch)	
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
summary(mr.ie.dmatch)	
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
summary(mr.ie.dmatch)	
anova(mr.me.fn, mr.ie.dmatch_drop) #keep dialect treatment

summary(mr.me.fn, verbose=T)

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
latex(coefs,file='/Users/stephan/Dropbox/Berkeley/classes/2013_Fall/290L/lexicalDivergence/finalPaper/regressions/lexDivMixedModel.tex',title="",table.env=FALSE,booktabs=TRUE)

