functionWords = c('about','across','against','along ','around','at','behind','beside','besides','by','despite','down','during','for','from','in','inside','into','near','of','off','on','onto','over','through','to','toward','with','within','without','you','he','she','me','her','him','my','mine','her','hers','his','myself','himself','herself','anything','everything','anyone','everyone','ones','such','it','we','they','us','them','our','ours','their','theirs','itself','ourselves','themselves','something','nothing','someone','the','some','this','that','every','all','both','one','first','other','next','many','much','more','most','several','no','a','an','any','each','no','half','twice','two','second','another','last','few','little','less','least','own','and','but','after','when','as','because','if','what','where','which','how','than','or','so','before','since','while','although','though','who','whose','can','may','will','shall','could','might','would','should','must','be','do','have','here','there','today ','tomorrow ','now','then','always ','never','sometimes','usually ','often','therefore','however','besides','moreover','though','otherwise','else','instead','anyway','incidentally','meanwhile','i','was', 'uh','um','um-hum','um-huh','uh-huh', "uh-hum",'is','are',"don't","can't", "isn't","wasn't","it's","that's","he's","she's","they're","we're","i'm",'oh','yeah','not','yes','okay','yep','were',"i'd","i-","i've","they've",'hi','hello','had', 'got','been', "there's", "you're", 'has', 'those',"we've", "i'll", "doesn't","haven't",'does','huh',"didn't","what's","wouldn't","they'll", "you've","couldn't")

uberIndex = function(wordVector){
	return(log(length(wordVector))^2/(log(length(wordVector)) - log(length(unique(wordVector)))))
}

getLexDiv = function(text){
	filename = tempfile(pattern = "file", tmpdir = '/tmp', fileext = ".txt")
	cat(text, file= filename, sep='\n')
	tagged.results <- treetag(filename, treetagger="manual", lang="en", TT.options=list(path="/Applications/treeTagger/", preset="en"))
	K.ld.results = K.ld(tagged.results)
	mtld.results = MTLD(tagged.results)
	return(data.frame(MTLD = unname(mtld.results@MTLD$MTLD), K = K.ld.results@K.ld))	
}


#special transformation of a data frame to allow Z ~ X *Y in levelplot to occupy uppper corner only
#Y must always be larger than X-- so parse the columns Speaker A and B and switch identities based on which is smaller and which is larger. Maintain factor identity if the input is a factor
triangularTransform= function(df, symCol1, symCol2){
	bigger = apply(df[,c(symCol1, symCol2)], 1, max)
	smaller = apply(df[,c(symCol1, symCol2)], 1, min)
	if (is.factor(df[[symCol1]])){
		df[[symCol1]] = as.factor(bigger)	
		df[[symCol2]] = as.factor(smaller)
	} else {
		df[[symCol1]] = bigger
		df[[symCol2]] = smaller
	}
	return(df)
}

#get proportion of shared types from a dataframe
getPropSharedTypesHelper = function(repetition, SpeakerAtokens, SpeakerBtokens, sampleSize){
		
		if (is.na(sampleSize)){
			#if sample size isn't set, take all tokens
			if (length(SpeakerAtokens) > length(SpeakerBtokens)){
				sa.sampled = SpeakerAtokens[sample(1:length(SpeakerAtokens), length(SpeakerBtokens), replace=T)]#downsample speaker A
				sb.sampled = SpeakerBtokens
			} else if (length(SpeakerBtokens) > length(SpeakerAtokens)){
				sb.sampled = SpeakerBtokens[sample(1:length(SpeakerBtokens), length(SpeakerAtokens), replace=T)]#downsample speaker A
				sa.sampled = SpeakerAtokens
			}
			else {
				#token sizes are the same; leave as is
				sa.sampled = SpeakerAtokens
				sb.sampled = SpeakerBtokens
			}
		} else {
			if (length(SpeakerAtokens) < sampleSize | length(SpeakerBtokens) < sampleSize ){
				return(NULL)				
			}
			
			sa.sampled = SpeakerAtokens[sample(1:length(SpeakerAtokens), sampleSize, replace=T)]
			sb.sampled = SpeakerBtokens[sample(1:length(SpeakerBtokens), sampleSize, replace=T)]			
		}
				
		SpeakerAtypeList = unique(sa.sampled)
		SpeakerBtypeList = unique(sb.sampled)
		numTotalTypes = length(union(SpeakerAtypeList, SpeakerBtypeList))	
		pShared = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / numTotalTypes
		numAtypes= length(SpeakerAtypeList)
		pAfromB = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / numAtypes
		numBtypes= length(SpeakerBtypeList)
		pBfromA = length(intersect(SpeakerAtypeList, SpeakerBtypeList)) / length(SpeakerBtypeList)
		
		return(data.frame(repetition=repetition, pShared=pShared, pAfromB=pAfromB,pBfromA=pBfromA, numTotalTypes=numTotalTypes, numAtypes=numAtypes, numBtypes = numBtypes))
}

#get the proportion of shared types from the downsampled token collections
getPropSharedTypesDS = function(x, numRepetitions, numTokens){   
	#x is ConvNumber  	
	SpeakerA = subset(bothSpeakers, ConvNumber == x )$SubjectNoA
	SpeakerB = subset(bothSpeakers, ConvNumber == x )$SubjectNoB
	
	SpeakerAtokens =  subset(d.content, ConvNumber == x & SubjectNo == SpeakerA)$Word
	SpeakerBtokens =  subset(d.content, ConvNumber == x & SubjectNo == SpeakerB)$Word
	
	
	measurements =  lapply(1:numRepetitions, function(y){getPropSharedTypesHelper(y, SpeakerAtokens, SpeakerBtokens, numTokens)})
	
	measurements = do.call('rbind',measurements)
	if(!is.null(measurements)){
		return(cbind(measurements, ConvNumber=x))	
	}			
}