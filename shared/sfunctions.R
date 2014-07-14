library(MASS)
library(RColorBrewer)
library('multicore')
library('grid')
library('ggplot2')

grepll = function(needles, haystack, fn){
	#grepll checks if any items in needles are in each item of haystack, returning a function on the returns of the grep values (which, dim, any, etc.)
	apply(do.call('rbind', lapply(needles, function(x){grepl(x,haystack)})),2,fn)
}

info = function(df){
	#returns the head, dim, and data types of a data frame
	rdf = list()
	rdf$data_types = unlist(lapply(names(df), function(x){typeof(df[[x]])}))
	rdf$head = head(df)
	rdf$dims = dim(df)
	return(rdf)
}

XYcontourPlot = function(k,x,y,...){	
	my.cols <- rev(brewer.pal(k, "RdYlBu"))
	XY = as.matrix(cbind(x,y))
	z <- kde2d(XY[,1], XY[,2], n=30)
	plot(x,y,...)
	contour(z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE, lwd=2)	
}

freqTable = function(x){
	y = table(x)
	y = transform(y, cumFreq = cumsum(Freq), relative = prop.table(Freq))
	return(y)	
}

#plot.style <- opts(panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),axis.line = theme_segment(colour="black",size=.5),axis.ticks = theme_segment(size=.5),axis.title.x = theme_text(vjust=-.5),axis.title.y = theme_text(angle=90,vjust=0.25),panel.margin = unit(1.5,"lines"))


.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}
# shorthand
lsos <- function(..., n=10) {
    .ls.objects(..., order.by="Size", decreasing=TRUE, head=F, n=n)
}


object.sizes <- function(){
	return(rev(sort(sapply(ls(envir=.GlobalEnv), function (object.name)object.size(get(object.name))))))
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#make a tex document with a set of key-value pairs that can be called inline with \input{outfile.tex} in the main .tex file


#keys = c('crab','whale')
#values = c(126,212)
#outfile = '~/Desktop/texTest.tex'
#RtexVars(keys, values, outfile)

RtexVars = function(keys, values, outfile){
if(length(keys) != length(values)){stop('keys and values must be of the same length')}
cat(paste(paste('\\newcommand{\\',keys, '}{', values, '}', sep=''), collapse ='\n'), file = outfile)	
}

daysToYearMonths = function(x){
	return(paste(floor((x/30.5)/12),';',round((x/30.5) %% 12, digits=0), sep=''))	
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

spellCheck= function(vector){
	sapply(vector, function(target){
		if (is.na(target) | nchar(target) == 0){
			F
		} else if (length(grep("'", target) > 0)) {
			warning(paste('Invalid punctuation detected:', target))
			T
		} else {
			system(paste('echo', target,'| /opt/local/bin/aspell -a'), intern=T)[2] == '*'	
		}
	})	
}

spellCheck2= function(vector){
	rdf = data.frame(word = vector, spell = F)
	
	NAitems = which(unname(sapply(vector, function(target){is.na(target)})))
	emptyItems = which(unname(sapply(vector, function(target){nchar(target) == 0})))
	invalidPunctuation = which(unname(sapply(vector, function(target){length(grep("'", target)) > 0})))
	
	toCheck = which(!(1:length(vector) %in% unique(c(NAitems, emptyItems, invalidPunctuation))))	
	
	indivWords = do.call('rbind', lapply(toCheck, function(index){ 
		return(data.frame(sentenceIndex = index, word= strsplit(vector[index], ' ')[[1]], stringsAsFactors = F))	
	}))
	
	uniqueWords = data.frame(word = unique(indivWords$word), spell = NA, override = F, stringsAsFactors=F)
	uniqueWords$numeric = sapply(uniqueWords$word, function(x){!is.na(as.numeric(x))})
	
	overrideWords = c('dietician')
	
	wordsToCheck = which(!uniqueWords$numeric)
	
	#get the spelling of each word; set all numeric inputs to true
	uw = uniqueWords$word[wordsToCheck]
	checkInput = paste(uw, collapse='\n')
	tf = tempfile()
	cat(checkInput, file=tf)
	rv = system(paste('cat', tf,'| aspell -a'), intern=T) 
	aspellResults = rv[grep('^[\\*&]',rv)]
	if (length(aspellResults) != length(uw)){stop('aspell produced one or more blank responses')}
	uniqueWords$spell[wordsToCheck] = aspellResults
	
	#pass if numeric or in the set of words to override
	uniqueWords$spell[which(uniqueWords$numeric)] = '*'
	uniqueWords$spell[uniqueWords$word %in% overrideWords] = '*'
	
	uniqueWords$correct = uniqueWords$spell == '*'
	print(paste('Misspelled Words:', paste(subset(uniqueWords, !correct)$word, collapse=', ')))
	
	 
	#merge uniqueWords back into indivWords
	indivWordsWithSpell = merge(indivWords, uniqueWords, by='word', all.x=T) 
	results = aggregate(correct ~ sentenceIndex, indivWordsWithSpell, all)
	
	rdf$spell[toCheck] = results$correct
	rdf$spell[NAitems] = T
	rdf$spell[emptyItems] = F
	rdf$spell[invalidPunctuation] = T
	
	if (nrow(rdf) != length(vector)){stop('results should be of equal length as input vector')}
	return(rdf$spell)
}

ifelse2 = function(test, yes, no){
	if(test){
		return(yes)
	} else {
		return(no)
	}	
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

tagSentence = function(sentence){
	if (length(strsplit(sentence, ' ')[[1]]) == 0){
		return('')
	} else {
		cat(sentence, file = 'temp.txt')
		system('java -mx1g -classpath /Users/stephan/Dropbox/Stanford/determiner_learning/corpus/POS_tagger/stanford-postagger-2013-11-12/stanford-postagger.jar edu.stanford.nlp.tagger.maxent.MaxentTagger -model /Users/stephan/Dropbox/Stanford/determiner_learning/corpus/POS_tagger/stanford-postagger-2013-11-12/models/english-bidirectional-distsim.tagger -textFile temp.txt > temp_tagged.txt')
		return(read.csv('temp_tagged.txt',header=F,stringsAsFactors=F)$V1[1])
	}	
}

renameColumn = function(df, oldname, newname){
	if (!is.data.frame(df)){
		stop('df must be a data frame')
	}
	if (!is.character(oldname) | !is.character(newname)){
		stop('oldname and newname must be character vectors')
	}
	if (!(oldname %in% names(df)) ){
		stop('oldname must be a column name in the data frame, df')
	}
	names(df)[grep(paste('^',oldname,'$', sep='') ,names(df))] = newname	
	return(df)
}

ensureTrailingSlash= function(string){
	#make sure that there is a trailing slash in a filename
	if (substr(string, nchar(string),nchar(string)) != '/'){
		return(paste(string,'/', sep=''))
	} else {
		return(string)
	}	
}

ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

isrc <- function(src){
	sapply(src, function(f){
		  t1 = try({source(f)})
		if (inherits(t1, 'try-error')){F} else{T}		
	})
}
