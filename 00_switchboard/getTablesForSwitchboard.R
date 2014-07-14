getTablesForSwitchboard = function(corpusDir, auxCorpusDir){
  
  #Word table
  system(paste('find ', corpusDir," -name '*word.text' -exec cat {} \\; > ", ensureTrailingSlash(getwd()),"input/switchboardWords.txt", sep=''))
  swbd = read.table('input/switchboardWords.txt', stringsAsFactors=F)
  names(swbd) <- c("File", "Start", "End", "Word")
  swbd$ConvNumber <- as.numeric(substring(swbd$File, 3,6))
  swbd$Role <- substring(swbd$File, 7,7) # Caller or callee, i.e. A or B
  swbd$UttrNo <- as.numeric(substring(swbd$File, 16,20))
  
  #Conversation Table
  conv <- read.csv(paste(auxCorpusDir, 'conv.tab', sep=''), F, stringsAsFactors=F)[,1:6]
  colnames(conv) <- c("ConvNumber", "Active", "SpeakerA", "SpeakerB", "TopicNo", "TalkDay")
  conv = melt(conv, id.vars=c('ConvNumber','Active','TopicNo','TalkDay'))
  names(conv)[5:6] = c('Speaker','SubjectNo')
  conv$Speaker = gsub('Speaker','',as.character(conv$Speaker))
  
  #Caller table
  caller <- read.csv(paste(auxCorpusDir,"caller.tab", sep=''), F, stringsAsFactors=F)[,1:7]
  colnames(caller) <- c("Caller", "Unknown1", "Unknown2", "Sex", "BirthYear", "Dialect", "Education")
  
  #merge toggether the word and conversation tables
  d = merge(swbd, conv, by.x=c('ConvNumber','Role'), by.y=c('ConvNumber','Speaker'))
  originalTokens = nrow(d)
  d$Word = as.character(d$Word)
  
  #Remove annotations and silences from the word table, make lower case
  d = d[-grep('\\[',d$Word),]
  silencesRemoved = nrow(d)
  d$Word = tolower(d$Word)
  d$Word  = gsub('[{}]','',d$Word)
  
  return(list(d, conv, caller))
}