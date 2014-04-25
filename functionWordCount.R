tWordsperSpeaker = aggregate(Word ~ SubjectNo + ConvNumber, d, length)
names(tWordsperSpeaker)[3] = 'totalWordCount'

fWordsperSpeaker = aggregate(Word ~ SubjectNo + ConvNumber, fd, length)
names(fWordsperSpeaker)[3] = 'functionWordCount'

fws.age = merge(merge(tWordsperSpeaker, fWordsperSpeaker), caller, by.x='SubjectNo', by.y='Caller')
fws.age$Age  = 1988 - fws.age$BirthYear


xyplot(totalWordCount ~ Age, fws.age)
xyplot(functionWordCount ~ Age, fws.age)
xyplot(functionWordCount/totalWordCount ~ Age, fws.age, type = c('p','r'))