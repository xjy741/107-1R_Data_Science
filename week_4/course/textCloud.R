rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines, encoding="UTF-8-BOM")
docs <- Corpus(VectorSource(files))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "�b")
docs <- tm_map(docs, toSpace, "�]")
docs <- tm_map(docs, toSpace, "�F")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�F��")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�i��")
docs <- tm_map(docs, toSpace, "����")
docs <- tm_map(docs, toSpace, "���X")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�{��")
docs <- tm_map(docs, toSpace, "�i��")
docs <- tm_map(docs, toSpace, "�|")
docs <- tm_map(docs, toSpace, "���X")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�O")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�M")
docs <- tm_map(docs, toSpace, "�N")
docs <- tm_map(docs, toSpace, "�w")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�P")
docs <- tm_map(docs, toSpace, "�]")
docs <- tm_map(docs, toSpace, "��")
docs <- tm_map(docs, toSpace, "�N")
docs <- tm_map(docs, toSpace, "����")



docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)


mixseg = worker()
jieba_tokenizer=function(d){
  unlist(segment(d[[1]],mixseg))
}
seg = lapply(docs, jieba_tokenizer)
freqFrame = as.data.frame(table(unlist(seg)))

wordcloud(freqFrame$Var1,freqFrame$Freq,
          scale=c(5,0.1),min.freq=15,max.words=150,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)