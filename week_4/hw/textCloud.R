rm(list=ls(all.names = TRUE))
library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)
print(getwd())
filenames <- list.files(getwd(), pattern="*.txt")
files <- lapply(filenames, readLines, encoding="UTF-8-BOM")
docs <- Corpus(VectorSource(files))
toSpace <- content_transformer(function(x, pattern) {
  return (gsub(pattern, " ", x))
}
)
docs <- tm_map(docs, toSpace, "在")
docs <- tm_map(docs, toSpace, "也")
docs <- tm_map(docs, toSpace, "了")
docs <- tm_map(docs, toSpace, "月")
docs <- tm_map(docs, toSpace, "政府")
docs <- tm_map(docs, toSpace, "對")
docs <- tm_map(docs, toSpace, "月")
docs <- tm_map(docs, toSpace, "可能")
docs <- tm_map(docs, toSpace, "市場")
docs <- tm_map(docs, toSpace, "指出")
docs <- tm_map(docs, toSpace, "[a-zA-Z]")
docs <- tm_map(docs, toSpace, "都")
docs <- tm_map(docs, toSpace, "認為")
docs <- tm_map(docs, toSpace, "可能")
docs <- tm_map(docs, toSpace, "會")
docs <- tm_map(docs, toSpace, "指出")
docs <- tm_map(docs, toSpace, "的")
docs <- tm_map(docs, toSpace, "是")
docs <- tm_map(docs, toSpace, "有")
docs <- tm_map(docs, toSpace, "和")
docs <- tm_map(docs, toSpace, "將")
docs <- tm_map(docs, toSpace, "已")
docs <- tm_map(docs, toSpace, "為")
docs <- tm_map(docs, toSpace, "不")
docs <- tm_map(docs, toSpace, "與")
docs <- tm_map(docs, toSpace, "因")
docs <- tm_map(docs, toSpace, "等")
docs <- tm_map(docs, toSpace, "就")
docs <- tm_map(docs, toSpace, "表示")
docs <- tm_map(docs, toSpace, "經濟")
docs <- tm_map(docs, toSpace, "預期")
docs <- tm_map(docs, toSpace, "公布")


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
          scale=c(5,0.1),min.freq=20,max.words=150,
          random.order=TRUE, random.color=FALSE, 
          rot.per=.1, colors=brewer.pal(8, "Dark2"),
          ordered.colors=FALSE,use.r.layout=FALSE,
          fixed.asp=TRUE)