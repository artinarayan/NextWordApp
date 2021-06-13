library(RSQLite)
library(data.table)

tokenize = function(x, insertLast=T) {
      temp = x
      temp = gsub("[^\\w\\.\\!\\?\\d\\%\\-\\$\\s]", "", temp, perl=T)
      temp = gsub("[\\.\\!\\?\\s]+$", "", temp, perl=T)
      
      if (insertLast) {
            temp = gsub("$", " </s>", temp, perl=T)
      }
      
      temp = gsub("[\\.\\!\\?]+", " </s> <s> ", temp, perl=T)
      temp = gsub("^", "<s> ", temp, perl=T)
      
      
      temp = gsub("\\d{1,2}:\\d{1,2}", " <time> ", temp, perl=T)
      temp = gsub("\\d{2,4}-\\d{2}-\\d{2,4}|\\d{2,4}/\\d{2}/\\d{2,4}", " <date> ", temp, perl=T)
      temp = gsub("\\d{3}-\\d{3}-\\d{4}|\\d{10}", " <phone> ", temp, perl=T)
      temp = gsub("\\$[\\d\\s]+", " <dollars> ", temp, perl=T)
      temp = gsub("[\\d\\s]+%", " <percent> ", temp, perl=T)
      temp = gsub("\\d+[\\W]+\\d+", " <number> ", temp, perl=T)
      temp = gsub('_+', ' ', temp, perl=T)
      temp = unlist(strsplit(temp, " "))
      
      temp = gsub("[\\.\\!\\?\\d\\%\\-\\$]", "", temp, perl=T)
      temp = temp[temp!=""]
      temp = tolower(temp)
      temp = iconv(temp)
      return (temp)
}

cleanInput = function(string)
{
      tkn = tokenize(string, F)
      tkn = data.table(tkn[((length(tkn)-min(4, length(tkn))) + 1):length(tkn)])
      tkn = tkn[, list(V1, rid = seq_along(V1))]
      
      result = unigrams[unigrams$V1 %in% tkn$V1]
      setkey(tkn, V1)
      setkey(result, V1)
      result = result[J(tkn)]
      result = result[order(rid)]$id
      result
}

getOrder = function(tokens)
{
      
      d = 0.75
      order = length(tokens)
      
      context_count = 0
      possible_words = 0
      ngrams_count = 0
      
      if (order == 4) {
            possible_words = quingrams[quingrams$x == tokens[1] & quingrams$y == tokens[2] & quingrams$a == tokens[3] & quingrams$b == tokens[4]]
            
            result = possible_words[order(pkn, decreasing=T), list(c, pkn)]
      } else if (order == 3) {
            possible_words = quadgrams[quadgrams$x == tokens[1] & quadgrams$y == tokens[2] & quadgrams$a == tokens[3]]
            
            result = possible_words[order(pkn, decreasing=T), list(b, pkn)]
      } else if (order == 2) {
            possible_words = trigrams[trigrams$x == tokens[1] & trigrams$y == tokens[2]]
            result = possible_words[order(pkn, decreasing=T), list(a, pkn)]
      } else if (order == 1) {
            possible_words = bigrams[bigrams$x == tokens[1]]
            result = possible_words[order(pkn, decreasing=T), list(y, pkn)]
      }
      
      result
}

getPossibleWords = function(sentence, num = 3)
{
      
      tokens = cleanInput(sentence)
      possibleWordIds = data.table()
      
     
      while (nrow(possibleWordIds) < num && length(tokens) >= 1) {
            idsVector = setnames(data.table(getOrder(tokens)), c('id', 'pkn'))
            
            possibleWordIds = rbind(possibleWordIds, idsVector[!(idsVector$id %in% possibleWordIds$id)])
            if (length(tokens) == 1) break;
            tokens = tokens[2:length(tokens)]
      }
      
      
      if (nrow(possibleWordIds) < num) {
            idsVector = setnames(data.table(getOrder(cleanInput(""))), c('id', 'pkn'))
            possibleWordIds = rbind(possibleWordIds, idsVector[!(idsVector$id %in% possibleWordIds$id)])
      }
      possibleWordIds = possibleWordIds[1:num]  
      
      possibleWordIds$size = seq(from=10000, to=10, length.out=nrow(possibleWordIds))
      possibleWordIds = possibleWordIds[, list(id, size, rid = seq_along(id))]
      
      setkey(possibleWordIds, id)
      setkey(unigrams, id)
      result = unigrams[possibleWordIds]
      result = result[order(rid)]
      
      result
}


# con<-dbConnect(SQLite(), "IndexedBaseNgramsDB")
# unigrams = data.table(dbGetQuery(con, "SELECT * FROM unigrams"))
# bigrams = data.table(dbGetQuery(con, "SELECT * FROM bigramsProb"))
# trigrams = data.table(dbGetQuery(con, "SELECT * FROM trigramsProb"))
# quadgrams = data.table(dbGetQuery(con, "SELECT * FROM quadgramsProb"))
# quingrams = data.table(dbGetQuery(con, "SELECT * FROM quinqgramsProb"))

con<-dbConnect(SQLite(), "data/ngrams_db")
unigrams = data.table(dbGetQuery(con, "SELECT * FROM unigrams"))
bigrams = data.table(dbGetQuery(con, "SELECT * FROM bigrams"))
trigrams = data.table(dbGetQuery(con, "SELECT * FROM trigrams"))
quadgrams = data.table(dbGetQuery(con, "SELECT * FROM quadgrams"))
quingrams = data.table(dbGetQuery(con, "SELECT * FROM quinqgrams"))
