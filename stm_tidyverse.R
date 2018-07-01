library(readr)
library(stringr)
library(stm)
library(tm)
library(Cairo)
library(extrafont)
library(xlsx)
library(tidyverse)
library(tidytext)
library(quanteda)

data <- as.data.frame(read_csv("processed_2002.csv"))
data$press <- as.factor(data$press)
data$date <- as.factor(data$dindex)
data = subset(data, select = c(pos, press, date, article))

corpus <- corpus(data$pos)
docvars(corpus, field='press') <- data$press
docvars(corpus, field='month') <- as.integer(data$date)

dfm <- dfm(dfm(corpus,
        tolower=F,
        stem=F))

stmdfm <- convert(dfm, to = "stm", docvars = docvars(corpus))
# plotRemoved(stmdfm$documents, lower.thresh = seq(1, 100, by = 10))

out <- prepDocuments(stmdfm$documents, stmdfm$vocab, stmdfm$meta, lower.thresh = 15)

short = strtrim(data$article, 300)
rm(data, dfm, stmdfm, corpus)

K = 0
seed = sample(1:10000, 1)
stmmodel <- stm(out$documents, out$vocab, K = K, prevalence =~ s(month) + press, 
                data = out$meta, init.type = "Spectral", seed = seed)

save.image(file=paste0(seed, "stmmodel", K, ".rdata"))

topicproportion = colMeans(stmmodel$theta[,seq(1, K, 1)])
labels = labelTopics(stmmodel, c(1:K), K)

topics = data.frame(matrix(nrow=101), stringsAsFactors = F)[-1]
for(i in 1:K){
  topic = as.data.frame(
    cbind(labels$prob[i, ][1:100], 
          labels$frex[i, ][1:100],
          labels$lift[i, ][1:100],
          labels$score[i, ][1:100]),
    stringsAsFactors = F)
  names(topic) = paste(i, c('Prob', 'FREX', 'LIFT', 'Score'))
  topic = rbind(c(topicproportion[i], NA, NA, NA), topic)
  topics = cbind(topics, topic)
}

write.xlsx(topics, paste0(seed, 'Topics ', K, '.xlsx'))
write_csv(topics[-1,], 'Topics 95.csv')

# write topic distribution table
propbydoc = make.dt(stmmodel)
write.csv(propbydoc, paste0(seed, 'propbydoc', K, '.csv'), row.names=F)

par("mar")
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(oma = c(0, 0, 0, 0))

# size of topics in total documents

cairo_pdf(paste0(seed, "summary", K, ".pdf"), family="Malgun Gothic", width=10, height=15)
plot(stmmodel, type="summary", text.cex=0.3)
dev.off()

sample(findThoughts(stmmodel, texts=short, n=300, topics=51
                    )$docs[[1]], 30)

mod.out.corr <- topicCorr(stmmodel)
adjmatrix = mod.out.corr$poscor*10
#sort(adjmatrix, decreasing = T)

library(igraph)

graph = simplify(graph_from_adjacency_matrix(adjmatrix, mode='undirected'))
cg = cluster_fast_greedy(graph)
V(graph)$label.cex = 1
plot(graph, 
     layout = layout.fruchterman.reingold(graph),
     vertex.size = 7,
     edge.width = 2)
