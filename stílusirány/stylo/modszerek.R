library(dplyr)
library(R.utils)

#load POS-tagged and lemmatized corpus and rename punctuation
input.dir <- "CORPUS"
files <- dir(input.dir, "\\.txt$")
my.corpus.l <- list ()
for(i in 1:length(files)){
  my.corpus.l [[i]] <- read.table(paste(input.dir, files[[i]], sep="/"), header = FALSE, fill = TRUE, encoding = "UTF-8", blank.lines.skip = F)
}

p <- my.corpus.l[[1]] %>% 
  mutate(V2 = case_when(V2 == "[,;:\\-\\–]"  ~ "COMMA")) %>%
  mutate(V2 = case_when(V2 == "[.!?]"  ~ "END")) 

#Change lemmas to main POS tags and save to folder "novels-with-postags"
pos <- list()
for(i in 1:length(files)){
  pos[[i]] <- my.corpus.l[[i]] %>% 
    filter(V3 != "NUM") %>% 
    mutate(V2= case_when(V3 %in% c("NOUN", "VERB","PROPN", "ADV", "ADJ") ~ V3,
                         TRUE ~ V2 ))

  pos[[i]] <- captureOutput(cat(pos[[i]]$V2))
  
  capture.output(pos[[i]], file=paste("/novels-with-postags",
                                      files[[i]], sep="/"))
  
  }


# Bootstrao Consensus Tree in stylo (use output in Gephi)
library(stylo)
stylo.results <- 
  stylo(gui = F,
      corpus.dir ="C:/Users/DELL/Desktop/TextAnalysisR/data/stilusirany/pos", 
      mfw.min = 20, #100 or 200
      mfw.max = 100, #100 or 200,
      mfw.incr = 20,
      analyzed.features = "w",
      ngram.size = 3,
      distance.measure = "wurzburg", #classic delta or cosine (wurzburg)
      sampling = "no.sampling",
      #sample.size = 2000, #2000 / 3000
      display.on.sreen = F,
      analysis.type = "BCT" #or just leave to a dendrogram
      #, pca.visual.flavour = "loadings" #to see what's happening
      )

stylo.results$features[1:100]
freqtable <- stylo.results$table.with.all.freqs
