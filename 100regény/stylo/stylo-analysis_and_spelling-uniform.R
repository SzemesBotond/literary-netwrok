setwd("YOUR DIR")
library(stylo)
stylo.results <- 
  stylo(gui = F,
        corpus.dir ="C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/regenyek", 
        corpus.lang = "Hungarian",
        mfw.min = 100, #100 or 200
        mfw.max = 1000, #100 or 200,
        mfw.incr = 100,
        analyzed.features = "w",
        ngram.size = 1,
        distance.measure = "wurzburg", #classic delta or cosine (wurzburg)
        sampling = "no.sampling",
        #sample.size = 2000, #2000 / 3000
        display.on.sreen = F,
        analysis.type = "BCT" #or just leave to a dendrogram
        #, pca.visual.flavour = "loadings" #to see what's happening
  )


# Make uniform spelling

input.dir <- "C:/Users/DELL/Desktop/TextAnalysisR/data/magyar_regeny/regenyek"
files.v <- dir(input.dir, "\\.txt$")
make.file.l <- function(files.v, input.dir){
  text.l <- list()
  for(i in 1:length(files.v)){
    text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                   what="character", sep="\f", quote = "", encoding = "UTF-8")
    text.l[[files.v[i]]] <- text.v
  }
  return(text.l)
}
my.corpus.l <- make.file.l(files.v, input.dir)

regenyek <- c()
for (i in 1:length(my.corpus.l)) {
  regenyek[[i]] <- unlist(my.corpus.l[[i]], recursive = TRUE, use.names = TRUE)
  regenyek [[i]] <- gsub("cz", "c", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" a ki ", " aki ", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" a mi ", " ami ", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" a mely ", " amely ", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" a hol ", " ahol ", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub("lly", "ly", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" ugy ", " úgy ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub(" igy ", " így ", regenyek[[i]], ignore.case = T)
  regenyek [[i]] <- gsub(" ur", " úr ", regenyek[[i]], ignore.case = TRUE)
  regenyek [[i]] <- gsub(" mig ", " míg ", regenyek[[i]], ignore.case = T)
}

# Define output directory
output_dir <- "output dict"

# Create directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Loop over the list and write each item to a file
for (i in seq_along(regenyek)) {
  # Get original filename without path
  filename <- basename(files.v[i])
  
  # Create full output path
  output_file <- file.path(output_dir, filename)
  
  # Write the modified content
  writeLines(regenyek[[i]], output_file, useBytes = TRUE)
}
