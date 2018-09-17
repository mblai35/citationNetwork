# citationProcessing.R
# R version 3.4.3 (2017-11-30)
# June 27, 2018. Mallory B. Lai.
# Reviewed by: TODO (Mallory B. Lai) : Find reviewer to proofread
# Bibliometrix results to Neo4j.    

#-----------------------------------------------------------------------
library(bibliometrix)
require(stringr)
library(beepr)
library(data.table)
#-----------------------------------------------------------------------

setwd("/Users/mblai/Downloads")

# Read in files. 
es.D <- readFiles("chdis_cit_net_jgb18_v1.txt")

# Convert to dataframe. 
scient_df <- isi2df(es.D)

# Convert to data.table for faster processing. 
scient_dt <- as.data.table(scient_df)

# add primary key
scient_dt$ID<- row.names(scient_dt)

# Split citations up at the ';'. Transpose for readability. 
# Resulting matrix will be a column for each paper. 
splitsies <- t(str_split(scient_dt$CR, ";", simplify = T))

# Split up each column (paper) by comma and simplify as a matrix. 
dubSplits <- str_split(splitsies, ", ", simplify = TRUE)

# Write .csv file. 
#write.csv(dubSplits, "roughReferences_chdis_cit_net_jgb18_v1.csv", row.names = F)

# Read intermediate .csv file in, replacing "" with NA.
ref <- fread("roughReferences_chdis_cit_net_jgb18_v1.csv", 
             na.strings = c("", "NA"))

# Remove rows with only NAs. 
ref <- ref[rowSums(is.na(ref)) < 9,]

# Note: there's a lot of QA to do. Lots of 2's for authors, inconsistent 
# columns
# Remove rows with 2-0 or 2 for author name. 
ref <- ref[-which(ref$V1 == "2-0" | ref$V1 == "2"), ]

# Create datatable for storing values. 
cited <- data.table(AuthorCited = ref$V1, 
                    PubDate = ref$V2, 
                    PubName = ref$V3)

# Initialize empty doi for storing the index of DOI information.
doi <- c()

for(i in 1:nrow(ref)){
  
  # Store the doiIndex. 
  doiIndex <- ifelse(length(which(str_detect(ref[i, ], "DOI") == T)) > 0, 
                     which(str_detect(ref[i, ], "DOI") == T), NA)
  
  # Store DOI. 
  doi <- c(doi, doiIndex)
  
}

# The percent of references without DOI's: 
sum(is.na(doi))/nrow(ref) * 100

# Due to data.table syntax, we need to convert back to data.frame.
ref <- as.data.frame(ref)

# Extract DOIs using doi index. 
DOI <- c()

for (i in 1:nrow(ref)){
  
  # Extract DOIs from ref using doi index.
  # If NA, then keep NA. 
  refDOI <- ifelse(is.na(doi[i]) == TRUE, NA, ref[i, doi[i]])
  
  # Add to DOI. 
  DOI <- c(DOI, refDOI)

  }

# Add to cited data.table. 
cited$DOI <- DOI





