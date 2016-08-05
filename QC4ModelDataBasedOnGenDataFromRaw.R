# QC 
# the Cmp cohort data for model based on two calculatio way are the same or not?

inDirNew <- 'F:/Jie/MS/03_Result/2016-07-27/2016-07-27 08.40.45/'
inDirOld <- 'F:/Jie/MS/03_Result/2016-08-05/2016-08-05 07.17.59/'

inDirFileNm <- "Cmp4Model.csv"

dtNew <- read.table(paste0(inDirNew, inDirFileNm)
                    , sep=','
                    , header = T
                    , stringsAsFactors = F)

dtOld <- read.table(paste0(inDirOld, inDirFileNm)
                    , sep=','
                    , header = T
                    , stringsAsFactors = F)

vars2newCheck <- setdiff(names(dtNew)
                      , grep("^dayssup|^avl_idx|^pre_dmts|^birth_region", names(dtNew), value=T))
vars2oldCheck <- setdiff(names(dtOld)
                      , grep("^dayssup|^avl_idx|^pre_dmts|^birth_region", names(dtOld), value=T))

rownames(dtNew) <- NULL
rownames(dtOld) <- NULL
varsNotCheck <- 
  
compareEachColumn <- function(var, dt1, dt2){
  vct1 <- dt1[, var]
  vct2 <- dt2[, var]
  return(all(vct1==vct2))
}  
all.equal(dtNew[order(dtNew$record_num), ], dtOld[order(dtOld$record_num),], ignore_col_order=T, ignore_row_order=T, convert=T)
bEqual <- unlist(lapply(vars2oldCheck
       , compareEachColumn
       , dtNew[order(dtNew$record_num), ]
       , dtOld[order(dtOld$record_num),]
       ))


timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
