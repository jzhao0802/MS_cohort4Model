# QC 
# the Cmp cohort data for model based on two calculatio way are the same or not?

inDirNew <- 'F:/Jie/MS/02_Code/MS_Cohort4Model/Results/2016-07-20 06.58.41/'
inDirOld <- 'F:/Jie/MS/02_Code/MS_Cohort4Model/Results/2016-07-20 08.27.48/'

inDirFileNm <- "Cmp4Model.csv"

dtNew <- read.table(paste0(inDirNew, inDirFileNm)
                    , sep=','
                    , header = T
                    , stringsAsFactors = F)

dtOld <- read.table(paste0(inDirOld, inDirFileNm)
                    , sep=','
                    , header = T
                    , stringsAsFactors = F)

rownames(dtNew) <- NULL
rownames(dtOld) <- NULL

all.equal(dtNew[order(dtNew$record_num), ], dtOld[order(dtOld$record_num),], ignore_col_order=T, ignore_row_order=T, convert=T)



timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")
