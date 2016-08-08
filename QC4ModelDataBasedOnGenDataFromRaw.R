# QC 
# the Cmp cohort data for model based on two calculatio way are the same or not?

inDirOld <- 'F:/Jie/MS/03_Result/2016-08-08/2016-08-08 06.41.05/'
inDirNew <- 'F:/Jie/MS/03_Result/2016-08-08/2016-08-08 05.39.00/'
cohList <- c("Cmp", "BConti", "B2B", "B2Fir", "B2Sec")
compareEachColumn <- function(var, dt1, dt2){
  vct1 <- dt1[, var]
  vct2 <- dt2[, var]
  return(all(vct1==vct2))
} 

compare_fun <- function(inDirOld, inDirNew, coh){
  cat(coh, '\n')
  inDirFileNm <- paste0(coh, "4Model.csv")
  dtNew <- read.table(paste0(inDirNew, inDirFileNm)
                      , sep=','
                      , header = T
                      , stringsAsFactors = F)
  
  dtOld <- read.table(paste0(inDirOld, inDirFileNm)
                      , sep=','
                      , header = T
                      , stringsAsFactors = F)
  
  # vars2newCheck <- setdiff(names(dtNew)
  #                       , grep("^dayssup|^avl_idx|^pre_dmts|^birth_region", names(dtNew), value=T))
  # vars2oldCheck <- setdiff(names(dtOld)
  #                       , grep("^dayssup|^avl_idx|^pre_dmts|^birth_region", names(dtOld), value=T))
  
  vars2newCheck <- names(dtNew)
  vars2oldCheck <- names(dtOld)
  rownames(dtNew) <- NULL
  rownames(dtOld) <- NULL
  
   
  # all.equal(dtNew[order(dtNew$record_num), ], dtOld[order(dtOld$record_num),], ignore_col_order=T, ignore_row_order=T, convert=T)
  bEqual <- all(unlist(lapply(vars2oldCheck
                              , compareEachColumn
                              , dtNew[order(dtNew$record_num), ]
                              , dtOld[order(dtOld$record_num),]
  )))
  
  cat('bEqual=', bEqual, '\n')
}

for(coh in cohList){
  compare_fun(inDirOld, inDirNew, coh=coh)
}
