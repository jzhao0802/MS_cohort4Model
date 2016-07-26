rm(list=ls())

library(dplyr)
inDir <- "F:/Jie/MS/03_Result/2016-07-26/2016-07-26 04.01.47/"
cohNames <- c("Cmp", "BConti", "B2B", "B2Fir", "B2Sec")
timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
outDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(outDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

for(coh in cohNames){
  inFile <- paste0(coh, "4Model.csv")
  
  if(!dir.exists(outDir))
    dir.create(outDir, showWarnings = T)
  
  dtOrg <- read.table(paste0(inDir, inFile)
                      , sep=','
                      , header = T
                      , stringsAsFactors = F)
  
  varLst <- names(dtOrg)
  dtMergeCate <- dtOrg %>%
    # merge a.	relapse_pre_90_01__1, relapse_pre_91to180_01__1 and relapse_pre_181to360_01__1 
    # (generate a new variable relapse_pre_0to1_01__1, which is the union of all three variables)
  {
    dataLastStep <- .
    # oldVars <- grep("relapse_pre_.+__1$", varLst, value=T)
    oldVars <- c("relapse_pre_90_01__1"
                 , "relapse_pre_91to180_01__1"
                 , "relapse_pre_181to360_01__1"
    )
    dataLastStep$relapse_pre_0to1_01__1 <- apply(dtOrg[, oldVars], 1, sum)
    dataLastStep <- dataLastStep %>% select(-one_of(oldVars))
    dataLastStep
    
  } %>%
    # b.	relapse_pre_1to2_01__1, relapse_pre_2to3_01__1 and relapse_pre_3to4_01__1 
    # (generate a new variable relapse_pre_1to4_01__1 as the union of all three)
    
  {
    dataLastStep <- .
    oldVars <- c("relapse_pre_1to2_01__1"
                 , "relapse_pre_2to3_01__1"
                 , "relapse_pre_3to4_01__1"
    )
    dataLastStep$relapse_pre_1to4_01__1 <- apply(dataLastStep[, oldVars], 1, sum)
    dataLastStep <- dataLastStep %>% select(-one_of(oldVars))
    dataLastStep
    # }  %>%
    #   # c.	baseline_edss_score__1d5_2 and baseline_edss_score__ge2d5 
    #   # (new variable baseline_edss_score__ge1d5 as the union of both)
    # {
    #   dataLastStep <- .
    #   oldVars <- c("baseline_edss_score__1d5_2"
    #                , "baseline_edss_score__ge2d5"
    #                )
    #   dataLastStep$baseline_edss_score__ge1d5 <- apply(dataLastStep[, oldVars], 1, sum)
    #   dataLastStep <- dataLastStep %>% select(-one_of(oldVars))
    #   dataLastStep
    
  } 
  
  write.table(dtMergeCate, paste0(outDir, coh, '4Model.csv')
              , sep=','
              , row.names = F)
}
