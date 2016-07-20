library(dplyr)
library(plyr)
library(stringr)
library(lazyeval)

rm(list=ls())

#
# Steps to further clean the data for modelling:
# 1. median impute the missing category of years_diag_idx and merge it with the others
# 2. choose and then remove the reference categories
# 3. Add 3 more variables about the sub-cohorts the records are from
# 4. Save the data
#



FindBinVarReference_WithoutMissing <- function(
  prefix, varNames, varValues
  )
{
  # if the first character of all vars is a number, use the number
  # else if the first two are c(ge, gt, le, lt), then use the le
  # else return NA
  
  cateStart <- str_locate(varNames[1], prefix)[1, "end"] + 1
  firstCharacters <- as.numeric(str_sub(varNames, start=cateStart, end=cateStart))
  
  if (!any(is.na(firstCharacters))) # if the first character of all vars is a number
    refVarName <- varNames[which.min(firstCharacters)]
  else
  {
    firstTwoCharacters <- str_sub(varNames, start=cateStart, end=cateStart+1)
    if (all(firstTwoCharacters %in% c("ge", "le", "gt", "lt")))
    {
      IDs_le <- which(firstTwoCharacters == "le")
      if (length(IDs_le) == 1)
        refVarName <- varNames[IDs_le]
      else
        refVarName <- NA
    }
    else
    {
      refVarName <- NA
    }
      
  }
  
  return (refVarName)
    
}

FindBinVarReference <- function(prefix, allVarNames, dataset, minRefPct)
{
  categories <- allVarNames[grepl(prefix, allVarNames)]
  #
  # if there's a 'missing', use 'missing' unless it's less than minRefPct; 
  # If the there's no missing, or missing is less than minRefPct, 
  # Then look at first character after '__'. 
  # If the first character of all other categories is a number, choose the smaller one;
  # If not all first characters is a number, look at the first two characters, 
  # If the first two are all %in% c(ge, gt, le, lt), then use the le;
  # If none of the above applies, returns NA so that they can be manually addressed
  # At the end before returning also check whether the reference is less than minRefPct, 
  # if so then returns NA
  #
  
  if (any(grepl(paste0(prefix, "missing"), allVarNames)))
  {
    missing_pct <- sum(dataset[, paste0(prefix, "missing")]) / nrow(dataset)
    if (missing_pct < minRefPct) # check whether missing < 10%
    {
      categories <- categories[categories != "missing"]
      refVarName <- FindBinVarReference_WithoutMissing(
        prefix, categories, dataset[, categories]
        )
    } else
    {
      refVarName <- paste0(prefix, "missing")
    }
  } else
  {
    refVarName <- FindBinVarReference_WithoutMissing(
      prefix, categories, dataset[, categories]
      )
  }
  
  if (!is.na(refVarName))
  {
    pctRef <- sum(dataset[, refVarName]) / nrow(dataset)
    if (pctRef < minRefPct)
      refVarName <- NA
  }
  
  return (refVarName)
}

bQcMode <-T 
inputDir4DS <- "F:/Jie/MS/03_Result/2016-07-19/2016-07-19 01.52.43/"
outcomes <- c("relapse_fu_any_01", "edssprog", "edssconf3",
              "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")

orgCohort <- tbl_df(read.csv(paste0(inputDir4DS, "Cmp.csv"))) %>%
  dplyr::rename(dayssup__gt360=dayssup_gt360) %>%
  dplyr::rename(dayssup__le360=dayssup_le360)

if(bQcMode==T){
  if(!is.null(c(orgCohort$dayssup_le360, orgCohort$dayssup_gt360)))
    stop('error!\n\n')
}

#
## Impute the years_diag_idx



missing_reps <- c('', 'NA', 'unknown', 'ambiguous')
cohortDir_Jie <- "F:/Lichao/work/Projects/MultipleSclerosis/Results/2016-07-05/cohortDt_jie_July06/"
medianYears <- median(
  read.csv(
    paste0(cohortDir_Jie, "dt_Cmp_withoutTransf.csv"), 
    na.strings=missing_reps
    )$years_diag_idx, na.rm=T
  )

if(bQcMode==T){
  if(!is.numeric(read.csv(
    paste0(cohortDir_Jie, "dt_Cmp_withoutTransf.csv"), 
    na.strings=missing_reps
  )$years_diag_idx))
    stop('not numeric!\n\n')
}


if (medianYears <= 2)
{
  orgCohort[orgCohort$years_diag_idx__missing==1, "years_diag_idx__le2"] <- 1
} else if (medianYears <= 5)
{
  orgCohort[orgCohort$years_diag_idx__missing==1, "years_diag_idx__gt2_le5"] <- 1
} else
  orgCohort[orgCohort$years_diag_idx__missing==1, "years_diag_idx__gt5"] <- 1

if(bQcMode){
  bImpComplte <- apply(
    orgCohort[, setdiff(grep('^years_diag_idx__'
                             , names(orgCohort)
                             , value=T
                             , ignore.case = T)
                        , 'years_diag_idx__missing')
              ], 1, sum)
  if(any(bImpComplte == 0) | any(is.na(bImpComplte)))
    stop('not impute completely!\n\n')
}

orgCohort <- select(orgCohort, -years_diag_idx__missing)



#
## find and remove reference categories



pct <- 0.15

referencesForBinVars <- 
  colnames(orgCohort) %>%
  str_extract(".*__") %>% # extract prefix
  .[!(is.na(.))] %>% # remove output vars
  unique %>%
  {
    uniquePrefixes <<- .
    .
  } %>%
  lapply(FindBinVarReference, allVarNames=colnames(orgCohort), dataset=orgCohort, 
         minRefPct=pct) %>%
  ldply(c)


naIndices <- which(is.na(referencesForBinVars))
remainPrefixes <- uniquePrefixes[naIndices]
refs4RemainPrefixes <- c(
  "age__le30",
  "baseline_edss_score__0",
  "birth_region__Central_Europe",
  "gender__F"
)


referencesForBinVars[is.na(referencesForBinVars)] <- refs4RemainPrefixes

if(bQcMode){
  uniqPrefixes4Qc <- gsub("(.*)__(.*)", "\\1", referencesForBinVars)
  if(length(unique(uniqPrefixes4Qc))!= length(referencesForBinVars)){
    stop("wrong!\n\n")
  }
}

# remove ref categories

remainVarNames <- colnames(orgCohort)[!(colnames(orgCohort) %in% referencesForBinVars$V1)]

recordIDColName <- "record_num"
refRemovedCohort <- select_(orgCohort, .dots=c(remainVarNames, recordIDColName))
if(bQcMode){
  if(any(referencesForBinVars %in% names(refRemovedCohort)))
    stop('reference varibles have not been removed completely!\n\n')
  diffVars <- setdiff(names(refRemovedCohort), remainVarNames)
  if( !all(length(diffVars)==1 & diffVars== "record_num"))
    stop("variables are not consistent!\n\n")
}



#
## add cohort flag


# be careful when changing the order of the names in subCohortNames
# it could affect functions below using them!
subCohortNames <- c("BConti", "B2B", "B2Fir", "B2Sec") 
rawDataFile <- "F:/Lichao/work/Projects/MultipleSclerosis/data/2016-07-01/MS_decsupp_analset_20160701.csv"
rawData <- tbl_df(read.csv(rawDataFile, na.strings=missing_reps))
# recordIDs_AllSubCohorts <- list()
# for (subCohortName in subCohortNames)
# {
#   recordIDs_AllSubCohorts[[subCohortName]] <-
#     read.csv(paste0(inputDir4DS, subCohortName, ".csv"))[, recordIDColName]
# }
# # check: is there any overlap between IDs from different cohort?
# for (name1 in subCohortNames[1:3])
# {
#   for (name2 in subCohortNames[2:4])
#   {
#     if (name1 != name2)
#     {
#       if (length(intersect(recordIDs_AllSubCohorts[[name1]], recordIDs_AllSubCohorts[[name2]])) != 0)
#         stop(paste0("Error! There's overlap in record IDs between ", name1, " and ", name2, "."))
#     }
#   }
# }

FindSubCohortOneRecord <- function(tblcoh)
{
  return (subCohortNames[tblcoh])
}

cohortFlagAppendedData <- 
  refRemovedCohort %>%
  {
    .[, subCohortNames] <- 0
#     .
#   }
    for (i in 1:nrow(.))
    {
      record_id <- as.data.frame(.)[i, recordIDColName]
      subCohortThisRecord <- FindSubCohortOneRecord(rawData$tblcoh[record_id])
      
      .[i, subCohortThisRecord] <- 1
    }
    .
  }

#   {
#     varval <- interp(quote(ifelse(recordIDColName %in% recordIDs_AllSubCohorts[[subCohortNames[1]]], 1, 0)), 
#                      recordIDColName=recordIDColName, recordIDs_AllSubCohorts=recordIDs_AllSubCohorts, 
#                      subCohortNames=subCohortNames)
#     aa<<-mutate_(., .dots= setNames(varval, subCohortNames[1]))
#   } %>%
#   {
#     varval <- interp(quote(ifelse(recordIDColName %in% recordIDs_AllSubCohorts[[subCohortNames[2]]], 1, 0)), 
#                      recordIDColName=recordIDColName, recordIDs_AllSubCohorts=recordIDs_AllSubCohorts, 
#                      subCohortNames=subCohortNames)
#     mutate_(., .dots= setNames(list(varval), subCohortNames[2]))
#   } %>%
#   {
#     varval <- interp(quote(ifelse(recordIDColName %in% recordIDs_AllSubCohorts[[subCohortNames[3]]], 1, 0)), 
#                      recordIDColName=recordIDColName, recordIDs_AllSubCohorts=recordIDs_AllSubCohorts, 
#                      subCohortNames=subCohortNames)
#     mutate_(., .dots= setNames(list(varval), subCohortNames[3]))
#   } %>%
#   {
#     varval <- interp(quote(ifelse(recordIDColName %in% recordIDs_AllSubCohorts[[subCohortNames[4]]], 1, 0)), 
#                      recordIDColName=recordIDColName, recordIDs_AllSubCohorts=recordIDs_AllSubCohorts, 
#                      subCohortNames=subCohortNames)
#     mutate_(., .dots= setNames(list(varval), subCohortNames[4]))
#   }
# 
#   
#   mutate_(B2B = ifelse(recordIDColName %in% recordIDs_AllSubCohorts[["B2B"]], 1, 0)) %>%
#   mutate_(B2Fir = ifelse(recordIDColName %in% recordIDs_AllSubCohorts[["B2Fir"]], 1, 0)) %>%
#   mutate_(B2Sec = ifelse(recordIDColName %in% recordIDs_AllSubCohorts[["B2Sec"]], 1, 0)) %>%
#   mutate_(BConti = ifelse(recordIDColName %in% recordIDs_AllSubCohorts[["BConti"]], 1, 0))
  
# CheckOneFlag <- function(subCohortName, allSubCohortNames, rowIdx, recordIDs, data)
# {
#   # if this record belongs to a subcohort, the flag of that subcohort has to be 1
#   # while others must be 0.
#   otherCohortNames <- allSubCohortNames[allSubCohortNames != subCohortName]
#   if (data[rowIdx, recordIDColName] %in% recordIDs[[subCohortName]])
#   {
#     if ((data[rowIdx, subCohortName] != 1) | any(data[rowIdx, otherCohortNames]))
#       stop(paste0("Error! Row ", rowIdx, " in the data has incorrect sub-cohort flags."))
#   }
#   # in this row there must be one and at least one value 1
#   if (!all(data[rowIdx, allSubCohortNames] %in% c(0,1)))
#     stop(paste0("Error! Row ", rowIdx, " has values other 0 and 1."))
#   if (sum(data[rowIdx, allSubCohortNames]) != 1)
#     stop(paste0("Error! Row ", rowIdx, " doesn't sum up to 1."))
# }

CheckCohortFlags <- function(rowIdxInProcessedData, vec_tblcoh, processedData)
{
  record_idx <- as.data.frame(processedData)[rowIdxInProcessedData, recordIDColName]
  subCohort <- subCohortNames[vec_tblcoh[record_idx]]
  otherCohorts <- subCohortNames[subCohortNames != subCohort]
  
  if ((processedData[rowIdxInProcessedData, subCohort] != 1) | 
    (any(processedData[rowIdxInProcessedData, otherCohorts])))
    stop(paste0("Error! row ", rowIdxInProcessedData, " has incorrect sub-cohort flags."))
}

# check whether each record is only in one cohort
lapply(
  (1:nrow(cohortFlagAppendedData)), 
  CheckCohortFlags, 
  vec_tblcoh=rawData$tblcoh, 
  processedData=cohortFlagAppendedData[, c(recordIDColName, subCohortNames)]
  )

# remove the BConti flag as the reference
cohortFlagAppendedData <- cohortFlagAppendedData %>% select(-BConti)




# # check whether the cohorts are the same as what Jie sent me
# cohort4Compare_Lichao <- tbl_df(read.csv(paste0(inputDir4DS, "Cmp.csv"))) %>% select(-record_num)
# cohort4Compare_Jie <- 
#   tbl_df(read.csv("F:/Lichao/work/Projects/MultipleSclerosis/Results/2016-07-05/cohortDt_lichao_July06/Cmp.csv"))
# if (!all.equal(cohort4Compare_Lichao, cohort4Compare_Jie))
#   stop("Error! the cohorts from 05Jul and 07Jul aren't consistent!")

# # use the new category of edss score related variables to replace those old ones
# new_edss_score_cate <- rawData[cohortFlagAppendedData$record_num, ] %>%
#   mutate(baseline_edss_score__1_4 = as.numeric(between(baseline_edss_score, 1, 4)),
#          baseline_edss_score__gt4 = as.numeric(baseline_edss_score > 4)) %>%
#   mutate(
#     pre1_edss_score__0 = ifelse(is.na(pre1_edss_score), 0, pre1_edss_score %in% c(0)),
#     pre1_edss_score__1_4 = ifelse(is.na(pre1_edss_score), 0,  between(pre1_edss_score, 1, 4)),
#     pre1_edss_score__gt4 = ifelse(is.na(pre1_edss_score), 0, pre1_edss_score > 4)
#   ) %>%
#   mutate(
#     pre2_edss_score__0 = ifelse(is.na(pre2_edss_score), 0, pre2_edss_score %in% c(0)),
#     pre2_edss_score__1_4 = ifelse(is.na(pre2_edss_score), 0,  between(pre2_edss_score, 1, 4)),
#     pre2_edss_score__gt4 = ifelse(is.na(pre2_edss_score), 0, pre2_edss_score > 4)
#   ) %>%
#   mutate(
#     pre3_edss_score__0 = ifelse(is.na(pre3_edss_score), 0, pre3_edss_score %in% c(0)),
#     pre3_edss_score__1_4 = ifelse(is.na(pre3_edss_score), 0,  between(pre3_edss_score, 1, 4)),
#     pre3_edss_score__gt4 = ifelse(is.na(pre3_edss_score), 0, pre3_edss_score > 4)
#   )
# 
# cohortFlagAppendedData1 <- cohortFlagAppendedData %>%
# {
#   dtLastStep <- .
#   oldVars <- grep("edss_score", names(dtLastStep), value = T)
#   newVars <- grep('edss_score__', names(new_edss_score_cate), value = T)
#   newDt <- dtLastStep %>% select(-one_of(oldVars)) %>% bind_cols(new_edss_score_cate[, newVars])
#   newDt
# }
#
## save

# re arrange the columns

sortedVarNames <- sort(colnames(cohortFlagAppendedData))
restVarNames <- sortedVarNames[!(sortedVarNames %in% c(recordIDColName, outcomes))]
cohortFlagAppendedData <- cohortFlagAppendedData[, c(recordIDColName, outcomes, restVarNames)]

#

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

write.table(cohortFlagAppendedData, paste0(resultDir, "Cmp4Model.csv"), sep=",", 
            row.names=F)

