library(dplyr)

rm(list=ls())

rawDataFile <- 
  "F:/Lichao/work/Projects/MultipleSclerosis/data/2016-07-01/MS_decsupp_analset_20160701.csv"


outcomes <- c("relapse_fu_any_01", "edssprog", "edssconf3",
              "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")
cohNames <- c("Cmp", "BConti", "B2B", "B2Fir", "B2Sec")
missing_reps <- c('', 'NA', 'unknown', 'ambiguous')

preDmtPrefixes <- c("rx_fing_", "rx_ga_", "rx_nat_", "rx_ext_", "rx_avo_", 
                    "rx_reb_", "rx_bet_", "rx_tecf_", "rx_teri_", "rx_alem_")
preDmtBoolVals_1 <- paste0(preDmtPrefixes, "1")
preDmtBoolVals_2 <- paste0(preDmtPrefixes, "2")
preDmtBoolVals_3 <- paste0(preDmtPrefixes, "3")
preDmtBoolVals_4 <- paste0(preDmtPrefixes, "4")

rawData <- tbl_df(read.csv(rawDataFile, na.string=missing_reps))

rawNames <- colnames(rawData)

vars2Copy <- outcomes

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


for(coh in cohNames){
  refData4ModelFile <- 
    paste0("F:/Jie/MS/03_Result/2016-07-25/2016-07-25 09.29.06/", coh, "4Model.csv")
  refData <- tbl_df(read.csv(refData4ModelFile, na.string=missing_reps))
  refData2 <- tbl_df(as.data.frame(sapply(refData, as.numeric)))
  
  resultData <- mutate(rawData, record_num = 1:nrow(rawData)) %>%
    filter(record_num %in% refData$record_num) %>%
    # impute years_diag_idx
    mutate(years_diag_idx_imputed = ifelse(!is.na(years_diag_idx), years_diag_idx, median(years_diag_idx, na.rm=T))) %>%
    select(-years_diag_idx) %>%
    dplyr::rename(years_diag_idx=years_diag_idx_imputed) %>%
    # "age__31to40","age__41to50","age__ge51"
    mutate(age__31to40 = as.numeric((age >= 31) & (age <= 40)),
           age__41to50 = as.numeric((age >= 41) & (age <= 50)), 
           age__ge51 = as.numeric((age >= 51))) %>%
    # "avl_idx_alem__1","avl_idx_fing__1","avl_idx_tecf__1","avl_idx_teri__1"
    mutate(
      avl_idx_alem__1 = avl_idx_alem,
      avl_idx_fing__1 = avl_idx_fing,
      avl_idx_tecf__1 = avl_idx_tecf, 
      avl_idx_teri__1 = avl_idx_teri
    ) %>% 
    # "B2B","B2Fir","B2Sec"
    mutate(B2B = as.numeric(tblcoh == 2), 
           B2Fir = as.numeric(tblcoh == 3),
           B2Sec = as.numeric(tblcoh == 4)) %>%
    # "baseline_edss_score__1d5_2","baseline_edss_score__ge2d5"
    mutate(baseline_edss_score__1_4 = as.numeric(between(baseline_edss_score, 1, 4)),
           baseline_edss_score__gt4 = as.numeric(baseline_edss_score > 4)) %>%
    # "birth_region__missing","birth_region__others"
    mutate(birth_region__missing = as.numeric(is.na(birth_region)), 
           birth_region__others = ifelse(is.na(birth_region), 0, birth_region != "Central Europe")) %>%
    # "dayssup__gt360"
    mutate(dayssup = ifelse(is.na(precont_dayssup), switch_rx_dayssup, precont_dayssup)) %>%
    mutate(dayssup__gt360 = ifelse(dayssup > 360, 1, 0)) %>%
    select(-dayssup) %>%
    # gender__M
    mutate(gender__M = as.numeric(gender == "M")) %>%
    # "last_cranial_num__gt8","last_cranial_num__le8"
    mutate(last_cranial_num__gt8 = ifelse(is.na(last_cranial_num), 0, last_cranial_num==">8"),
           last_cranial_num__le8 = ifelse(is.na(last_cranial_num), 0, last_cranial_num %in% c("0", "1", "2-5", "6-8"))) %>%
    # "last_spinal_num__gt2","last_spinal_num__le2"
    mutate(last_spinal_num__gt2 = ifelse(is.na(last_spinal_num), 0, last_spinal_num==">2"),
           last_spinal_num__le2 = ifelse(is.na(last_spinal_num), 0, last_spinal_num %in% c("0", "1", "2"))) %>%
    # "pre_dmts_1__2","pre_dmts_2__1_OR_2_OR_3","pre_dmts_3__1_OR_2","pre_dmts_4__1_OR_2_OR_3"
    mutate(
      rx_fing_1 = ifelse(idx_rx==1, 0, rx_fing_1),
      rx_fing_2 = ifelse(idx_rx==1, 0, rx_fing_2),
      rx_fing_3 = ifelse(idx_rx==1, 0, rx_fing_3),
      rx_fing_4 = ifelse(idx_rx==1, 0, rx_fing_4),
      
      rx_ga_1 = ifelse(idx_rx==2, 0, rx_ga_1),
      rx_ga_2 = ifelse(idx_rx==2, 0, rx_ga_2),
      rx_ga_3 = ifelse(idx_rx==2, 0, rx_ga_3),
      rx_ga_4 = ifelse(idx_rx==2, 0, rx_ga_4),
      
      rx_nat_1 = ifelse(idx_rx==3, 0, rx_nat_1),
      rx_nat_2 = ifelse(idx_rx==3, 0, rx_nat_2),
      rx_nat_3 = ifelse(idx_rx==3, 0, rx_nat_3),
      rx_nat_4 = ifelse(idx_rx==3, 0, rx_nat_4),
      
      rx_ext_1 = ifelse(idx_rx==4, 0, rx_ext_1),
      rx_ext_2 = ifelse(idx_rx==4, 0, rx_ext_2),
      rx_ext_3 = ifelse(idx_rx==4, 0, rx_ext_3),
      rx_ext_4 = ifelse(idx_rx==4, 0, rx_ext_4),
      
      rx_bet_1 = ifelse(idx_rx==5, 0, rx_bet_1),
      rx_bet_2 = ifelse(idx_rx==5, 0, rx_bet_2),
      rx_bet_3 = ifelse(idx_rx==5, 0, rx_bet_3),
      rx_bet_4 = ifelse(idx_rx==5, 0, rx_bet_4),
      
      rx_avo_1 = ifelse(idx_rx==6, 0, rx_avo_1),
      rx_avo_2 = ifelse(idx_rx==6, 0, rx_avo_2),
      rx_avo_3 = ifelse(idx_rx==6, 0, rx_avo_3),
      rx_avo_4 = ifelse(idx_rx==6, 0, rx_avo_4),
      
      rx_reb_1 = ifelse(idx_rx==7, 0, rx_reb_1),
      rx_reb_2 = ifelse(idx_rx==7, 0, rx_reb_2),
      rx_reb_3 = ifelse(idx_rx==7, 0, rx_reb_3),
      rx_reb_4 = ifelse(idx_rx==7, 0, rx_reb_4),
      
      rx_tecf_1 = ifelse(idx_rx==10, 0, rx_tecf_1),
      rx_tecf_2 = ifelse(idx_rx==10, 0, rx_tecf_2),
      rx_tecf_3 = ifelse(idx_rx==10, 0, rx_tecf_3),
      rx_tecf_4 = ifelse(idx_rx==10, 0, rx_tecf_4),
      
      rx_teri_1 = ifelse(idx_rx==11, 0, rx_teri_1),
      rx_teri_2 = ifelse(idx_rx==11, 0, rx_teri_2),
      rx_teri_3 = ifelse(idx_rx==11, 0, rx_teri_3),
      rx_teri_4 = ifelse(idx_rx==11, 0, rx_teri_4),
      
      rx_alem_1 = ifelse(idx_rx==13, 0, rx_alem_1),
      rx_alem_2 = ifelse(idx_rx==13, 0, rx_alem_2),
      rx_alem_3 = ifelse(idx_rx==13, 0, rx_alem_3),
      rx_alem_4 = ifelse(idx_rx==13, 0, rx_alem_4)
    ) %>%
    {
      eval(parse(text=paste0(
        "mutate(., pre_dmts_1=", paste(preDmtBoolVals_1, collapse="+"), ")"
      )))
    } %>%
    {
      eval(parse(text=paste0(
        "mutate(., pre_dmts_2=", paste(preDmtBoolVals_2, collapse="+"), ")"
      )))
    } %>%
    {
      eval(parse(text=paste0(
        "mutate(., pre_dmts_3=", paste(preDmtBoolVals_3, collapse="+"), ")"
      )))
    } %>%
    {
      eval(parse(text=paste0(
        "mutate(., pre_dmts_4=", paste(preDmtBoolVals_4, collapse="+"), ")"
      )))
    } %>%
    # "pre_dmts_1__2","pre_dmts_2__1_OR_2_OR_3","pre_dmts_3__1_OR_2","pre_dmts_4__1_OR_2_OR_3"
    mutate(
      pre_dmts_1__2 = as.numeric(pre_dmts_1==2),
      pre_dmts_2__1_OR_2_OR_3 = as.numeric(pre_dmts_2 %in% c(1,2,3)),
      pre_dmts_3__1_OR_2 = as.numeric(pre_dmts_3 %in% c(1,2)),
      pre_dmts_4__1_OR_2_OR_3 = as.numeric(pre_dmts_4 %in% c(1,2,3))
    ) %>%
    select(-pre_dmts_1, -pre_dmts_2, -pre_dmts_3, -pre_dmts_4) %>%
    #   # because pre_dmts don't seem to affect the importance of dayssup, just copy from refData
    #   mutate(pre_dmts_1__2 = refData$pre_dmts_1__2[order(refData$record_num)], 
    #          pre_dmts_2__1_OR_2_OR_3 = refData$pre_dmts_2__1_OR_2_OR_3[order(refData$record_num)], 
    #          pre_dmts_3__1_OR_2 = refData$pre_dmts_3__1_OR_2[order(refData$record_num)], 
    #          pre_dmts_4__1_OR_2_OR_3 = refData$pre_dmts_4__1_OR_2_OR_3[order(refData$record_num)]) %>%
    # "pre1_edss_score__0_1","pre1_edss_score__1d5_2","pre1_edss_score__ge2d5"
    mutate(
      pre1_edss_score__0 = ifelse(is.na(pre1_edss_score), 0, pre1_edss_score %in% c(0)),
      pre1_edss_score__1_4 = ifelse(is.na(pre1_edss_score), 0,  between(pre1_edss_score, 1, 4)),
      pre1_edss_score__gt4 = ifelse(is.na(pre1_edss_score), 0, pre1_edss_score > 4)
    ) %>%
    # "pre1_edssconf3__0","pre1_edssconf3__1","pre1_edssprog__0","pre1_edssprog__1"
    mutate(
      pre1_edssconf3__0 = ifelse(is.na(pre1_edssconf3), 0, pre1_edssconf3 == 0),
      pre1_edssconf3__1 = ifelse(is.na(pre1_edssconf3), 0, pre1_edssconf3 == 1), 
      pre1_edssprog__0 = ifelse(is.na(pre1_edssprog), 0, pre1_edssprog == 0),
      pre1_edssprog__1 = ifelse(is.na(pre1_edssprog), 0, pre1_edssprog == 1)
    ) %>%
    # "pre2_edss_score__0_1","pre2_edss_score__1d5_2","pre2_edss_score__ge2d5"
    mutate(
      pre2_edss_score__0 = ifelse(is.na(pre2_edss_score), 0, pre2_edss_score %in% c(0)),
      pre2_edss_score__1_4 = ifelse(is.na(pre2_edss_score), 0,  between(pre2_edss_score, 1, 4)),
      pre2_edss_score__gt4 = ifelse(is.na(pre2_edss_score), 0, pre2_edss_score > 4)
    ) %>%
    # "pre2_edssconf3__0","pre2_edssconf3__1","pre2_edssprog__0","pre2_edssprog__1"
    mutate(
      pre2_edssconf3__0 = ifelse(is.na(pre2_edssconf3), 0, pre2_edssconf3 == 0),
      pre2_edssconf3__1 = ifelse(is.na(pre2_edssconf3), 0, pre2_edssconf3 == 1), 
      pre2_edssprog__0 = ifelse(is.na(pre2_edssprog), 0, pre2_edssprog == 0),
      pre2_edssprog__1 = ifelse(is.na(pre2_edssprog), 0, pre2_edssprog == 1)
    ) %>%
    # "pre3_edss_score__0_1","pre3_edss_score__1d5_2","pre3_edss_score__ge2d5"
    mutate(
      pre3_edss_score__0 = ifelse(is.na(pre3_edss_score), 0, pre3_edss_score %in% c(0)),
      pre3_edss_score__1_4 = ifelse(is.na(pre3_edss_score), 0,  between(pre3_edss_score, 1, 4)),
      pre3_edss_score__gt4 = ifelse(is.na(pre3_edss_score), 0, pre3_edss_score > 4)
    ) %>%
    # "pre3_edssconf3__0","pre3_edssconf3__1","pre3_edssprog__0","pre3_edssprog__1"
    mutate(
      pre3_edssconf3__0 = ifelse(is.na(pre3_edssconf3), 0, pre3_edssconf3 == 0),
      pre3_edssconf3__1 = ifelse(is.na(pre3_edssconf3), 0, pre3_edssconf3 == 1), 
      pre3_edssprog__0 = ifelse(is.na(pre3_edssprog), 0, pre3_edssprog == 0),
      pre3_edssprog__1 = ifelse(is.na(pre3_edssprog), 0, pre3_edssprog == 1)
    ) %>%
    # "relapse_pre_181to360_01__1","relapse_pre_1to2_01__1","relapse_pre_2to3_01__1",
    # "relapse_pre_3to4_01__1","relapse_pre_90_01__1","relapse_pre_91to180_01__1"
    mutate(relapse_pre_181to360_01__1 = relapse_pre_181to360_01,
           relapse_pre_1to2_01__1 = relapse_pre_1to2_01, 
           relapse_pre_2to3_01__1 = relapse_pre_2to3_01,
           relapse_pre_3to4_01__1 = relapse_pre_3to4_01,
           relapse_pre_90_01__1 = relapse_pre_90_01,
           relapse_pre_91to180_01__1 = relapse_pre_91to180_01) %>%
    # "years_diag_idx__gt2_le5","years_diag_idx__gt5"
    mutate(years_diag_idx__gt2_le5 = as.numeric((years_diag_idx > 2) & (years_diag_idx <= 5)),
           years_diag_idx__gt5 = as.numeric(years_diag_idx > 5)) %>%
    # remove variables not to be kept in the result
           {
             names2Remove <- rawNames[!(rawNames %in% vars2Copy)]
             select(., -one_of(names2Remove))
           }
  
  
  sortedVarNames <- sort(colnames(resultData))
  restVarNames <- sortedVarNames[!(sortedVarNames %in% c("record_num", outcomes))]
  resultData <- resultData[, c("record_num", outcomes, restVarNames)]
  
  #
  
  
  
  print("checking..")
  
  CompareOneCol <- function(columnName, df1, df2, recordColName)
  {
    if (columnName == recordColName)
    {
      if (!all.equal(sort(as.data.frame(df1)[, columnName]), sort(as.data.frame(df2)[, columnName])))
        stop("Error! record_num inconsistent!")
      else 
      {
        return (T)
      }
    }
    select(df1, one_of(c(recordColName, columnName))) %>%
      rename_(V1 = columnName) %>%
      left_join(
        select(df2, one_of(c(recordColName, columnName))) %>%
          rename_(V2 = columnName), 
        by=recordColName
      ) %>%
      mutate(difference = (as.numeric(V1) - as.numeric(V2))) %>%
      summarise(flag=any(difference != 0)) %>%
      {
        if (.$flag[1])
          stop(paste0("Error! ", columnName, " inconsistent with ref result!"))
      }
    
    T
    
  }
  
#   if (!(all.equal(colnames(resultData), colnames(refData))))
#     stop("Error! resultData and refData have different column names!")
#   
  # lapply(colnames(resultData), CompareOneCol, df1=resultData, df2=refData, recordColName="record_num")
  
  
  
  # for (iRowInResult in 1:nrow(resultData))
  # {
  #   for (iVar in 1:ncol(resultData))
  #   {
  #     r_id <- resultData$record_num[iRowInResult]
  #     valResult <- as.numeric(resultData[iRowInResult, iVar])
  #     valRef <- as.numeric(refData[refData$record_num==r_id, iVar])
  #     difference <- valResult - valRef
  #     if (difference != 0)
  #     {
  #       stop(paste0("Error! iRowInResult: ", iRowInResult, "; iVar: ", iVar))
  #     }
  #   }
  # }
  print("checking passed.")
  
  
  write.table(resultData, paste0(resultDir, coh, "4Model.csv"), sep=",", 
              row.names=F)
  
  
  
}



