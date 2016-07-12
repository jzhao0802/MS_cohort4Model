library(dplyr)

rm(list=ls())

#
# 1. merge some categories
# 2. change some reference categories
#
#
#




print("Attention! The script should be adjusted for each cohort specifically.")

bTest <- T

inputFile <- 
  "F:/Lichao/work/Projects/MultipleSclerosis/Results/2016-07-11/2016-07-11 20.23.00/Cmp4Model.csv"

outcomes <- c("relapse_fu_any_01", "edssprog", "edssconf3",
              "relapse_or_prog", "relapse_and_prog", "relapse_or_conf")

varNames2Merge <- c(
  "relapse_pre_90_01__1","relapse_pre_91to180_01__1","relapse_pre_181to360_01__1",
  "relapse_pre_1to2_01__1","relapse_pre_2to3_01__1","relapse_pre_3to4_01__1",
  "baseline_edss_score__1d5_2","baseline_edss_score__ge2d5"
  )
nNewVars <- 3

inputData <- tbl_df(read.csv(inputFile))

resultData <- 
  # relapse_pre_90_01__1, relapse_pre_91to180_01__1 and relapse_pre_181to360_01__1
  mutate(
    inputData, 
    relapse_pre_0to1_01__1 = 
      as.numeric(relapse_pre_90_01__1 | relapse_pre_91to180_01__1 | relapse_pre_181to360_01__1)
    ) %>%
  # check
  {
    if (bTest)
    {
      if (all.equal(
        .$relapse_pre_0to1_01__1,
        apply(
          .[, c("relapse_pre_90_01__1",
                 "relapse_pre_91to180_01__1",
                 "relapse_pre_181to360_01__1")],
          1,
          function(x) as.numeric(any(x))
        )
      ) != T)
        stop(paste0("Error! Incorrected relapse_pre_0to1_01__1 value!"))
    }
    .
  } %>%
  select(-one_of(c("relapse_pre_90_01__1","relapse_pre_91to180_01__1","relapse_pre_181to360_01__1"))) %>%
  # relapse_pre_1to2_01__1, relapse_pre_2to3_01__1 and relapse_pre_3to4_01__1
  mutate(
    relapse_pre_1to4_01__1 = 
      as.numeric(relapse_pre_1to2_01__1 | relapse_pre_2to3_01__1 | relapse_pre_3to4_01__1)
  ) %>%
  {
    if (bTest)
    {
      if (all.equal(
        .$relapse_pre_1to4_01__1,
        apply(
          .[, c("relapse_pre_1to2_01__1",
                 "relapse_pre_2to3_01__1",
                 "relapse_pre_3to4_01__1")],
          1,
          function(x) as.numeric(any(x))
        )
      ) != T)
        stop(paste0("Error! row ", i, " has an incorrected relapse_pre_1to4_01__1 value!"))
    }
    .
  } %>%
  select(-one_of(c("relapse_pre_1to2_01__1","relapse_pre_2to3_01__1","relapse_pre_3to4_01__1"))) %>%
  # baseline_edss_score__1d5_2 and baseline_edss_score__ge2d5 
  mutate(
    baseline_edss_score__ge1d5 = 
      as.numeric(baseline_edss_score__1d5_2 | baseline_edss_score__ge2d5)
  ) %>%
  {
    if (bTest)
    {
      if (all.equal(
        .$baseline_edss_score__ge1d5,
        apply(
          .[, c("baseline_edss_score__1d5_2",
                     "baseline_edss_score__ge2d5")], 
          1, 
          function(x) as.numeric(any(x))
          )
      ) != T)
        stop(paste0("Error! row ", i, " has an incorrected baseline_edss_score__ge1d5 value!"))
    }
    .
  } %>%
  select(-one_of(c("baseline_edss_score__1d5_2","baseline_edss_score__ge2d5"))) %>%
  # Change the ref category of birth_region
  mutate(
    birth_region__Central_Europe = 
      as.numeric(!(birth_region__missing | birth_region__others))
    ) %>%
  {
    if (bTest)
    {
      dataLastStep <- .
      if (all.equal(.$birth_region__Central_Europe, 
                     apply(
                       dataLastStep[, c(
                         "birth_region__missing", "birth_region__others"
                         )], 
                       1, 
                       function(x) as.numeric(!any(x))
                     )) != T)
        stop("Error! birth_region__Central_Europe generated incorrectly.")
    }
    .
  } %>%
  select(-birth_region__missing) %>%
  {
    if (bTest)
    {
      if (ncol(.) != (ncol(inputData) - length(varNames2Merge) + nNewVars))
        stop("Error! Incorrect total number of variables after merge")
    }
    .
  }




if (bTest) print("All test passed successfully. ") else print("finished without testing.")

sortedNames <- sort(colnames(resultData))
otherPredNames <- 
  sortedNames[!(sortedNames %in% c("record_num", outcomes))]
resultData <- resultData[, c("record_num", outcomes, otherPredNames)]


timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

write.table(resultData, paste0(resultDir, "Cmp4Model_VarChanged.csv"), sep=",", 
            row.names=F)

