# Here are the tasks for you after reviewing NonRegularisedGLM: investigate the impact of some new variable groups. 
# 
# Group 1: init_symptom (9 boolean vars)
# Group 2: relapse_pre0_psev3 (years 1 - 4), for each year breaking into at most 3 categories including missing
# Group 3: pre0_deg_disab (years 1 - 4), for each year breaking into at most 3 categories including missing
#
library(dplyr)
cmpJieDir <- "F:\\Jie\\MS\\01_Data\\ModelData\\CohDt_Jie_Jul08\\"
cmpJieFile <- "dt_Cmp_withTransf_seed20"

cmp4ModelDir <- "F:\\Lichao\\work\\Projects\\MultipleSclerosis\\Results\\2016-07-11\\2016-07-11 20.07.54\\mergeCateg\\"
cmp4ModelFile <- "Cmp4Model"

outDir <- './modelDt4InvestNewVarGroups/'
timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
outDir <- paste(outDir, timeStamp, "/", sep = '')

if(!dir.exists(outDir)){
  dir.create(outDir)
}

cmpJie <- read.table(paste0(cmpJieDir, cmpJieFile, '.csv')
                     , sep=','
                     , header = T
                     , stringsAsFactors = F)
cmp4Model <- read.table(paste0(cmp4ModelDir, cmp4ModelFile, '.csv')
                     , sep=','
                     , header = T
                     , stringsAsFactors = F)

# Group 1:init_symptom (9 boolean vars)
init_symVars <- grep('^init_', names(cmpJie), value = T)
init_symDt <- cmpJie[, init_symVars] %>%
{
  .[is.na(.)] <- 0
  .
} %>%
{
  tb_check_sym <<- lapply(init_symVars, function(var){
    table(.[, var])
  })
  .
} 

cmp4ModelAddSym <- inner_join(cmp4Model, cbind(init_symDt, record_num=cmpJie$record_num), by="record_num") %>%
{
  naNum <- sum(apply(apply(., 2, is.na), 2, sum))
  if(naNum > 0){
    stop('having NA!\n\n')
  }
  .
}

write.table(cmp4ModelAddSym
            , paste0(outDir, 'cmp4ModelAddSym.csv')
            , row.names = F
            , sep=',')


# Group 2: relapse_pre0_psev3 (years 1 - 4), for each year breaking into at most 3 categories including missing
relapse_pre0_psev3Vars <- grep("^relapse_pre0_psev3", names(cmpJie), value = T)

havingRefCheck <- table(apply(cmpJie[, relapse_pre0_psev3Vars], 1, sum))
# havingRefCheck
# 1 
# 4354

distr <- sapply(cmpJie[, relapse_pre0_psev3Vars], sum)
# relapse_pre0_psev3__0 
# 4332 
# relapse_pre0_psev3__1_OR_2 
# 22 
varsRmRef <- relapse_pre0_psev3Vars[-which(distr==max(distr))]
relapse_pre0_psev3Dt <- cmpJie[, varsRmRef]
if(length(varsRmRef)==1){
  relapse_pre0_psev3Dt <- data.frame(relapse_pre0_psev3Dt)
  names(relapse_pre0_psev3Dt) <- varsRmRef
  
}

cmp4ModelAddRelapse_pre0_psev3 <- inner_join(cmp4Model, cbind(relapse_pre0_psev3Dt, record_num=cmpJie$record_num), by="record_num") %>%
{
  naNum <- sum(apply(apply(., 2, is.na), 2, sum))
  if(naNum > 0){
    stop('having NA!\n\n')
  }
  .
}

write.table(cmp4ModelAddRelapse_pre0_psev3
            , paste0(outDir, 'cmp4ModelAddRelapse_pre0_psev3.scv')
            , row.names = F
            , sep=',')

# Group 3: pre0_deg_disab (years 1 - 4), for each year breaking into at most 3 categories including missing
pre0_qdeg_disabVars <- grep("^pre0_qdeg_disab", names(cmpJie), value = T)

havingRefCheck <- table(apply(cmpJie[, pre0_qdeg_disabVars], 1, sum))
# havingRefCheck
# 1 
# 4354

distr <- sapply(cmpJie[, pre0_qdeg_disabVars], sum)
# pre0_qdeg_disab__0_1 
# 2603 
# pre0_qdeg_disab__2_4 
# 478 
# pre0_qdeg_disab__missing 
# 1273 
missIdx <- grep("__missing$", pre0_qdeg_disabVars)
if(length(missIdx)==1){
  varsRmRef <- pre0_qdeg_disabVars[-missIdx]

}else if(length(missIdx)==0){
  varsRmRef <- pre0_qdeg_disabVars[-which(distr==max(distr))]
  
}
pre0_qdeg_disabDt <- cmpJie[, varsRmRef]

if(length(varsRmRef)==1){
  pre0_qdeg_disabDt <- data.frame(pre0_qdeg_disabDt)
  names(pre0_qdeg_disabDt) <- varsRmRef
  
}

cmp4ModelAddPre0_qdeg_disab <- inner_join(cmp4Model, cbind(pre0_qdeg_disabDt, record_num=cmpJie$record_num), by="record_num") %>%
{
  naNum <- sum(apply(apply(., 2, is.na), 2, sum))
  if(naNum > 0){
    stop('having NA!\n\n')
  }
  .
}

write.table(cmp4ModelAddPre0_qdeg_disab
            , paste0(outDir, 'cmp4ModelAddPre0_qdeg_disab.csv')
            , row.names = F
            , sep=',')





