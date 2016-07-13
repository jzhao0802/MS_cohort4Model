rm(list=ls())
library(dplyr)


modelDataDir <- "F:\\Lichao\\work\\Projects\\MultipleSclerosis\\Results\\2016-07-11\\2016-07-11 20.07.54\\"

RayDataDir <- "F:\\Jie\\MS\\01_Data\\"

RayFile <- "MS_decsupp_analset_20160701.csv"

modelDataFile <- 'Cmp4Model.csv'

outcomeLst <- c("edssprog"
                , 'edssconf3'
                , 'relapse_fu_any_01'
                , 'relapse_or_prog'
                , 'relapse_and_prog'
                , 'relapse_or_conf')

na_represents <- c('', 'NA', 'unknown', 'ambiguous')



modelDt <- read.table(paste0(modelDataDir, modelDataFile)
                      , sep=','
                      , header = T
                      , stringsAsFactors = F
                      )


rayDt <- read.table(paste0(RayDataDir, RayFile)
                    , sep=','
                    , header = T
                    , stringsAsFactors = F
                    , na.strings = na_represents
)


rayDt <- rayDt[modelDt$record_num,]

# QC for each column
varLst <- names(modelDt)
# response variables
modelDt_resp <- modelDt[, outcomeLst]
row.names(modelDt_resp) <- NULL
rayDt_resp <- rayDt[, outcomeLst]
row.names(rayDt_resp) <- NULL
bEq <- all.equal(modelDt_resp
          , rayDt_resp
          , ignore_col_order=T
          , ignore_row_order=T
          , convert=T)
if(!bEq){
    stop("response variables are not consistent!\n")
}
# age
varAge <- grep('^age__', varLst, value=T, ignore.case = T)
if(any(apply(modelDt[, varAge], 1, sum) > 1))
    stop("error! have overlapped categories!\n\n")

cohDt_new1 <- rayDt %>%
{
    dtReplaeNA <- .
    dtReplaeNA[is.na(dtReplaeNA)] <- 1e9
    dtReplaeNA
} %>%
    #age
    
{
    ageVarLst <- grep('^age__', varLst, value = T, ignore.case = T)
    if(any(apply(modelDt[, varAge], 1, sum) > 1))
        stop("error! have overlapped categories!\n\n")
    .
} %>%
    
    #     mutate(age__le30=as.numeric(age<=30)) %>%
    mutate(age__31to40=as.numeric((age<=40)&(age>=31))) %>%
    mutate(age__41to50=as.numeric(age>=41 & age<=50)) %>%
    mutate(age__ge51=as.numeric(age>=51)) %>%
    select(-age) %>%
    # avl_idx
    {
#         speVarLst <- grep('^avl_idx', varLst, value = T, ignore.case = T)
#         if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
#             stop("error! have overlapped categories!\n\n")
        avlVarLst <<- grep("avl_idx", names(.), value=T)
        .
    } %>%
    
    mutate(avl_idx_alem__1=as.numeric(avl_idx_alem==1)) %>%
    mutate(avl_idx_fing__1=as.numeric(avl_idx_fing==1)) %>%
    mutate(avl_idx_tecf__1=as.numeric(avl_idx_tecf==1)) %>%
    mutate(avl_idx_teri__1=as.numeric(avl_idx_teri==1)) %>%
    select(-one_of(avlVarLst)) %>%
    # baseline_edss_score
    {
        speVarLst <- grep('^baseline_edss_score', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
    } %>%
    # mutate(baseline_edss_score__0_1=as.numeric(baseline_edss_score<=1)) %>%
    mutate(baseline_edss_score__1d5_2=as.numeric((baseline_edss_score<=2)&(baseline_edss_score>1))) %>%
    mutate(baseline_edss_score__ge2d5=as.numeric(baseline_edss_score>=2.5)) %>%
    select(-baseline_edss_score) %>%
    
    #     {
    #         .$birth_region[is.na(.$birth_region)] <- 1e9
    #         .
    #     } %>%
    # mutate(birth_region__Central_Europe=as.numeric(birth_region=="Central Europe")) %>%
    mutate(birth_region__missing=as.numeric(birth_region==1e9)) %>%
    mutate(birth_region__others=as.numeric(birth_region!="Central Europe" & birth_region!=1e9)) %>%
    select(-birth_region) %>%
    
    # dayssup
    {
        speVarLst <- grep('^dayssup', varLst, value = T, ignore.case = T)
        if(length(speVarLst)>1){
            if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
                stop("error! have overlapped categories!\n\n")

        }
        .
    } %>%
    
    {
        dataLastStep <- .
        vec <- rep(-1, nrow(dataLastStep))
        for (i in 1:length(vec))
        {
            if (dataLastStep$switch_rx_dayssup[i]==1e9)
                vec[i] <- dataLastStep$precont_dayssup[i]
            else
                vec[i] <- dataLastStep$switch_rx_dayssup[i]
        }
        dataLastStep$dayssup <- vec
        dataLastStep %>%
            select(-one_of(c("switch_rx_dayssup", "precont_dayssup")))
        
    } %>%
    #         mutate(dayssup_le360=as.numeric(dayssup<=360)) %>%
    mutate(dayssup__gt360=as.numeric(dayssup>360)) %>%
    select(-dayssup) %>%
    
    {
        speVarLst <- grep('^last_cranial_num', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
    } %>%
    
    
    {
        result <- .
        result$last_cranial_num__gt8 <- as.numeric(result$last_cranial_num==">8")
        #         result$last_cranial_num__missing <- as.numeric(is.na(result$last_cranial_num))
        result$last_cranial_num__le8 <- as.numeric(result$last_cranial_num!=1e9 & result$last_cranial_num!=">8")
        result
        
    } %>%
    select(-last_cranial_num) %>%
    {
        speVarLst <- grep('^last_spinal_num', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
    } %>%
    
    
    {
        result <- .
        result$last_spinal_num__gt2 <- as.numeric(result$last_spinal_num==">2")
        #         result$last_spinal_num__missing <- as.numeric(is.na(result$last_spinal_num))
        result$last_spinal_num__le2 <- as.numeric( result$last_spinal_num!=1e9 & result$last_spinal_num!=">2")
        result
        
    } %>%
    select(-last_spinal_num) %>%
    
    #     {
    #         .$pre1_edss_score[is.na(.$pre1_edss_score)] <- 1e9
    #         .
    #     } %>%
    {
        speVarLst <- grep('^pre1_edss_score', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
    } %>%
    
    mutate(pre1_edss_score__0_1=as.numeric(pre1_edss_score<=1)) %>%
    mutate(pre1_edss_score__1d5_2=as.numeric((pre1_edss_score<=2)&(pre1_edss_score>1))) %>%
    mutate(pre1_edss_score__ge2d5=as.numeric(pre1_edss_score>=2.5 & pre1_edss_score<1e9)) %>%
    # mutate(pre1_edss_score__missing=as.numeric(pre1_edss_score==1e9)) %>%
    select(-pre1_edss_score) %>%
    # "pre2_edss_score", 
    {
        #         .$pre2_edss_score[is.na(.$pre2_edss_score)] <- 1e9
        #         .
        speVarLst <- grep('^pre2_edss_score', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre2_edss_score__0_1=as.numeric(pre2_edss_score<=1)) %>%
    mutate(pre2_edss_score__1d5_2=as.numeric((pre2_edss_score<=2)&(pre2_edss_score>1))) %>%
    mutate(pre2_edss_score__ge2d5=as.numeric(pre2_edss_score>=2.5 & pre2_edss_score<1e9)) %>%
    # mutate(pre2_edss_score__missing=as.numeric(pre2_edss_score==1e9)) %>%
    select(-pre2_edss_score) %>%
    # "pre3_edss_score", 
    {
        #         .$pre3_edss_score[is.na(.$pre3_edss_score)] <- 1e9
        speVarLst <- grep('^pre3_edss_score', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre3_edss_score__0_1=as.numeric(pre3_edss_score<=1)) %>%
    mutate(pre3_edss_score__1d5_2=as.numeric((pre3_edss_score<=2)&(pre3_edss_score>1))) %>%
    mutate(pre3_edss_score__ge2d5=as.numeric(pre3_edss_score>=2.5 & pre3_edss_score<1e9)) %>%
    # mutate(pre3_edss_score__missing=as.numeric(pre3_edss_score==1e9)) %>%
    select(-pre3_edss_score) %>%
    
    #pre1/2/3_edssconf3 pre1/2/3_edssprog 
    {
        speVarLst <- grep('^pre1_edssconf3', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
    } %>%
    
    mutate(pre1_edssconf3__0=as.numeric(pre1_edssconf3==0)) %>%
    mutate(pre1_edssconf3__1=as.numeric(pre1_edssconf3==1)) %>%
    select(-pre1_edssconf3) %>%
    # "pre2_edss_score", 
    {
        #         .$pre2_edss_score[is.na(.$pre2_edss_score)] <- 1e9
        #         .
        speVarLst <- grep('^pre2_edssconf3', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre2_edssconf3__0=as.numeric(pre2_edssconf3==0)) %>%
    mutate(pre2_edssconf3__1=as.numeric(pre2_edssconf3==1)) %>%
    select(-pre2_edssconf3) %>%
    {
        #         .$pre3_edss_score[is.na(.$pre3_edss_score)] <- 1e9
        speVarLst <- grep('^pre3_edssconf3', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre3_edssconf3__0=as.numeric(pre3_edssconf3==0)) %>%
    mutate(pre3_edssconf3__1=as.numeric(pre3_edssconf3==1)) %>%
    select(-pre3_edssconf3) %>%
    
    {
        #         .$pre3_edss_score[is.na(.$pre3_edss_score)] <- 1e9
        speVarLst <- grep('^pre1_edssprog', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre1_edssprog__0=as.numeric(pre1_edssprog==0)) %>%
    mutate(pre1_edssprog__1=as.numeric(pre1_edssprog==1)) %>%
    select(-pre1_edssprog) %>%
    
    {
        #         .$pre3_edss_score[is.na(.$pre3_edss_score)] <- 1e9
        speVarLst <- grep('^pre2_edssprog', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre2_edssprog__0=as.numeric(pre2_edssprog==0)) %>%
    mutate(pre2_edssprog__1=as.numeric(pre2_edssprog==1)) %>%
    select(-pre2_edssprog) %>%
    
    {
        #         .$pre3_edss_score[is.na(.$pre3_edss_score)] <- 1e9
        speVarLst <- grep('^pre3_edssprog', varLst, value = T, ignore.case = T)
        if(any(apply(modelDt[, speVarLst], 1, sum) > 1))
            stop("error! have overlapped categories!\n\n")
        .
        
    } %>%
    mutate(pre3_edssprog__0=as.numeric(pre3_edssprog==0)) %>%
    mutate(pre3_edssprog__1=as.numeric(pre3_edssprog==1)) %>%
    select(-pre3_edssprog) %>%
    
    # years_diag_idx
    #     {
    #         .$years_diag_idx[is.na(.$years_diag_idx)] <- 1e9
    #         .
    #     } %>%
    # mutate(years_diag_idx__le2=as.numeric(years_diag_idx<=2)) %>%
    mutate(years_diag_idx__gt2_le5=as.numeric((years_diag_idx>2)&(years_diag_idx<=5))) %>%
    mutate(years_diag_idx__gt5=as.numeric((years_diag_idx>5)&(years_diag_idx!=1e9))) %>% 
    {
        medianValue <- median(.$years_diag_idx[.$years_diag_idx != 1e9])
        missingRows <- which(.$years_diag_idx==1e9)
        if(medianValue>2 & medianValue<=5){
            .$years_diag_idx__gt2_le5[missingRows] <- 1
        }else if(medianValue>5){
            .$years_diag_idx__gt5[missingRows] <- 1
        }
        .
    } %>%
    # mutate(years_diag_idx__missing=as.numeric(years_diag_idx==1e9)) %>%
    select(-years_diag_idx) %>%
    
    # gender
    mutate(gender__M=as.numeric(gender=='M')) %>%
    select(-gender) %>%
    # relapse_pre
    mutate(relapse_pre_181to360_01__1=as.numeric(relapse_pre_181to360_01==1)) %>%
    mutate(relapse_pre_1to2_01__1=as.numeric(relapse_pre_1to2_01==1)) %>%
    mutate(relapse_pre_2to3_01__1=as.numeric(relapse_pre_2to3_01==1)) %>%
    mutate(relapse_pre_3to4_01__1=as.numeric(relapse_pre_3to4_01==1)) %>%
    mutate(relapse_pre_90_01__1=as.numeric(relapse_pre_90_01==1)) %>%
    mutate(relapse_pre_91to180_01__1=as.numeric(relapse_pre_91to180_01==1)) %>%
    {
        dtLastStep <- .
        specVarLst <- grep('^relapse_pre.*01$', names(.), value=T)
        dtLastStep  <- dtLastStep %>% select(-one_of(specVarLst))
        dtLastStep
    } 

vars2comp <- intersect(names(cohDt_new1), varLst)
# cohDt_new2 <- cohDt_new1[order(cohDt_new1$record_num), vars2comp]
# cohDt_old <- cohDt_old[order(cohDt_old$record_num), vars2comp]
# cohDt_new3 <- as.data.frame(t(ldply(lapply(names(cohDt_new2), function(var){
#     vct <- cohDt_new2[, var]
#     vct[is.na(vct)] <- 0
#     vct <- as.integer(vct)
#     return(vct)
# }), quickdf)))
# names(cohDt_new3) <- vars2comp



      bEq <- all.equal(cohDt_new1[, vars2comp]
                , modelDt[, vars2comp]
                , ignore_col_order = T
                , ignore_row_order = T
                , convert = T)
#       unlist(lapply(vars2comp, function(var)class(cohDt_new3[, var])==class(cohDt_old[, var])))
#       unlist(lapply(vars2comp, function(var)class(cohDt_new3[, var])))
#       unlist(lapply(vars2comp, function(var)class(cohDt_old[, var])))
#       
bEq <- unlist(lapply(vars2comp, function(var){
    return(all(cohDt_new1[, var]==modelDt[, var]))
}))
vars2comp[!bEq]

cmpDayssup <- cbind(modelDt$dayssup__gt360, cohDt_new1$dayssup__gt360)
cmpYeas_diag_idx <- cbind(modelDt$years_diag_idx__gt2_le5, cohDt_new1$years_diag_idx__gt2_le5)
unmatchedRows <- c(1:nrow(modelDt))[!apply(cmpYeas_diag_idx, 1, function(x)x[1]==x[2])]
