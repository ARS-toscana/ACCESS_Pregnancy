# load D3_included_pregnancies  
load(paste0(dirtemp,"D3_groups_of_pregnancies.RData"))

# load D3_Stream_CONCEPTSETS 
for (conceptvar in concept_set_our_study_pre){
  load(paste0(dirtemp,conceptvar,".RData"))
}

# Keep the highest quality record for each group of pregnancy

# ordering 
D3_gop<-D3_groups_of_pregnancies[order(person_id,group_identifier, order_quality, -record_date),]
# creating record number for each group of pregnancy
D3_gop<-D3_gop[,n:=seq_along(.I), by=.(group_identifier, person_id, highest_quality)]
#creating unique identifier for each group of pregnancy
D3_gop<-D3_gop[,pers_group_id:=paste0(person_id,"_", group_identifier_colored)]
# keeping the first record 
D3_gop_for_outcomes<-D3_gop[n==1 & (year(pregnancy_start_date)>=2017 & year(pregnancy_start_date)<=2019),
                            .(person_id, pers_group_id, highest_quality, pregnancy_start_date, pregnancy_end_date, type_of_pregnancy_end )]

################################################################################
##########################        Insuline       ###############################
################################################################################
# insuline during pregnancy (with no use in previous years before pregnancy from 2016)
INSULIN_first_record <- INSULIN[,.(first_date=min(date)), by = person_id]

MFC_INSULIN <- MergeFilterAndCollapse(list(INSULIN_first_record),
                                       datasetS= D3_gop_for_outcomes,
                                       key = "person_id",
                                       condition = "first_date >= pregnancy_start_date & first_date <= pregnancy_end_date",
                                       strata=c("pers_group_id"),
                                       summarystat = list(list(c("exist"), "first_date" , "INSULINE")))

pregnancy_outcomes_0 <- merge(D3_gop_for_outcomes, MFC_INSULIN, by = c("pers_group_id"), all.x = T)[is.na(INSULINE), INSULINE := 0]

################################################################################
######################    Gestational diabetes    ##############################
################################################################################
#Gestational diabetes: OR code of gest diab is btw start and end (MFC) OR (one code of insulin btw start and end AND NOT any code of insulin befor start pregnacy)

#Narrow
MFC_GESTDIAB_narrow <- MergeFilterAndCollapse(list(GESTDIAB_narrow),
                                              datasetS= D3_gop_for_outcomes,
                                              key = "person_id",
                                              condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                              strata=c("pers_group_id"),
                                              summarystat = list(list(c("exist"), "date" , "GESTDIAB_code_narrow")))
                
pregnancy_outcomes_1 <- merge(pregnancy_outcomes_0, MFC_GESTDIAB_narrow, by = c("pers_group_id"), all.x = T)[is.na(GESTDIAB_code_narrow), GESTDIAB_code_narrow := 0]

#Possible
MFC_GESTDIAB_possible <- MergeFilterAndCollapse(list(GESTDIAB_possible),
                                              datasetS= D3_gop_for_outcomes,
                                              key = "person_id",
                                              condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                              strata=c("pers_group_id"),
                                              summarystat = list(list(c("exist"), "date" , "GESTDIAB_code_possible")))

pregnancy_outcomes_2 <- merge(pregnancy_outcomes_1, MFC_GESTDIAB_possible, by = c("pers_group_id"), all.x = T)[is.na(GESTDIAB_code_possible), GESTDIAB_code_possible := 0]

### codes during pregnancy + insuline during pregnancy (with no use in previous years before pregnancy from 2016)
pregnancy_outcomes_2 <- pregnancy_outcomes_2[GESTDIAB_code_narrow== 1 |  INSULINE == 1 , GESTDIAB_narrow := 1][is.na(GESTDIAB_narrow), GESTDIAB_narrow := 0]
pregnancy_outcomes_2 <- pregnancy_outcomes_2[GESTDIAB_code_possible== 1 |  INSULINE == 1 , GESTDIAB_possible := 1][is.na(GESTDIAB_possible), GESTDIAB_possible := 0]

# View(pregnancy_outcomes_2[GESTDIAB_code_possible==1 & INSULINE ==0])
# View(pregnancy_outcomes_2[GESTDIAB_code_narrow==0 & INSULINE ==1])

################################################################################
############################      FGR      #####################################
################################################################################
#Narrow
MFC_FGR_narrow <- MergeFilterAndCollapse(list(FGR_narrow),
                                              datasetS= D3_gop_for_outcomes,
                                              key = "person_id",
                                              condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                              strata=c("pers_group_id"),
                                              summarystat = list(list(c("exist"), "date" , "FGR_narrow")))

pregnancy_outcomes_3 <- merge(pregnancy_outcomes_2, MFC_FGR_narrow, by = c("pers_group_id"), all.x = T)[is.na(FGR_narrow), FGR_narrow := 0]

#Possible
MFC_FGR_possible <- MergeFilterAndCollapse(list(FGR_possible),
                                         datasetS= D3_gop_for_outcomes,
                                         key = "person_id",
                                         condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                         strata=c("pers_group_id"),
                                         summarystat = list(list(c("exist"), "date" , "FGR_possible")))

pregnancy_outcomes_4 <- merge(pregnancy_outcomes_3, MFC_FGR_possible, by = c("pers_group_id"), all.x = T)[is.na(FGR_possible), FGR_possible := 0]

################################################################################
########################       MAJORCA         #################################
################################################################################
#Narrow
MFC_MAJORCA_narrow <- MergeFilterAndCollapse(list(MAJORCA_narrow),
                                         datasetS= D3_gop_for_outcomes,
                                         key = "person_id",
                                         condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                         strata=c("pers_group_id"),
                                         summarystat = list(list(c("exist"), "date" , "MAJORCA_narrow")))

pregnancy_outcomes_5 <- merge(pregnancy_outcomes_4, MFC_MAJORCA_narrow, by = c("pers_group_id"), all.x = T)[is.na(MAJORCA_narrow), MAJORCA_narrow := 0]

#Possible
MFC_MAJORCA_possible <- MergeFilterAndCollapse(list(MAJORCA_possible),
                                             datasetS= D3_gop_for_outcomes,
                                             key = "person_id",
                                             condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                             strata=c("pers_group_id"),
                                             summarystat = list(list(c("exist"), "date" , "MAJORCA_possible")))

pregnancy_outcomes_6 <- merge(pregnancy_outcomes_5, MFC_MAJORCA_possible, by = c("pers_group_id"), all.x = T)[is.na(MAJORCA_possible), MAJORCA_possible := 0]

################################################################################
#######################      MATERNAL DEATH      ###############################
################################################################################
#Narrow
MFC_MATERNALDEATH_narrow <- MergeFilterAndCollapse(list(MATERNALDEATH_narrow),
                                             datasetS= D3_gop_for_outcomes,
                                             key = "person_id",
                                             condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                             strata=c("pers_group_id"),
                                             summarystat = list(list(c("exist"), "date" , "MATERNALDEATH_narrow")))

pregnancy_outcomes_7 <- merge(pregnancy_outcomes_6, MFC_MATERNALDEATH_narrow, by = c("pers_group_id"), all.x = T)[is.na(MATERNALDEATH_narrow), MATERNALDEATH_narrow := 0]

#Possible
MFC_MATERNALDEATH_possible <- MergeFilterAndCollapse(list(MATERNALDEATH_possible),
                                                   datasetS= D3_gop_for_outcomes,
                                                   key = "person_id",
                                                   condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                   strata=c("pers_group_id"),
                                                   summarystat = list(list(c("exist"), "date" , "MATERNALDEATH_possible")))

pregnancy_outcomes_8 <- merge(pregnancy_outcomes_7, MFC_MATERNALDEATH_possible, by = c("pers_group_id"), all.x = T)[is.na(MATERNALDEATH_possible), MATERNALDEATH_possible := 0]


################################################################################
#########################     MICROCEPHALY      ################################
################################################################################
#Narrow
MFC_MICROCEPHALY_narrow <- MergeFilterAndCollapse(list(MICROCEPHALY_narrow),
                                                   datasetS= D3_gop_for_outcomes,
                                                   key = "person_id",
                                                   condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                   strata=c("pers_group_id"),
                                                   summarystat = list(list(c("exist"), "date" , "MICROCEPHALY_narrow")))

pregnancy_outcomes_9 <- merge(pregnancy_outcomes_8, MFC_MICROCEPHALY_narrow, by = c("pers_group_id"), all.x = T)[is.na(MICROCEPHALY_narrow), MICROCEPHALY_narrow := 0]

#Possible
MFC_MICROCEPHALY_possible <- MergeFilterAndCollapse(list(MICROCEPHALY_possible),
                                                  datasetS= D3_gop_for_outcomes,
                                                  key = "person_id",
                                                  condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                  strata=c("pers_group_id"),
                                                  summarystat = list(list(c("exist"), "date" , "MICROCEPHALY_possible")))

pregnancy_outcomes_10 <- merge(pregnancy_outcomes_9, MFC_MICROCEPHALY_possible, by = c("pers_group_id"), all.x = T)[is.na(MICROCEPHALY_possible), MICROCEPHALY_possible := 0]

################################################################################
#########################     Pre eclampsia      ###############################
################################################################################
#Narrow
MFC_PREECLAMP_narrow <- MergeFilterAndCollapse(list(PREECLAMP_narrow),
                                                  datasetS= D3_gop_for_outcomes,
                                                  key = "person_id",
                                                  condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                  strata=c("pers_group_id"),
                                                  summarystat = list(list(c("exist"), "date" , "PREECLAMP_narrow")))

pregnancy_outcomes_11 <- merge(pregnancy_outcomes_10, MFC_PREECLAMP_narrow, by = c("pers_group_id"), all.x = T)[is.na(PREECLAMP_narrow), PREECLAMP_narrow := 0]

#POSSIBLE
MFC_PREECLAMP_possible <- MergeFilterAndCollapse(list(PREECLAMP_possible),
                                               datasetS= D3_gop_for_outcomes,
                                               key = "person_id",
                                               condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                               strata=c("pers_group_id"),
                                               summarystat = list(list(c("exist"), "date" , "PREECLAMP_possible")))

pregnancy_outcomes_12 <- merge(pregnancy_outcomes_11, MFC_PREECLAMP_possible, by = c("pers_group_id"), all.x = T)[is.na(PREECLAMP_possible), PREECLAMP_possible := 0]

################################################################################
########################       PRETERMBIRTH      ###############################
################################################################################
#NARROW
MFC_PRETERMBIRTH_narrow <- MergeFilterAndCollapse(list(PRETERMBIRTH_narrow),
                                               datasetS= D3_gop_for_outcomes,
                                               key = "person_id",
                                               condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                               strata=c("pers_group_id"),
                                               summarystat = list(list(c("exist"), "date" , "PRETERMBIRTH_narrow")))

pregnancy_outcomes_13 <- merge(pregnancy_outcomes_12, MFC_PRETERMBIRTH_narrow, by = c("pers_group_id"), all.x = T)[is.na(PRETERMBIRTH_narrow), PRETERMBIRTH_narrow := 0]

#POSSIBLE
MFC_PRETERMBIRTH_possible <- MergeFilterAndCollapse(list(PRETERMBIRTH_possible),
                                                  datasetS= D3_gop_for_outcomes,
                                                  key = "person_id",
                                                  condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                  strata=c("pers_group_id"),
                                                  summarystat = list(list(c("exist"), "date" , "PRETERMBIRTH_possible")))

pregnancy_outcomes_14 <- merge(pregnancy_outcomes_13, MFC_PRETERMBIRTH_possible, by = c("pers_group_id"), all.x = T)[is.na(PRETERMBIRTH_possible), PRETERMBIRTH_possible := 0]
################################################################################
####################       Spontaneous Abortion        #########################
################################################################################
#NARROW
MFC_SPONTABO_narrow <- MergeFilterAndCollapse(list(SPONTABO_narrow),
                                                  datasetS= D3_gop_for_outcomes,
                                                  key = "person_id",
                                                  condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                  strata=c("pers_group_id"),
                                                  summarystat = list(list(c("exist"), "date" , "SPONTABO_narrow")))

pregnancy_outcomes_15 <- merge(pregnancy_outcomes_14, MFC_SPONTABO_narrow, by = c("pers_group_id"), all.x = T)[is.na(SPONTABO_narrow), SPONTABO_narrow := 0]

#POSSIBLE
MFC_SPONTABO_possible <- MergeFilterAndCollapse(list(SPONTABO_possible),
                                              datasetS= D3_gop_for_outcomes,
                                              key = "person_id",
                                              condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                              strata=c("pers_group_id"),
                                              summarystat = list(list(c("exist"), "date" , "SPONTABO_possible")))

pregnancy_outcomes_16 <- merge(pregnancy_outcomes_15, MFC_SPONTABO_possible, by = c("pers_group_id"), all.x = T)[is.na(SPONTABO_possible), SPONTABO_possible := 0]

################################################################################
########################       STILLBIRTH        ###############################
################################################################################
#NARROW
MFC_STILLBIRTH_narrow <- MergeFilterAndCollapse(list(STILLBIRTH_narrow),
                                              datasetS= D3_gop_for_outcomes,
                                              key = "person_id",
                                              condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                              strata=c("pers_group_id"),
                                              summarystat = list(list(c("exist"), "date" , "STILLBIRTH_narrow")))

pregnancy_outcomes_17 <- merge(pregnancy_outcomes_16, MFC_STILLBIRTH_narrow, by = c("pers_group_id"), all.x = T)[is.na(STILLBIRTH_narrow), STILLBIRTH_narrow := 0]

#POSSIBLE
MFC_STILLBIRTH_possible <- MergeFilterAndCollapse(list(STILLBIRTH_possible),
                                                datasetS= D3_gop_for_outcomes,
                                                key = "person_id",
                                                condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                strata=c("pers_group_id"),
                                                summarystat = list(list(c("exist"), "date" , "STILLBIRTH_possible")))

pregnancy_outcomes_18 <- merge(pregnancy_outcomes_17, MFC_STILLBIRTH_possible, by = c("pers_group_id"), all.x = T)[is.na(STILLBIRTH_possible), STILLBIRTH_possible := 0]

################################################################################
#########################          TOPFA         ###############################
################################################################################
#NARROW
MFC_TOPFA_narrow <- MergeFilterAndCollapse(list(TOPFA_narrow),
                                                datasetS= D3_gop_for_outcomes,
                                                key = "person_id",
                                                condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                                strata=c("pers_group_id"),
                                                summarystat = list(list(c("exist"), "date" , "TOPFA_narrow")))

pregnancy_outcomes_19 <- merge(pregnancy_outcomes_18, MFC_TOPFA_narrow, by = c("pers_group_id"), all.x = T)[is.na(TOPFA_narrow), TOPFA_narrow := 0]

#POSSIBLE
MFC_TOPFA_possible <- MergeFilterAndCollapse(list(TOPFA_possible),
                                           datasetS= D3_gop_for_outcomes,
                                           key = "person_id",
                                           condition = "date >= pregnancy_start_date & date <= pregnancy_end_date",
                                           strata=c("pers_group_id"),
                                           summarystat = list(list(c("exist"), "date" , "TOPFA_possible")))

pregnancy_outcomes_20 <- merge(pregnancy_outcomes_19, MFC_TOPFA_possible, by = c("pers_group_id"), all.x = T)[is.na(TOPFA_possible), TOPFA_possible := 0]

################################################################################
D3_pregnancy_outcomes <- pregnancy_outcomes_20[, -c("GESTDIAB_code_narrow", "GESTDIAB_code_possible", "INSULINE")]
D3_pregnancy_outcomes <- D3_pregnancy_outcomes[highest_quality == "B_Yellow" | highest_quality == "A_Green", 
                                               denominator := 1][is.na(denominator), denominator:=0]
D3_pregnancy_outcomes <- D3_pregnancy_outcomes[, denominator_sensitivity:=1]

# clean environment
for (i in seq(0,20)) {
  rm(list = c(paste0("pregnancy_outcomes_", i)))
}
for (i in concept_set_our_study_pre) {
  rm(list = c(paste0("MFC_", i)))
  rm(list = i)
}
rm(INSULIN_first_record)

################################################################################
r <- vector(mode = "list")
den <- D3_pregnancy_outcomes[, sum(denominator)]
for (i in concept_set_our_study_pre[-1]) {
  r[i] <- D3_pregnancy_outcomes[denominator==1, sum(get(i))]/den
}
r_sen <- vector(mode = "list")
den <- D3_pregnancy_outcomes[, sum(denominator_sensitivity)]
for (i in concept_set_our_study_pre[-1]) {
  r_sen[i] <- D3_pregnancy_outcomes[denominator_sensitivity==1, sum(get(i))]/den
}
