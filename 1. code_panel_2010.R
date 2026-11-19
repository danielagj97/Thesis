############################


###     PANEL DATA 2010   ####


###########################
 


#library 
library (naniar)
library (tidyverse)
library (dplyr)
library (haven)
library (magrittr)
library (janitor)
library (stringr)
library (skimr)
library (pointblank)
library (assertthat)
library (purrr)


#
setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")



 #DATA OF INTEREST IN RELATION TO KIDS WITHIN THE PERSONAS_2010 DATABASE 
ninospersonas_2010 <- read.table ("UPersonas2010.tab", sep="\t", header=TRUE, fileEncoding = "latin1") %>% 
  select (consecutivo, 
          orden,
          llave_ID_lb, 
          sexo, 
          etnia,
          edad, 
          beneficiario_sss, 
          ceguera, 
          sordera, 
          mudez, 
          dif_moverse, 
          dif_banarse, 
          dif_calle, 
          dif_aprender, 
          estudia, 
          tipo_estab, 
          prev_med, 
          prev_pediatra, 
          prev_odo, 
          prev_opto, 
          prev_malter) 




#DATA OF INTEREST IN RELATION TO ADULTS WITHIN THE PERSONAS_2010 DATABASE
padrespersonas_2010 <-  read.table ("UPersonas2010.tab", sep="\t", header=TRUE, fileEncoding = "latin1") %>% 
  select (consecutivo,
          llave_ID_lb,
          orden, 
          edad, 
          ocupacion, 
          estado_civil, 
          prev_med, 
          prev_odo, 
          prev_opto, 
          prev_malter, 
          prev_planif, 
          lee_escribe, 
          nivel_educ)



#KIDS DATASET 
load ("UNinosde0a9.RData")

#Taking the data related to children's cognitive results
ninos_2010 <- x %>% select (consecutivo, 
                            llave_ID_lb, 
                            orden_padre, 
                            orden_madre, 
                            padre_vive, 
                            educ_padre, 
                            madre_vive, 
                            educ_madre)


#HOME VARIABLE DATASET

load ("UHogar2010.RData") 

hogar_2010 <- x  %>% select (consecutivo, 
                             consecutivo_c, 
                             region, 
                             estrato, 
                             t_personas, 
                             tipo_vivienda, 
                             tercil2010, 
                             sp_energia, 
                             sp_gasnatural, 
                             sp_acueducto, 
                             sp_alcantarillado,
                             tenencia_vivienda, 
                             tienen_deudas, 
                             tipo_deuda_1,
                             tipo_deuda_2, 
                             tipo_deuda_3, 
                             tipo_deuda_4, 
                             tipo_deuda_5, 
                             tipo_deuda_6, 
                             tipo_deuda_7, 
                             tipo_deuda_8, 
                             tipo_deuda_9, 
                             tipo_deuda_10, 
                             tipo_deuda_11, 
                             tipo_deuda_12, 
                             tipo_deuda_13, 
                             tipo_deuda_14, 
                             tipo_deuda_15, 
                             tipo_deuda_16)


#COMMUNITY DATASET
comunidades_2010 <- read.table ("UComunidades2010.tab", sep="\t", header=TRUE, fileEncoding = "latin1") %>% 
  select (consecutivo_c, 
          preescolar, 
          esc_primaria, 
          col_secundaria, 
          puesto_salud, 
          canchas_deportivas, 
          parques_publicos, 
          puesto_policia, 
          seguridad, 
          atracos_robos, 
          homicidios, 
          pandillas, 
          drogas_alucinogenas, 
          alcohol_publicos, 
          prostibulos, 
          duermen_calles)


#KIDS AND PARENTS DATASET WRANGLING

#2891 niños con puntuacion directa y unicos

unique_ninos_2010 <- ninos_2010 %>%  
  distinct(consecutivo, .keep_all = TRUE)



unique_ninos_2010




#joining data of kids cognitive results to their demographic results
persninos_2010 <-inner_join (ninospersonas_2010, unique_ninos_2010, by = c("llave_ID_lb", "consecutivo"))




#joining data of parents to their kids cognitive results


#Taking the data that identifies the father of the child from the unique_ninos_2010 dataset
padres_2010 <- unique_ninos_2010 %>% 
  select (consecutivo, orden_padre)



names (padres_2010) [names (padres_2010) == "orden_padre"] <- "orden"


#Taking the data the data that identifies the mother of the child from the unique_ninos_2010 dataset 
madres_2010 <- unique_ninos_2010 %>% 
  select (consecutivo, orden_madre)




names (madres_2010) [names (madres_2010) == "orden_madre"] <- "orden"


#unifying the identification data of the parents to their corresponding answers in the personas_2010 dataset


papasninos_2010 <-inner_join (padres_2010, padrespersonas_2010, by = c("consecutivo","orden"))



madresninos_2010 <- inner_join (madres_2010, padrespersonas_2010, by = c("consecutivo","orden"))


#Changing variable names to be able to differentiate the data from the mother to that of the father 

colnames(papasninos_2010)[colnames(papasninos_2010) != "consecutivo"] <- 
  paste0("padre_", colnames(papasninos_2010)[colnames(papasninos_2010) != "consecutivo"])



colnames(madresninos_2010)[colnames(madresninos_2010) != "consecutivo"] <- 
  paste0("madre_", colnames(madresninos_2010)[colnames(madresninos_2010) != "consecutivo"])



#Unifying both datasets in a long format. 
padres_2010 <- full_join (papasninos_2010, madresninos_2010,  by = "consecutivo")


#unifying datasets with the children's info
#join <- c ("consecutivo","educ_padre", "educ_madre")
padresninos_2010 <- full_join(persninos_2010, padres_2010, by = "consecutivo")


#HOME DATASET WRANGLING 

familiahogar_2010 <- inner_join (padresninos_2010, hogar_2010, by = "consecutivo")

#FULL DATASET WRANGLING

panel_2010 <- inner_join (familiahogar_2010, comunidades_2010, by = "consecutivo_c")


#Making all blank spaces into NA´s
panel_2010 [panel_2010== " "] <- NA



#TRANSFORMING VARIABLES 

# tranforming gender variable 

panel_2010 <- panel_2010 %>% 
  mutate(
    across(sexo, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Male", 
                                "Female")
           ))
  )





#transforming ethnicity variable 
panel_2010 <- panel_2010 %>% 
  mutate(
    across(etnia, 
           ~case_when (
             . %in% c(1:5, 7,8) ~ 1, 
             . == 6 ~ 2, 
             TRUE ~ NA_real_
           )
           )
  ) %>% 
  mutate(
    across( etnia, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Non-White", 
                                "White")
           ))
  )



#Transforming study variable for children to factor with labels

panel_2010 <- panel_2010 %>% 
  mutate(
    across(estudia, 
    ~ factor (., 
      levels = 1:2, 
      labels = c("Yes", "No")
    ))
  )


#transforming variable for whether children lives with parents

parents_live <- c( "padre_vive", "madre_vive")

panel_2010 <- panel_2010 %>% 
  mutate (
    across(
      all_of(parents_live),
      ~ case_when (
        . == 1 ~ 1,
        . %in% c(2, 3) ~ 2,
        TRUE ~ 3)
    )
  ) %>% 
  mutate (
    across(
      all_of(parents_live),
      ~factor(., 
              levels=c(1,2,3),
              labels = c("Yes", "No", "No info")))
  )



#Transforming whether establishment of education is official into factor with labels

panel_2010 <- panel_2010 %>% 
  mutate(
    across(tipo_estab, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )



#Transforming social health beneficiary variable for children to factor with labels

panel_2010 <- panel_2010 %>% 
  mutate(
    across(beneficiario_sss, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )

#Trasnforming indicator variable of debts into factor with labels

panel_2010 <- panel_2010 %>% 
  mutate(
    across(tienen_deudas, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )



### Transforming variables from parents' job status 

padres_ocupacion <- c("padre_ocupacion", "madre_ocupacion")

panel_2010 <- panel_2010 %>% 
  mutate(
    across(
      all_of(padres_ocupacion),
      ~ case_when(
        . %in% c(1, 3) ~ 1,
        . %in% c(2, 4) ~ 2,
        . %in% c(5:9) ~ 3,
        . %in% c(10, 11)~ 4,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(
    across(
      all_of(padres_ocupacion),
      ~ factor(
        .,
        levels = 1:4,
        labels = c(
          "Indefinite end contract",
          "Definite end contract",
          "Independent worker",
          "Other"
        )
      )
    )
  )



# Transforming parents' educational levels variable


padres_educacion <- c("padre_nivel_educ", "educ_padre", "madre_nivel_educ", "educ_madre")

panel_2010 <- panel_2010 %>% 
  mutate(
    across(
      all_of(padres_educacion),
      ~ case_when(
        . %in% c(1, 2, 3) ~ 1,
        . == 4 ~ 2,
        . %in% c(5, 6, 7, 8, 9, 10, 11, 12) ~ 3,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(
    across(
      all_of(padres_educacion),
      ~ factor(
        .,
        levels = 1:3,
        labels = c(
          "None or primary school",
          "Middle or High School",
          "Higher education"
        )
      )
    )
  )

#transforming parents' literacy into factor with labels 

literacy_var <- c("madre_lee_escribe", "padre_lee_escribe") 

panel_2010 <- panel_2010 %>% 
  mutate (across
          (all_of (literacy_var), 
            ~ factor (., 
                      levels =1:2, 
                      labels = c(
                        "Yes", 
                        "No"
                      ))
            )
          )


#Transforming parents' civil status 

civil_st <- c ("padre_estado_civil", "madre_estado_civil")

panel_2010 <- panel_2010 %>% 
  mutate (across
          (all_of (civil_st), 
            ~  case_when (
              . %in% c(1, 2) ~ 1, 
              . %in% c(3:5) ~ 2, 
              TRUE ~ NA_real_
            )
          )
  ) %>% mutate(across
               (all_of(civil_st),
              ~factor (., 
                      levels = c(1, 2),
                      labels = c("Partnered", "Non-partnered"
                      ))
            )
          )





#Transforming variables related to neighborhood safety 

#security variable where 1 = unsafe perception of neighborhood
panel_2010 <- panel_2010 %>%
  mutate(
    across(seguridad ,
           ~ case_when(
             . == 1 ~ 2,
             . == 2 ~ 2,
             . == 3 ~ 1, 
             . == 4 ~ 1, 
             TRUE ~ NA_real_
           ))
  )





#transforming into binomial variables with 0 and 1 values
safe_var <- c ('seguridad', 'atracos_robos', 'homicidios', 'pandillas', 'drogas_alucinogenas', 'alcohol_publicos', 'prostibulos', 'duermen_calles')

#transforming into binomial variables with 0 and 1 values  
panel_2010 <- panel_2010 %>%
  mutate(
    across(all_of(safe_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )

#generating count variable for safety in neighborhood 
panel_2010$tot_insec <- rowSums(panel_2010[, safe_var], na.rm=TRUE)



panel_2010 <- panel_2010 %>% 
  mutate(
    across(tot_insec,
      ~ case_when(
        . %in% c(0:2) ~ 1,
        . %in% c(3:5) ~ 2,
        . %in% c(6:8) ~ 3,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(
    across(tot_insec,
      ~ factor(
        .,
        levels = 1:3,
        labels = c(
          "Low",
          "Medium",
          "High"
      ))
    )
  )




# Transforming variables related to disabilities
discap_var <- c ('dif_aprender', 'ceguera', 'sordera', 'mudez', 'dif_moverse', 
                 'dif_banarse', 'dif_calle')




panel_2010$discap_tot <- as.integer(rowSums(panel_2010[, discap_var], na.rm = TRUE) > 0)



sum (panel_2010$discap_tot) #50 kids with disabilities


panel_2010 <- panel_2010 %>% 
  mutate(
    across(discap_tot,
           ~ factor ( .,
             levels = 0:1,
              labels = c("No", "Yes")
           ))
  )
  


#transforming region into factor with labels

panel_2010 <- panel_2010 %>% 
  mutate (across
          (region,
            ~ case_when( 
              . %in% c(1, 6) ~ 1, 
              . %in% c(3,7,8,9) ~ 2, 
              . == 2 ~ 3, 
              . == 4 ~ 4,  
              . == 5 ~ 5, 
            TRUE ~ NA_real_
          ))
  ) %>% 
  mutate (across 
          (region,
            ~ factor (.,
                      levels = 1:5,
                      labels = c(
                        "Atlántica",
                        "Central",
                        "Oriental",
                        "Pacífica",
                        "Bogotá"
                      ))
          )
  )



#transforming type of housing into factor with labels 
panel_2010 <- panel_2010 %>% 
  mutate (across
          (tipo_vivienda, 
            ~ factor (., 
                      levels = 1:5, 
                      labels = c("House", 
                                 "Apartment", 
                                 "Room", 
                                 "Other type of housing", 
                                 "Indigenous housing"
                                 ))
            )
          )


#transforming type of ownership into factor with labels and different levels to minimize # of categories

panel_2010 <- panel_2010 %>% 
  mutate(
    across(
      tenencia_vivienda,
      ~ case_when(
        . %in% c(1,2) ~ 1,
        . %in% c(3,4,5) ~ 2,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(
    across(
      tenencia_vivienda,
      ~ factor(
        .,
        levels = 1:2,
        labels = c(
          "Owner",
          "Non-owner"
        )
      )
    )
  )


#transforming socioeconomic stratum into factor with labels and different levels 
panel_2010 <- panel_2010 %>% 
  mutate (
    across(
      estrato,
      ~ case_when (
        . %in% c(3,4) ~ 3,
        . == 1 ~ 1, 
        . == 2 ~ 2,
        TRUE ~ NA_real_
      )
    ) %>% 
      mutate(
        across (
          estrato,
          ~ factor (., 
                    levels = 1:3, 
                    labels = c("Low-low", 
                               "Low", 
                               "Medium-Low"))
        )
      )
  )


#changing name to variable for codebook simplicity 

names (panel_2010) [names (panel_2010) == "tercil2010"] <- "tercil"

#SAVING COMPLETE DATASET FOR RESEARCHER'S INTEREST
save(panel_2010, file = "panel_2010.RData")



#deleting variables 
short_panel_2010 <- panel_2010 %>% select (-dif_aprender, -ceguera, -sordera, -mudez, -dif_moverse, 
                -dif_banarse, -dif_calle, -madre_prev_med, -madre_prev_odo, 
                -madre_prev_opto, -madre_prev_malter, -madre_prev_planif, 
                -padre_prev_med, -padre_prev_odo, -padre_prev_opto, 
                -padre_prev_malter, -padre_prev_planif, -prev_med, -prev_odo, -prev_opto, -prev_malter, 
                -prev_pediatra, -tipo_deuda_1,-tipo_deuda_2, -tipo_deuda_3, 
                -tipo_deuda_4, -tipo_deuda_5, -tipo_deuda_6, -tipo_deuda_7, -tipo_deuda_8, 
                -tipo_deuda_9, -tipo_deuda_10, -tipo_deuda_11, -tipo_deuda_12,
                -tipo_deuda_13, -tipo_deuda_14, -tipo_deuda_15, -tipo_deuda_16, 
                -seguridad, -atracos_robos, -homicidios, -pandillas, -drogas_alucinogenas, 
                -alcohol_publicos, -prostibulos, -duermen_calles, -sp_energia, -sp_gasnatural, 
                -sp_acueducto, -sp_alcantarillado, -preescolar, 
                -esc_primaria, -col_secundaria, -puesto_salud, -canchas_deportivas, -parques_publicos, -puesto_policia)





#PLACING 2010 IN ALL VARIABLES

stable <- c("consecutivo", "llave_ID_lb", "orden", "sexo", "etnia" )

short_panel_2010 <- short_panel_2010 %>%
  rename_with (~paste0(., "_2010"), !all_of(stable))


#SAVING FILE

save(short_panel_2010, file = "short_panel_2010.RData")


summary (short_panel_2010)
