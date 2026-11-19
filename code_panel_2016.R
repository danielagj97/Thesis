

############################


###     PANEL DATA 2016   ####



setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")


source ("code_panel_2013.R")



#DATA OF INTEREST IN RELATION TO KIDS WITHIN THE PERSONAS_2016 DATABASE 

load ("UPersonas2016.RData") 


ninospersonas_2016 <- x %>% 
  select (consecutivo, 
          orden,
          llave_ID_lb, 
          hogar,
          hogar_n16, 
          sexo, 
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
          ) 




#DATA OF INTEREST IN RELATION TO ADULTS WITHIN THE PERSONAS_2016 DATABASE
padrespersonas_2016 <-  x %>% 
  select (consecutivo,
          llave_ID_lb,
          orden, 
          edad, 
          ocupacion, 
          estado_civil, 
          lee_escribe, 
          nivel_educ)



#Taking the data related to children's cognitive results

load ("UNinos6a16.RData") 

ninos_2016 <- x %>% 
  select (consecutivo,
          llave_ID_lb,
          orden,
          hogar_n16,
          puntuaciondirecta,
          tot_dificultades,
          prosocial,
          edad_ames,
          orden_padre, 
          orden_madre, 
          padre_vive, 
          educ_padre, 
          madre_vive, 
          educ_madre)



#HOME VARIABLE DATASET

load ("UHogar2016.RData") 

hogar_2016 <- x  %>% select (consecutivo,
                             hogar_n16, 
                             consecutivo_c, 
                             estrato, 
                             region_2016, 
                             t_personas, 
                             tipo_vivienda, 
                             tercil2016,
                             tenencia_vivienda, 
                             tienen_creditos, 
                             cuantos_creditos)


#COMMUNITY DATASET

load ("UComunidades2016.RData")

comunidades_2016 <- x %>% select (consecutivo_c, 
                                  seguridad, 
                                  atracos_robos, 
                                  homicidios, 
                                  pandillas, 
                                  drogas_alucinogenas, 
                                  alcohol_publicos, 
                                  prostibulos, 
                                  duermen_calles)


#KIDS AND PARENTS DATASET WRANGLING

ninos_2013_2016 <- short_panel_2013 %>%
  select(llave_ID_lb) 


unique_ninos_2016 <- inner_join(
  ninos_2013_2016,
  ninos_2016,
  by = c("llave_ID_lb")
)

#unique_ninos_2016 <- unique_ninos_2016 %>%
 # filter(!is.na(puntuaciondirecta))




#joining data of kids cognitive results to their demographic results
persninos_2016 <-inner_join (ninospersonas_2016, unique_ninos_2016, by = c("llave_ID_lb", "consecutivo", "orden","hogar_n16"))






#joining data of parents to their kids cognitive results


#Taking the data that identifies the father of the child from the unique_ninos_2016 dataset
padres_2016 <- unique_ninos_2016 %>% 
  select (consecutivo, orden_padre)



names (padres_2016) [names (padres_2016) == "orden_padre"] <- "orden"

#elimination due to many to many warning
padres_2016 <- padres_2016 [-343,]

#Taking the data the data that identifies the mother of the child from the unique_ninos_2016 dataset 

madres_2016 <- unique_ninos_2016 %>% 
  select (consecutivo, orden_madre)

madres_2016 <- madres_2016 [-366,]


names (madres_2016) [names (madres_2016) == "orden_madre"] <- "orden"



#unifying the identification data of the parents to their corresponding answers in the personas_2016 dataset

papasninos_2016 <-inner_join (padres_2016, padrespersonas_2016, by = c("consecutivo","orden"))

papasninos_2016 <- papasninos_2016 [-8,]



#Upon obtaining warning of many-to-many relationship, we further investigate 
length(unique(papasninos_2016$consecutivo)) == nrow(papasninos_2016)
n_occur2016 <- data.frame (table (papasninos_2016$consecutivo))
n_occur2016[n_occur2016$Freq > 1,]




madresninos_2016 <- inner_join (madres_2016, padrespersonas_2016, by = c("consecutivo","orden"))

length(unique(madresninos_2016$consecutivo)) == nrow(madresninos_2016)
n_occur2016.m <- data.frame (table (madresninos_2016$consecutivo))
n_occur2016.m [n_occur2016.m$Freq > 1,]


#eliminating duplicated rows that are confirmed through checking past panel data to be erroneous 
madresninos_2016 <- madresninos_2016 [-c(12, 667, 876, 963, 1102), ]

length(unique(madresninos_2016$consecutivo)) == nrow(madresninos_2016)





#Changing variable names to be able to differentiatethe data from the mother to that of the father 

colnames(papasninos_2016)[colnames(papasninos_2016) != "consecutivo"] <- 
  paste0("padre_", colnames(papasninos_2016)[colnames(papasninos_2016) != "consecutivo"])




colnames(madresninos_2016)[colnames(madresninos_2016) != "consecutivo"] <- 
  paste0("madre_", colnames(madresninos_2016)[colnames(madresninos_2016) != "consecutivo"])



#Unifying both datasets in a long format. 
padres_2016 <- full_join (papasninos_2016, madresninos_2016,  by = "consecutivo")




#unifying datasets with the children's info
padresninos_2016 <- full_join(persninos_2016, padres_2016, by = "consecutivo")



#HOME DATASET WRANGLING 

familiahogar_2016 <- inner_join (padresninos_2016, hogar_2016, by = c("consecutivo", "hogar_n16"))

familiahogar_2016 <- familiahogar_2016 %>%
 filter(!is.na(estrato))


#FULL DATASET WRANGLING

panel_2016 <- inner_join (familiahogar_2016, comunidades_2016, by = "consecutivo_c")


#HARMONIZING VARIABLES' NAMES TO BE THE SAME AS IN 2010 WAVE 

names (panel_2016) [names (panel_2016) == "tienen_creditos"] <- "tienen_deudas"

names (panel_2016) [names (panel_2016) == "region_2016"] <- "region"

names (panel_2016) [names (panel_2016) == "tercil2016"] <- "tercil"




#TRANSFORMING VARIABLES 

# tranforming gender variable 

panel_2016 <- panel_2016 %>% 
  mutate(
    across(sexo, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Male", 
                                "Female")
           ))
  )


#Transforming study variable for children 

panel_2016 <- panel_2016 %>% 
  mutate(
    across(estudia, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )


#transforming study variable for whether child lives with parents

parents_live <- c( "padre_vive", "madre_vive")


panel_2016 <- panel_2016 %>% 
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


#Transforming social health beneficiary variable for children to factor with labels

panel_2016 <- panel_2016 %>% 
  mutate(
    across(beneficiario_sss, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )


#Transforming whether establishment of education is official into factor with labels

panel_2016 <- panel_2016 %>% 
  mutate(
    across(tipo_estab, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )



#Transforming regions variable to match previous waves

panel_2016 <- panel_2016 %>% 
  mutate (
    region = as.numeric(sub("\\..*", "", region)),
    across
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





### Transforming variables from job status

padres_ocupacion <- c("padre_ocupacion", "madre_ocupacion")

panel_2016 <- panel_2016 %>% 
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




# Transforming parents educational levels variable


padres_educacion <- c("padre_nivel_educ", "educ_padre", "madre_nivel_educ", "educ_madre")

panel_2016 <- panel_2016 %>% 
  mutate(
    across(
      all_of(padres_educacion),
      ~ case_when(
        . %in% c(1,2,3) ~ 1,
        . == 4 ~ 2,
        . %in% c(5, 6, 7, 8, 9, 10, 11,12) ~ 3,
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



#Transforming parents' literacy into factor variables with labels 

literacy_var <- c("madre_lee_escribe", "padre_lee_escribe") 

panel_2016 <- panel_2016 %>% 
  mutate (across
          (all_of (literacy_var), 
            ~ factor (., 
                      levels =1:2, 
                      labels = c(
                        "Yes", 
                        "No"
                      ))))


#Transforming parents' civil status 

civil_st <- c ("padre_estado_civil", "madre_estado_civil")

panel_2016 <- panel_2016 %>% 
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






#Trasnforming indicator variable of debts into factor with labels

panel_2016 <- panel_2016 %>% 
  mutate(
    across(tienen_deudas, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )





#Transforming variables related to neighborhood safety 

#security variable where 1 = unsafe perception of neighborhood
panel_2016 <- panel_2016 %>%
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
panel_2016 <- panel_2016 %>%
  mutate(
    across(all_of(safe_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )

#generating count variable for safety in neighborhood 
panel_2016$tot_insec <- rowSums(panel_2016[, safe_var], na.rm=TRUE)


panel_2016 <- panel_2016 %>% 
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


panel_2016 <- panel_2016 %>%
  mutate(
    across(all_of(discap_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )

panel_2016$discap_tot <- as.integer(rowSums(panel_2016[, discap_var], na.rm = TRUE) > 0)



sum (panel_2016$discap_tot) 

#adding labels
panel_2016 <- panel_2016 %>% 
  mutate(
    across(discap_tot,
           ~ factor ( .,
                      levels = 0:1,
                      labels = c("No", "Yes")
           ))
  )





#transforming type of housing into factor with labels 
panel_2016 <- panel_2016 %>% 
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

panel_2016 <- panel_2016 %>% 
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
panel_2016 <- panel_2016 %>% 
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


length(unique(panel_2016$consecutivo)) == nrow(panel_2016)
n_occur2016 <- data.frame (table (panel_2016$consecutivo))
n_occur2016 [n_occur2016$Freq > 1,]

?filter

panel_2016 <- panel_2016 %>%
  dplyr::filter(!is.na(tot_dificultades))


#SAVING FILE 
save(panel_2016, file = "panel_2016.RData")


#deleting variables 
short_panel_2016 <- panel_2016 %>% select (-hogar_n16, -dif_aprender, -ceguera, -sordera, -mudez, -dif_moverse, 
                                           -dif_banarse, -dif_calle, -seguridad, -atracos_robos, -homicidios, -pandillas, -drogas_alucinogenas, 
                                           -alcohol_publicos, -prostibulos, -duermen_calles)


#PLACING 2016 IN ALL VARIABLES

stable <- c("consecutivo", "llave_ID_lb", "orden", "sexo" )

short_panel_2016 <- short_panel_2016 %>%
  rename_with (~paste0(., "_2016"), !all_of(stable))

#SAVING FILE 
save(short_panel_2016, file = "short_panel_2016.RData")

