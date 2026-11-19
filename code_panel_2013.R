################################


###     PANEL DATA 2013   ####


################################


#
setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")


source ("code_panel_2010.R")



#DATA OF INTEREST IN RELATION TO KIDS WITHIN THE PERSONAS_2013 DATABASE 
ninospersonas_2013 <- read.table ("UPersonas2013.tab", sep="\t", header=TRUE, fileEncoding = "Windows-1252") %>% 
  select (consecutivo,
          orden,
          llave_ID_lb, 
          sexo, 
          llave,
          hogar,
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
          tipo_estab) 




#DATA OF INTEREST IN RELATION TO ADULTS WITHIN THE PERSONAS_2013 DATABASE
padrespersonas_2013 <-  read.table ("UPersonas2013.tab", sep="\t", header=TRUE, fileEncoding = "Windows-1252") %>% 
  select (consecutivo,
          llave_ID_lb,
          orden, 
          hogar,
          edad, 
          edad_tenia, 
          ocupacion, 
          estado_civil, 
          lee_escribe, 
          nivel_educ)



#Taking the data related to children's cognitive results
ninos_2013 <- read.table ("UNinos0a13.tab", sep="\t", header=TRUE, fileEncoding = "Windows-1252") %>% 
  select (consecutivo, 
          llave_ID_lb,
          orden,
          orden_padre, 
          padre_vive, 
          educ_padre, 
          orden_madre,
          madre_vive, 
          educ_madre)



#HOME VARIABLE DATASET

load ("UHogar_2013.RData") 

hogar_2013 <- x  %>% select (consecutivo, 
                             consecutivo_c,
                             llave, 
                             hogar, 
                             region,
                             estrato, 
                             t_personas, 
                             tipo_vivienda, 
                             tercil2013, 
                             tenencia_vivienda, 
                             tienen_creditos)


#COMMUNITY DATASET

load ("UComunidades2013.RData")

comunidades_2013 <- x %>% select (consecutivo_c, 
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
                                  duermen_calles, 
                                  drogas_alucinogenas, 
                                  alcohol_publicos, 
                                  prostibulos, 
                                  exigen_dinero)


#KIDS AND PARENTS DATASET WRANGLING

#2891 niños con puntuacion directa y unicos

ninos_2010_2013 <- short_panel_2010 %>%
  select(consecutivo, llave_ID_lb,orden)


unique_ninos_2013 <- inner_join(
  ninos_2010_2013,
  ninos_2013,
  by = c("consecutivo", "llave_ID_lb", "orden"))
  

#we filter out the NA's 
#unique_ninos_2013 <- unique_ninos_2013 %>%
 # filter(!is.na(puntuaciondirecta))




#joining data of kids cognitive results to their demographic results
persninos_2013 <-inner_join (ninospersonas_2013, unique_ninos_2013, by = c("llave_ID_lb", "consecutivo", "orden"))



#joining data of parents to their kids cognitive results


#Taking the data that identifies the father of the child from the unique_ninos_2013 dataset
padres_2013 <- unique_ninos_2013 %>% 
  select (consecutivo, orden_padre)


names (padres_2013) [names (padres_2013) == "orden_padre"] <- "orden"


#Taking the data the data that identifies the mother of the child from the unique_ninos_2013 dataset 
madres_2013 <- unique_ninos_2013 %>% 
  select (consecutivo, orden_madre)


names (madres_2013) [names (madres_2013) == "orden_madre"] <- "orden"



#unifying the identification data of the parents to their corresponding answers in the personas_2013 dataset

papasninos_2013 <-inner_join (padres_2013, padrespersonas_2013, by = c("consecutivo","orden"))



length(unique(papasninos_2013$consecutivo)) == nrow(papasninos_2013)
n_occur2013.pn <- data.frame (table (papasninos_2013$consecutivo))
n_occur2013.pn [n_occur2013.pn$Freq > 1,]

papasninos_2013 <- papasninos_2013 [-c(230, 542, 803, 981, 999,1000), ] #eliminated due to age incongruencies with previous panel data


madresninos_2013 <- inner_join (madres_2013, padrespersonas_2013, by = c("consecutivo","orden"))

length(unique(madresninos_2013$consecutivo)) == nrow(madresninos_2013)
n_occur2013.mn <- data.frame (table (madresninos_2013$consecutivo))
n_occur2013.mn [n_occur2013.mn$Freq > 1,]


madresninos_2013 <- madresninos_2013 [-c(18, 199, 868, 1149, 1165 ), ] #eliminated since duplicates lack consistency with information obtained in other databases or on previous panel or cases deleted completely due to lack of clarity 

#Changing variable names to be able to differentiatethe data from the mother to that of the father 

colnames(papasninos_2013)[colnames(papasninos_2013) != "consecutivo"] <- 
  paste0("padre_", colnames(papasninos_2013)[colnames(papasninos_2013) != "consecutivo"])




colnames(madresninos_2013)[colnames(madresninos_2013) != "consecutivo"] <- 
  paste0("madre_", colnames(madresninos_2013)[colnames(madresninos_2013) != "consecutivo"])



#Unifying both datasets in a long format. 
padres_2013 <- full_join (papasninos_2013, madresninos_2013,  by = "consecutivo")




#unifying datasets with the children's info
padresninos_2013 <- full_join(persninos_2013, padres_2013, by = "consecutivo")



#HOME DATASET WRANGLING 

familiahogar_2013 <- inner_join (padresninos_2013, hogar_2013, by = c("consecutivo", "llave", "hogar"))



#FULL DATASET WRANGLING

panel_2013 <- inner_join (familiahogar_2013, comunidades_2013, by = "consecutivo_c")



#HARMONIZING VARIABLES' NAMES TO BE THE SAME AS IN 2010 WAVE 

names (panel_2013) [names (panel_2013) == "tienen_creditos"] <- "tienen_deudas"

names (panel_2013) [names (panel_2013) == "tercil2013"] <- "tercil"



panel_2013$region <- as.factor (panel_2013$region)
summary (panel_2013$region)

#TRANSFORMING VARIABLES 

# tranforming gender variable 

panel_2013 <- panel_2013 %>% 
  mutate(
    across(sexo, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Male", 
                                "Female")
           ))
  )


#Transforming study variable for children 

panel_2013 <- panel_2013 %>% 
  mutate(
    across(estudia, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )


#transforming study variable for whether child lives with parents

parents_live <- c( "padre_vive", "madre_vive")



panel_2013 <- panel_2013 %>% 
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

panel_2013 <- panel_2013 %>% 
  mutate(
    across(beneficiario_sss, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )


#Transforming whether establishment of education is official into factor with labels

panel_2013 <- panel_2013 %>% 
  mutate(
    across(tipo_estab, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )





### Transforming variables from job status

padres_ocupacion <- c("padre_ocupacion", "madre_ocupacion")

panel_2013 <- panel_2013 %>% 
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

panel_2013 <- panel_2013 %>% 
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


#Transofrming parents' literacy into factor variables with labels 

literacy_var <- c("madre_lee_escribe", "padre_lee_escribe") 

panel_2013 <- panel_2013 %>% 
  mutate (across
          (all_of (literacy_var), 
            ~ factor (., 
                      levels =1:2, 
                      labels = c(
                        "Yes", 
                        "No"
                      ))))

#Transofrming parents' civil status into factor variables with labels 

civil_st <- c ("padre_estado_civil", "madre_estado_civil")


panel_2013 <- panel_2013 %>% 
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

panel_2013 <- panel_2013 %>% 
  mutate(
    across(tienen_deudas, 
           ~ factor (., 
                     levels = 1:2, 
                     labels = c("Yes", "No")
           ))
  )




# Transforming variables related to public services 
ps_var <- c ('preescolar', 
             'esc_primaria', 
             'col_secundaria', 
             'puesto_salud', 
             'canchas_deportivas', 
             'parques_publicos', 
             'puesto_policia')



#transforming into binomial variables with 0 and 1 values  
panel_2013 <- panel_2013 %>%
  mutate(
    across(all_of(ps_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )


#generating count variable for public services 
panel_2013$tot_ps <- rowSums(panel_2013 [, ps_var], na.rm=TRUE)



#Transforming variables related to neighborhood safety 

#security variable where 1 = unsafe perception of neighborhood
panel_2013 <- panel_2013 %>%
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
panel_2013 <- panel_2013 %>%
  mutate(
    across(all_of(safe_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )

#generating count variable for safety in neighborhood 
panel_2013$tot_insec <- rowSums(panel_2013[, safe_var], na.rm=TRUE)


#turning into categories 
panel_2013 <- panel_2013 %>% 
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


panel_2013 <- panel_2013 %>%
  mutate(
    across(all_of(discap_var),
           ~ case_when(
             . == 1 ~ 1,
             . == 2 ~ 0,
             TRUE ~ NA_real_
           ))
  )

panel_2013$discap_tot <- as.integer(rowSums(panel_2013[, discap_var], na.rm = TRUE) > 0)



sum (panel_2013$discap_tot) 

#adding levels 
panel_2013 <- panel_2013 %>% 
  mutate(
    across(discap_tot,
           ~ factor ( .,
                      levels = 0:1,
                      labels = c("No", "Yes")
           ))
  )



#transforming region into factor with labels

panel_2013 <- panel_2013 %>% 
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
panel_2013 <- panel_2013 %>% 
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

panel_2013 <- panel_2013 %>% 
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
panel_2013 <- panel_2013 %>% 
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



length(unique(panel_2013$consecutivo)) == nrow(panel_2013)
n_occur2013 <- data.frame (table (panel_2013$consecutivo))
n_occur2013 [n_occur2013$Freq > 1,]


panel_2013 <- panel_2013 [-1063,]


#SAVING FILE
save(panel_2013, file = "panel_2013.RData")




#deleting variables 

short_panel_2013 <- panel_2013 %>% select (-dif_aprender, -llave, -hogar, -ceguera, -sordera, -mudez, -dif_moverse, 
                                           -dif_banarse, -dif_calle, -seguridad, -atracos_robos, -homicidios, -pandillas, -drogas_alucinogenas, 
                                           -alcohol_publicos, -prostibulos, -duermen_calles, 
                                           -exigen_dinero)



#PLACING 2013 IN ALL VARIABLES
stable <- c("consecutivo", "orden", "llave_ID_lb", "sexo")

short_panel_2013 <- short_panel_2013 %>%
  rename_with (~paste0(., "_2013"), !all_of(stable))


#SAVING FILE
save(short_panel_2013, file = "short_panel_2013.RData")

summary (panel_2013$region)
