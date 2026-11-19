
#######################


# Descriptives for children


######################


library (dplyr)
library (ggplot2)

setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")

source ("code_FULL_panel.R")

#static descriptives (gender, ethnicity)

#gender 
gender_descriptives <- full_panel %>% 
  count(sexo, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N)))

gender_descriptives

#ethnicity

ethn_desc <- full_panel %>% 
  count(etnia, name = "N") %>% 
  mutate(Percent = round(100* N / sum(N)))


# descriptives across waves and visualizations

###########

#age 

###########

age_2010 <- full_panel %>%
  summarise(
    Mean = mean(edad_2010, na.rm = TRUE),
    SD = sd(edad_2010, na.rm = TRUE), 
    Max = max (edad_2010, na.rm = TRUE), 
    Min = min (edad_2010, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2010")

age_2013 <- full_panel %>%
  summarise(
    Mean = mean(edad_2013, na.rm = TRUE),
    SD = sd(edad_2013, na.rm = TRUE), 
    Max = max (edad_2013, na.rm = TRUE), 
    Min = min (edad_2013, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2013")

age_2016 <- full_panel %>%
  summarise(
    Mean = mean(edad_2016, na.rm = TRUE),
    SD = sd(edad_2016, na.rm = TRUE), 
    Max = max (edad_2016, na.rm = TRUE), 
    Min = min (edad_2016, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2016")

age_table <- bind_rows(
  age_2010,
  age_2013,
  age_2016
) %>%
  select(Wave, Mean, SD, Min, Max)

age_table


#age visualization


age_long <- full_panel %>%
  select(edad_2010, edad_2013, edad_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Age"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      edad_2010 = "2010",
      edad_2013 = "2013",
      edad_2016 = "2016"
    )
  )

ggplot(age_long, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  facet_wrap(~ Wave) +
  labs(
    x = "Age",
    y = "Frequency",
    title = "Age distribution by wave"
  ) +
  theme_minimal()



#########

#social stratum 

#########

#per wave

stratum_2010 <- full_panel %>% 
  count(estrato_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")

stratum_2013 <- full_panel %>% 
  count(estrato_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")

stratum_2016 <- full_panel %>% 
  count(estrato_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")

# full table

stratum_2010 <- stratum_2010 %>%
  rename(Stratum = estrato_2010)

stratum_2013 <- stratum_2013 %>%
  rename(Stratum = estrato_2013)

stratum_2016 <- stratum_2016 %>%
  rename(Stratum = estrato_2016)

stratum_table <- bind_rows(
  stratum_2010,
  stratum_2013,
  stratum_2016
)

stratum_table

#visualization of frequencies

ggplot(stratum_table, aes(x = Stratum, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Stratum",
    y = "Frequency",
    title = "Stratum frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()

####

# studying status

#####

study_2010 <- full_panel %>% 
  count(estudia_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


study_2013 <- full_panel %>% 
  count(estudia_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


study_2016 <- full_panel %>% 
  count(estudia_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 

study_2010 <- study_2010 %>%
  rename(Studies = estudia_2010)

study_2013 <- study_2013 %>%
  rename(Studies = estudia_2013)

study_2016 <- study_2016 %>%
  rename(Studies = estudia_2016)

study_table <- bind_rows(
  study_2010,
  study_2013,
  study_2016
)

study_table

#visualization of frequencies

ggplot(study_table, aes(x = Studies, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Study Satus",
    y = "Frequency",
    title = "Study Satus frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()



# type of establishment


establishment <- full_panel %>% 
  select (tipo_estab_2010, tipo_estab_2013, tipo_estab_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Official"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      tipo_estab_2010 = "2010",
      tipo_estab_2013 = "2013",
      tipo_estab_2016 = "2016"
    )
  )
  


full_panel%>% 
  summarise (Min=min(edad_ames_2016, na.rm=TRUE),
             Max=max(edad_ames_2016, na.rm=TRUE))

#visualization 

establishment_table <- establishment %>%
  group_by(Wave) %>%
  count(Official, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N)))


ggplot(establishment_table, aes(x = Official, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Official",
    y = "Frequency",
    title = "Official school attendance frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()





####### 

# father lives

#######

father_2010 <- full_panel %>% 
  count(padre_vive_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


father_2013 <- full_panel %>% 
  count(padre_vive_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


father_2016 <- full_panel %>% 
  count(padre_vive_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")



#full table 

father_2010 <- father_2010 %>%
  rename(father_lives_at_home = padre_vive_2010)

father_2013 <- father_2013 %>%
  rename(father_lives_at_home = padre_vive_2013)

father_2016 <- father_2016 %>%
  rename(father_lives_at_home = padre_vive_2016)

father_table <- bind_rows(
  father_2010,
  father_2013,
  father_2016
)


#visualization of frequencies

ggplot(father_table, aes(x = father_lives_at_home, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Father lives at home with child",
    y = "Frequency",
    fill = "Wave"
  ) +
  theme_minimal()


#####

#mother lives at home 

#####

mother_2010 <- full_panel %>% 
  count(madre_vive_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


mother_2013 <- full_panel %>% 
  count(madre_vive_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


mother_2016 <- full_panel %>% 
  count(madre_vive_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")



#full table 

mother_2010 <- mother_2010 %>%
  rename(mother_lives_at_home = madre_vive_2010)

mother_2013 <- mother_2013 %>%
  rename(mother_lives_at_home = madre_vive_2013)


mother_2016 <- mother_2016 %>%
  rename(mother_lives_at_home = madre_vive_2016)

mother_table <- bind_rows(
  mother_2010,
  mother_2013,
  mother_2016
)


#visualization of frequencies

ggplot(mother_table, aes(x = mother_lives_at_home, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Mother lives at home with child",
    y = "Frequency",
    fill = "Wave"
  ) +
  theme_minimal()


#####

# beneficiary of social security

#####

beneficiary_2010 <- full_panel %>% 
  count(beneficiario_sss_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


beneficiary_2013 <- full_panel %>% 
  count(beneficiario_sss_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


beneficiary_2016 <- full_panel %>% 
  count(beneficiario_sss_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 
beneficiary_2010 <- beneficiary_2010 %>%
  rename(Beneficiary = beneficiario_sss_2010)

beneficiary_2013 <- beneficiary_2013 %>%
  rename(Beneficiary = beneficiario_sss_2013)

beneficiary_2016 <- beneficiary_2016 %>%
  rename(Beneficiary = beneficiario_sss_2016)

beneficiary_table <- bind_rows(
  beneficiary_2010,
  beneficiary_2013,
  beneficiary_2016
)

beneficiary_table

#visualization of frequencies

ggplot(study_table, aes(x = Studies, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Study Satus",
    y = "Frequency",
    title = "Study Satus frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()


######

# region

#####

region_2010 <- full_panel %>% 
  count(region_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


region_2013 <- full_panel %>% 
  count(region_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


region_2016 <- full_panel %>% 
  count(region_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")



#full table 
region_2010 <- region_2010 %>%
  rename(Region = region_2010)

region_2013 <- region_2013 %>%
  rename(Region = region_2013)

region_2016 <- region_2016 %>%
  rename(Region = region_2016)

region_table <- bind_rows(
  region_2010,
  region_2013,
  region_2016
)

region_table


#visualization of frequencies

region_table <- region_table %>%
  droplevels()

ggplot(region_table, aes(x = Region, y = N, fill = Wave)) +
  geom_col() +
  facet_wrap(~ Wave, scales = "free_x") +
  labs(
    x = "Region",
    y = "Frequency",
    title = "Region frequencies by wave"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "none"
  )

#disabilities
disab_2010 <- full_panel  %>% 
  mutate (Disability = discap_tot_2010) %>% 
  count(Disability, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


disab_2013 <- full_panel  %>% 
  mutate (Disability = discap_tot_2013) %>% 
  count(Disability, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


disab_2016 <- full_panel %>% 
  mutate (Disability = discap_tot_2016)%>% 
  count(Disability, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016") 



disab_table <- bind_rows(
  disab_2010,
  disab_2013,
  disab_2016
)

disab_table


#visualization of frequencies

ggplot(disab_table, aes(x = Disability, y = N, fill = Wave)) +
  geom_col() +
  facet_wrap(~ Wave, scales = "free_x") +
  labs(
    x = "Children with/without diabilities",
    y = "Frequency",
    title = "Diability frequencies by wave"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ),
    legend.position = "none"
  )



#socio-emotional development

sdq_outcome <- full_panel %>% 
  summarise (
    Mean = mean (tot_dificultades_2016, na.rm = TRUE), 
    SD = sd (tot_dificultades_2016, na.rm = TRUE)
  )


ggplot(full_panel, aes(x = tot_dificultades_2016)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(
    x = "Score",
    y = "Frequency",
    title = "SDQ Scores distribution - Outcome"
  ) +
  theme_minimal()


  