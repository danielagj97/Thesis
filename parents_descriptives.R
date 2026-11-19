
library (dplyr)
library (ggplot2)

#setwd("C:/Users/Daniela/OneDrive/Documentos/UvT/Second year/Thesis/Data bases")
#
#source ("code_FULL_panel.R")



#######################
#######################

# Descriptives for FATHERS

#######################
#######################







# Age

age_father_2010 <- full_panel %>%
  summarise(
    Mean = mean(padre_edad_2010, na.rm = TRUE),
    SD = sd(padre_edad_2010, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2010")

age_father_2013 <- full_panel %>%
  summarise(
    Mean = mean(padre_edad_2013, na.rm = TRUE),
    SD = sd(padre_edad_2013, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2013")

age_father_2016 <- full_panel %>%
  summarise(
    Mean = mean(padre_edad_2016, na.rm = TRUE),
    SD = sd(padre_edad_2016, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2016")

age_father_table <- bind_rows(
  age_father_2010,
  age_father_2013,
  age_father_2016
) %>%
  select(Wave, Mean, SD)

age_father_table


#age visualization


age_father_long <- full_panel %>%
  select(padre_edad_2010, padre_edad_2013, padre_edad_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Age"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      padre_edad_2010 = "2010",
      padre_edad_2013 = "2013",
      padre_edad_2016 = "2016"
    )
  )

ggplot(age_father_long, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  facet_wrap(~ Wave) +
  labs(
    x = "Fathers' Age",
    y = "Frequency",
    title = "Fathers' age distribution by wave"
  ) +
  theme_minimal()


########

# alphabetism

########

father_readswrites_2010 <- full_panel %>% 
  count(padre_lee_escribe_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010", 
         padre_lee_escribe_2010 = factor(
           padre_lee_escribe_2010))


father_readswrites_2013 <- full_panel %>% 
  count(padre_lee_escribe_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013", 
         padre_lee_escribe_2013 = factor(
           padre_lee_escribe_2013
         ))


father_readswrites_2016 <- full_panel %>% 
  count(padre_lee_escribe_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016", 
         padre_lee_escribe_2016 = factor(
           padre_lee_escribe_2016
         ))

#full table 

father_readswrites_2010 <- father_readswrites_2010 %>%
  rename(reads_writes = padre_lee_escribe_2010)

father_readswrites_2013 <- father_readswrites_2013 %>%
  rename(reads_writes = padre_lee_escribe_2013)

father_readswrites_2016 <- father_readswrites_2016 %>%
  rename(reads_writes = padre_lee_escribe_2016)

father_readswrites_table <- bind_rows(
  father_readswrites_2010,
  father_readswrites_2013,
  father_readswrites_2016
)

father_readswrites_table

#visualization of frequencies

ggplot(father_readswrites_table, aes(x = reads_writes, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Father reads and writes?",
    y = "Frequency",
    title = "Fathers' alphabetism frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()


######

#marital status 

######

father_marriagestatus_2010 <- full_panel %>% 
  count(padre_estado_civil_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010", 
         padre_estado_civil_2010 = factor(
           padre_estado_civil_2010
         ))

father_marriagestatus_2013 <- full_panel %>% 
  count(padre_estado_civil_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013", 
         padre_estado_civil_2013 = factor(
           padre_estado_civil_2013
         ))

father_marriagestatus_2016 <- full_panel %>% 
  count(padre_estado_civil_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016", 
         padre_estado_civil_2016 = factor(
           padre_estado_civil_2016))

#full table 
father_marriagestatus_2010 <- father_marriagestatus_2010 %>%
  rename(Status = padre_estado_civil_2010)

father_marriagestatus_2013 <- father_marriagestatus_2013 %>%
  rename(Status = padre_estado_civil_2013)

father_marriagestatus_2016 <- father_marriagestatus_2016 %>%
  rename(Status = padre_estado_civil_2016)

father_marriagestatus_table <- bind_rows(
  father_marriagestatus_2010,
  father_marriagestatus_2013,
  father_marriagestatus_2016
)

father_marriagestatus_table


#visualization of frequencies

ggplot(father_marriagestatus_table, aes(x = Status, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Fathers' civil status",
    y = "Frequency",
    fill = "Wave"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )


######

#employment

######


father_jobstatus_2010 <- full_panel %>% 
  count(padre_ocupacion_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")



father_jobstatus_2013 <- full_panel %>% 
  count(padre_ocupacion_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


father_jobstatus_2016 <- full_panel %>% 
  count(padre_ocupacion_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 

father_jobstatus_2010 <- father_jobstatus_2010 %>%
  rename(Status = padre_ocupacion_2010)

father_jobstatus_2013 <- father_jobstatus_2013 %>%
  rename(Status = padre_ocupacion_2013)

father_jobstatus_2016 <- father_jobstatus_2016 %>%
  rename(Status = padre_ocupacion_2016)

father_jobstatus_table <- bind_rows(
  father_jobstatus_2010,
  father_jobstatus_2013,
  father_jobstatus_2016
)

father_jobstatus_table

#visualization of frequencies

ggplot(father_jobstatus_table, aes(x = Status, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Father work status",
    y = "Frequency",
    title = "Fathers' work situation",
    fill = "Wave"
  ) +
  theme_minimal()+ 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )


 #educational level

father_education_2010 <- full_panel %>% 
  count(padre_nivel_educ_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


father_education_2013 <- full_panel %>% 
  count(padre_nivel_educ_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


father_education_2016 <- full_panel %>% 
  count(padre_nivel_educ_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 

father_education_2010 <- father_education_2010 %>%
  rename(Education = padre_nivel_educ_2010)

father_education_2013 <- father_education_2013 %>%
  rename(Education = padre_nivel_educ_2013)

father_education_2016 <- father_education_2016 %>%
  rename(Education = padre_nivel_educ_2016)

father_education_table <- bind_rows(
  father_education_2010,
  father_education_2013,
  father_education_2016
)

father_education_table

#visualization of frequencies

ggplot(father_education_table, aes(x = Education, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Father educational level",
    y = "Frequency",
    title = "Fathers' educational level",
    fill = "Wave"
  ) +
  theme_minimal()+ 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )










####################
####################

#Descriptives MOTHER

####################
####################


# Age

age_mother_2010 <- full_panel %>%
  summarise(
    Mean = mean(madre_edad_2010, na.rm = TRUE),
    SD = sd(madre_edad_2010, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2010")

age_mother_2013 <- full_panel %>%
  summarise(
    Mean = mean(madre_edad_2013, na.rm = TRUE),
    SD = sd(madre_edad_2013, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2013")

age_mother_2016 <- full_panel %>%
  summarise(
    Mean = mean(madre_edad_2016, na.rm = TRUE),
    SD = sd(madre_edad_2016, na.rm = TRUE)
  ) %>%
  mutate(Wave = "2016")

age_mother_table <- bind_rows(
  age_mother_2010,
  age_mother_2013,
  age_mother_2016
) %>%
  select(Wave, Mean, SD)

age_mother_table


#age visualization


age_mother_long <- full_panel %>%
  select(madre_edad_2010, madre_edad_2013, madre_edad_2016) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Wave",
    values_to = "Age"
  ) %>%
  mutate(
    Wave = recode(
      Wave,
      madre_edad_2010 = "2010",
      madre_edad_2013 = "2013",
      madre_edad_2016 = "2016"
    )
  )

ggplot(age_mother_long, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  facet_wrap(~ Wave) +
  labs(
    x = "Mothers' Age",
    y = "Frequency",
    title = "Mothers' age distribution by wave"
  ) +
  theme_minimal()


########

# literacy

########

mother_readswrites_2010 <- full_panel %>% 
  count(madre_lee_escribe_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


mother_readswrites_2013 <- full_panel %>% 
  count(madre_lee_escribe_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


mother_readswrites_2016 <- full_panel %>% 
  count(madre_lee_escribe_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")

#full table 

mother_readswrites_2010 <- mother_readswrites_2010 %>%
  rename(reads_writes = madre_lee_escribe_2010)

mother_readswrites_2013 <- mother_readswrites_2013 %>%
  rename(reads_writes = madre_lee_escribe_2013)

mother_readswrites_2016 <- mother_readswrites_2016 %>%
  rename(reads_writes = madre_lee_escribe_2016)

mother_readswrites_table <- bind_rows(
  mother_readswrites_2010,
  mother_readswrites_2013,
  mother_readswrites_2016
)

mother_readswrites_table

#visualization of frequencies

ggplot(mother_readswrites_table, aes(x = reads_writes, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Mother reads and writes?",
    y = "Frequency",
    title = "Mothers' alphabetism frequencies by wave",
    fill = "Wave"
  ) +
  theme_minimal()


######

#marital status 

######

mother_marriagestatus_2010 <- full_panel %>% 
  count(madre_estado_civil_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")

mother_marriagestatus_2013 <- full_panel %>% 
  count(madre_estado_civil_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")

mother_marriagestatus_2016 <- full_panel %>% 
  count(madre_estado_civil_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")

#full table 
mother_marriagestatus_2010 <- mother_marriagestatus_2010 %>%
  rename(Status = madre_estado_civil_2010)

mother_marriagestatus_2013 <- mother_marriagestatus_2013 %>%
  rename(Status = madre_estado_civil_2013)

mother_marriagestatus_2016 <- mother_marriagestatus_2016 %>%
  rename(Status = madre_estado_civil_2016)

mother_marriagestatus_table <- bind_rows(
  mother_marriagestatus_2010,
  mother_marriagestatus_2013,
  mother_marriagestatus_2016
)

mother_marriagestatus_table


#visualization of frequencies

ggplot(mother_marriagestatus_table, aes(x = Status, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Mothers' civil status",
    y = "Frequency",
    fill = "Wave"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )


######

#employment

######


mother_jobstatus_2010 <- full_panel %>% 
  count(madre_ocupacion_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")



mother_jobstatus_2013 <- full_panel %>% 
  count(madre_ocupacion_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


mother_jobstatus_2016 <- full_panel %>% 
  count(madre_ocupacion_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 

mother_jobstatus_2010 <- mother_jobstatus_2010 %>%
  rename(Status = madre_ocupacion_2010)

mother_jobstatus_2013 <- mother_jobstatus_2013 %>%
  rename(Status = madre_ocupacion_2013)

mother_jobstatus_2016 <- mother_jobstatus_2016 %>%
  rename(Status = madre_ocupacion_2016)

mother_jobstatus_table <- bind_rows(
  mother_jobstatus_2010,
  mother_jobstatus_2013,
  mother_jobstatus_2016
)

mother_jobstatus_table

#visualization of frequencies

ggplot(mother_jobstatus_table, aes(x = Status, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Mother work status",
    y = "Frequency",
    title = "Mothers' work situation",
    fill = "Wave"
  ) +
  theme_minimal()+ 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )


 #educational level

mother_education_2010 <- full_panel %>% 
  count(madre_nivel_educ_2010, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2010")


mother_education_2013 <- full_panel %>% 
  count(madre_nivel_educ_2013, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2013")


mother_education_2016 <- full_panel %>% 
  count(madre_nivel_educ_2016, name = "N") %>% 
  mutate(Percent = round(100 * N / sum(N))) %>%
  mutate(Wave = "2016")


#full table 

mother_education_2010 <- mother_education_2010 %>%
  rename(Education = madre_nivel_educ_2010)

mother_education_2013 <- mother_education_2013 %>%
  rename(Education = madre_nivel_educ_2013)

mother_education_2016 <- mother_education_2016 %>%
  rename(Education = madre_nivel_educ_2016)

mother_education_table <- bind_rows(
  mother_education_2010,
  mother_education_2013,
  mother_education_2016
)

mother_education_table

#visualization of frequencies

ggplot(mother_education_table, aes(x = Education, y = N, fill = Wave)) +
  geom_col(position = "dodge") +
  labs(
    x = "Mother educational level",
    y = "Frequency",
    title = "Mothers' educational level",
    fill = "Wave"
  ) +
  theme_minimal()+ 
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )


