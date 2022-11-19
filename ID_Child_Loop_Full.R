library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(data.table)
library(tidyverse)


rm(list = ls())
setwd('C:/Users/ext-belche/rise-prelim-analysis')

# loading data into R
child_data_T0_ID <- read.csv(file='ID_data/hhd_child_loop_T0.csv')
child_data_T4_ID <- read.csv(file='ID_data/hhd_child_loop_T4.csv')
child_data_T6_ID <- read.csv(file='ID_data/hhd_child_symptoms_T6.csv')
child_data_T8_ID <- read.csv(file='ID_data/hhd_child_loop_T8.csv')
child_data_T10_ID <- read.csv(file='ID_data/hhd_child_loop_T10.csv')

T0_ID <- cbind(child_data_T0_ID)
T4_ID <- cbind(child_data_T4_ID)
T6_ID <- cbind(child_data_T6_ID)
T8_ID <- cbind(child_data_T8_ID)
T10_ID <- cbind(child_data_T10_ID)

# TABLE SIZE 
t0id_size <- nrow(T0_ID)
t4id_size <- nrow(T4_ID)
t6id_size <- nrow(T6_ID)
t8id_size <- nrow(T8_ID)
t10id_size <- nrow(T10_ID)

# CREATING THE COMBO TABLE 
# add column 
num_rows <- nrow(T0_ID)
T0_ID['Set'] <- rep('T0', num_rows)
num_rows <- nrow(T4_ID)
T4_ID['Set'] <- rep('T4', num_rows)
num_rows <- nrow(T6_ID)
T6_ID['Set'] <- rep('T6', num_rows)
num_rows <- nrow(T8_ID)
T8_ID['Set'] <- rep('T8', num_rows)
num_rows <- nrow(T10_ID)
T10_ID['Set'] <- rep('T10', num_rows)


# set column 
set <- c(T0_ID$Set, T4_ID$Set, head(T6_ID$Set, -1332), T8_ID$Set, T10_ID$Set)
length(set)
unique(set)

d_a_0 <- T0_ID %>% filter(doctor_yn>0)
d_a_0 <- nrow(d_a_0)
d_a_4 <- T4_ID %>% filter(doctor_yn>0)
d_a_4<- nrow(d_a_4)
d_a_6<- T6_ID %>% filter(doctor_yn>0)
d_a_6 <- nrow(d_a_6)
d_a_8<- T8_ID %>% filter(doctor_yn>0)
d_a_8 <- nrow(d_a_8)
d_a_10<- T10_ID %>% filter(doctor_yn>0)
d_a_10 <- nrow(d_a_10)


# health general
gen_health_overall <- c(T0_ID$health_general_child, T4_ID$health_general_child,
                        T6_ID$health_general_child, T8_ID$health_general_child,
                        T10_ID$health_general_child)
length(gen_health_overall)
unique(gen_health_overall)

injury <- c(T0_ID$injury, T4_ID$injury,
            head(T6_ID$injury, -1332), T8_ID$injury, T10_ID$injury)
length(injury)
unique(injury)

injury_types <- c(T0_ID$injury_type, T4_ID$injury_type,
                  T6_ID$injury_type, T8_ID$injury_type, T10_ID$injury_type)
length(injury_types)
unique(injury_types)

medication_worms_yn <- c(rep('Nonexistent', nrow(T0_ID)), T4_ID$medication_worms_yn,
                         head(rep('Nonexistent', nrow(T6_ID)), -1332), T8_ID$medication_worms_yn,
                         T10_ID$medication_worms_yn)
length(medication_worms_yn)
unique(medication_worms_yn)

doctor_yn <- c(T0_ID$doctor_yn, T4_ID$doctor_yn,
               head(T6_ID$doctor_yn, -1332), T8_ID$doctor_yn,
               T10_ID$doctor_yn)
length(doctor_yn)
unique(doctor_yn)

doctor_antibiotics <- c(T0_ID$doctor_antibiotics, T4_ID$doctor_antibiotics,
                        T6_ID$doctor_antibiotics, T8_ID$doctor_antibiotics,
                        T10_ID$doctor_antibiotics)
length(doctor_antibiotics)
unique(doctor_antibiotics)

hospital_yn <- c(T0_ID$hospital_yn, T4_ID$hospital_yn,
                 head(rep('Nonexistent', nrow(T6_ID)), -1332), T8_ID$hospital_yn,
                 T10_ID$hospital_yn)
length(hospital_yn)
unique(hospital_yn)

hospital_antibiotics <- c(T0_ID$hospital_antibiotics, T4_ID$hospital_antibiotics,
                          head(rep('Nonexistent', nrow(T6_ID)), -1332), T8_ID$hospital_antibiotics,
                          T10_ID$hospital_antibiotics)
length(hospital_antibiotics)
unique(hospital_antibiotics)

#water_yn <- c(rep('Nonexistent', nrow(T0_ID)), rep('Nonexistent', nrow(T4_ID)),
#rep('Nonexistent', nrow(T6_ID)), T8_ID$water_yn, T10_ID$water_yn)
#length(water_yn)
#unique(water_yn)

vaccinations_yn <- c(rep('Nonexistent', nrow(T0_ID)), T4_ID$vaccinations_yn,
                     head(rep('Nonexistent', nrow(T6_ID)), -1332), T8_ID$vaccinations_yn,
                     T10_ID$vaccinations_yn_a)

length(vaccinations_yn)
unique(vaccinations_yn)


breastfed_3m_yn <- c(rep('Nonexistent', nrow(T0_ID)), rep("Nonexistent", nrow(T4_ID)), 
                     head(rep('Nonexistent', nrow(T6_ID)), -1332), T8_ID$breastfed_3m_yn,
                     T10_ID$breastfed_3m_yn)
length(breastfed_3m_yn)
unique(breastfed_3m_yn)


# THIS IS THE NEW COMBINATION DATA FRAME WE ARE WORKING ON 
combo <- data.frame(gen_health_overall, injury, injury_types, medication_worms_yn, hospital_yn, hospital_antibiotics, doctor_yn, doctor_antibiotics, vaccinations_yn,breastfed_3m_yn, set, stringsAsFactors = FALSE)
combo 

# STARTING GRAPHS/ANALYSES 

# GENERAL HEALTH
# bar plot for health score 

# handles NA values and big negative values
combo <- mutate_at(combo, c("gen_health_overall"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$gen_health_overall[combo$gen_health_overall == -66] <- -2

unique(combo['gen_health_overall'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
gen_health <- combo %>% select(gen_health_overall, set)
gen_health <- gen_health[gen_health$gen_health_overall != -1,]
gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_gen_health <- aggregate(gen_health, by=list(set=gen_health$set, gen_health_score=gen_health$gen_health_overall), FUN=length)
group_gen_health <- group_gen_health[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_gen_health <- group_gen_health %>% rename(prop = gen_health_overall) # rename(new_name = old_name)
group_gen_health$prop[group_gen_health$set == 'T0'] <- group_gen_health$prop[group_gen_health$set == 'T0']/t0id_size
group_gen_health$prop[group_gen_health$set == 'T4'] <- group_gen_health$prop[group_gen_health$set == 'T4']/t4id_size
group_gen_health$prop[group_gen_health$set == 'T6'] <- group_gen_health$prop[group_gen_health$set == 'T6']/t6id_size
group_gen_health$prop[group_gen_health$set == 'T8'] <- group_gen_health$prop[group_gen_health$set == 'T8']/t8id_size
group_gen_health$prop[group_gen_health$set == 'T10'] <- group_gen_health$prop[group_gen_health$set == 'T10']/t10id_size
add <- data.frame(c("T0", "T4", "T6", "T8", "T10"), c(1, 1, 1, 1, 1), c(0.0, 0.0, 0.0, 0.0, 0.0))
names(add) <- c('set', 'gen_health_score', 'prop')

group_gen_health <- rbind(group_gen_health, add)
group_gen_health 
# plotting 
general_health <- ggplot(group_gen_health, aes(x=gen_health_score, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + 
  xlim(1, 5.5) + ylim(0.0, 100)+
  ggtitle("General Child Health") + labs(y='% Respondents', x='General Child Health Scores', fill = 'Set')
general_health

# INJURY
# analysis on injuries distribution
injury <- combo %>% select(injury, set)
injury <- injury[injury$injury != 'Nonexistent',]
injury
unique(injury)
names(injury)

# group
group_injury <- aggregate(injury, by=list(set=injury$set, injury_yn=injury$injury), FUN=length)
group_injury <- group_injury[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_injury <- group_injury %>% rename(prop = injury) # rename(new_name = old_name)
group_injury$prop[group_injury$set == 'T0'] <- group_injury$prop[group_injury$set == 'T0']/t0id_size
group_injury$prop[group_injury$set == 'T4'] <- group_injury$prop[group_injury$set == 'T4']/t4id_size
group_injury$prop[group_injury$set == 'T6'] <- group_injury$prop[group_injury$set == 'T6']/t6id_size
group_injury$prop[group_injury$set == 'T8'] <- group_injury$prop[group_injury$set == 'T8']/t8id_size
group_injury$prop[group_injury$set == 'T10'] <- group_injury$prop[group_injury$set == 'T10']/t10id_size
group_injury

#injury plotting
injury_plot  <- ggplot(group_injury, aes(x=injury_yn, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Injuries") + labs(y='% Respondents', x='Yes/No Injuries Reported', fill = "Set")
injury_plot

# INJURY TYPES ANALYSIS 
#handling injury types
injury_type <- combo %>% select(injury_types, set)
injury_type
unique(injury_type)
injury_type <- injury_type[injury_type$injury_types != 'Nonexistent',]
unique(injury_type) 

# replacing negative values
#combo <- mutate_at(combo, c("injury_types"), ~replace(., is.na(.), -1)) # Replace NA values with -1
#combo$injury_types[combo$injury_types == -66] <- -1
injury_type[injury_type == -66] <- -1
injury_type[injury_type == -77] <- -2
injury_type[injury_type == -88] <- -3
injury_type[injury_type == -99] <- -4
names(injury_type)

t0_in <- sum(injury$injury[injury_type$set == 'T0'] == 1, na.rm=TRUE)
t4_in <- sum(injury$injury[injury_type$set == 'T4'] == 1, na.rm=TRUE)
t6_in <- sum(injury$injury[injury_type$set == 'T6'] == 1, na.rm=TRUE)
t8_in <- sum(injury$injury[injury_type$set == 'T8'] == 1, na.rm=TRUE)
t10_in <- sum(injury$injury[injury_type$set == 'T10'] == 1, na.rm=TRUE)



# group
group_injury_type <- aggregate(injury_type, by=list(set=injury_type$set, injury_type=injury_type$injury_types), FUN=length)
group_injury_type
group_injury_type <- group_injury_type[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_injury_type <- group_injury_type %>% rename(prop = injury_types) # rename(new_name = old_name)
group_injury_type$prop[group_injury_type$set == 'T0'] <- group_injury_type$prop[group_injury_type$set == 'T0']/t0_in *100
group_injury_type$prop[group_injury_type$set == 'T4'] <- group_injury_type$prop[group_injury_type$set == 'T4']/t4_in *100
group_injury_type$prop[group_injury_type$set == 'T6'] <- group_injury_type$prop[group_injury_type$set == 'T6']/t6_in *100
group_injury_type$prop[group_injury_type$set == 'T8'] <- group_injury_type$prop[group_injury_type$set == 'T8']/t8_in *100
group_injury_type$prop[group_injury_type$set == 'T10'] <- group_injury_type$prop[group_injury_type$set == 'T8']/t10_in *100

group_injury_type
write.csv(group_injury_type, './tables/injury_type.csv')

# VACCINATIONS
combo <- mutate_at(combo, c("vaccinations_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$vaccinations_yn[combo$vaccinations_yn == -99] <- "Don't Know"

unique(combo['vaccinations_yn'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
vax_yn <- combo %>% select(vaccinations_yn, set)
vax_yn <- vax_yn[vax_yn$vaccinations_yn != "Nonexistent",]
vax_yn <- vax_yn[vax_yn$vaccinations_yn >=0,]
vax_yn

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_vax <- aggregate(vax_yn, by=list(set=vax_yn$set, vax_yn=vax_yn$vaccinations_yn), FUN=length)
group_vax <- group_vax[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_vax <- group_vax %>% rename(prop = vaccinations_yn) # rename(new_name = old_name)
group_vax$prop[group_vax$set == 'T0'] <- group_vax$prop[group_vax$set == 'T0']/t0id_size
group_vax$prop[group_vax$set == 'T4'] <- group_vax$prop[group_vax$set == 'T4']/t4id_size
group_vax$prop[group_vax$set == 'T6'] <- group_vax$prop[group_vax$set == 'T6']/t6id_size
group_vax$prop[group_vax$set == 'T8'] <- group_vax$prop[group_vax$set == 'T8']/t8id_size
group_vax$prop[group_vax$set == 'T10'] <- group_vax$prop[group_vax$set == 'T10']/t10id_size

group_vax
# plotting 
vax <- ggplot(group_vax, aes(x=vax_yn, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Vaccinations") + labs(y='% Respondents', x='Yes/No Received Vaccines', fill = 'Set')
vax


#injury types plot
#injury_type_plot  <- ggplot(group_injury_type, aes(x=injury_type, fill=set, y=prop)) + 
#  geom_bar(position='dodge', stat='identity') + 
#  ggtitle("Injury Type") + labs(y='# Respondents', x='Type of Injury') + 
#  scale_x_discrete(labels=c('1'='Broken Bone', '2'='Burn', '3'='Cut/laceration', '4'='Concussion/loss of consciousness', '5'='Poisoning', '6'='Swollen muscle/bruising', '-1'='Question not asked', '-2'="Other Injury", '-3'='Refused to answer', '-4'='Unknown')) +
#  theme(axis.text.x = element_text(angle=0, face='bold', color='black', size='6')) #+coord_flip()
#injury_type_plot 

# _________________________________________________________________________________________________________________

# RONIT CODE

# handles NA values and big negative values
# HOSPITAL Y/N
#combo <- mutate_at(combo, c("hospital_yn"), ~replace(., is.na(.), -1)) # Replace NA values with -1
#combo$hospital_yn[combo$hospital_yn == -99] <- -2
unique(combo['hospital_yn'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
hospital_visited <- combo %>% select(hospital_yn, set)
# hospital_visited <- mutate_at(hospital_visited, c("hospital_yn"), ~replace(., is.na(.), -1))
# hospital_visited[hospital_visited == -99] <- -2
hospital_visited$hospital_yn[hospital_visited$hospital_yn == -99] <- "DK"
hospital_visited <- hospital_visited[is.na(hospital_visited$hospital_yn) == FALSE,]
hospital_visited <- hospital_visited[hospital_visited$hospital_yn != 'Nonexistent',]
hospital_visited <- hospital_visited[hospital_visited$hospital_yn != -1,]
# hospital_visited <- hospital_visited[hospital_visited$hospital_yn != -99,]
#unique(hospital_visited)
#hospital_visited[hospital_yn == -99] <- -2
hospital_visited

# reorganize data, group by set # and the variable
# hospital_visited: dataframe of variable of interest (general health) and set
# intermediate_hospital_yn: intermediate var name for aggregating 
# hospital_yn: the actual column we are getting information/values from 
group_hospital_yn <- aggregate(hospital_visited, by=list(set=hospital_visited$set, intermediate_hospital_yn=hospital_visited$hospital_yn), FUN=length)
group_hospital_yn <- group_hospital_yn[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_hospital_yn <- group_hospital_yn %>% rename(prop = hospital_yn) # rename(new_name = old_name)
group_hospital_yn$prop[group_hospital_yn$set == 'T0'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T0']/t0id_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T4'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T4']/t4id_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T6'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T6']/t6id_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T8'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T8']/t8id_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T10'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T10']/t10id_size * 100

# plotting 
hospital_yn_id_plot <- ggplot(group_hospital_yn, aes(x=intermediate_hospital_yn, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Hospital Visits (Indonesia)") + labs(y='Percentage (%)', x='Hospital Visits', fill = "Set")
hospital_yn_id_plot

# yes_doctor_anti_T4_ID <- T4_ID %>% filter(doctor_yn >0) %>% select(doctor_antibiotics)

# HOSPITAL ANTIBIOTICS
hospital_anti <- combo %>% filter(hospital_yn > 0) %>% select(hospital_antibiotics, set)
# hospital_anti <- combo %>% select(hospital_antibiotics, set)
hospital_anti <- hospital_anti[hospital_anti$hospital_antibiotics != 'Nonexistent',]
hospital_anti
unique(hospital_anti)

# hospital_anti[hospital_antibiotics == -99] <- -1
hospital_anti$hospital_antibiotics[hospital_anti$hospital_antibiotics == -99] <- "Didn't Know"
# hospital_anti <- hospital_anti[hospital_anti$hospital_antibiotics != -99,]

# hospital_anti_size <- nrow(hospital_anti)
hospital_anti_t0id_filtered <- T0_ID %>% filter(hospital_yn > 0)
hospital_anti_t0id_size <- nrow(hospital_anti_t0id_filtered)
hospital_anti_t4id_filtered <- T4_ID %>% filter(hospital_yn > 0)
hospital_anti_t4id_size <- nrow(hospital_anti_t4id_filtered)
hospital_anti_t6id_filtered <- T6_ID %>% filter(hospital_yn > 0)
hospital_anti_t6id_size <- nrow(hospital_anti_t6id_filtered)
hospital_anti_t8id_filtered <- T8_ID %>% filter(hospital_yn > 0)
hospital_anti_t8id_size <- nrow(hospital_anti_t8id_filtered)
hospital_anti_t10id_filtered <- T10_ID %>% filter(hospital_yn > 0)
hospital_anti_t10id_size <- nrow(hospital_anti_t10id_filtered)

names(hospital_anti)

# group
group_hospital_anti <- aggregate(hospital_anti, by=list(set=hospital_anti$set, intermediate_hospital_anti=hospital_anti$hospital_antibiotics), FUN=length)
group_hospital_anti
group_hospital_anti <- group_hospital_anti[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_hospital_anti <- group_hospital_anti %>% rename(prop = hospital_antibiotics) # rename(new_name = old_name)
group_hospital_anti$prop[group_hospital_anti$set == 'T0'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T0']/hospital_anti_t0id_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T4'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T4']/hospital_anti_t4id_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T6'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T6']/hospital_anti_t6id_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T8'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T8']/hospital_anti_t8id_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T10'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T10']/hospital_anti_t10id_size * 100


add_hospital_anti_zeroes <- data.frame(c("T4", "T8", "T10"), c(0, 0, 0), c(0.0, 0.0, 0.0))
names(add_hospital_anti_zeroes) <- c("set", "intermediate_hospital_anti", "prop")
add_hospital_anti_zeroes
group_hospital_anti <- rbind(group_hospital_anti, add_hospital_anti_zeroes)
group_hospital_anti

add_hospital_anti_dk <- data.frame(c("T4", "T8", "T10"), c("Didn't Know","Didn't Know", "Didn't Know"), c(0.0, 0.0, 0.0))
names(add_hospital_anti_dk) <- c("set", "intermediate_hospital_anti", "prop")
add_hospital_anti_dk
group_hospital_anti <- rbind(group_hospital_anti, add_hospital_anti_dk)
group_hospital_anti

#hospital antibiotics plotting
hospital_antibiotics_plot  <- ggplot(group_hospital_anti, aes(x=intermediate_hospital_anti, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Hospital Antibiotics") + labs(y='Percentage (%)', x='Yes/No Hospital Antibiotics', fill = "Set") + 
  ylim(0, 60)
hospital_antibiotics_plot


D# WATER ANALYSIS 
#handling water
#water <- combo %>% select(water_yn, set)
#unique(water)
#water <- water[water$water_yn != 'Nonexistent',]
#unique(water) 

# replacing negative values
#water[water == -99] <- -2
#water <- mutate_at(water, c("water_yn"), ~replace(., is.na(.), -1)) # Replace NA values with -1

#names(water)

# group
#group_water <- aggregate(water, by=list(set=water$set, intermediate_water=water$water_yn), FUN=length)
#group_water
#group_water <- group_water[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
#group_water <- group_water %>% rename(count = water_yn) # rename(new_name = old_name)
#group_water

#water plot
#water_plot  <- ggplot(group_water, aes(x=intermediate_water, fill=set, y=count)) + 
#  geom_bar(position='dodge', stat='identity') + 
#  ggtitle("Drank Water") + labs(y='# Respondents', x='Drank Water')
#water_plot 

# NICOL CODE
# STARTING GRAPHS/ANALYSES 
#doctor visit
combo <- mutate_at(combo, c("doctor_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$doctor_yn[combo$doctor_yn == -66] <- -2
combo$doctor_yn[combo$doctor_yn == -99] <- "Didn't know"

unique(combo['doctor_yn'])

doctor_visit <- combo %>% select(doctor_yn, set)
doctor_visit <- doctor_visit[doctor_visit$doctor_yn != -1,]
doctor_visit <- doctor_visit[doctor_visit$doctor_yn != "Nonexistent",]

# reorganize data, group by set # and the variable
# doctor_visit: dataframe of variable of interest (doctor visit) and set
# doc_visit_score: intermediate var name for aggregating 
# doctor_yn: the actual column we are getting information/values from 
group_doctor_visit <- aggregate(doctor_visit, by=list(set=doctor_visit$set, doc_visit_score=doctor_visit$doctor_yn), FUN=length)
group_doctor_visit <- group_doctor_visit[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_doctor_visit <- group_doctor_visit %>% rename(prop = doctor_yn) # rename(new_name = old_name)

group_doctor_visit$prop[group_doctor_visit$set == 'T0'] <- group_doctor_visit$prop[group_doctor_visit$set == 'T0']/t0id_size
group_doctor_visit$prop[group_doctor_visit$set == 'T4'] <- group_doctor_visit$prop[group_doctor_visit$set == 'T4']/t4id_size
group_doctor_visit$prop[group_doctor_visit$set == 'T6'] <- group_doctor_visit$prop[group_doctor_visit$set == 'T6']/t6id_size
group_doctor_visit$prop[group_doctor_visit$set == 'T8'] <- group_doctor_visit$prop[group_doctor_visit$set == 'T8']/t8id_size
group_doctor_visit$prop[group_doctor_visit$set == 'T10'] <- group_doctor_visit$prop[group_doctor_visit$set == 'T10']/t10id_size

group_doctor_visit
group_doctor_visit$doc_visit_score <- sapply(group_doctor_visit$doc_visit_score, as.integer)
group_doctor_visit$doc_visit_score
group_doctor_visit <- group_doctor_visit[-c(24),]
group_doctor_visit$doc_visit_score
group_doctor_visit[is.na(group_doctor_visit)] <- -1
# plotting 
bt= 0:12
doc_vis <- ggplot(group_doctor_visit, aes(x=doc_visit_score, y=prop*100, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')))) + 
  geom_bar(position='dodge', stat='identity') + theme(axis.text.x = element_text(angle = 90, vjust = 1)) + xlim(0.0, 15.0) + ylim(0.0, 100) +
  ggtitle("Doctor Visits") + labs(y='% of Respondents', x='Number of Doctor Visits', fill = 'Set')
doc_vis


# Antibiotics
combo <- mutate_at(combo, c("doctor_antibiotics"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$doctor_antibiotics[combo$doctor_antibiotics == -66] <- -2
combo$doctor_antibiotics[combo$doctor_antibiotics == -99] <- "Didn't know" 

unique(combo['doctor_antibiotics'])

doctor_antibio <- combo %>% filter(doctor_yn > 0) %>% select(doctor_antibiotics, set)
doctor_antibio
doctor_antibio <- doctor_antibio[doctor_antibio$doctor_antibiotics != -1,]
doctor_antibio <- doctor_antibio[doctor_antibio$doctor_antibiotics != "Nonexistent",]


group_doctor_antibio <- aggregate(doctor_antibio, by=list(set=doctor_antibio$set, doc_antibio_score=doctor_antibio$doctor_antibiotics), FUN=length)
group_doctor_antibio <- group_doctor_antibio[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_doctor_antibio <- group_doctor_antibio %>% rename(prop = doctor_antibiotics) # rename(new_name = old_name)
group_doctor_antibio
group_doctor_antibio$prop[group_doctor_antibio$set == 'T0'] <- group_doctor_antibio$prop[group_doctor_antibio$set == 'T0']/d_a_0
group_doctor_antibio$prop[group_doctor_antibio$set == 'T4'] <- group_doctor_antibio$prop[group_doctor_antibio$set == 'T4']/d_a_4
group_doctor_antibio$prop[group_doctor_antibio$set == 'T6'] <- group_doctor_antibio$prop[group_doctor_antibio$set == 'T6']/d_a_6
group_doctor_antibio$prop[group_doctor_antibio$set == 'T8'] <- group_doctor_antibio$prop[group_doctor_antibio$set == 'T8']/d_a_8
group_doctor_antibio$prop[group_doctor_antibio$set == 'T10'] <- group_doctor_antibio$prop[group_doctor_antibio$set == 'T10']/d_a_10

group_doctor_antibio
t0id_size
# plotting 
doc_anti <- ggplot(group_doctor_antibio, aes(x=doc_antibio_score, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) +
  ggtitle("Antiobiotics Given During Doctor Visits") + labs(y='% Respondents', x='Recieved Antibiotics', fill = "Set")
doc_anti


#medication for worms
combo <- mutate_at(combo, c("medication_worms_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$medication_worms_yn[combo$medication_worms_yn == -66] <- -2
combo$medication_worms_yn[combo$medication_worms_yn == -99] <- "Didn't know"

unique(combo['medication_worms_yn'])

med_worms <- combo %>% select(medication_worms_yn, set)
med_worms
med_worms <- med_worms[med_worms$medication_worms_yn != -1,]
med_worms <- med_worms[med_worms$medication_worms_yn != "Nonexistent",]

group_med_worms <- aggregate(med_worms, by=list(set=med_worms$set, med_worms_score=med_worms$medication_worms_yn), FUN=length)
group_med_worms <- group_med_worms[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_med_worms <- group_med_worms %>% rename(prop = medication_worms_yn) # rename(new_name = old_name)
group_med_worms
group_med_worms$prop[group_med_worms$set == 'T0'] <- group_med_worms$prop[group_med_worms$set == 'T0']/t0id_size
group_med_worms$prop[group_med_worms$set == 'T4'] <- group_med_worms$prop[group_med_worms$set == 'T4']/t4id_size
group_med_worms$prop[group_med_worms$set == 'T6'] <- group_med_worms$prop[group_med_worms$set == 'T6']/t6id_size
group_med_worms$prop[group_med_worms$set == 'T8'] <- group_med_worms$prop[group_med_worms$set == 'T8']/t8id_size
group_med_worms$prop[group_med_worms$set == 'T10'] <- group_med_worms$prop[group_med_worms$set == 'T10']/t10id_size
group_med_worms

# plotting 
worms_med <- ggplot(group_med_worms, aes(x=med_worms_score, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')), y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) +
  ggtitle("Given Deworming Treatment") + labs(y='% Respondents', x='Recieved Treatment', fill = 'Set')
worms_med

#breastfeed
combo <- mutate_at(combo, c("breastfed_3m_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
#combo$doctor_yn[combo$doctor_yn == -66] <- -2
#combo$doctor_yn[combo$doctor_yn == -99] <- "Didn't know"

unique(combo['breastfed_3m_yn'])

breastfed_3 <- combo %>% select(breastfed_3m_yn, set)
breastfed_3 <- breastfed_3[breastfed_3$breastfed_3m_yn != -1,]
breastfed_3 <- breastfed_3[breastfed_3$breastfed_3m_yn != "Nonexistent",]
breastfed_3
# reorganize data, group by set # and the variable
# doctor_visit: dataframe of variable of interest (doctor visit) and set
# doc_visit_score: intermediate var name for aggregating 
# doctor_yn: the actual column we are getting information/values from 
group_breastfed_3 <- aggregate(breastfed_3, by=list(set=breastfed_3$set, breastfed_3_score=breastfed_3$breastfed_3m_yn), FUN=length)
group_breastfed_3
group_breastfed_3 <- group_breastfed_3[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_breastfed_3 <- group_breastfed_3 %>% rename(prop = breastfed_3m_yn) # rename(new_name = old_name)

group_breastfed_3$prop[group_breastfed_3$set == 'T0'] <- group_breastfed_3$prop[group_breastfed_3$set == 'T0']/t0id_size
group_breastfed_3$prop[group_breastfed_3$set == 'T4'] <- group_breastfed_3$prop[group_breastfed_3$set == 'T4']/t4id_size
group_breastfed_3$prop[group_breastfed_3$set == 'T6'] <- group_breastfed_3$prop[group_breastfed_3$set == 'T6']/t6id_size
group_breastfed_3$prop[group_breastfed_3$set == 'T8'] <- group_breastfed_3$prop[group_breastfed_3$set == 'T8']/t8id_size
group_breastfed_3$prop[group_breastfed_3$set == 'T10'] <- group_breastfed_3$prop[group_breastfed_3$set == 'T10']/t10id_size
group_breastfed_3
# plotting 
breastfed_3m <- ggplot(group_breastfed_3, aes(x=breastfed_3_score, y=prop*100, fill=factor(set, levels=c('T0', 'T4', 'T6', 'T8', 'T10')))) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) +
  ggtitle("Consumption of Breast Milk") + labs(y='% of Respondents', x='Drank Breast Milk (within 3 months)', fill = 'Set')
breastfed_3m

