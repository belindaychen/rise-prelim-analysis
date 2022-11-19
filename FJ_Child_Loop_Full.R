library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(data.table)

rm(list = ls())
setwd('C:/Users/ext-belche/rise-prelim-analysis')

# loading data into R 
child_data_T0_FJ <- read.csv(file='FJ_data/hhd_child_loop_T0_FJ.csv')
child_data_T2_FJ <- read.csv(file='FJ_data/hhd_child_loop_T2_FJ.csv')
child_data_T4_FJ <- read.csv(file='FJ_data/hhd_child_symptoms_T4_FJ.csv')
child_data_T6_FJ <- read.csv(file='FJ_data/hhd_child_loop_T6_FJ.csv')
child_data_T8_FJ <- read.csv(file='FJ_data/hhd_child_loop_T8_FJ.csv')


T0_FJ <- cbind(child_data_T0_FJ)
T2_FJ <- cbind(child_data_T2_FJ)
T4_FJ <- cbind(child_data_T4_FJ)
T6_FJ <- cbind(child_data_T6_FJ)
T8_FJ <- cbind(child_data_T8_FJ)


# TABLE SIZE 
t0fj_size <- nrow(T0_FJ)
t2fj_size <- nrow(T2_FJ)
t4fj_size <- nrow(T4_FJ)
t6fj_size <- nrow(T6_FJ)
t8fj_size <- nrow(T8_FJ)


# CREATING THE COMBO TABLE 
# add column 
num_rows <- nrow(T0_FJ)
T0_FJ['Set'] <- rep('T0', num_rows)
num_rows <- nrow(T2_FJ)
T2_FJ['Set'] <- rep('T2', num_rows)
num_rows <- nrow(T4_FJ)
T4_FJ['Set'] <- rep('T4', num_rows)
num_rows <- nrow(T6_FJ)
T6_FJ['Set'] <- rep('T6', num_rows)
num_rows <- nrow(T8_FJ)
T8_FJ['Set'] <- rep('T8', num_rows)

# set column 
set <- c(T0_FJ$Set, T2_FJ$Set, head(T4_FJ$Set, -1055), T6_FJ$Set, T8_FJ$Set)
length(set)
unique(set)

d_a_0_F <- T0_FJ %>% filter(doctor_yn>0)
d_a_0_F<- nrow(d_a_0_F)
d_a_2_F <- T2_FJ %>% filter(doctor_yn>0)
d_a_2_F<- nrow(d_a_2_F)
d_a_4_F <- T4_FJ %>% filter(doctor_yn>0)
d_a_4_F <- nrow(d_a_4_F)
d_a_6_F<- T6_FJ %>% filter(doctor_yn>0)
d_a_6_F <- nrow(d_a_6_F)
d_a_8_F<- T8_FJ %>% filter(doctor_yn>0)
d_a_8_F <- nrow(d_a_8_F)

# health general
gen_health_overall <- c(T0_FJ$health_general_child, T2_FJ$health_general_child,
                        T4_FJ$health_general_child, T6_FJ$health_general_child,
                        T8_FJ$health_general_child)
length(gen_health_overall)
unique(gen_health_overall)

injury <- c(T0_FJ$injury, T2_FJ$injury,
            head(T4_FJ$injury, -1055), T6_FJ$injury, T8_FJ$injury)
length(injury)
unique(injury)

injury_types <- c(T0_FJ$injury_type, T2_FJ$injury_type,
                  head(T4_FJ$injury_type, -1055), T6_FJ$injury_type, T8_FJ$injury_type)
length(injury_types)
unique(injury_types)

medication_worms_yn <- c(rep('Nonexistent', nrow(T0_FJ)), T2_FJ$medication_worms_yn,
                         head(rep('Nonexistent', nrow(T4_FJ)), -1055), T6_FJ$medication_worms_yn,
                         T8_FJ$medication_worms_yn)
length(medication_worms_yn)
unique(medication_worms_yn)

doctor_yn <- c(T0_FJ$doctor_yn, T2_FJ$doctor_yn,
               head(T4_FJ$doctor_yn, -1055), T6_FJ$doctor_yn,
               T8_FJ$doctor_yn)
length(doctor_yn)
unique(doctor_yn)

doctor_antibiotics <- c(T0_FJ$doctor_antibiotics, T2_FJ$doctor_antibiotics,
                        head(T4_FJ$doctor_antibiotics, -1055), T6_FJ$doctor_antibiotics,
                        T8_FJ$doctor_antibiotics)
length(doctor_antibiotics)
unique(doctor_antibiotics)

hospital_yn <- c(T0_FJ$hospital_yn, T2_FJ$hospital_yn,
                 head(T4_FJ$hospital_yn, -1055), T6_FJ$hospital_yn, T8_FJ$hospital_yn)
length(hospital_yn)
unique(hospital_yn)

hospital_antibiotics <- c(T0_FJ$hospital_antibiotics, T2_FJ$hospital_antibiotics,
                          head(T4_FJ$hospital_antibiotics, -1055), T6_FJ$hospital_antibiotics,
                          T8_FJ$hospital_antibiotics)
length(hospital_antibiotics)
unique(hospital_antibiotics)


breastfed_3m_yn <- c(rep('Nonexistent', nrow(T0_FJ)), rep('Nonexistent', nrow(T2_FJ)),
                     head(rep('Nonexistent', nrow(T4_FJ)), -1055), T6_FJ$breastfed_3m_yn, T8_FJ$breastfed_3m_yn)
length(breastfed_3m_yn)
unique(breastfed_3m_yn)

vaccinations_yn <- c(rep('Nonexistent', nrow(T0_FJ)), T2_FJ$vaccinations_yn,
                     head(rep('Nonexistent', nrow(T4_FJ)), -1055), rep('Nonexistent', nrow(T6_FJ)),
                     T8_FJ$vaccinations_yn_a)

length(vaccinations_yn)
unique(vaccinations_yn)

#water_yn <- c(rep('Nonexistent', nrow(T0_ID)), rep('Nonexistent', nrow(T4_ID)),
#              rep('Nonexistent', nrow(T6_ID)), T8_ID$water_yn)
#length(water_yn)
#unique(water_yn)

# THIS IS THE NEW COMBINATION DATA FRAME WE ARE WORKING ON 
combo <- data.frame(gen_health_overall, injury, injury_types, medication_worms_yn, hospital_yn, hospital_antibiotics, doctor_yn, doctor_antibiotics,breastfed_3m_yn, vaccinations_yn, set, stringsAsFactors = FALSE)
combo 

# STARTING GRAPHS/ANALYSES 

# GENERAL HEALTH
# bar plot for health score 

# handles NA values and big negative values
combo <- mutate_at(combo, c("gen_health_overall"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$gen_health_overall[combo$gen_health_overall == -66] <- -2
combo$gen_health_overall[combo$gen_health_overall == -88] <- -3
combo$gen_health_overall[combo$gen_health_overall == -99] <- -4

unique(combo['gen_health_overall'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
gen_health <- combo %>% select(gen_health_overall, set)
gen_health <- gen_health[gen_health$gen_health_overall >= 0,]
#gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_gen_health <- aggregate(gen_health, by=list(set=gen_health$set, gen_health_score=gen_health$gen_health_overall), FUN=length)
group_gen_health <- group_gen_health[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_gen_health <- group_gen_health %>% rename(prop = gen_health_overall) # rename(new_name = old_name)
group_gen_health$prop[group_gen_health$set == 'T0'] <- group_gen_health$prop[group_gen_health$set == 'T0']/t0fj_size
group_gen_health$prop[group_gen_health$set == 'T2'] <- group_gen_health$prop[group_gen_health$set == 'T2']/t2fj_size
group_gen_health$prop[group_gen_health$set == 'T4'] <- group_gen_health$prop[group_gen_health$set == 'T4']/t4fj_size
group_gen_health$prop[group_gen_health$set == 'T6'] <- group_gen_health$prop[group_gen_health$set == 'T6']/t6fj_size
group_gen_health$prop[group_gen_health$set == 'T8'] <- group_gen_health$prop[group_gen_health$set == 'T8']/t8fj_size

group_gen_health
# plotting 
general_health <- ggplot(group_gen_health, aes(x=gen_health_score, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') +
  xlim(1, 5.5) + ylim(0, 100)+
  ggtitle("General Child Health") + labs(y='% Respondents', x='General Child Health Scores')
general_health

# INJURY
# analysis on injuries distribution

injury <- combo %>% select(injury, set)
injury
injury <- injury[injury$injury != 'Nonexistent',]
injury <- injury[is.na(injury$injury) == FALSE,]
injury <- injury[injury$injury != -99,]
injury <- injury[injury$injury != -66,]
#injury[injury == -77,] <- 0
#injury <- injury[injury$injury >= 0,]
#injury[injury == -66,] <- -2
#injury[injury == -99,] <- -3
injury
unique(injury)
names(injury)

# group
group_injury <- aggregate(injury, by=list(set=injury$set, injury_yn=injury$injury), FUN=length)
group_injury <- group_injury[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_injury <- group_injury %>% rename(prop = injury) # rename(new_name = old_name)
group_injury$prop[group_injury$set == 'T0'] <- group_injury$prop[group_injury$set == 'T0']/t0fj_size
group_injury$prop[group_injury$set == 'T2'] <- group_injury$prop[group_injury$set == 'T2']/t2fj_size
group_injury$prop[group_injury$set == 'T4'] <- group_injury$prop[group_injury$set == 'T4']/t4fj_size
group_injury$prop[group_injury$set == 'T6'] <- group_injury$prop[group_injury$set == 'T6']/t6fj_size
group_injury$prop[group_injury$set == 'T8'] <- group_injury$prop[group_injury$set == 'T8']/t8fj_size
group_injury

#injury plot
injury_plot  <- ggplot(group_injury, aes(x=injury_yn, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Injuries") + labs(y='% Respondents', x='Yes/No Injuries Reported')
injury_plot

# INJURY TYPES ANALYSIS 
#handling injury types
injury_type <- combo %>% select(injury_types, set)
unique(injury_type)
injury_type <- injury_type[injury_type$injury_types != 'Nonexistent',]
injury_type <- injury_type[is.na(injury_type$injury_types) == FALSE,]
injury_type
injury_type$injury_types[injury_type$injury_types == -77] <- -2
#injury <- injury[injury$injury >= 0,]
#injury_type <- injury_type[injury_type$injury_types != -77,]
unique(injury_type) 

t0_in <- sum(injury$injury[injury_type$set == 'T0'] == 1, na.rm=TRUE)
t2_in <- sum(injury$injury[injury_type$set == 'T2'] == 1, na.rm=TRUE)
t4_in <- sum(injury$injury[injury_type$set == 'T4'] == 1, na.rm=TRUE)
t6_in <- sum(injury$injury[injury_type$set == 'T6'] == 1, na.rm=TRUE)
t8_in <- sum(injury$injury[injury_type$set == 'T8'] == 1, na.rm=TRUE)


# replacing negative values
#combo <- mutate_at(combo, c("injury_types"), ~replace(., is.na(.), -1)) # Replace NA values with -1
#combo$injury_types[combo$injury_types == -66] <- -1
#injury_type[injury_type == -66] <- -1
#injury_type[injury_type == -77] <- -2
#injury_type[injury_type == -88] <- -3
#injury_type[injury_type == -99] <- -4

names(injury_type)

# group
group_injury_type <- aggregate(injury_type, by=list(set=injury_type$set, injury_type=injury_type$injury_types), FUN=length)
group_injury_type
group_injury_type <- group_injury_type[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_injury_type <- group_injury_type %>% rename(prop = injury_types) # rename(new_name = old_name)
group_injury_type$prop[group_injury_type$set == 'T0'] <- group_injury_type$prop[group_injury_type$set == 'T0']/t0_in *100
group_injury_type$prop[group_injury_type$set == 'T2'] <- group_injury_type$prop[group_injury_type$set == 'T2']/t2_in *100
group_injury_type$prop[group_injury_type$set == 'T4'] <- group_injury_type$prop[group_injury_type$set == 'T4']/t4_in *100
group_injury_type$prop[group_injury_type$set == 'T6'] <- group_injury_type$prop[group_injury_type$set == 'T6']/t6_in *100
group_injury_type$prop[group_injury_type$set == 'T8'] <- group_injury_type$prop[group_injury_type$set == 'T8']/t6_in *100
group_injury_type
write.csv(group_injury_type, './tables/injury_type_fj.csv')

# vaccinations 
vax_yn <- combo %>% select(vaccinations_yn, set)
vax_yn
vax_yn$vaccinations_yn[vax_yn$vaccinations_yn == "-99"] <- "Don't Know"
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
group_vax$prop[group_vax$set == 'T0'] <- group_vax$prop[group_vax$set == 'T0']/t0fj_size
group_vax$prop[group_vax$set == 'T2'] <- group_vax$prop[group_vax$set == 'T2']/t2fj_size
group_vax$prop[group_vax$set == 'T4'] <- group_vax$prop[group_vax$set == 'T4']/t4fj_size
group_vax$prop[group_vax$set == 'T6'] <- group_vax$prop[group_vax$set == 'T6']/t6fj_size
group_vax$prop[group_vax$set == 'T8'] <- group_vax$prop[group_vax$set == 'T8']/t8fj_size
group_vax
# plotting 
vax <- ggplot(group_vax, aes(x=vax_yn, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Vaccinations") + labs(y='% Respondents', x='Yes/No Received Vaccines') + 
  ylim(0, 30)
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
combo <- mutate_at(combo, c("hospital_yn"), ~replace(., is.na(.), -1)) # Replace NA values with -1
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
group_hospital_yn$prop[group_hospital_yn$set == 'T0'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T0']/t0fj_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T2'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T2']/t2fj_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T4'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T4']/t4fj_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T6'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T6']/t6fj_size * 100
group_hospital_yn$prop[group_hospital_yn$set == 'T8'] <- group_hospital_yn$prop[group_hospital_yn$set == 'T8']/t8fj_size * 100
#group_hospital_yn$intermediate_hospital_yn <- sapply(group_hospital_yn$intermediate_hospital_yn, as.integer)
#group_hospital_yn <- group_hospital_yn[-c(24),]
# group_hospital_yn <- data.frame(group_hospital_yn[1, ], group_hospital_yn[2, ], group_hospital_yn[3, ], group_hospital_yn[4, ], group_hospital_yn[5, ], group_hospital_yn[6, ], group_hospital_yn[9, ], group_hospital_yn[10, ], group_hospital_yn[11, ], group_hospital_yn[12, ], group_hospital_yn[15, ], group_hospital_yn[16, ], group_hospital_yn[7, ], group_hospital_yn[8, ], group_hospital_yn[13, ], group_hospital_yn[14, ], group_hospital_yn[17, ])
# group_hospital_yn[1, ] <- 
# plotting 
#bt <- -1:65
hospital_yn_fj_plot <- ggplot(group_hospital_yn, aes(x=intermediate_hospital_yn, fill=set, y=prop)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Hospital Visits (Fiji)") + labs(y='Percentage (%)', x='Hospital Visits')
#scale_x_continuous("Yes/No Hospital Antibiotics", labels = as.character(bt), breaks = bt)
hospital_yn_fj_plot

# yes_doctor_anti_T4_ID <- T4_ID %>% filter(doctor_yn >0) %>% select(doctor_antibiotics)

# HOSPITAL ANTIBIOTICS
hospital_anti <- combo %>% filter(hospital_yn > 0) %>% select(hospital_antibiotics, set)
# hospital_anti <- combo %>% select(hospital_antibiotics, set)
hospital_anti <- hospital_anti[hospital_anti$hospital_antibiotics != 'Nonexistent',]
hospital_anti
unique(hospital_anti)

# hospital_anti[hospital_antibiotics == -99] <- -1
# hospital_anti$hospital_antibiotics[hospital_anti$hospital_antibiotics == -99] <- -1

hospital_anti_t0fj_filtered <- T0_FJ %>% filter(hospital_yn > 0)
hospital_anti_t0fj_size <- nrow(hospital_anti_t0fj_filtered)
hospital_anti_t2fj_filtered <- T2_FJ %>% filter(hospital_yn > 0)
hospital_anti_t2fj_size <- nrow(hospital_anti_t2fj_filtered)
hospital_anti_t4fj_filtered <- T4_FJ %>% filter(hospital_yn > 0)
hospital_anti_t4fj_size <- nrow(hospital_anti_t4fj_filtered)
hospital_anti_t6fj_filtered <- T6_FJ %>% filter(hospital_yn > 0)
hospital_anti_t6fj_size <- nrow(hospital_anti_t6fj_filtered)
hospital_anti_t8fj_filtered <- T8_FJ %>% filter(hospital_yn > 0)
hospital_anti_t8fj_size <- nrow(hospital_anti_t8fj_filtered)

names(hospital_anti)

# group
group_hospital_anti <- aggregate(hospital_anti, by=list(set=hospital_anti$set, intermediate_hospital_anti=hospital_anti$hospital_antibiotics), FUN=length)
group_hospital_anti
group_hospital_anti <- group_hospital_anti[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_hospital_anti <- group_hospital_anti %>% rename(prop = hospital_antibiotics) # rename(new_name = old_name)
group_hospital_anti$prop[group_hospital_anti$set == 'T0'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T0']/hospital_anti_t0fj_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T2'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T2']/hospital_anti_t2fj_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T4'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T4']/hospital_anti_t4fj_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T6'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T6']/hospital_anti_t6fj_size * 100
group_hospital_anti$prop[group_hospital_anti$set == 'T8'] <- group_hospital_anti$prop[group_hospital_anti$set == 'T8']/hospital_anti_t8fj_size * 100

add_hospital_anti_zeroes <- data.frame(c("T0", "T2"), c(0, 0), c(0.0, 0.0))
names(add_hospital_anti_zeroes) <- c("set", "intermediate_hospital_anti", "prop")
add_hospital_anti_zeroes
group_hospital_anti <- rbind(group_hospital_anti, add_hospital_anti_zeroes)
group_hospital_anti

add_hospital_anti_dk <- data.frame(c("T0", "T2", "T6", "T8"), c("Didn't know","Didn't know","Didn't know", "Didn't know"), c(0.0, 0.0, 0.0, 0.0))
names(add_hospital_anti_dk) <- c("set", "intermediate_hospital_anti", "prop")
add_hospital_anti_dk
group_hospital_anti <- rbind(group_hospital_anti, add_hospital_anti_dk)
group_hospital_anti

#hospital antiobiotics plot
hospital_antibiotics_plot  <- ggplot(group_hospital_anti, aes(x=intermediate_hospital_anti, fill=set, y=prop)) + 
  geom_bar(position='dodge', stat='identity') + 
  ggtitle("Hospital Antibiotics") + labs(y='Percentage (%)', x='Yes/No Hospital Antibiotics')
hospital_antibiotics_plot


# WATER ANALYSIS 
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
combo$doctor_yn[combo$doctor_yn == -88] <- -3
combo$doctor_yn[combo$doctor_yn == -99] <- "Didn't Know"

unique(combo['doctor_yn'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
doc_visit <- combo %>% select(doctor_yn, set)
doc_visit <- doc_visit[doc_visit$doctor_yn != -1,]
doc_visit <- doc_visit[doc_visit$doctor_yn != "Nonexistent",]
#gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_doc_visit <- aggregate(doc_visit, by=list(set=doc_visit$set, doc_visit_score=doc_visit$doctor_yn), FUN=length)
group_doc_visit <- group_doc_visit[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_doc_visit <- group_doc_visit %>% rename(prop = doctor_yn) # rename(new_name = old_name)
group_doc_visit$prop[group_doc_visit$set == 'T0'] <- group_doc_visit$prop[group_doc_visit$set == 'T0']/t0fj_size
group_doc_visit$prop[group_doc_visit$set == 'T2'] <- group_doc_visit$prop[group_doc_visit$set == 'T2']/t2fj_size
group_doc_visit$prop[group_doc_visit$set == 'T4'] <- group_doc_visit$prop[group_doc_visit$set == 'T4']/t4fj_size
group_doc_visit$prop[group_doc_visit$set == 'T6'] <- group_doc_visit$prop[group_doc_visit$set == 'T6']/t6fj_size
group_doc_visit$prop[group_doc_visit$set == 'T8'] <- group_doc_visit$prop[group_doc_visit$set == 'T8']/t8fj_size

group_doc_visit$doc_visit_score <- sapply(group_doc_visit$doc_visit_score, as.integer)
group_doc_visit$doc_visit_score
group_doc_visit <- group_doc_visit[-c(24),]
group_doc_visit$doc_visit_score
group_doc_visit[is.na(group_doc_visit)] <- -1
#group_doc_visit
#group_doc_visit$doc_visit_score
# plotting 
bt= -1:15
doctor_visit <- ggplot(group_doc_visit, aes(x=doc_visit_score, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  scale_x_continuous("Number of Doctor Visits", labels = as.character(bt), breaks = bt) + xlim(0.0, 15.0) + ylim(0.0, 100) +
  ggtitle("Doctor Visit") + labs(y='% Respondents', x='Number of Doctor Visits')
doctor_visit


#antibotics
combo <- mutate_at(combo, c("doctor_antibiotics"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$doctor_antibiotics[combo$doctor_antibiotics == -66] <- -2
combo$doctor_antibiotics[combo$doctor_antibiotics == -88] <- -3
combo$doctor_antibiotics[combo$doctor_antibiotics == -99] <- "Didn't know"

unique(combo['doctor_antibiotics'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
doc_antis <- combo %>% filter(doctor_yn >0) %>% select(doctor_antibiotics, set)
doc_antis <- doc_antis[doc_antis$doctor_antibiotics != -1,]
doc_antis <- doc_antis[doc_antis$doctor_antibiotics != "Nonexistent",]
#gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_doc_antis <- aggregate(doc_antis, by=list(set=doc_antis$set, doc_antis_score=doc_antis$doctor_antibiotics), FUN=length)
group_doc_antis<- group_doc_antis[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_doc_antis <- group_doc_antis %>% rename(prop = doctor_antibiotics) # rename(new_name = old_name)
group_doc_antis$prop[group_doc_antis$set == 'T0'] <- group_doc_antis$prop[group_doc_antis$set == 'T0']/d_a_0_F
group_doc_antis$prop[group_doc_antis$set == 'T2'] <- group_doc_antis$prop[group_doc_antis$set == 'T2']/d_a_2_F
group_doc_antis$prop[group_doc_antis$set == 'T4'] <- group_doc_antis$prop[group_doc_antis$set == 'T4']/d_a_4_F
group_doc_antis$prop[group_doc_antis$set == 'T6'] <- group_doc_antis$prop[group_doc_antis$set == 'T6']/d_a_6_F
group_doc_antis$prop[group_doc_antis$set == 'T8'] <- group_doc_antis$prop[group_doc_antis$set == 'T8']/d_a_8_F


add_anti <- data.frame(c("T2", "T6", "T8"), c("Didn't know","Didn't know", "Didn't know"), c(0.0, 0.0, 0.0))
names(add_anti) <- c("set", "doc_antis_score", "prop")
add_anti
group_doc_antis <- rbind(group_doc_antis, add_anti)

group_doc_antis
# plotting 
antibiotics <- ggplot(group_doc_antis, aes(x=doc_antis_score, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) + 
  ggtitle("Antiobiotics Given During Doctor Visits") + labs(y='% Respondents', x='Recieved Antibiotics')
antibiotics

#medication worms
combo <- mutate_at(combo, c("medication_worms_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$medication_worms_yn[combo$medication_worms_yn == -66] <- -2
combo$medication_worms_yn[combo$medication_worms_yn == -88] <- -3
combo$medication_worms_yn[combo$medication_worms_yn == -99] <- "Didn't Know"

unique(combo['medication_worms_yn'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
medi_wor <- combo %>% select(medication_worms_yn, set)
medi_wor <- medi_wor[medi_wor$medication_worms_yn != -1,]
medi_wor <- medi_wor[medi_wor$medication_worms_yn != -2,]
medi_wor <- medi_wor[medi_wor$medication_worms_yn != "Nonexistent",]
#gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_medi_wor <- aggregate(medi_wor, by=list(set=medi_wor$set, medi_wor_score=medi_wor$medication_worms_yn), FUN=length)
group_medi_wor<- group_medi_wor[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_medi_wor<- group_medi_wor %>% rename(prop = medication_worms_yn) # rename(new_name = old_name)
group_medi_wor$prop[group_medi_wor$set == 'T0'] <- group_medi_wor$prop[group_medi_wor$set == 'T0']/t0fj_size
group_medi_wor$prop[group_medi_wor$set == 'T2'] <- group_medi_wor$prop[group_medi_wor$set == 'T2']/t2fj_size
group_medi_wor$prop[group_medi_wor$set == 'T4'] <- group_medi_wor$prop[group_medi_wor$set == 'T4']/t4fj_size
group_medi_wor$prop[group_medi_wor$set == 'T6'] <- group_medi_wor$prop[group_medi_wor$set == 'T6']/t6fj_size
group_medi_wor$prop[group_medi_wor$set == 'T8'] <- group_medi_wor$prop[group_medi_wor$set == 'T8']/t8fj_size

group_medi_wor
# plotting 
worms_medi <- ggplot(group_medi_wor, aes(x=medi_wor_score, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) + 
  ggtitle("Given Deworming Treatment") + labs(y='% Respondents', x='Recieved Treatment')
worms_medi


#breastfed
combo <- mutate_at(combo, c("breastfed_3m_yn"), ~replace(., is.na(.), -1)) # Replace NA values with 0
combo$breastfed_3m_yn[combo$breastfed_3m_yn == -66] <- -2
combo$breastfed_3m_yn[combo$breastfed_3m_yn == -88] <- -3
combo$breastfed_3m_yn[combo$breastfed_3m_yn == -99] <- "Didn't Know"

unique(combo['doctor_yn'])

# selecting the columns necessary for analysis (generally the var or interest + set column) 
bf_3m <- combo %>% select(breastfed_3m_yn, set)
bf_3m <- bf_3m[bf_3m$breastfed_3m_yn != -1,]
bf_3m <- bf_3m[bf_3m$breastfed_3m_yn != "Nonexistent",]
bf_3m <- bf_3m[bf_3m$breastfed_3m_yn != "Didn't Know",]
#gen_health <- gen_health[gen_health$gen_health_overall != -2,]

# reorganize data, group by set # and the variable
# gen_health: dataframe of variable of interest (general health) and set
# gen_health_score: intermediate var name for aggregating 
# gen_health_overall: the actual column we are getting information/values from 
group_bf_3m <- aggregate(bf_3m, by=list(set=bf_3m$set, bf_3m_score=bf_3m$breastfed_3m_yn), FUN=length)
group_bf_3m <- group_bf_3m[-c(4)] # dropping the extra set column (R IS ONE INDEXED, SAD)
group_bf_3m <- group_bf_3m %>% rename(prop = breastfed_3m_yn) # rename(new_name = old_name)
group_bf_3m$prop[group_bf_3m$set == 'T0'] <- group_bf_3m$prop[group_bf_3m$set == 'T0']/t0fj_size
group_bf_3m$prop[group_bf_3m$set == 'T2'] <- group_bf_3m$prop[group_bf_3m$set == 'T2']/t2fj_size
group_bf_3m$prop[group_bf_3m$set == 'T4'] <- group_bf_3m$prop[group_bf_3m$set == 'T4']/t4fj_size
group_bf_3m$prop[group_bf_3m$set == 'T6'] <- group_bf_3m$prop[group_bf_3m$set == 'T6']/t6fj_size
group_bf_3m$prop[group_bf_3m$set == 'T8'] <- group_bf_3m$prop[group_bf_3m$set == 'T8']/t8fj_size

group_bf_3m
# plotting 
breastfed <- ggplot(group_bf_3m, aes(x=bf_3m_score, fill=set, y=prop*100)) + 
  geom_bar(position='dodge', stat='identity') + ylim(0.0, 30) +
  ggtitle("Consumption of Breast Milk") + labs(y='% of Respondents', x='Drank Breast Milk (within 3 months)')
breastfed
