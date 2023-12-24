library(dplyr)
library(readr)
library(ggplot2)



heart <- read.csv('heart_failure_clinical_records_dataset_new.csv')
#will test serum creatinine first


# Getting rid of outliers
serum_creatinine_iqr <- IQR(heart$serum_creatinine)
serum_creatinine <- heart %>% 
  filter(serum_creatinine > quantile(serum_creatinine, 0.25) - 1.5 * serum_creatinine_iqr,
         serum_creatinine < quantile(serum_creatinine, 0.72) + 1.5 * serum_creatinine_iqr) %>%
  select(serum_creatinine, DEATH_EVENT)

serum_creatinine_death <- filter(serum_creatinine, DEATH_EVENT == 'deceased')
serum_creatinine_alive <- filter(serum_creatinine, DEATH_EVENT == 'alive')

# Lets view data set distribution in a histogram
ggplot(serum_creatinine_alive, aes(x=serum_creatinine, y=after_stat(density))) +
  geom_histogram(color='#000000', fill='purple', bins = 15) +
  geom_density()
  labs(title="Serum Creatinine levels For Individuals who are alive",
       x="Serum Creatinine")

ggplot(serum_creatinine_death, aes(x=serum_creatinine, y=after_stat(density))) +
  geom_histogram(color='#000000', fill='purple', bins = 15) +
  geom_density() +
  labs(title="Serum Creationine levels For Individuals who are deceased",
       x="Serum Creatinine")

mean(serum_creatinine_alive$serum_creatinine)
mean(serum_creatinine_death$serum_creatinine)

var(serum_creatinine_alive$serum_creatinine)
var(serum_creatinine_death$serum_creatinine)


# test for difference in mean 
#Null hypothesis: There is no significant difference between the mean 
#of serum creatinine in individuals who are alive and those who are deceased.

#Alternative hypothesis: The mean of serum creatinine is higher
#in deceased individuals compared to those who are alive.
t.test(serum_creatinine_death$serum_creatinine,
       serum_creatinine_alive$serum_creatinine,
       alternative = 'greater')

#According to the results, we have p value of 2.048e-07, which is significant at
#5% significant level; thus, we reject the null hypothesis in support of the
#alternative hypothesis


# test of variance using F distribution
var.test(serum_creatinine_death$serum_creatinine,
         serum_creatinine_alive$serum_creatinine,
         alternative = 'two.sided')


#proportions
#anaemia and high blood pressure
anaemia_table <- heart %>%
  group_by(DEATH_EVENT, anaemia) %>%
  summarize(Freq=n()) %>%
  mutate(per = Freq/sum(Freq))
anaemia_table

blood_pressure <- heart %>%
  group_by(DEATH_EVENT, high_blood_pressure) %>%
  summarize(Freq=n()) %>%
  mutate(per = Freq/sum(Freq))

ggplot(anaemia_table, aes(x=DEATH_EVENT, y=per, fill=anaemia)) +
  geom_col(position = 'dodge')

ggplot(blood_pressure, aes(x=DEATH_EVENT, y=per, fill=high_blood_pressure)) +
  geom_col(position = 'dodge')

  



 

