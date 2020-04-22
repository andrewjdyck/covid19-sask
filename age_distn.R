
library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
ages <- data.frame(cases=t(read_csv('./data/covid_sk_age_dist_20200420.csv')))

age_cats <- c(
  "Age 19 and Under",
  "Age 20 to 39",
  "Age 40 to 59",
  "Age 60 to 79",
  "Age 80+")

# data comes from 2011 census: https://www12.statcan.gc.ca/census-recensement/2011/as-sa/fogs-spg/Facts-pr-eng.cfm?Lang=Eng&GK=PR&GC=47
ad <- read_tsv('./statcan_age_dist.tsv')
st <- data.frame(popn=t(data.frame(
  age_0_19 = sum(ad[2:5,"Total"]),
  age_20_39 = sum(ad[6:9,"Total"]),
  age_40_59 = sum(ad[10:13,"Total"]),
  age_60_79 = sum(ad[14:17,"Total"]),
  age_gte_80 = sum(ad[18:22,"Total"])
)))
row.names(st) <- age_cats


df <- cbind(cases=ages[2:6,], st)

df$pop_pct <- 100*df$popn/sum(df$popn)
df$case_pct <- 100*df$cases/sum(df$cases)
df$id <- row.names(df)



gd_sk <- df %>%
  select(pop_pct, case_pct, id) %>%
  melt(id.vars="id")
gd_sk$variable <- ifelse(gd_sk$variable == 'pop_pct', 'Population', 'COVID-19 Cases')

ggplot(data=gd_sk, aes(x=id, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title='Saskatechwan COVID-19 cases compared to population', x='Age category', y='Share (%)')



### Doing quebec now

ages <- data.frame(cases=read_csv('./qc_confirmed_pct_by_age_20200419.csv'))

ages <- rbind(
  sum(ages[1:2,2]),
  sum(ages[3:4,2]),
  sum(ages[5:6,2]),
  sum(ages[7:8,2]),
  sum(ages[9:10,2])
)

age_cats <- c(
  "Age 19 and Under",
  "Age 20 to 39",
  "Age 40 to 59",
  "Age 60 to 79",
  "Age 80+")

# this is from 2011 census
# https://www12.statcan.gc.ca/census-recensement/2011/as-sa/fogs-spg/Facts-pr-eng.cfm?Lang=Eng&GK=PR&GC=24
ad <- read_tsv('./qc_statcan_age_dist_2011.tsv')
st <- data.frame(popn=t(data.frame(
  age_0_19 = sum(ad[2:5,"Total"]),
  age_20_39 = sum(ad[6:9,"Total"]),
  age_40_59 = sum(ad[10:13,"Total"]),
  age_60_79 = sum(ad[14:17,"Total"]),
  age_gte_80 = sum(ad[18:22,"Total"])
)))
row.names(st) <- age_cats

# this is from 2016 census
# ad <- read_tsv('./qc_statcan_age_dist.tsv')
# st <- data.frame(popn=t(data.frame(
#   age_0_19 = sum(ad[1:4,"Total"]),
#   age_20_39 = sum(ad[4:8,"Total"]),
#   age_40_59 = sum(ad[9:12,"Total"]),
#   age_60_79 = sum(ad[13:16,"Total"]),
#   age_gte_80 = sum(ad[17:18,"Total"])
# )))
# row.names(st) <- age_cats


df <- cbind(cases=ages, st)

df$pop_pct <- 100*df$popn/sum(df$popn)
df$id <- row.names(df)



gd_qc <- df %>%
  select(pop_pct, cases, id) %>%
  melt(id.vars="id")
gd_qc$variable <- ifelse(gd_qc$variable == 'pop_pct', 'Population', 'COVID-19 Cases')

ggplot(data=gd_qc, aes(x=id, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title='Quebec COVID-19 cases compared to population', x='Age category', y='Share (%)')



# comparing SK & QC

gd_sk$province <- 'SK'
gd_qc$province <- 'QC'

gd_all <- rbind(gd_sk, gd_qc) %>%
  filter(variable=='COVID-19 Cases')

ggplot(data=gd_all, aes(x=id, y=value, fill=province)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title='COVID-19 cases by demographics in SK & QC', x='Age category', y='Share (%)')

