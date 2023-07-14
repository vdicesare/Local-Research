library(tidyverse)
library(dplyr)
library(DescTools)
library(reshape2)
library(ggplot2)
#library(treemap)
#library(networkD3)

# read Dimensions file
df <- read.csv2("~/Desktop/Local.Research/dataset_journals.csv", col.names = c("pubs.refs.cits", "source.type", "journal.id", "journal.name", "issn", "eissn", "publisher", "country", "count"))

# subset journals source type
df.journals <- subset(df, df$source.type == "Journal", select = c("pubs.refs.cits", "journal.id", "journal.name", "issn", "eissn", "publisher", "country", "count"))

# convert pubs.refs.cits variable to factor
df.journals$pubs.refs.cits <- as.factor(df.journals$pubs.refs.cits)

# aggregate all variables by journal
df.journals.aggr <- df.journals %>% group_by(journal.id, journal.name, country, pubs.refs.cits) %>% 
                      summarise(count = sum(count), .groups = 'drop') %>%
                      as.data.frame()

##
# reshape to wide format in order to work individually with the variables pubs, refs and cits
df.journals.wide <- dcast(df.journals.aggr, journal.id + journal.name + country ~ pubs.refs.cits)

# sum total cits, pubs and refs per journal
df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(cits.n = sum(cits, na.rm = TRUE))

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(pubs.n = sum(pubs, na.rm = TRUE))

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(refs.n = sum(refs, na.rm = TRUE))

# compute proportions per journals' pubs, refs and cits
df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(cits.prop = cits / cits.n)
df.journals.wide$cits.prop <- sprintf("%.7f", df.journals.wide$cits.prop)
df.journals.wide$cits.prop <- as.numeric(df.journals.wide$cits.prop)

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(pubs.prop = pubs / pubs.n)
df.journals.wide$pubs.prop <- sprintf("%.7f", df.journals.wide$pubs.prop)
df.journals.wide$pubs.prop <- as.numeric(df.journals.wide$pubs.prop)

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(refs.prop = refs / refs.n)
df.journals.wide$refs.prop <- sprintf("%.7f", df.journals.wide$refs.prop)
df.journals.wide$refs.prop <- as.numeric(df.journals.wide$refs.prop)

# compute Gini coefficient per journals' pubs, refs and cits
gini.cits <- aggregate(cits ~ journal.id, df.journals.wide, FUN = Gini)
names(gini.cits)[2] <- "cits.gini"
gini.cits$cits.gini[is.nan(gini.cits$cits.gini)] <- 1
df.journals.wide <- merge(df.journals.wide, gini.cits, by = "journal.id", all.x = TRUE)

gini.pubs <- aggregate(pubs ~ journal.id, df.journals.wide, FUN = Gini)
names(gini.pubs)[2] <- "pubs.gini"
gini.pubs$pubs.gini[is.nan(gini.pubs$pubs.gini)] <- 1
df.journals.wide <- merge(df.journals.wide, gini.pubs, by = "journal.id", all.x = TRUE)

gini.refs <- aggregate(refs ~ journal.id, df.journals.wide, FUN = Gini)
names(gini.refs)[2] <- "refs.gini"
gini.refs$refs.gini[is.nan(gini.refs$refs.gini)] <- 1
df.journals.wide <- merge(df.journals.wide, gini.refs, by = "journal.id", all.x = TRUE)

# count total countries per journal's cits, pubs and refs
df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(cits.country.n = n_distinct(country[!is.na(cits)])) %>%
                    ungroup()

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(pubs.country.n = n_distinct(country[!is.na(pubs)])) %>%
                    ungroup()

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(refs.country.n = n_distinct(country[!is.na(refs)])) %>%
                    ungroup()

##
# filter cits per journal to obtain the countries with max cits values
df.journals.max.cits <- df.journals.wide %>%
                          group_by(journal.id) %>%
                          filter(cits == max(cits, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.cits <- df.journals.max.cits %>% rename("cits.max.country" = "country",
                                                        "cits.max" = "cits")
df.journals.max.cits <- select(df.journals.max.cits, -c(pubs, refs, pubs.n, refs.n, pubs.prop, refs.prop, pubs.gini, refs.gini, pubs.country.n, refs.country.n))

# filter pubs per journal to obtain the countries with max pubs values
df.journals.max.pubs <- df.journals.wide %>%
                          group_by(journal.id) %>%
                          filter(pubs == max(pubs, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.pubs <- df.journals.max.pubs %>% rename("pubs.max.country" = "country",
                                                        "pubs.max" = "pubs")
df.journals.max.pubs <- select(df.journals.max.pubs, -c(cits, refs, cits.n, refs.n, cits.prop, refs.prop, cits.gini, refs.gini, cits.country.n, refs.country.n))

# filter refs per journal to obtain the countries with max refs values
df.journals.max.refs <- df.journals.wide %>%
                          group_by(journal.id) %>%
                          filter(refs == max(refs, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.refs <- df.journals.max.refs %>% rename("refs.max.country" = "country",
                                                        "refs.max" = "refs")
df.journals.max.refs <- select(df.journals.max.refs, -c(pubs, cits, pubs.n, cits.n, pubs.prop, cits.prop, pubs.gini, cits.gini, pubs.country.n, cits.country.n))

# merge all dataframes with cits, pubs and refs max values
df.journals.max <- merge(merge(df.journals.max.cits, df.journals.max.pubs, by = c("journal.id", "journal.name"), all = TRUE), df.journals.max.refs, by = c("journal.id", "journal.name"), all = TRUE)
write.csv2(df.journals.max, file = "~/Desktop/Local.Research/dataset_journals_max.csv")

##
# Figure 1. Plot histogram of Gini Coefficient per individual journals per cits, pubs and refs
df.figure1 <- subset(df.journals.wide, select = c("journal.id", "cits.gini", "pubs.gini", "refs.gini"))
df.figure1 <- df.figure1 %>% distinct()
colnames(df.figure1) <- c("journal.id", "cits","pubs", "refs")
df.figure1 <- pivot_longer(df.figure1, cits:pubs:refs, names_to = "cits.pubs.refs", values_to = "gini.coefficient")

ggplot(df.figure1) +
  geom_histogram(aes(x = gini.coefficient, color = cits.pubs.refs, fill = cits.pubs.refs), position = 'identity', alpha=0.6) +
  theme_minimal() +
  facet_wrap(~cits.pubs.refs) +
  xlab("Gini Coefficient") +
  ylab("Journal count")
ggsave("~/Desktop/Local.Research/Figure1.jpg")

# Figure 2. Plot scatterplot of Gini Coefficient per number of countries in each journal per cits, pubs and refs
df.figure2 <- subset(df.journals.wide, select = c("journal.id", "cits.country.n", "pubs.country.n", "refs.country.n"))
colnames(df.figure2) <- c("journal.id", "cits","pubs", "refs")
df.figure2 <- pivot_longer(df.figure2, cits:pubs:refs, names_to = "cits.pubs.refs", values_to = "country.n")
df.figure2 <- merge(x = df.figure2, y = df.figure1, by = c("journal.id","cits.pubs.refs"), all = TRUE)
df.figure2 <- df.figure2 %>% distinct()

ggplot(df.figure2) + 
  geom_point(aes(x = gini.coefficient, y = country.n, color = cits.pubs.refs, fill = cits.pubs.refs), alpha=0.6) +
  theme_minimal() +
  xlab("Gini Coefficient") +
  ylab("Country count per journal")
ggsave("~/Desktop/Local.Research/Figure2.jpg")

# Figure 2A. Plot scatterplot of Gini Coefficient per number of countries in each journal per cits
df.figure2A <- filter(df.figure2, cits.pubs.refs == "cits")
ggplot(df.figure2A) + 
  geom_point(aes(x = gini.coefficient, y = country.n), color = "red", alpha=0.6) +
  theme_minimal() +
  xlab("Gini Coefficient") +
  ylab("Country count per journal")
ggsave("~/Desktop/Local.Research/Figure2A.jpg")

# Figure 2B. Plot scatterplot of Gini Coefficient per number of countries in each journal per pubs
df.figure2B <- filter(df.figure2, cits.pubs.refs == "pubs")
ggplot(df.figure2B) + 
  geom_point(aes(x = gini.coefficient, y = country.n), color = "lightgreen", alpha=0.6) +
  theme_minimal() +
  xlab("Gini Coefficient") +
  ylab("Country count per journal")
ggsave("~/Desktop/Local.Research/Figure2B.jpg")

# Figure 2C. Plot scatterplot of Gini Coefficient per number of countries in each journal per refs
df.figure2C <- filter(df.figure2, cits.pubs.refs == "refs")
ggplot(df.figure2C) + 
  geom_point(aes(x = gini.coefficient, y = country.n), color = "lightblue", alpha=0.6) +
  theme_minimal() +
  xlab("Gini Coefficient") +
  ylab("Country count per journal")
ggsave("~/Desktop/Local.Research/Figure2C.jpg")

# Table 1A. Filter journals with only one country per cits
df.table1A <- filter(df.figure2, cits.pubs.refs == "cits" & country.n == 1)
df.table1A <- merge(x = df.table1A, y = df.journals.wide[,c("journal.id", "journal.name")], by = "journal.id")
df.table1A <- df.table1A %>% distinct()
write.csv2(df.table1A, file = "~/Desktop/Local.Research/Table1A.csv")

# Table 1B. Filter journals with only one country per pubs
df.table1B <- filter(df.figure2, cits.pubs.refs == "pubs" & country.n == 1)
df.table1B <- merge(x = df.table1B, y = df.journals.wide[,c("journal.id", "journal.name")], by = "journal.id")
df.table1B <- df.table1B %>% distinct()
write.csv2(df.table1B, file = "~/Desktop/Local.Research/Table1B.csv")

# Table 1C. Filter journals with only one country per refs
df.table1C <- filter(df.figure2, cits.pubs.refs == "refs" & country.n == 1)
df.table1C <- merge(x = df.table1C, y = df.journals.wide[,c("journal.id", "journal.name")], by = "journal.id")
df.table1C <- df.table1C %>% distinct()
write.csv2(df.table1C, file = "~/Desktop/Local.Research/Table1C.csv")

# Table 1. Filter journals with only one country per cits, pubs and refs in the same case
df.table1 <- merge(merge(df.table1A[,c("journal.id", "journal.name", "country.n")], df.table1B[,c("journal.id", "journal.name", "country.n")], by = c("journal.id", "journal.name")), df.table1C[,c("journal.id", "journal.name", "country.n")], by = c("journal.id", "journal.name"))
colnames(df.table1) <- c("journal.id", "journal.name", "cits.country.n","pubs.country.n", "refs.country.n")
write.csv2(df.table1, file = "~/Desktop/Local.Research/Table1.csv")

# Table 2. Find same max country between cits and pubs per journal
df.table2 <- merge(x = df.journals.max.cits[,c("journal.id", "journal.name", "cits.max.country")], y = df.journals.max.pubs[,c("journal.id", "journal.name", "pubs.max.country")], by = c("journal.id", "journal.name"))
df.table2 <- filter(df.table2, cits.max.country == pubs.max.country)
write.csv2(df.table2, file = "~/Desktop/Local.Research/Table2.csv")

# Table 3. Find same max country between refs and pubs per journal
df.table3 <- merge(x = df.journals.max.refs[,c("journal.id", "journal.name", "refs.max.country")], y = df.journals.max.pubs[,c("journal.id", "journal.name", "pubs.max.country")], by = c("journal.id", "journal.name"))
df.table3 <- filter(df.table3, refs.max.country == pubs.max.country)
write.csv2(df.table3, file = "~/Desktop/Local.Research/Table3.csv")

# Table 4. Find same max country between cits, pubs and refs per journal
df.table4 <- merge(merge(df.journals.max.cits[,c("journal.id", "journal.name", "cits.max.country")], df.journals.max.pubs[,c("journal.id", "journal.name", "pubs.max.country")], by = c("journal.id", "journal.name")), df.journals.max.refs[,c("journal.id", "journal.name", "refs.max.country")], by = c("journal.id", "journal.name"))
df.table4 <- filter(df.table4, (cits.max.country == pubs.max.country) & (pubs.max.country == refs.max.country))
write.csv2(df.table4, file = "~/Desktop/Local.Research/Table4.csv")