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
df.journals.wide <- merge(df.journals.wide, gini.cits, by = "journal.id", all.x = TRUE)
df.journals.wide$cits.gini[is.nan(df.journals.wide$cits.gini)] <- 0

gini.pubs <- aggregate(pubs ~ journal.id, df.journals.wide, FUN = Gini)
names(gini.pubs)[2] <- "pubs.gini"
df.journals.wide <- merge(df.journals.wide, gini.pubs, by = "journal.id", all.x = TRUE)
df.journals.wide$pubs.gini[is.nan(df.journals.wide$pubs.gini)] <- 0

gini.refs <- aggregate(refs ~ journal.id, df.journals.wide, FUN = Gini)
names(gini.refs)[2] <- "refs.gini"
df.journals.wide <- merge(df.journals.wide, gini.refs, by = "journal.id", all.x = TRUE)
df.journals.wide$refs.gini[is.nan(df.journals.wide$refs.gini)] <- 0

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
# Figure 1. Plot histogram with gini distribution per cits, pubs and refs
df.figure1 <- subset(df.journals.max, select = c("journal.id", "cits.gini", "pubs.gini", "refs.gini"))
colnames(df.figure1) <- c("journal.id", "cits","pubs", "refs")
df.figure1 <- pivot_longer(df.figure1, cits:pubs:refs, names_to = "cits.pubs.refs", values_to = "gini.coefficient")

ggplot(df.figure1) +
  geom_histogram(aes(x = gini.coefficient, color = cits.pubs.refs, fill = cits.pubs.refs), position = 'identity', alpha=0.6) +
  theme_minimal() +
  facet_wrap(~cits.pubs.refs) +
  xlab("Gini Coefficient") +
  ylab("Count")
ggsave("~/Desktop/Local.Research/Figure1.jpg")

# Figure 2. Plot scatterplot of ginis per max countries proportion
df.figure2 <- subset(df.journals.max, select = c("journal.id", "cits.prop", "pubs.prop", "refs.prop"))
colnames(df.figure2) <- c("journal.id", "cits","pubs", "refs")
df.figure2 <- pivot_longer(df.figure2, cits:pubs:refs, names_to = "cits.pubs.refs", values_to = "proportion")
df.figure2 <- merge(x = df.figure2, y = df.figure1, by = c("journal.id","cits.pubs.refs"), all = TRUE)

ggplot(df.figure2) + 
  geom_point(aes(x = gini.coefficient, y = proportion, color = cits.pubs.refs, fill = cits.pubs.refs), alpha=0.6) +
  theme_minimal() +
  xlab("Gini Coefficient") +
  ylab("Proportion of countries' max cits/pubs/refs")
ggsave("~/Desktop/Local.Research/Figure2.jpg")
