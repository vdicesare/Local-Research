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

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(pubs.prop = pubs / pubs.n)
df.journals.wide$pubs.prop <- sprintf("%.7f", df.journals.wide$pubs.prop)

df.journals.wide <- df.journals.wide %>%
                      group_by(journal.id) %>%
                      mutate(refs.prop = refs / refs.n)
df.journals.wide$refs.prop <- sprintf("%.7f", df.journals.wide$refs.prop)

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
                      mutate(cits.country.n = count(country, na.rm = TRUE))


# subset journals to keep only the countries with max cits, pubs and refs
df.journals.max <- df.journals.wide %>%
                    group_by(journal.id) %>%
                    filter(cits == max(cits, na.rm = TRUE) |
                      pubs == max(pubs, na.rm = TRUE) |
                      refs == max(refs, na.rm = TRUE)) %>%
                    ungroup()

# filter cits per journal to obtain the countries with max values
df.journals.max.cits <- df.journals.max %>%
                          group_by(journal.id) %>%
                          filter(cits == max(cits, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.cits <- df.journals.max.cits %>% rename("cits.max.country" = "country",
                                                        "cits.max" = "cits")
df.journals.max.cits <- select(df.journals.max.cits, -c(pubs, refs, pubs.gini, refs.gini, pubs.prop, refs.prop))

# filter pubs per journal to obtain the countries with max values
df.journals.max.pubs <- df.journals.wide %>%
                          group_by(journal.id) %>%
                          filter(pubs == max(pubs, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.pubs <- df.journals.max.pubs %>% rename("pubs.max.country" = "country",
                                                        "pubs.max" = "pubs")
df.journals.max.pubs <- select(df.journals.max.pubs, -c(cits, refs, cits.gini, refs.gini, cits.prop, refs.prop))

# filter refs per journal to obtain the countries with max values
df.journals.max.refs <- df.journals.wide %>%
                          group_by(journal.id) %>%
                          filter(refs == max(refs, na.rm = TRUE)) %>%
                        ungroup()
df.journals.max.refs <- df.journals.max.refs %>% rename("refs.max.country" = "country",
                                                        "refs.max" = "refs")
df.journals.max.refs <- select(df.journals.max.refs, -c(cits, pubs, cits.gini, pubs.gini, cits.prop, pubs.prop))




# data to long format?
# plot histogram
ggplot(df.journals.wide) +
  geom_histogram(aes(x=cits, fill=type), color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

# plot scatterplot 




# Figure 1
# aggregate countries per pubs, refs and cits
df.aggr.countries <- df.journals %>% group_by(country, pubs.refs.cits) %>% 
  summarise(count = sum(count), .groups = 'drop') %>%
  as.data.frame()


# Figure 2
# aggregate journals per pubs, refs and cits
df.aggr.journals <- df.journals %>% group_by(journal.id, journal.name, pubs.refs.cits) %>% 
  summarise(count = sum(count), .groups = 'drop') %>%
  as.data.frame()

# reshape to wide format in order to filter the journals by publication count
df.resh.journals <- dcast(df.aggr.journals, journal.id + journal.name ~ pubs.refs.cits)

# filter journals with > 15000 publications
df.filt.journals <- filter(df.resh.journals, pubs > 15000)

# reshape to long format in order to plot Figure 2
df.filt.journals <- melt(df.filt.journals, id.vars = c("journal.id", "journal.name"),
                         variable.name = "pubs.refs.cits",
                         value.name = "count")


# Figure 3
# aggregate journals per pubs, refs, cits and countries
df.aggr.journals.countries <- df.journals %>% group_by(journal.id, journal.name, pubs.refs.cits, country) %>% 
  summarise(count = sum(count), .groups = 'drop') %>%
  as.data.frame()

# keep publications only
df.journals.countries.pubs <- filter(df.aggr.journals.countries, pubs.refs.cits == "pubs")
df.journals.countries.pubs3000 <- filter(df.journals.countries.pubs, count > 3000)

links <- data.frame(
  source = df.journals.countries.pubs3000$journal.id,
  target = df.journals.countries.pubs3000$country,
  value = df.journals.countries.pubs3000$count)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique())

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


# PLOTS
# Figure 1. Proportion of publications, references and citations per country
treemap(df.aggr.countries, index = c("country", "pubs.refs.cits"), vSize = "count", type = "index", palette = "Set3",
        fontsize.labels=c(15,12),
        fontcolor.labels=c("black","darkgrey"),
        fontface.labels=c(1,1),
        bg.labels=c("transparent"),
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")),                                
        overlap.labels=0.5,
        inflate.labels=F,
        border.col=c("black","darkgrey"),
        border.lwds=c(1,0.5)
        )

# Figure 2. Distribution of publications, references and citations per top journals (> 15000 pubs)
ggplot(df.filt.journals, aes(x = journal.id, y = count, color = pubs.refs.cits)) +
  geom_point() +
  #facet_wrap(~pubs.refs.cits) +
  theme_minimal() +
  xlab("Journals") +
  guides(x =  guide_axis(angle = 90)) +
  ylab("Count")
ggsave("Figure2.jpg")

# Figure 3. Publication flows between journals and authors’ countries of origin
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)
p

