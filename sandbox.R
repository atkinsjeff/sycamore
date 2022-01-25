# bring in test data

require(tidyverse)
x <- read.csv("./data-raw/vst_mappingTagging2016_goughSites.csv")
y <- read.csv( "./data-raw/vst_apparentIndividuals2016_goughSites.csv")



x %>%
    select(siteid, plotType, plotid, individualid, taxonid, stemazimuth, stemdistance) %>%
    filter(siteid == "OSBS" & plotType == "tower" & taxonid != "2PLANT") %>%
    data.frame() -> mapping

y %>%
    select(siteid, plotType, plotid, individualid, growthform, stemdiameter, plantstatus) %>%
    filter(siteid == "OSBS" & plotType == "tower" & stemdiameter < 100) %>%
    data.frame() -> stems

# merge together
df <- merge(stems, mapping)

# make basal area
df$ba <- (df$stemdiameter/2) * pi

# Density (D) and basal area (BA)
df %>%
    filter(plantstatus == 1) %>%
    select(taxonid, ba) %>%
    group_by(taxonid) %>%
    summarize(ba_total = sum(ba, na.rm = TRUE),
              n = n())%>%
    mutate(D = n/sum(n),
           BA = ba_total / sum(ba_total)) %>%
    select(taxonid, D, BA) -> D_BA


df %>%
    group_by(taxonid) %>%
    summarise(n = n()) %>%
    mutate(Freq = n/sum(n))

#####
# ROUNDING FUNCTION
round.choose <- function(x, roundTo, dir = 1) {
    if(dir == 1) {  ##ROUND UP
        x + (roundTo - x %% roundTo)
    } else {
        if(dir == 0) {  ##ROUND DOWN
            x - (x %% roundTo)
        }
    }
}

# make size classes
df$sizeclass <- round.choose(df$stemdiameter, 10, 0)

# number of size classes
no.sc <- length(unique(df$sizeclass))

x11()
hist(df$sizeclass, breaks = 5)

df %>%
    group_by(taxonid) %>%
    summarize(SC = (length(unique(sizeclass)) / no.sc)) -> SC

# frequency
no.plots <- length(unique(df$plotid))

df %>%
    group_by(taxonid) %>%
    summarize(F = (length(unique(plotid)) / no.plots)) -> F


# bring together
bob <- merge(D_BA, F)
bob <- merge(bob, SC)

# make the correct order
all <- bob[, c("taxonid", "D", "F", "SC", "BA")]
names(all)[1] <- "Species"
# making basal area

