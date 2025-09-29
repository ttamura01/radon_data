setwd("/Users/takayukitamura/Documents/R_Computing/radon_data")

library(tidyverse)
library(dplyr)
library(ggtext)
library(glue)

# to create rad_pa and rad_pgh from radon open-data: https://data.pa.gov/Energy-and-the-Environment/Radon-Test-Results-September-1986-Current-Annual-C/vkjb-sx3k/about_data

# rad_pgh <- read_csv("/Users/takayukitamura/Desktop/Radon_Test_Results.csv") %>% 
#   select(`Test End Date`, `County Name`, `Measure Value` ) %>%
#   filter(`County Name` == "Allegheny" | `County Name` == "Lawrence" |`County Name` ==  "Butler" |
#            `County Name` == "Beaver" | `County Name` ==  "Westmoreland" |`County Name` ==  "Washington" |
#            `County Name` == "Armstrong"
#   ) %>%
#   na.omit(`Measure Value`)

rad_pgh <- read_csv("radon_test_pa.csv") %>% 
  filter(`County Name` == "Allegheny" | `County Name` == "Lawrence" |`County Name` ==  "Butler" |
           `County Name` == "Beaver" | `County Name` ==  "Westmoreland" |`County Name` ==  "Washington" |
           `County Name` == "Armstrong"
  ) %>%
  na.omit(`Measure Value`)

str(rad_pgh)

write_csv(rad_pgh, "rad_pgh.csv")

rad_pgh <- read.csv("data/rad_pgh.csv") 


# to create stats 

pgh_stats <-rad_pgh %>% 
  summarise(minimum = min(`Measure Value`, round(2)),
            median = median(`Measure Value`, round(2)),
            mean = mean(`Measure Value`, round(2)),
            maximum = max(`Measure Value`, round(2)),
            sd = sd(`Measure Value`, round(2)),
            n = length(`Measure Value`))

pgh_stats

## understand the distribution better with percentage of each segment
# PGH

rad_pgh_2.0 <- rad_pgh %>% 
  filter(`Measure Value` < 2.0)
length(rad_pgh_2.0$`Measure Value`)
(length(rad_pgh_2.0$`Measure Value`)/550399)*100

rad_pgh_4.0 <- rad_pgh %>% 
  filter(`Measure Value` >= 2.0 & `Measure Value` < 4.0)
length(rad_pgh_4.0$`Measure Value`)
(length(rad_pgh_4.0$`Measure Value`)/550399)*100

rad_pgh_10.0 <- rad_pgh %>% 
  filter(`Measure Value` >= 4.0 & `Measure Value` < 10.0)
length(rad_pgh_10.0$`Measure Value`)
(length(rad_pgh_10.0$`Measure Value`)/550399)*100

rad_pgh_20.0 <- rad_pgh %>% 
  filter(`Measure Value` >= 10.0 & `Measure Value` < 20.0)
length(rad_pgh_20.0$`Measure Value`)
(length(rad_pgh_20.0$`Measure Value`)/550399)*100

rad_pgh_50.0 <- rad_pgh %>% 
  filter(`Measure Value` >= 20 & `Measure Value` < 50.0)
length(rad_pgh_50.0$`Measure Value`)
(length(rad_pgh_50.0$`Measure Value`)/550399)*100

rad_pgh_100.0 <- rad_pgh %>% 
  filter(`Measure Value` >= 100.0)
length(rad_pgh_100.0$`Measure Value`)
(length(rad_pgh_100.0$`Measure Value`)/550399)*100

rad_pgh %>% arrange(-`Measure Value`)


## histogram 

# PGH

ggplot(rad_pgh, aes(x = `Measure Value`)) +
  geom_histogram(fill = 'red') +
  scale_x_log10()+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh') +
  theme_bw()

## adding limits = c(0.5, 150)

# PGH

ggplot(rad_pgh, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(`Measure Value`, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh') +
  theme_bw()

## adding annotate 
# PGH

ggplot(rad_pgh, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.8) +
  geom_vline(aes(xintercept=mean(`Measure Value`, na.rm=TRUE)), linetype="dashed", size=1)+
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', x = 'Counts', 
       title = 'Measured Radon in Great Pittsburgh',
       caption = "source: open data PA") +
  theme_bw() +
  annotate("text", x = c(20), y = c(90000), size = 5, label=c("n = 550399")) +
  annotate("text", x = c(30), y = c(80000), size = 5, label=c("median = 2.8pCi/L")) +
  annotate("text", x = c(30), y = c(70000), size = 5, label=c("mean = 5.5pCi/L ")) +
  annotate("text", x = c(17), y = c(60000), size = 5, label=c("sd = 18.5"))

## adding stats and annotate
# PGH

ggplot(rad_pgh, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pgh_stats, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh',
       caption = "source: Open Data PA") +
  annotate("text", x=c(5), y=c(92000), label=c("median=2.8pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(9.0), y=c(86000), label=c("mean=5.5pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(17.0), y=c(75000), label=c("mean+sd=23.9pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(25.0), y=c(70000), label=c("mean+2sd=39.3pCi/l"), size = 5, face = "bold")+
  annotate("text", x=c(20), y=c(65000), label=c("(n=550,399)"), size = 5, face = "bold")+
  annotate("text", x=c(11), y=c(80000),label=c("sd=18.4pCi/l"), size = 5, face = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown()) 

ggsave("radon_pgh.png", height = 5, width = 5)

##Excluding outliers from the universe

a <- rad_pgh %>% 
  filter(`Measure Value` <= 1000)

count(a)
count(rad_pgh)

pgh_stats_2 <- a %>% 
  summarise(minimum = min(`Measure Value`, round(2)),
            median = median(`Measure Value`, round(2)),
            mean = mean(`Measure Value`),
            maximum = max(`Measure Value`, round(2)),
            sd = sd(`Measure Value`, round(2)),
            n = length(`Measure Value`))

pgh_stats_2$n
N <- format(pgh_stats_2$n, big.mark = ",")


round(pgh_stats_2[5],1)
(round(pgh_stats_2[5],1)*2 + round(pgh_stats_2[3],1))

# N <- format(550395, big.mark=",")
# N <- format(round(pgh_stats_2[6]), big.mark = ",")

ggplot(a, aes(x = `Measure Value`)) +
  geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pgh_stats_2, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats_2, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats_2, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats_2, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 150))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts', 
       title = 'Measured Radon in Greater Pittsburgh',
       subtitle = "Extluded outliers (1,000pCi/L or higher from the universe)",
       caption = "source: Open Data PA") +
  annotate("text", x=c(5), y=c(92000), label=c(glue("median={round(pgh_stats_2[2],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(9.0), y=c(86000), label=c(glue("mean={round(pgh_stats_2[3],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(10.0), y=c(80000), label=c(glue("sd={round(pgh_stats_2[5],1)}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(25.0), y=c(70000), label=c(glue("mean+2sd={(round(pgh_stats_2[5],1)*2 + round(pgh_stats_2[3],1))}pCi/l")), size = 5, face = "bold")+
  annotate("text", x=c(18), y=c(65000), label=c(glue("(n={N})")), size = 5, face = "bold")+
  annotate("text", x=c(18), y=c(75000),label=c(glue("mean+sd={(round(pgh_stats_2[5],1) + round(pgh_stats_2[3],1))}pCi/l")), size = 5, face = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown()) 

ggsave("radon_pgh_norm.png", height = 5, width = 5)

clean_radon_data <- rad_pgh %>%
  mutate(z_score = (`Measure Value` - mean(`Measure Value`, na.rm = TRUE)) / sd(`Measure Value`, na.rm = TRUE)) %>%
  filter(abs(z_score) <= 3) 
clean_radon_data %>% 
  slice_max(`Measure Value`)

pgh_stats_3 <- clean_radon_data %>% 
  summarise(minimum = min(`Measure Value`, round(2)),
            median = median(`Measure Value`, round(2)),
            mean = mean(`Measure Value`),
            maximum = max(`Measure Value`, round(2)),
            sd = sd(`Measure Value`, round(2)),
            n = length(`Measure Value`))

round(pgh_stats_3[5],1)
(round(pgh_stats_3[5],1)*2 + round(pgh_stats_2[3],1))
N <- format(pgh_stats_3$n, big.mark=",")

ggplot(clean_radon_data, aes(x = `Measure Value`)) +
  geom_histogram(bins = 16, fill = 'red', alpha = 0.5) +
  # geom_histogram(bins = 15, fill = 'red', alpha = 0.5) +
  geom_vline(data=pgh_stats_3, aes(xintercept=mean)) +
  geom_vline(data =pgh_stats_3, aes(xintercept=median), linetype="dashed") +
  geom_vline(data = pgh_stats_3, aes(xintercept=mean+sd), linetype = "dashed", color = "blue")+
  geom_vline(data = pgh_stats_3, aes(xintercept=mean+sd*2), linetype = "dotted", color = "blue") +
  scale_x_log10(limits = c(0.5, 80))+
  labs(x = 'Log10: Measured Radon (pCi/L)', y = 'Counts',
       title = 'Measured Radon in Greater Pittsburgh',
       subtitle = "Excluded outliers (61.7pCi/L or higher = 3SD from the universe)",
       caption = "source: Open Data PA") +
  annotate("text", x=c(3), y=c(61000), label=c(glue("median={round(pgh_stats_3[2],1)}pCi/l")), size = 5, fontface = "bold")+
  annotate("text", x=c(5.0), y=c(55000), label=c(glue("mean={round(pgh_stats_3[3],1)}pCi/l")), size = 5, fontface = "bold")+
  annotate("text", x=c(8.0), y=c(49000), label=c(glue("sd={round(pgh_stats_3[5],1)}pCi/l")), size = 5, fontface = "bold")+
  annotate("text", x=c(22.0), y=c(37000), label=c(glue("mean+2sd={(round(pgh_stats_3[5],1)*2 + round(pgh_stats_3[3],1))}pCi/l")), size = 5, fontface = "bold")+
  annotate("text", x=c(10), y=c(30000), label=c(glue("(n={N})")), size = 5, fontface = "italic")+
  annotate("text", x=c(13), y=c(43000),label=c(glue("mean+sd={(round(pgh_stats_3[5],1) + round(pgh_stats_3[3],1))}pCi/l")), size = 5, fontface = "bold")+
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 20, face = "bold"))

ggsave("radon_pgh_3sd.png", height = 6, width = 6)
