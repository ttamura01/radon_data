library(tidyverse)
library(glue)
library(ggtext)

setwd("/Users/takayukitamura/Documents/R_Computing/rad_pa")

crm_data <- read_csv("crm_2025-09-10.csv")

table <- crm_data %>% 
  summarise(n = length(value), min = min(value), mean = mean(value), median = median(value),sd = sd(value), 
            "mean + sd" = mean+sd, "mean+2sd" = mean + sd*2, max = max(value))
  
mean <- table$mean
median <- table$median
sd <- round(table$sd, 1)
"mean+sd" <- table$`mean + sd`
z1 <- round(table$`mean + sd`, 1)
z2 <- round(table$`mean+2sd`, 1)
max <- table$max

crm_data %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins =100) +
  scale_x_log10() +
  labs(title = "Radon distribution in Pittsburgh",
       x = "pCi/L(log10)",
       y = "observation") +
  theme(
    axis.text.x = element_text(),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "gray")
    
  )
  

crm_data %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line() +
  geom_hline(yintercept = table$mean, color = "skyblue") +
  geom_hline(yintercept = z1, color = "red", linetype = "dashed") +
  geom_hline(yintercept = z2, color = "red") +
  annotate(geom = "text",
           x = as.Date("2024-10-01"),
           y = 75,
           label = glue("median = {median}pCi/L\nmean = {round(mean,1)}pCi/L\nsd = {sd}pCi/L\nmean + sd = {z1}pCi/L\nmean + 2sd = {z2}pCi/L\nmax = {max}pCi/L"),
           size = 5, color = "blue") +
  annotate(geom = "label",
           x = as.Date("2022-10-01"),
           y = (c(z1, z2)),
           label = c(glue("{z1}pCi/L"), 
                     glue("{z2}pCi/L")), color = "red") +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     labels = c("0", "25.0", "50.0", "75.0", "100.0")) +
  labs(title = "Radon test results in Pittsburgh",
       subtitle = "c.5% of homes have 20pCi/L or higher radon level, 36 out of 1,000 people may develop lung cancers over time",
       y = "Radon level (pCi/L)",
       x = NULL,
       caption = "by T.Tamura") +
  theme(
    panel.background = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_textbox_simple(size = 15, face = "italic")
  )
  
