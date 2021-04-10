## Yuexia Tian
## u976100

## Load packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

## Load data EXAMPLE------------------------------------------------------------
AC <- read.delim("input/AC.txt", stringsAsFactors = FALSE)

## Question 1 -----------------------------------------------------------------

AC01 <- AC
mean_value <- mean(AC01$Setting_cool, na.rm = TRUE)
AC01$Setting_cool[AC01$Setting_cool == 0] <- mean_value
answer1 <- AC01

## Question 2 -----------------------------------------------------------------

AC02 <- AC
filter_AC02 <- AC02 %>% 
  filter(EERtestAvg >= 4 & COPtestAvg >= 4) %>%
  select(contains("Star2010") | Brand )
answer2 <- filter_AC02

## Question 3 -----------------------------------------------------------------

plot03 <- ggplot() +
  geom_bar(data = AC, aes(x = c_star_mixed)) + 
  scale_x_continuous(limits = c(0, 6), 
                     breaks = c(1, 2, 3, 4, 5),
                     name = "Energy Rating Label")
answer3 <- plot03

## Question 4 -----------------------------------------------------------------

AC04 <- AC
aggregated <- AC04 %>%
  group_by(Brand) %>%
  summarise(
    NumberOfProducts = n(), 
    AvCoolingLabel = mean(Star2010_Cool, na.rm = TRUE),
    AvHeatingLabel = mean(Star2010_Heat, na.rm = TRUE)
    ) %>%
  mutate(AvLabel = (AvCoolingLabel + AvHeatingLabel) / 2)
answer4 <- aggregated

## Question 5 -----------------------------------------------------------------

AC05 <- AC
remove_underscores <- function(x){
  gsub("_", ".", x, fixed = TRUE)
}
the_answer <- AC05 %>%
  filter(MEPSComp == "MEPS 2011" | MEPSComp == "MEPS 2019") %>%
  rename_with(remove_underscores) %>%
  select(-contains("configuration", ignore.case = TRUE))
answer5 <- the_answer

## Question 6 -----------------------------------------------------------------

brands_top10 <- answer4 %>%
  filter(NumberOfProducts >= 5) %>%
  arrange(desc(AvLabel)) %>%
  slice(0:10)

plot06 <- ggplot() +
  geom_point(data = brands_top10, aes(x = AvCoolingLabel, y = AvHeatingLabel, 
                                      color = AvLabel, 
                                      size = NumberOfProducts)) +
  scale_color_continuous(low = "white", high = "black") +
  theme_minimal() +
  scale_color_viridis_c()

answer6 <- plot06

## Question 7 -----------------------------------------------------------------

answer7 <- filter(AC, Configuration1 %in% c("Ducted", "Non Ducted")) %>%
  ggplot(aes(x = Configuration1, fill = Country)) +
  geom_bar(position = "fill") +
  scale_x_discrete(name = element_blank()) +
  scale_y_continuous(name = element_blank())

## Question 8 -----------------------------------------------------------------

answer8_c <- AC %>% 
  filter(grepl("Multiple split", Configuration2))  %>%
  ggplot(aes(x = Configuration1, y = C.Total.Cool.Rated, 
             fill = Configuration2)) +
  geom_violin() +
  scale_fill_discrete(breaks = c("Multiple split - Fixed head",
                                 "Multiple split - VRF"), 
                      labels = c("Fixed Head", "VRF")) +
  labs(fill = "Multiple Split Type", 
       y = "Total Cooling Output (kW)", 
       x = "Indoor Air Distribution")
answer8 <- plot08

## Question 9 -----------------------------------------------------------------

chinese_AC <- AC[grep("China", AC$Country, ignore.case = TRUE), ]
heating <- chinese_AC %>%
  group_by(Phase,) %>%
  summarize(Mean.APC = mean(H.Total.Heat.Rated, na.rm = T),  .groups = "keep")

refrigerants <- chinese_AC %>%
  group_by(Refrigerant) %>%
  summarise(n = n(), .groups = "keep") %>%
  arrange(desc(n)) %>%
  select(-n)

answer9 <- list(Heating = heating, Refrigerant = refrigerants$Refrigerant)


## Question 10 -----------------------------------------------------------------

answer10_c <- AC %>%
  filter(outdoortype != "") %>%
  ggplot(aes(x = AnnualOutputCOP, fill = outdoortype)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(name = "COP", 
                     limits = c(2.5,6), 
                     breaks = seq(2.5,6,0.5)) +
  scale_y_continuous(name = "Number of AC Units", 
                     breaks = c(0,400)) + 
  labs(fill = "Outdoor Type") +
  theme_linedraw() +
  theme(panel.grid.minor = element_blank()) +
  ggtitle(
    "Distribution of heating energy efficiency (kW/kW) per Outdoor Type")




