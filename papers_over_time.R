
# Plotting number of publications over time
library(tidyverse)
library(ggpubr)

raw_data <- read.csv("ARRE_Data.csv")

# Count observations per year
year_counts <- raw_data %>%
  count(Year)

# Plot
pubs_by_year <- ggplot(year_counts, aes(x = Year, y = n)) +
  geom_col() +
  labs(x = "Year", y = "Number of publications") +
  theme_minimal() +
  ggtitle("A")

#ggsave("pubs_by_year.pdf", pubs_by_year, width = 4, height = 4)


# Binned version

df2 <- raw_data %>%
  mutate(year_bin = cut(Year,
                        breaks = c(2005, 2009, 2014, 2019, 2024, 2026),
                        labels = c("2005–2009", "2010–2014", "2015-2019", "2020–2024", 
                                   "2025-May 2026"),
                        include.lowest = TRUE))

bin_counts <- df2 %>%
  count(year_bin)

binned_figure <-ggplot(bin_counts, aes(x = year_bin, y = n)) +
  geom_col() +
  labs(x = "Year range", y = "Number of publications") +
  theme_minimal() +
  ggtitle("B")


number_pubs<-ggarrange(pubs_by_year, binned_figure, nrow=1, ncol=2)

ggsave("number_pubs.pdf", number_pubs, width = 10, height = 4)

#Calculations 
# average increase
year_counts_2 <- year_counts %>% filter(Year != 2026)
avg_increase <- (tail(year_counts_2$n, 1) - year_counts_2$n[1]) /
  (tail(year_counts_2$Year, 1) - year_counts_2$Year[1])
avg_increase

# percent increase
years <- max(year_counts_2$Year) - min(year_counts_2$Year)
cagr <- ((tail(year_counts_2$n, 1) / year_counts_2$n[1])^(1 / years) - 1) * 100
cagr




