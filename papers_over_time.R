
# Plotting number of publications over time
library(tidyverse)
library(ggpubr)

raw_data <- read.csv("ARRE_Data.csv")

# Count observations per year
year_counts <- raw_data %>%
  count(Year)

# Plot
pubs_by_year <- ggplot(year_counts %>% filter(Year < 2026), aes(x = Year, y = n)) +
  geom_col() +
  labs(x = "Year", y = "Number of publications") +
  theme_minimal()

ggsave("pubs_by_year.pdf", pubs_by_year, width = 4, height = 4)


# Binned version

df2 <- raw_data %>%
  filter(Year < 2025) %>%
  mutate(year_bin = cut(Year,
                        breaks = c(2005, 2009, 2014, 2019, 2024),
                        labels = c("2005–2009", "2010–2014", "2015-2019", "2020–2024"),
                        include.lowest = TRUE))

bin_counts <- df2 %>%
  count(year_bin)

binned_figure <-ggplot(bin_counts, aes(x = year_bin, y = n)) +
  geom_col() +
  labs(x = "Year range", y = "Number of publications") +
  theme_minimal()



# #normalize by year
# bin_counts <- bin_counts %>%
#   mutate(years_in_bin = c(5, 5, 5, 5),
#          pubs_per_year = n / years_in_bin)
# 
# 
# p2<-ggplot(bin_counts, aes(x = year_bin, y = pubs_per_year)) +
#   geom_col() +
#   labs(x = "Year range", y = "Avg publications per year") +
#   theme_minimal()
#binned_figure <- ggarrange(p1,p2, nrow=1, ncol=2)


ggsave("pubs_by_year_bin.pdf", binned_figure, width = 4, height = 4)
