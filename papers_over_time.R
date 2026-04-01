
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
  theme_minimal()

ggsave("pubs_by_year.pdf", pubs_by_year, width = 4, height = 4)


# Binned version

df2 <- raw_data %>%
  mutate(year_bin = cut(Year,
                        breaks = c(2005, 2011, 2016, 2021, 2026),
                        labels = c("2005–2011", "2012–2016", "2017–2021", "2022–2026"),
                        include.lowest = TRUE))

bin_counts <- df2 %>%
  count(year_bin)

p1<-ggplot(bin_counts, aes(x = year_bin, y = n)) +
  geom_col() +
  labs(x = "Year range", y = "Number of publications") +
  theme_minimal()



#normalize by year
bin_counts <- bin_counts %>%
  mutate(years_in_bin = c(7, 5, 5, 5),
         pubs_per_year = n / years_in_bin)


p2<-ggplot(bin_counts, aes(x = year_bin, y = pubs_per_year)) +
  geom_col() +
  labs(x = "Year range", y = "Avg publications per year") +
  theme_minimal()


binned_figure <- ggarrange(p1,p2, nrow=1, ncol=2)
ggsave("Number_of_pubs_binned.pdf", binned_figure, width = 8, height = 4)
