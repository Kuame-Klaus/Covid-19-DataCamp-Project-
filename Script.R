library(readr)
library(ggplot2)
library(dplyr)

datasets_confirmed_cases_worldwide <-read_csv("datasets_confirmed_cases_worldwide.csv")

View(datasets_confirmed_cases_worldwide)

datasets_confirmed_cases_worldwide

WorlwideCumCases <- ggplot(datasets_confirmed_cases_worldwide, aes(y=cum_cases, x=date))+
                      geom_line()+
                        ylab("Cumulative confirmed cases") +
                          theme_classic() +
                           labs(title = 'Worldwide Cumulative Cases')
WorlwideCumCases

who_events <- tribble(
                ~ date, ~ event,
                "2020-01-30", "Global health\nemergency declared",
                "2020-03-11", "Pandemic\ndeclared",
                "2020-02-13", "China reporting\nchange") %>%
                   mutate(date = as.Date(date))

datasets_confirmed_cases_china_vs_world <- read_csv("datasets_confirmed_cases_china_vs_world.csv")


plt_cum_confirmed_cases_china_vs_world <- ggplot(datasets_confirmed_cases_china_vs_world, aes(y=cum_cases, x=date, color = is_china))+
                                           geom_line() +
                                            ylab("Cumulative confirmed cases") + 
                                              theme_classic() +
                                                labs(title = 'China vs World Cumulative Cases')

plt_cum_confirmed_cases_china_vs_world

plt_cum_confirmed_cases_china_vs_world1 <- ggplot(datasets_confirmed_cases_china_vs_world, aes(y=cum_cases, x=date))+
                                            geom_line() +
                                             ylab("Cumulative confirmed cases") + 
                                              theme_classic() +
                                               labs(title = 'China vs World Cumulative Cases with Key Dates')
plt_cum_confirmed_cases_china_vs_world1 + 
  geom_vline(aes(xintercept=date), data=who_events, linetype="dashed") +
   geom_text(aes(x=date, label=event), data=who_events, y= 100000)


View(datasets_confirmed_cases_china_vs_world)

not_china <- datasets_confirmed_cases_china_vs_world %>% filter((is_china == "Not China"))


plt_not_china_trend_lin <- ggplot(not_china, aes(x=date, y=cum_cases)) +
                            geom_line() + 
                              geom_smooth (method="lm", se= FALSE) +
                                ylab("Cumulative confirmed cases")  +
                                  theme_classic()
plt_not_china_trend_lin +
  labs(title = 'Cases not China Trend')

plt_not_china_trend_lin + scale_y_log10() +
  labs(title = 'Cases not China Trend Scale at Log10')

confirmed_cases_by_country <- read_csv("confirmed_cases_by_country.csv")

glimpse(confirmed_cases_by_country)


top_countries_by_total_cases <- confirmed_cases_by_country %>% 
                                  group_by(country) %>% summarise(total_cases = sum(cum_cases)) %>%
                                    top_n(7, total_cases)
top_countries_by_total_cases


confirmed_cases_top7_outside_china <- read_csv("confirmed_cases_top7_outside_china.csv")

glimpse(confirmed_cases_top7_outside_china)

top7_outside_chinaCumCases <- ggplot(confirmed_cases_top7_outside_china, aes(date, cum_cases, color = country, group = country)) +
                              geom_line() +
                                ylab("Cumulative confirmed cases") + 
                                  theme_classic()
top7_outside_chinaCumCases



Top7CountriesTotalRecordedCases <- ggplot(top_countries_by_total_cases, aes(x = total_cases, y = reorder(country, total_cases))) +
                                    geom_col() + 
                                      theme_minimal() +
                                        labs(title = 'Top 7 Countries Total Recorded Cases') +
                                          geom_text(aes(label = total_cases, color = "00FF00")) +
                                            theme(legend.key = element_blank(), axis.ticks.x = element_blank(), panel.grid = element_blank())
Top7CountriesTotalRecordedCases