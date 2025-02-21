

rm(list=ls())

### load libraries
library(fredr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)

### set plot theme
theme_set(
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(size=16),
      plot.title = element_text(hjust = 0.5, face="bold", size=18),
      plot.subtitle = element_text(hjust = 0.5, face="bold", size=16),
      plot.caption = element_text(size=12)
    )
)

### set fred key
fredr_set_key("8454091b420f6979c70e84de8e611118")

#######################################
############ read in data ############
#######################################
### cpi
cpi <- fredr("CPIAUCSL") %>% 
  rename(cpi = value) %>% 
  select(-realtime_start, -realtime_end)

### pce
pce <- fredr("PCEPI") %>% 
  rename(pce = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

chained_cpi <- fredr("SUUR0000SA0") %>% 
  rename(chained_cpi = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

### avg hourly earnings
hourly_earnings <- fredr("CES0500000003") %>% 
  rename(avg_hourly_earnings = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

### weekly_nominal_wages
weekly_nominal_wages <- fredr("LES1252881500Q") %>% 
  rename(weekly_wages = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

### non-managerial wages
weekly_production_wages <- fredr("CES0500000030") %>% 
  rename(production_wages = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

### lowest quintile of wage earners (hispanic/latino)
lowest_quintile_wages <- fredr("LEU0252885400A") %>% 
  rename(lowest_quintile_wages = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

#######################################
############ clean data ############
#######################################
### agg chained & pce-pi indices
### chained cpi begins in 1999-12-01
pce_1999_value <- pce %>% 
  filter(date == "1999-12-01") %>% 
  pull(pce)

chained_1999_value <- chained_cpi %>% 
  filter(date == "1999-12-01") %>% 
  pull(chained_cpi)

### equalizing ratio
equalizing_ratio <- pce_1999_value/chained_1999_value

### creating the chained index is a two step process of putting
### chained cpi in terms of pce pi then filling pre 1999 values with pce pi index
### combine indices and create a single index
chained_index <- pce %>% 
  left_join(chained_cpi, by="date") %>% 
  mutate(chained_index = ifelse(!is.na(chained_cpi),
                                chained_cpi * equalizing_ratio, NA))

### fill pce-pi values when chained index is NA
chained_index <- chained_index %>% 
  left_join(cpi, by="date") %>% 
  mutate(
    chained_index = ifelse(is.na(chained_index), pce, chained_index)
  )

#######################################
##### evaluate inflation measures #####
#######################################
chained_index %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=cpi, color="Official CPI"), size=2) +
  geom_line(aes(y=chained_index, color="Chained CPI"), size=2) +
  scale_color_manual(values = c("Official CPI" = "black",
                                "Chained CPI" = "blue")) +
  guides(color=guide_legend(title = "Inflation Index")) +
  labs(title="Inflation is Grossly Overstated",
       y="Index Value", x="", caption= "Source: FRED") 

### determine growth rates
### since 1967: cpi 865% / chained_index 642%
### cpi overstated: 35% since 1967!
chained_index %>% 
  filter(date %in% c("1967-01-01", "2024-12-01")) %>% 
  mutate(
    cpi_change = ((cpi - lag(cpi))/lag(cpi))*100,
    chained_change = ((chained_index-lag(chained_index))/lag(chained_index))*100
  ) %>% 
  select(date, cpi, cpi_change, chained_index, chained_change)

### extent of cpi over
((865 - 642) / 642) *100

#######################################
########### real income ##############
#######################################

### determine base year values of inflation
chained_base_year <- chained_index %>% 
  filter(date == "2017-01-01") %>% 
  pull(chained_index)

cpi_base_year <- chained_index %>% 
  filter(date == "2017-01-01") %>% 
  pull(cpi)

### standardize both inf indices
income_data <- chained_index %>% 
  mutate(
    chained_2017_index = chained_index/chained_base_year,
    cpi_2017_index = cpi/cpi_base_year
  ) %>% 
  left_join(weekly_nominal_wages, by="date") %>% 
  select(date, weekly_wages, cpi_2017_index, chained_2017_index)

### adjust nominal weekly wages for both inf measures
income_data <- income_data %>% 
  mutate(
    cpi_adj_wages = weekly_wages / cpi_2017_index,
    chained_adj_wages = weekly_wages / chained_2017_index
  ) %>% 
  drop_na(cpi_adj_wages, chained_adj_wages) 

### plot real wages
income_data %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y=cpi_adj_wages, color="CPI Adj Wages"), size=1) +
  geom_line(aes(y=chained_adj_wages, color="Chained Adj Wages"), size=1) +
 # geom_line(aes(y=weekly_wages, color="Nominal Wages"), size=1) +
  scale_color_manual(values = c(
    "CPI Adj Wages" = "black",
    "Chained Adj Wages" = "blue"
   # "Nominal Wages" = "darkgreen"
  )) +
  labs(title = "Real Weekly Wages", subtitle = "2017 Dollars", y="Weekly Income",
       x="", caption= "Source: FRED") +
  guides(color=guide_legend(title="Real Wages")) +
  scale_y_continuous(labels=scales::dollar_format())

### determine % growth in real wages
### since 1979
income_data %>% 
  filter(date %in% c("1979-01-01", "2024-01-01")) %>% 
  mutate(
    cpi_income_change = ((cpi_adj_wages - lag(cpi_adj_wages))/lag(cpi_adj_wages))*100,
    chained_income_change = ((chained_adj_wages - lag(chained_adj_wages))/
                               lag(chained_adj_wages))*100
  ) %>% 
  select(date, cpi_adj_wages, cpi_income_change, chained_adj_wages, chained_income_change)

### calc downward bias
((31.6-8.17) / 8.17) *100

### racial income
#### racial incomes & income gap
weekly_nominal_black_wages <- fredr("LEU0252896500Q") %>% 
  rename(black_nom_income = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

weekly_nominal_white_wages <- fredr("LEU0253203900Q") %>% 
  rename(white_nom_income = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

### merge racial income data sets with income_data
racial_income_df <- purrr::reduce(
  list(weekly_nominal_black_wages, weekly_nominal_white_wages, income_data,
       weekly_production_wages, lowest_quintile_wages),
  ~left_join(.x, .y, by="date")
)

### lowest income growth master df
lowest_income_df <- purrr::reduce(
  list(income_data, weekly_production_wages, lowest_quintile_wages),
  ~left_join(.x, .y, by="date")
)
2023-50
### calc real wage changes in low wages
lowest_income_df <- lowest_income_df %>% 
  mutate(
    production_cpi_real = production_wages/cpi_2017_index,
    production_chained_real = production_wages/chained_2017_index,
    lowest_cpi_real = lowest_quintile_wages/cpi_2017_index,
    lowest_chained_real = lowest_quintile_wages/chained_2017_index)

### calc change in poor american's real wages
lowest_income_df %>% 
  filter(date %in% c("1979-01-01", "2024-01-01")) %>% 
  mutate(
    prod_cpi_change = ((production_cpi_real - lag(production_cpi_real))/
                         production_cpi_real)*100,
    prod_chained_change = ((production_chained_real - lag(production_chained_real))/
                             production_chained_real)*100,
    lowest_cpi_change = ((lowest_cpi_real - lag(lowest_cpi_real))/
                           lowest_cpi_real)*100,
    lowest_chained_change = ((lowest_chained_real - lag(lowest_chained_real))/
                               lowest_chained_real)*100
  ) %>% 
  select(date, prod_cpi_change, prod_chained_change, lowest_cpi_change, lowest_chained_change)
### adjust nominal wages for inf indices
racial_income_df <- racial_income_df %>% 
  mutate(
    white_cpi_wages = white_nom_income/cpi_2017_index,
    white_chained_wages = white_nom_income/chained_2017_index,
    black_cpi_wages = black_nom_income/cpi_2017_index,
    black_chained_wages = black_nom_income/chained_2017_index,
    prod_cpi_wages = production_wages/cpi_2017_index,
    prod_chained_wages = production_wages/chained_2017_index,
    lowest_quintile_cpi_wages = lowest_quintile_wages/cpi_2017_index,
    lowest_quintile_chained_wages = lowest_quintile_wages/chained_2017_index
  )

### plot real wages
racial_income_df %>% 
  ggplot(aes(x=date)) +
  ### white wages
  geom_line(aes(y=white_cpi_wages, color="White - CPI"), size=1) +
  geom_point(aes(y=white_cpi_wages, color="White - CPI"), size=3, shape=17) +
  geom_line(aes(y=white_chained_wages, color="White - Chained"), size=1) +
  geom_point(aes(y=white_chained_wages, color="White - Chained"), size=3, shape=16) +
  ### black wages
  geom_line(aes(y=black_cpi_wages, color="Black - CPI"), size=1) +
  geom_point(aes(y=black_cpi_wages, color="Black - CPI"), size=3, shape=17) +
  geom_line(aes(y=black_chained_wages, color="Black - Chained"), size=1) +
  geom_point(aes(y=black_chained_wages, color="Black - Chained"), size=3, shape=16) +
  scale_color_manual(values = c(
    "White - CPI" = "blue",
    "White - Chained" = "darkblue",
    "Black - CPI" = "red",
    "Black - Chained" = "darkred"
  )) +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(color=guide_legend(title="Race:Income Type")) +
  labs(title = "Wages Aren't Black & White", subtitle= "2017 Dollars",
       y="Weekly Wages", x="", caption = "Source: FRED")

### calc racial income differences
racial_income_changes <- racial_income_df %>% 
  filter(date %in% c("2000-01-01", "2024-01-01")) %>% 
  mutate(
    white_cpi_change = ((white_cpi_wages - lag(white_cpi_wages))/
                          lag(white_cpi_wages))*100,
    white_chained_change = ((white_chained_wages - lag(white_chained_wages))/
                          lag(white_chained_wages))*100,
    black_cpi_change = ((black_cpi_wages - lag(black_cpi_wages))/
                          lag(black_cpi_wages))*100,
    black_chained_change = ((black_chained_wages - lag(black_chained_wages))/
                          lag(black_chained_wages))*100,
    prod_cpi_change = ((prod_cpi_wages - lag(prod_cpi_wages))/
                         lag(prod_cpi_wages))*100,
    prod_chained_change = ((prod_chained_wages - lag(prod_chained_wages))/
                         lag(prod_chained_wages))*100,
    lowest_cpi_change = ((lowest_quintile_cpi_wages - lag(lowest_quintile_cpi_wages))/
                             lag(lowest_quintile_cpi_wages))*100,
    lowest_chained_change = ((lowest_quintile_chained_wages - lag(lowest_quintile_chained_wages))/
                           lag(lowest_quintile_chained_wages))*100,
  )

### lollipop plot
### black wage growth since 2020
chained_black_wage_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(black_chained_change)
cpi_black_wage_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(black_cpi_change)

### white wage growth since 2020
chained_white_wage_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(white_chained_change)

cpi_black_wage_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(white_cpi_change)

### production / nonsupervisory wages
prod_cpi_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(prod_cpi_change)

prod_chained_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(prod_chained_change)

### lowest quintile wage changes
lowest_chained_growth <- racial_income_changes %>% 
  slice(2) %>%
  pull(lowest_chained_change)

lowest_cpi_growth <- racial_income_changes %>% 
  slice(2) %>% 
  pull(prod_cpi_change)

### inf growth 
cpi_change <- chained_index %>% 
  filter(date %in% c("2000-01-01", "2024-12-01")) %>% 
  mutate(
    cpi_change = ((cpi - lag(cpi))/lag(cpi))*100,
    chained_change = ((chained_index-lag(chained_index))/lag(chained_index))*100
  ) %>% 
  slice(2) %>% 
  pull(cpi_change)

chained_change <- chained_index %>% 
  filter(date %in% c("2000-01-01", "2024-12-01")) %>% 
  mutate(
    cpi_change = ((cpi - lag(cpi))/lag(cpi))*100,
    chained_change = ((chained_index-lag(chained_index))/lag(chained_index))*100
  ) %>% 
  slice(2) %>% 
  pull(chained_change)

### general wage growth
chained_income_change <- income_data %>% 
  filter(date %in% c("2000-01-01", "2024-10-01")) %>% 
  mutate(
    cpi_income_change = ((cpi_adj_wages - lag(cpi_adj_wages))/lag(cpi_adj_wages))*100,
    chained_income_change = ((chained_adj_wages-lag(chained_adj_wages))/lag(chained_adj_wages))*100
  ) %>% 
  slice(2) %>% 
  pull(chained_income_change)

### cpi income change
cpi_income_change <- income_data %>% 
  filter(date %in% c("2000-01-01", "2024-10-01")) %>% 
  mutate(
    cpi_income_change = ((cpi_adj_wages - lag(cpi_adj_wages))/lag(cpi_adj_wages))*100,
    chained_income_change = ((chained_adj_wages-lag(chained_adj_wages))/lag(chained_adj_wages))*100
  ) %>% 
  slice(2) %>% 
  pull(cpi_income_change)

### agg data into lollipop plot
# Create dataset with all well-being measures
lollipop_data <- tibble(
  measure = c("Total Wage Growth",
              "Black Wage Growth", 
              "White Wage Growth",
              "Non-Managerial Wage Growth",
              "Lowest 25%: Hispanic"
            ),
  `CPI Adjusted` = c(cpi_income_change,
                    cpi_black_wage_growth, 
                    cpi_black_wage_growth,
                    prod_cpi_growth,
                    lowest_cpi_growth
                 ),
  `Chained Adjusted` = c(chained_income_change,
                       chained_black_wage_growth, 
                       chained_white_wage_growth, 
                       prod_chained_growth,
                       lowest_chained_growth
                   )
) %>%
  pivot_longer(cols = c(`CPI Adjusted`, `Chained Adjusted`), 
               names_to = "Inflation_Index", 
               values_to = "Value")

### plot data
ggplot(lollipop_data, aes(x = measure, y = Value, color = Inflation_Index)) +
  geom_segment(aes(x = measure, xend = measure, 
                   y = min(Value), yend = max(Value)), 
               size = 1, color = "black") +  # Lollipop stick
  geom_point(aes(color=Inflation_Index), size = 7) +  # Lollipop ends
  scale_color_manual(values = c("CPI Adjusted" = "red", "Chained Adjusted" = "blue")) +
  coord_flip() +  # Horizontal orientation
  labs(
    title = "Progress is Better Than Advertised",
    subtitle = "2000-2024",
    x = "",
    y = "% Change",
    color = "Inflation Index", caption="Source: FRED"
  ) +
  theme(axis.text = element_text(face="bold", size=14))
  theme_minimal()


#cpi_racial_income_inequality <- racial_income_changes %>% 
 # filter(date == "2024-10-01") %>% 
#  mutate(racial_inequality = white_cpi_wages/black_cpi_wages) %>% 
#  pull(racial_inequality)

#chained_racial_income_inequality <- racial_income_changes %>% 
#  filter(date == "2024-10-01") %>% 
#  mutate(racial_inequality = white_chained_wages/black_chained_wages) %>% 
#  pull(racial_inequality)
  
########################

#######################################
##### combine chained cpi & pce-pi ########
#######################################



# Plot the results
ggplot(chained_df, aes(x = date)) +
  geom_line(aes(y = combined_index, color = "Combined Index"), size = 1) +
  geom_line(aes(y = pce, color = "PCEPI"), linetype = "dashed") +
  geom_line(aes(y = chained_cpi_scaled, color = "Rebased Chained CPI"), linetype = "dotted") +
  labs(title = "Merged Inflation Index (PCEPI + Chained CPI)",
       y = "Index (2017 = 100)",
       x = "Year") +
  theme_minimal() +
  scale_color_manual(values = c("Combined Index" = "blue", "PCEPI" = "red", "Rebased Chained CPI" = "green"))

#######################################
##### compare inflation data ########
#######################################

### change in inflation since 1967
### 626%
chained_df %>% 
  filter(date %in% c("1967-01-01", "2024-01-01")) %>% 
  mutate(chained_cpi_change = ((combined_index - lag(combined_index))/lag(combined_index))*100) %>% 
  select(date, combined_index, chained_cpi_change)

### 841%
cpi %>% 
  filter(date %in% c("1967-01-01", "2024-01-01")) %>% 
  mutate(cpi_change = ((cpi - lag(cpi))/lag(cpi))*100) %>% 
  select(date, cpi, cpi_change)

### cpi overstates inflation by...
((841 - 626)/626)*100

#######################################
##### adjusted real wages ########
#######################################

### cpi 2016 = base year
base_year_cpi <- cpi %>% 
  filter(date == as.Date("2016-01-01")) %>% 
  pull(cpi)

### chained cpi 2016 = base year
base_year_chained <- chained_df %>% 
  filter(date == as.Date("2016-01-01")) %>% 
  pull(pce)

### index cpi to 2016
cpi <- cpi %>% 
  mutate(cpi_indexed = cpi / base_year_cpi)

### index pce to 2016
chained_df <- chained_df  %>% 
 mutate(chained_cpi_indexed = combined_index / base_year_chained)

### merge all income data
income_data <- purrr::reduce(
  list(
    cpi,
    chained_df, 
    weekly_nominal_wages),
  ~left_join(.x, .y, by="date"))

### calc real wages
income_data %>%
  mutate(
    chained_real_wages = weekly_wages / chained_cpi_indexed,  # Prevents NA issues
    cpi_real_wages = weekly_wages / cpi_indexed              # Prevents NA issues
  ) %>% 
  drop_na(chained_real_wages, cpi_real_wages, weekly_wages) %>%  # Removes only rows with NA in these columns
  ggplot(aes(x=date)) +
  geom_line(aes(y=chained_real_wages, color="Chained Real Income"), size=1) +
  geom_line(aes(y=cpi_real_wages, color="CPI Real Income"), size=1) +
  geom_line(aes(y=weekly_wages, color="Nominal Weekly Wages"), size=1) +
  theme_minimal() +
  scale_color_manual(values = c(
    "Chained Real Income" = "darkgreen",
    "CPI Real Income" = "blue",
    "Nominal Weekly Wages" = "black"
  )) +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(color = guide_legend(title="Income Types")) +
  labs(title = "Weekly Income",
       y="", x="")

#######################################
##### real income growth ########
#######################################
### 1980-2024
### cpi weekly wage increase = 12.5%
### cpi weekly wage increase = 32.8%

income_data %>%
  mutate(
    chained_real_wages = weekly_wages / chained_cpi_indexed,  
    cpi_real_wages = weekly_wages / cpi_indexed              
  ) %>% 
  drop_na(chained_real_wages, cpi_real_wages, weekly_wages) %>%  # Removes only rows with NA in these columns
  filter(date %in% c("1980-01-01", "2024-01-01")) %>% 
  mutate(
    chained_income_change = ((chained_real_wages - lag(chained_real_wages))/lag(chained_real_wages))*100,
    cpi_income_change = ((cpi_real_wages - lag(cpi_real_wages))/lag(cpi_real_wages))*100
  ) %>% 
  select(date, chained_real_wages, cpi_real_wages, cpi_income_change, chained_income_change) 

#### racial incomes & income gap
annual_black_income <- fredr("CXUINCAFTTXLB0905M") %>% 
  rename(black_income = value) %>% 
  select(-realtime_start, -realtime_end)

weekly_nominal_black_wages <- fredr("LEU0252896500Q") %>% 
  rename(weekly_black_nom_income = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

weekly_nominal_white_wages <- fredr("LEU0253203900Q") %>% 
  rename(weekly_white_nom_income = value) %>% 
  select(-realtime_start, -realtime_end, -series_id)

annual_income_non_black <- fredr("CXUINCAFTTXLB0903M") %>% 
  rename(black_income = value) %>% 
  select(-realtime_start, -realtime_end)
  
### weekly white v black income
racial_weekly_wages <- weekly_nominal_black_wages %>% 
  left_join(weekly_nominal_white_wages, by="date")

racial_income <- purrr:::reduce(
  list(income_data,
  weekly_nominal_black_wages,
  weekly_nominal_white_wages),
~left_join(.x, .y, by="date")
)

### adjust nominal wages for inflation
racial_income %>% 
  mutate(
    black_cpi_adj_wages = weekly_black_nom_income / cpi_indexed,
    white_cpi_adj_wages = weekly_white_nom_income / cpi_indexed,
    white_chained_adj_wages = weekly_white_nom_income / chained_cpi_indexed,
    black_chained_adj_wages = weekly_black_nom_income / chained_cpi_indexed
  ) %>% 
  drop_na(black_cpi_adj_wages, white_cpi_adj_wages, white_chained_adj_wages, 
          black_chained_adj_wages) %>% 
  ggplot(aes(x=date)) +
  # CPI-adjusted wages (Triangle Points)
  geom_line(aes(y = black_cpi_adj_wages, color = "Black - CPI"), size = 1) +
  geom_line(aes(y = white_cpi_adj_wages, color = "White - CPI"), size = 1) +
  geom_point(aes(y = black_cpi_adj_wages, color = "Black - CPI"), shape = 17, size = 2) + # Triangle
  geom_point(aes(y = white_cpi_adj_wages, color = "White - CPI"), shape = 17, size = 2) + # Triangle
  
  # Chained CPI-adjusted wages (Circle Points)
  geom_line(aes(y = white_chained_adj_wages, color = "White - Chained CPI"), size = 1) +
  geom_line(aes(y = black_chained_adj_wages, color = "Black - Chained CPI"), size = 1) +
  geom_point(aes(y = black_chained_adj_wages, color = "Black - Chained CPI"), shape = 16, size = 2) + # Circle
  geom_point(aes(y = white_chained_adj_wages, color = "White - Chained CPI"), shape = 16, size = 2) + # Circle
  
  # Formatting
  theme_minimal() +
  scale_color_manual(values = c(
    "Black - CPI" = "darkred",
    "White - CPI" = "darkblue",
    "Black - Chained CPI" = "red",
    "White - Chained CPI" = "blue"
  )) +
  labs(
    title = "Inflation-Adjusted Wages by Race",
    y = "Adjusted Weekly Wages",
    x = "Year",
    color = "Adjustment Type"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  guides(color = guide_legend(title = "Race & Adjustment Type"))
  

### whats the wage increase from white & black ?
racial_income %>% 
  mutate(
    black_cpi_adj_wages = weekly_black_nom_income / cpi_indexed,
    white_cpi_adj_wages = weekly_white_nom_income / cpi_indexed,
    white_chained_adj_wages = weekly_white_nom_income / chained_cpi_indexed,
    black_chained_adj_wages = weekly_black_nom_income / chained_cpi_indexed
  ) %>% 
  filter(date %in% c("2000-01-01", "2024-01-01")) %>% 
  select(date, black_cpi_adj_wages, white_cpi_adj_wages, 
         white_chained_adj_wages, black_chained_adj_wages) 
  mutate(
    wh_chained_incr = ((white_chained_adj_wages - lag(white_chained_adj_wages)) 
                       /lag(white_chained_adj_wages))*100,
    wh_cpi_incr = ((white_cpi_adj_wages - lag(white_cpi_adj_wages)) 
                       /lag(white_cpi_adj_wages))*100,
    bl_chained_incr = ((black_chained_adj_wages - lag(black_chained_adj_wages)) 
                       /lag(black_chained_adj_wages))*100,
    bl_cpi_incr = ((black_cpi_adj_wages - lag(black_cpi_adj_wages)) 
                       /lag(black_cpi_adj_wages))*100,
  ) %>% 
    select(date, wh_chained_incr, wh_cpi_incr, bl_chained_incr, bl_cpi_incr)

### median household income
median_household_income <- freedr("MEHOINUSA646N") %>% 
  rename(black_income = value) %>% 
  select(-realtime_start, -realtime_end)

### poverty rate


  ######## 
