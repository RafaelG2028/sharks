# ==============================================================================
#                     INSTALL PACKAGES AND LOAD LIBRARIES  
# ==============================================================================

# Installs Tidyverse: Packages for data manipulation and visualization
install.packages("tidyverse")
# Installs plotly: For creating interactive plots
install.packages("plotly")
# Installs Scales: For better control over axis and legend formatting
install.packages("scales")
# Installs DT: For creating interactive data tables
install.packages("DT")
# Loads the Tidyverse package for data analysis and visualization
library(tidyverse)
# Loads the plotly package for interactive plotting
library(plotly)
# Loads Tibble for creating and working with data frames
library(tibble)
# Loads Scales to format and scale visuals
library(scales)
# Loads DT for displaying data tables interactively
library(DT)

# ==============================================================================
#                                LOAD DATASET    
# ==============================================================================

# Loads the Shark Dataset for analysis
shark <- read.csv("data/shark_data.csv")

# Displays the contents of the "shark" DataFrame
View(shark)

# ==============================================================================
#                              METADATA OVERVIEW
# ==============================================================================

# Summarizes the Dataframe "shark", showing structure and data types
glimpse(shark)

# ==============================================================================
#                             DATA TYPE CONVERSION    
# ==============================================================================

# Converts date-related columns to enable accurate analysis of timelines
shark$Season.Start <- as.Date(shark$Season.Start, format="%d-%b-%y")
shark$Season.End <- as.Date(shark$Season.End, format = "%d-%b-%y")
shark$Original.Air.Date <- as.Date(shark$Original.Air.Date, format="%d-%b-%y")


# Verifies that the date-related columns were properly converted
sapply(shark[, c("Original.Air.Date", "Season.Start", "Season.End")], class)

# Displays the contents of the "shark" Dataframe
View(shark)

# ==============================================================================
#                               REMOVE COLUMNS    
# ==============================================================================

# Streamlines the Dataframe by removing non-essential columns
shark <- shark %>% 
  select(-c("Company.Website", "Multiple.Entrepreneurs",
                 "Deal.Has.Conditions", "Advisory.Shares.Equity", 
                 "Pitchers.Average.Age", "Pitchers.City", "Pitchers.State"))

# Displays the contents of the "shark" data frame
View(shark)

# ==============================================================================
#                                 SECTION 1   
#                             SEASONS ANALYSIS
# ==============================================================================

# ==============================================================================
#                              TOTAL SEASONS 
# ==============================================================================

# Determines the total number of unique seasons
total_seasons <- n_distinct(shark$Season.Number)

# Displays the total number of seasons 
View(total_seasons)

# ==============================================================================   
#                            DURATION OF SEASONS   
# ==============================================================================

# Extracts unique "Season.End" dates and organizes them into a DataFrame
season_end_dates <- unique(shark$Season.End) 
season_end_dates <- as.data.frame(season_end_dates)

# Displays unique season end dates 
View(season_end_dates)

# Extracts unique "Season.Start" dates and organizes them into a DataFrame
season_start_dates <- unique(shark$Season.Start)
season_start_dates <-as.data.frame(season_start_dates)

# Displays unique season start dates  
View(season_start_dates)

# Computes the duration of each season in days (timedelta)
duration <- season_end_dates - season_start_dates 

# Renames the "season_end_dates" column to "Days" for a clearer representation
duration <- duration %>% 
  rename(Days = season_end_dates)
  
# Converts the "Days" variable to numeric format to remove the character "days"
duration$Days <- as.numeric(duration$Days)

# Displays the duration of each season in days 
View(duration)

# Adds a "Months" column by converting days to months and rounding to nearest whole number
duration$Months = round(duration$Days/30.45)

# Displays the duration of each season in days and months 
View(duration)

# ==============================================================================
#                         AVERAGE DURATION OF SEASONS  
# ==============================================================================

# Creates a DataFrame to summarize average season duration in days and months
average_duration <- data.frame(avg_in_days = round(mean(duration$Days)), 
                               avg_in_months = round(mean(duration$Months)))

# Displays the average season duration in days and months
View(average_duration)

# ==============================================================================
#                         TOTAL EPISODES PER SEASON
# ==============================================================================

# Determines the total number of episodes per season 
episodes_per_season <- shark %>%
  select(Season.Number, Episode.Number) %>% 
  group_by(Season.Number) %>%
  summarise(Total_Episodes = n_distinct(Episode.Number))

# Displays the total number of episodes per season
View(episodes_per_season)

# ==============================================================================
#                             TOTAL EPISODES    
# ==============================================================================

# Computes the total number of episodes aired
total_episodes <- sum(episodes_per_season$Total_Episodes)

# Displays the total number of episodes aired over 15 seasons
View(total_episodes)

# ==============================================================================
#                             SEASONS SUMMARY
# ==============================================================================

# Combines DataFrames to compile a comprehensive overview of season duration
duration_table <- cbind(season_start_dates, season_end_dates, 
                                   episodes_per_season, duration)

# Displays the contents of the data frame
View(duration_table)

# ==============================================================================
#                                 SECTION 2
#                            VIEWERSHIP ANALYSIS
# ============================================================================== 

# ============================================================================== 
#                           PEAK US VIEWERSHIP DATE
# ==============================================================================

# Option #1

# Identifies the date of the highest US viewership 
highest_viewership_date <- shark$Original.Air.Date[which.max(shark$US.Viewership)] 

# Displays the date the show achieved its highest US viewership
View(highest_viewership_date)
                      
# Option #2

# Identifies the date along with the highest US viewership total
highest_viewership_day <- shark %>%     
  select(Original.Air.Date, US.Viewership) %>% 
  arrange(desc(US.Viewership)) %>% 
  slice(1)

# Displays the date and the highest US viewership total (in millions)
View(highest_viewership_day)

# ==============================================================================
#                         LOWEST US VIEWERSHIP DATE  
# ============================================================================== 

# Option #1  

# Identifies the date of the lowest US viewership 
lowest_viewership_date <- shark$Original.Air.Date[which.min(shark$US.Viewership)]

# Displays the date the show achieved its lowest US viewership
View(lowest_viewership_date)

# Option #2

# Identifies the date along with the lowest US viewership total
lowest_viewership_day <- shark %>% 
  select(Original.Air.Date, US.Viewership) %>% 
  arrange(US.Viewership) %>% 
  slice(1)

# Displays the date and the lowest US viewership total (in millions)
View(lowest_viewership_day)

# ==============================================================================
#                       AVERAGE US VIEWERSHIP PER SEASON  
# ==============================================================================

# Calculates the average US viewership (in millions) per season
avg_viewership_per_season <- shark %>%
  select(Season.Number, US.Viewership) %>% 
  group_by(Season.Number) %>% 
  summarize(Avg_Viewership_M = round(mean(US.Viewership),2))
  

# Displays the average viewership per season
View(avg_viewership_per_season)

# ==============================================================================
#                               -  PLOT -
#                       AVG_VIEWERSHIP_PER_SEASON
# ==============================================================================

# Creates a column chart to visualize average viewership per season
fig_1 <- avg_viewership_per_season %>% 
  ggplot(aes(x = Season.Number, y = Avg_Viewership_M))+
  geom_col(fill = "blue4") +
  geom_text(aes(label = Avg_Viewership_M), vjust = -0.5, hjust = 0.5)+
  labs(title = "AVERAGE US VIEWERSHIP", x = "Season_Number", 
       y = "Avg_Viewership_M") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust =1)) +
  scale_x_continuous(breaks = seq(min(avg_viewership_per_season$Season.Number), 
                                  max(avg_viewership_per_season$Season.Number), 
                                  by = 1))

# Displays all named colors available in R's color palette
colors()

# Displays the column chart visualizing average viewership per season
fig_1

# ==============================================================================
#                           MOST-WATCHED SEASON
# ==============================================================================

# Identifies the season with the highest average viewership
top_season <- avg_viewership_per_season %>% 
  arrange(desc(Avg_Viewership_M)) %>% 
  slice(1)

# Displays the season with the highest average viewership
View(top_season)

# ==============================================================================
#                          LEAST-WATCHED SEASON
# ==============================================================================

# Identifies the season with the lowest average viewership
lowest_season <- avg_viewership_per_season %>% 
  arrange(Avg_Viewership_M) %>% 
  slice(1)

# Displays the season with the lowest average viewership
View(lowest_season)

# ==============================================================================
#                   AVERAGE US VIEWERSHIP OVER 15 SEASONS  
# ==============================================================================

# Computes the overall average viewership (in millions) across all seasons
overall_season_average <- round(mean(avg_viewership_per_season$Avg_Viewership_M)
                                ,2)

# Displays the overall 15-season average viewership
View(overall_season_average)

# ==============================================================================
#                                 SECTION 3
#                               PITCH ANALYSIS
# ==============================================================================
 
# ==============================================================================
#                               TOTAL PITCHES
# ==============================================================================

# Option #1

# Counts the total number of pitches using n(), and extracts the summary result 
# as a standalone integer with pull()
total_pitches <- shark %>% 
  select(Pitch.Number) %>% 
  summarize(Total = n()) %>% 
  pull(Total)

# Displays the total number of pitches 
View(total_pitches)

# Option #2

# Counts the total number of pitches using the length function 
length(shark$Pitch.Number)

# ==============================================================================
#                   IDENTIFYING AND REPLACING EMPTY FIELDS  
# ==============================================================================
# NOTE: The "Pitchers.Gender" column has 7 rows with missing values 

# Computes total pitches by gender 
total_pitches_by_gender <- shark %>% 
  group_by(Pitchers.Gender) %>% 
  summarize(Total = n())

# Displays the total pitches by gender, including 7 rows with missing values 
View(total_pitches_by_gender)

# Locates the row numbers in the "Pitchers.Gender" column with missing values

# Option 1
missing_genders <- which(shark$Pitchers.Gender == "")

View(missing_genders)

# Option 2
missing_genders2 <- shark %>% 
  filter(Pitchers.Gender == "")

View(missing_genders2)

# Accesses the specific indices in column 11, retrieving the names of the 
# entrepreneurs, and displaying the results to the console
shark[c(995,999,1001,1005,1071,1100,1122),11]

# Updates the specific indices in column 10 to input the value "Male"
shark[c(995, 999, 1122), 10] <- "Male"

# Updates the specific indices in column 10 to input the value "Mixed Team"
shark[c(1001, 1005, 1071, 1100), 10] <- "Mixed Team"

# Verifies gender corrections for the specific rows in the "shark" DataFrame
test_gender_correction <- shark %>% 
  select(Pitchers.Gender) %>% 
  slice(c(995,999,1001,1005,1071,1100,1122))

# Displays the corrected gender data for verification
View(test_gender_correction)

# Summarizes total pitches by gender from the updated "shark" DataFrame
total_pitches_by_gender_corrected <- shark %>% 
  group_by(Pitchers.Gender) %>% 
  summarize(Total = n()) %>% 
  arrange(desc(Total))
  
# Displays the total pitches by gender 
View(total_pitches_by_gender_corrected)

# ==============================================================================
#                         PERCENT OF PITCHES BY GENDER  
# ==============================================================================

# Adds a new column with the percentage of total pitches by gender
pitch_percentage_by_gender <- total_pitches_by_gender_corrected %>% 
  mutate(Percent = paste0(round((Total / sum(Total) * 100),1),"%")) %>% 
  arrange(desc(Percent))

# Displays total pitches and their percentages by gender
View(pitch_percentage_by_gender)

# ==============================================================================
#                                 - PLOT -
#                             PITCHES BY GENDER
# ==============================================================================

# Creates a pie chart illustrating pitch percentages by gender 
fig_2 <- plot_ly(pitch_percentage_by_gender, labels = ~Pitchers.Gender, 
        values = ~Total, type = "pie") %>% 
        layout(title = list(text = "PITCH BY GENDER", x = 0))

# Displays the pie chart illustrating total pitches and percentages by gender
fig_2

# ==============================================================================
#                          TOTAL SUCCESSFUL PITCHES   
# ==============================================================================

# Counts total approved pitches 
pitches_approved <- shark %>% 
  filter(Got.Deal == 1) %>% 
  summarize(Approved = n()) %>% 
  pull(Approved)

# Displays the total number of approved pitches
View(pitches_approved)

# ==============================================================================
#                         TOTAL UNSUCCESSFUL PITCHES  
# ==============================================================================

# Counts total denied pitches
pitches_denied <- shark %>% 
  filter(Got.Deal == 0) %>% 
  summarize(Denied = n())
  
# Displays the total number of denied pitches
View(pitches_denied)

# ==============================================================================
#                               SUCCESS RATE   
# ==============================================================================

# Summarizes total pitches and calculates percentages categorized by approvals
# and denials 
success_rate <- shark %>% 
  group_by(Got.Deal) %>% 
  summarize(Total = n()) %>% 
  mutate(Percent = paste0(round((Total / total_pitches * 100),1),"%")) %>% 
  arrange(desc(Percent))

# Displays a summary of total pitches and their percentages by approvals & denials
View(success_rate)

# ==============================================================================
#                                 - PLOT -
#                               SUCCESS RATE   
# ==============================================================================

# Creates a donut chart illustrating success rates of pitches
fig_3 <- plot_ly(success_rate, labels = ~Got.Deal, values = ~Total, 
                 type = 'pie', hole = 0.5) %>%  
                 layout(title = list(text = "SUCCESS RATE", x = 0))

# Displays the donut chart representing success rates of pitches
fig_3

# ==============================================================================
#                          TOTAL PITCHES BY INDUSTRY
# ==============================================================================

# Summarizes total pitches by industry
total_pitches_by_industry <-shark %>%  
  group_by(Industry) %>% 
  summarize(Total = n()) %>% 
  arrange(desc(Total)) 

# Displays a summary of total pitches by industry
View(total_pitches_by_industry)

# ==============================================================================
#                                 -  PLOT -
#                          TOTAL PITCHES BY INDUSTRY    
# ==============================================================================

# Creates a column chart illustrating total pitches by industry
fig_4 <- total_pitches_by_industry %>% 
  ggplot(aes(x = reorder(Industry, -Total), y = Total, fill = Industry))+
  geom_col() +
  geom_text(aes(label = Total), vjust = -0.5, hjust = 0.5)+
  labs(title = "TOTAL PITCHES BY INDUSTRY", x = "Industry", y = "Total Pitches") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))
  
# Displays the column chart visualizing total pitches by industry
fig_4  

# ==============================================================================
#                       INDUSTRY SHARE OF TOTAL PITCHES  
# ==============================================================================

# Calculates and adds percentages to total pitches by industry
percent_of_total_pitches_per_industry <- total_pitches_by_industry %>% 
  mutate(Percent = paste0(round((Total/total_pitches * 100)),"%")) %>% 
  select(Industry, Percent)

# Displays total pitches and their percentages by industry 
View(percent_of_total_pitches_per_industry)

# ==============================================================================
#                      TOTAL APPROVED PITCHES BY INDUSTRY
# ==============================================================================

# Summarizes approved pitches by industry
total_approved_pitches_per_industry <-shark %>% 
  filter(Got.Deal == 1) %>% 
  group_by(Industry) %>% 
  summarize(Total = n()) %>% 
  arrange(desc(Total)) 

# Displays a summary of total approved pitches by industry
View(total_approved_pitches_per_industry)

# ==============================================================================
#                                 -  PLOT -
#                      TOTAL APPROVED PITCHES BY INDUSTRY  
# ==============================================================================

# Creates a column chart illustrating total approved pitches by industry
fig_5 <- total_approved_pitches_per_industry %>% 
  ggplot(aes(x = reorder(Industry, -Total), y = Total, fill = Industry))+
  geom_col()+
  geom_text(aes(label = Total), vjust = -0.5, hjust = 0.5)+
  labs(title = "APPROVED PITCHES BY INDUSTRY", x = "Industry", 
       y = "Approved Pitches")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))

# Displays the column chart visualizing total approved pitches by industry
fig_5

# ==============================================================================
#                        APPROVAL RATE BY INDUSTRY  
# ==============================================================================

# Joins two DataFrames to calculate success rates by industry
percentage_success_rate_per_industry <- total_pitches_by_industry %>% 
  left_join(total_approved_pitches_per_industry, by="Industry") %>% 
  rename(Total_Pitches = Total.x, Approved_Pitches = Total.y) %>% 
  mutate(Percent = round(Approved_Pitches / Total_Pitches *100, 0)) %>% 
  arrange(desc(Percent))

# NOTE: The values in the column "Percent" are being left as raw data for
# plotting purposes

# Displays total pitches delivered, total pitches approved, & success percentages
View(percentage_success_rate_per_industry)


# ==============================================================================
#                                 - PLOT - 
#                              APPROVAL RATES   
# ==============================================================================

# Computes the average success percentage across industries
average <- percentage_success_rate_per_industry %>% 
  select(Percent) %>% 
  summarize(avg = round(mean(Percent))) %>% 
  pull(avg)

# Displays the average success percentage across industries
View(average)

# Creates a column chart illustrating success rates across the industry sectors
fig_6 <- percentage_success_rate_per_industry %>% 
  ggplot(aes(x = reorder(Industry, -Percent), y = Percent))+
  geom_col(fill = 'lightblue2')+
  geom_text(aes(label = Percent), vjust = -.05, hjust = 0.5)+
  geom_hline(yintercept = average, linetype = "solid", color ="red")+
  annotate("text", x=15, y=average +2, label=paste0("Average: ",average,"%"))+
  labs(title = "APPROVAL RATE BY INDUSTRY", x = "Industry", 
       y = "Approved_Pitches (%)")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing pitch success rates by industry
fig_6

# ==============================================================================
#                                SECTION 4
#                           INVESTMENT ANALYSIS
# ==============================================================================

# ==============================================================================
#                          TOTAL AMOUNT INVESTED   
# ==============================================================================
 
# Computes the total amount invested on approved deals 
total_invested <- shark %>% 
  filter(Got.Deal==1) %>% 
  summarize(Total = sum(Total.Deal.Amount)) %>% 
  pull(Total)

# Note: Keeping the value as raw numeric data for future use in other calculations

# Displays the cumulative total amount invested in approved deals by the sharks
View(total_invested)

# ==============================================================================
#                  TOTAL AMOUNT INVESTED -- TOP 3 INDUSTRIES 
# ==============================================================================

# Computes the total amount invested in the top 3 industries
total_invested_on_top_3_industries <- shark %>% 
  filter(Industry %in% c("Food and Beverage", "Lifestyle/Home", "Fashion/Beauty")) %>% 
  summarize(Total = sum(Total.Deal.Amount, na.rm = TRUE)) %>% 
  pull(Total)

# Formats the total investment value with "$" and "," for better readability
total_invested_on_top_3_industries <- dollar(total_invested_on_top_3_industries)

# Displays the total amount invested in the top 3 industries
View(total_invested_on_top_3_industries)

# ==============================================================================
#                     TOTAL INVESTMENTS ACROSS INDUSTRIES
# ==============================================================================

# Calculates total investment by industry
total_investment_per_industry <- shark %>% 
  select(Industry, Total.Deal.Amount) %>% 
  group_by(Industry) %>% 
  summarize(Total_Investment = sum(Total.Deal.Amount, na.rm = TRUE)) %>% 
  arrange(desc(Total_Investment)) 

# Note: The values in the column "Total_Investment" are being left as raw data

# Displays total investment by industry
View(total_investment_per_industry)

# ==============================================================================
#                                 -  PLOT - 
#                    TOTAL INVESTMENTS ACROSS INDUSTRIES
# ==============================================================================

# Creates a horizontal column chart (bar) showing total investment by industry
fig_7 <- total_investment_per_industry %>% 
  ggplot(aes(x = Total_Investment, y = reorder(Industry, 
                                                     -Total_Investment))) + 
  geom_col(fill='blue') + 
  geom_text(aes(label = scales::number(Total_Investment, scale = 1e-6, 
                                       suffix = "M", 
                                       accuracy = 0.01)), 
                                       hjust = -0.1) + 
  labs(title = "TOTAL INVESTMENTS PER INDUSTRY SECTOR", x = "Total Investment", 
       y = "Industry") + 
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M"),
                                           expand = c(0, 0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing total investment by industry
fig_7

# ==============================================================================                               
#                    AVERAGE INVESTMENTS ACROSS INDUSTRIES
# ==============================================================================

# Joins total investment per industry and total approved pitches per industry 
avg_invested_per_industry <- merge(total_investment_per_industry, 
                               total_approved_pitches_per_industry)

# Enhances data presentation to ensure clarity while providing actionable insights
# into the average investment per deal
avg_invested_per_industry <- avg_invested_per_industry %>% 
  rename(Approved_Deals = Total) %>% 
  mutate(Average_Deal_Amount = format(Total_Investment / Approved_Deals, 
                                      scientific = FALSE)) %>% 
  arrange(desc(Average_Deal_Amount)) 

# Transforms the columns with "$" and "," for clarity and readability
avg_invested_per_industry <- avg_invested_per_industry %>%
  mutate(Average_Deal_Amount = dollar(as.numeric(Average_Deal_Amount)),
    Total_Investment = dollar(as.numeric(Total_Investment)))
  
# Displays total average investments across industry sectors
View(avg_invested_per_industry)

# ==============================================================================
#                         HIGHEST INVESTMENT AMOUNT
# ==============================================================================

# Option #1

# Highlights the largest approved deal amount 
highest_deal_amount1 <- shark %>% 
  filter(Got.Deal==1) %>% 
  summarize(Highest_Deal = format(max(Total.Deal.Amount), scientific=FALSE))

# Displays the largest investment made on a single approved deal  
View(highest_deal_amount1)

# Option #2

# Highlights the largest approved deal amount
highest_deal_amount2 <- max(shark$Total.Deal.Amount, na.rm = TRUE)

# Displays the largest investment made on a single approved deal 
View(highest_deal_amount2)

# Option #3

# Highlights the largest approved deal amount
highest_deal_amount3 <- shark %>% 
  arrange(desc(Total.Deal.Amount)) %>% 
  slice(1) %>% 
  mutate(Total.Deal.Amount = format(Total.Deal.Amount, scientific = FALSE))

# Displays the largest investment made on a single approved deal 
View (highest_deal_amount3)

# ==============================================================================
#                      TOTAL FAVORABLE INVESTMENT DEALS  
# ==============================================================================

# Note: The perception of whether a deal is favorable or unfavorable is inherently 
# subjective, as it depends on the individual entrepreneur's goals and expectations. 
# For the purpose of this analysis, however, the focus is to evaluate whether 
# entrepreneurs with approved deals received investments that aligned with their 
# initial requests. Values less than or equal to 0 will indicate a favorable 
# outcome, whereas values greater than 0 will indicate an unfavorable outcome.

# Analyzes approved deals to highlight differences between original asking amounts
# and final deal amounts
total_deals <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Original.Ask.Amount, Total.Deal.Amount) %>% 
  mutate(gain_loss = Original.Ask.Amount - Total.Deal.Amount)

# Identifies the total number of favorable deals, where participants secured
# their requested or higher investment amounts
total_favorable_investments <- total_deals %>% 
  filter(gain_loss <= 0) %>% 
  summarize(Total = n())

# Displays the count of favorable investment deals for the participants
View(total_favorable_investments)

# ==============================================================================
#                  PERCENTAGE OF FAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the percentage of favorable deals to the entrepreneurs
favorable_investment_pct <- total_favorable_investments %>% 
summarize(Percent = round(Total / nrow(total_deals) * 100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
favorable_investment_pct$Percent <- paste0(favorable_investment_pct$Percent,"%")

# Displays the percentage of favorable deals in favor of the participants
View(favorable_investment_pct)

# ==============================================================================
#                    TOTAL UNFAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the total number of unfavorable deals, where participants received
# less than their asking investment amount
total_unfavorable_investments <- total_deals %>% 
  filter(gain_loss > 0) %>% 
  summarize(Total = n()) 

# Displays the count of unfavorable investment deals for the participants
View(total_unfavorable_investments)

# ==============================================================================
#                PERCENTAGE OF UNFAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the percentage of unfavorable deals to the entrepreneurs
unfavorable_investment_pct <- total_unfavorable_investments %>% 
  summarize(Percent = round(Total / nrow(total_deals) * 100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
unfavorable_investment_pct$Percent <- paste0(unfavorable_investment_pct$Percent,"%")

# Displays the percentage of unfavorable investment deals for entrepreneurs
View(unfavorable_investment_pct)

# ==============================================================================
#                                SECTION 5
#                             EQUITY ANALYSIS
# ==============================================================================

# ==============================================================================
#                           TOTAL AVERAGE EQUITY
# ==============================================================================

# Calculates the average equity surrendered by participants across 824 approved 
# deals
total_average_equity <- shark %>% 
  select(Total.Deal.Equity) %>% 
  summarize(Avg = round(mean(Total.Deal.Equity, na.rm = TRUE))) 

# Note: Maintaining the "avg" value as raw data for plotting purposes

# Displays the average equity surrendered by participants over 824 approved deals
View(total_average_equity)

# ==============================================================================
#                 AVERAGE EQUITY SURRENDERED ACROSS INDUSTRIES
# ==============================================================================

# Calculates the average equity surrendered by participants across all industries
avg_equity_per_industry <- shark %>% 
  group_by(Industry) %>% 
  summarize(avg_equity_surrendered = round(mean(Total.Deal.Equity, na.rm = TRUE))) %>% 
  arrange(desc(avg_equity_surrendered)) 

# Note: Maintaining the "avg" values as raw data for plotting purposes

# Displays the average equity surrendered by participants across all industries
View(avg_equity_per_industry)

# ==============================================================================
#                                  - PLOT - 
#                 AVERAGE EQUITY SURRENDERED ACROSS INDUSTRIES
# ==============================================================================

# Creates a column chart visualizing average equity surrendered by participants 
# across all industries
fig_8 <- avg_equity_per_industry %>% 
  ggplot(aes(x = reorder(Industry, -avg_equity_surrendered), 
             y = avg_equity_surrendered)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = avg_equity_surrendered, vjust = -0.5, hjust = 0.5)) +
  geom_hline(yintercept = total_average_equity$Avg, linetype = "solid", 
             color = "red") +
  annotate("text", x = 15, y = total_average_equity$Avg + 2, 
           label = paste0("Average Equity: ", total_average_equity$Avg, "%")) +
  labs(title = "AVERAGE EQUITY SURRENDERED PER INDUSTRY", x = "INDUSTRY", 
       y = "PERCENT EQUITY (%)") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing the average equity surrendered by participants 
# across industries. Although the Automotive and Travel industries stood out in 
# earlier analyses as advantageous for participants, deals in these sectors tend 
# to require entrepreneurs to surrender higher equity stakes.
fig_8

# ==============================================================================
#                       TOTAL FAVORABLE EQUITY DEALS
# ==============================================================================

# For the purpose of this analysis, the focus is to evaluate whether 
# entrepreneurs with approved deals relinquished more equity than expected. 
# Values greater than or equal to 0 indicate a favorable outcome, whereas
# values less than 0 indicate an unfavorable outcome.

# Analyzes the approved deals to highlight differences between the original 
# offered equity and the final deal equity surrendered
total_equity <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Original.Offered.Equity, Total.Deal.Equity) %>% 
  mutate(gain_loss = Original.Offered.Equity - Total.Deal.Equity) 

# Counts the number of equity deals deemed favorable, where participants kept 
# their original share or gave up less equity than initially proposed
total_favorable_equity_deals <- total_equity %>% 
  filter(gain_loss >= 0) %>% 
  summarize(Total = n())

# Displays the count of favorable equity deals for the participants
View(total_favorable_equity_deals)

# ==============================================================================
#                    PERCENTAGE OF FAVORABLE EQUITY DEALS
# ==============================================================================

# Calculates the percentage of favorable equity deals 
favorable_equity_pct <- total_favorable_equity_deals %>% 
  summarize(Percent = round(Total / nrow(total_equity) * 100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
favorable_equity_pct$Percent <- paste0(favorable_equity_pct$Percent,"%")

# Displays the percentage of favorable equity deals for the participants
View(favorable_equity_pct)

# ==============================================================================
#                       TOTAL UNFAVORABLE EQUITY DEALS
# ==============================================================================

# Counts unfavorable equity deals where participants surrendered more than their 
# original offered equity
total_unfavorable_equity_deals <- total_equity %>% 
  filter(gain_loss < 0) %>% 
  summarize(Total = n())

# Displays the count of unfavorable equity deals for the participants
View(total_unfavorable_equity_deals)

# ==============================================================================
#                   PERCENTAGE OF UNFAVORABLE EQUITY DEALS
# ==============================================================================

# Calculates the percentage of unfavorable equity deals to the entrepreneurs
unfavorable_equity_pct <- total_unfavorable_equity_deals %>% 
  summarize(Percent = round(Total / nrow(total_equity) * 100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
unfavorable_equity_pct$Percent <- paste0(unfavorable_equity_pct$Percent,"%")

# Displays the percentage of unfavorable equity deals to the participants
View(unfavorable_equity_pct)


# ==============================================================================
#                                 SECTION 6
#                            VALUATION ANALYSIS
# ==============================================================================

# The objective of this section is to compare valuations between approved and 
# denied deals. By analyzing the differences in these valuations, the aim is to 
# determine whether discrepancies or potential misalignments in company valuations 
# influenced the decision-making process of the sharks.

# ==============================================================================
#                           AVERAGE DEAL VALUATION
# ==============================================================================

# Calculates the average company valuation as determined by the sharks
avg_valuation_sharks <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Deal.Valuation) %>% 
  summarize(Average = mean(Deal.Valuation)) %>% 
  pull(Average)

# Note: Retaining the average value as raw data to enable further computation

# Displays the average valuation   
View(avg_valuation_sharks)

# ==============================================================================
#                     AVERAGE VALUATION OF DENIED PITCHES
# ==============================================================================

# Calculates the average valuation as determined by participants of denied pitches
avg_valuation_denied <- shark %>% 
  filter(Got.Deal == 0) %>% 
  select(Valuation.Requested) %>% 
  summarize(Average = mean(Valuation.Requested)) 

# Formats the average valuation as currency
avg_valuation_denied$Average <- dollar(avg_valuation_denied$Average)

# Displays the average valuation of denied deals
View(avg_valuation_denied)

# ==============================================================================
#                TOTAL PITCHES EXCEEDING SHARK AVERAGE VALUATION 
# ==============================================================================

# Determines the count of denied pitches where the valuation presented exceeded
# the sharks' average
total_pitches_denied_exceeding_shark_valuation <- shark %>% 
  filter(Got.Deal == 0, Valuation.Requested > avg_valuation_sharks ) %>% 
  summarize(Total = n())

# Displays the total count of denied pitches where the requested valuation 
# exceeded the sharks' average valuation
View(total_pitches_denied_exceeding_shark_valuation)

# ==============================================================================
#                      AVERAGE VALUATION OF APPROVED PITCHES
# ==============================================================================

# Calculates the average valuation as determined by participants of approved pitches
avg_valuation_approved <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Valuation.Requested) %>% 
  summarize(Average = mean(Valuation.Requested)) 

# Formats the average valuation as currency
avg_valuation_approved$Average <- dollar(avg_valuation_approved$Average)

# Displays the average valuation of approved deals 
View(avg_valuation_approved)

# ==============================================================================
#                TOTAL PITCHES EXCEEDING SHARK AVERAGE VALUATION
# ==============================================================================

# Determines the count of approved pitches where the valuation presented exceeded
# the sharks' average
total_pitches_approved_exceeding_shark_valuation <- shark %>% 
  filter(Got.Deal == 1 & Valuation.Requested > avg_valuation_by_sharks) %>% 
  summarize(Total = n())

# Displays the total count of approved pitches where the requested valuation 
# exceeded the sharks' average valuation 
View(total_pitches_approved_exceeding_shark_valuation)

# ==============================================================================
#                                   SECTION 7
#                  SHARK ANALYSIS - Investments, Deals, Equity
# ==============================================================================

# ==============================================================================
#                                TOTAL INVESTMENTS    
# ==============================================================================

# Selects the "Industry" column along with the "Investment amount" for each shark   
investment_per_shark <- shark %>% 
  select(Industry, Barbara.Corcoran.Investment.Amount, Mark.Cuban.Investment.Amount,
         Lori.Greiner.Investment.Amount, Robert.Herjavec.Investment.Amount,
         Daymond.John.Investment.Amount, Kevin.O.Leary.Investment.Amount,
         Guest.Investment.Amount) 
  
# Displays individual investment totals by industry for each shark
View(investment_per_shark)

# Summarizes total investments by shark
total_investment_per_shark <- investment_per_shark %>% 
  summarize(`Barbara Corcoran` = sum(Barbara.Corcoran.Investment.Amount, na.rm = TRUE),
            `Mark Cuban` = sum(Mark.Cuban.Investment.Amount, na.rm = TRUE),
            `Lori Greiner` = sum(Lori.Greiner.Investment.Amount, na.rm = TRUE),
            `Robert Herjavec` = sum(Robert.Herjavec.Investment.Amount, na.rm = TRUE),
            `Daymond John` = sum(Daymond.John.Investment.Amount, na.rm = TRUE),
            `Kevin O Leary` = sum(Kevin.O.Leary.Investment.Amount, na.rm = TRUE),
             Guest = sum(Guest.Investment.Amount, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(), names_to = "Shark", values_to = "Amount_Invested") %>% 
  arrange(desc(Amount_Invested)) %>% 
  mutate(Percent = round(Amount_Invested / total_invested * 100, 1))

# Note: Retaining "Amount_Invested" and "Percent" as raw data for plotting

# Displays the summarized data of total investments and percentages for each shark
View(total_investment_per_shark) 

# ==============================================================================
#                                  - PLOT - 
#                             TOTAL INVESTMENTS     
# ==============================================================================

# Creates a column chart to visualize the total investments per shark
fig_9 <- total_investment_per_shark %>%  
  ggplot(aes(x= reorder(Shark, -Amount_Invested), y=Amount_Invested))+
  geom_col(fill="navy")+
  geom_text(aes(label = scales::dollar(Amount_Invested, scale = 1e-6, 
                                       suffix = "M", accuracy = 0.1)), 
            hjust = 0.5, vjust = 2, color="white")+
  labs(title = "TOTAL INVESTMENT PER SHARK", x = "Shark", y = "Amount Invested (M)")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix ="M", 
                                                   accuracy = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Displays the column chart visualizing the total investments per shark
fig_9

# ==============================================================================
#                                -  PLOT - 
#                             PERCENT INVESTED    
# ==============================================================================

# Creates a pie chart to visualize the percentages invested per shark
fig_10 <- plot_ly(total_investment_per_shark, labels = ~Shark, 
        values = ~Amount_Invested, type = 'pie') %>% 
  layout(title = list(text = "PERCENT INVESTED PER SHARK", x = 0))

# Displays the pie chart visualizing the percentages invested per shark
fig_10

# ==============================================================================
#                   INVESTMENT DISTRIBUTION ACROSS INDUSTRIES
# ==============================================================================

# Summarizes investments by industry sector for each shark
industry_investment_per_shark <- investment_per_shark %>% 
  group_by(Industry) %>% 
  summarize(Mark_Cuban_Investment = 
              sum(Mark.Cuban.Investment.Amount, na.rm = TRUE),
            Mark_Cuban_Percent = 
              paste0(round(Mark_Cuban_Investment/62857667 * 100),"%"),
            Lori_Greiner_Investment = 
              sum(Lori.Greiner.Investment.Amount,na.rm = TRUE),
            Lori_Greiner_Percent = 
              paste0(round(Lori_Greiner_Investment/46492500 * 100),"%"),
            Robert_Herjavec_Investment = 
              sum(Robert.Herjavec.Investment.Amount, na.rm = TRUE),
            Robert_Herjavec_Percent = 
              paste0(round(Robert_Herjavec_Investment/36330666 *100),"%"),
            Kevin_O_Leary_Investment = 
              sum(Kevin.O.Leary.Investment.Amount, na.rm = TRUE),
            Kevin_O_Leary_Percent = 
              paste0(round(Kevin_O_Leary_Investment/30530833 *100),"%"),
            Guest_Investment = 
              sum(Guest.Investment.Amount, na.rm = TRUE),
            Guest_Percent = 
              paste0(round(Guest_Investment/25115833 * 100),"%"),
            Daymond_John_Investment = 
              sum(Daymond.John.Investment.Amount, na.rm = TRUE),
            Daymond_John_Percent = 
              paste0(round(Daymond_John_Investment/20965667 *100),"%"),
            Barbara_Corcoran_Investment = 
              sum(Barbara.Corcoran.Investment.Amount, na.rm = TRUE),
            Barbara_Corcoran_Percent = 
              paste0(round(Barbara_Corcoran_Investment/19490000 *100),"%"))

# Note: Retaining invested values as raw data for plotting

# Displays the total investments and percentage breakdown per industry for each shark
View(industry_investment_per_shark)

# ==============================================================================
#                                  - PLOT - 
#                  INVESTMENT DISTRIBUTIONS ACROSS INDUSTRIES
# ==============================================================================

# Reshapes the data to analyze individual shark investments by sector
pivoted_df <- industry_investment_per_shark %>% 
  pivot_longer(cols = where(is.numeric), names_to = "Shark", 
               values_to = "Investment")

# Generates faceted column charts showing how much each Shark invested across industry sectors
fig_11 <- pivoted_df %>% 
  ggplot(aes(x=Industry, y=Investment, fill = Industry))+
  geom_col()+
  facet_wrap(~Shark, scales = "free_y", ncol=2)+
  scale_y_continuous(labels=scales::number_format(scale = 1e-6, suffix = "M"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "SHARK INVESTMENTS ACROSS INDUSTRIES", x = "Industry", 
       y = "Investment Amount")

# Displays the faceted column charts visualizing total shark investments by 
# industry sector 
fig_11

# ==============================================================================
#                                 TOTAL DEALS 
# ==============================================================================

# Highlights total deals per shark, reshaping data for visualization
total_deals_per_shark <- investment_per_shark %>% 
  summarize(Barbara_Corcoran = sum(!is.na(Barbara.Corcoran.Investment.Amount)), 
            Mark_Cuban = sum(!is.na(Mark.Cuban.Investment.Amount)), 
            Lori_Greiner = sum(!is.na(Lori.Greiner.Investment.Amount)), 
            Robert_Herjavec = sum(!is.na(Robert.Herjavec.Investment.Amount)), 
            Daymond_John = sum(!is.na(Daymond.John.Investment.Amount)), 
            Kevin_O_Leary = sum(!is.na(Kevin.O.Leary.Investment.Amount)), 
            Guest = sum(!is.na(Guest.Investment.Amount))) %>% 
  pivot_longer(cols = everything(), names_to = "Shark", values_to = "Total_Deals") %>% 
  arrange(desc(Total_Deals)) 

View(total_deals_per_shark)

# Note:
# While there were 824 approved deals overall, the total number of individual 
# shark deal participations was 1,087. This difference reflects instances where 
# multiple sharks partnered on a single deal — meaning a single deal may be counted 
# once for the entrepreneur, but multiple times from the sharks’ perspective.

# Displays the combined total number of deals made by the sharks
sum(total_deals_per_shark$Total_Deals)

# ==============================================================================
#                                   - PLOT - 
#                                 TOTAL DEALS 
# ==============================================================================

# Creates a column chart to visualize the total number of deals made by each shark
fig_12 <- total_deals_per_shark %>% 
  ggplot(aes(x= reorder(Shark, -Total_Deals), y=Total_Deals))+
  geom_col(fill="blue") +
  geom_text(aes(label = Total_Deals), vjust = -0.5, hjust = 0.5)+
  labs(title = "Total Deals Per Shark", x= "Shark", y="Total Deals")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart showing the total number of deals made by each shark
fig_12

#===============================================================================
#                        TOTAL DEALS ACROSS INDUSTRIES 
# ==============================================================================

# Calculates the total number of deals made by each shark across all industries
total_deals_per_industry <- investment_per_shark %>% 
  group_by(Industry) %>% 
  summarize(Barbara_Corcoran_Deals = sum(!is.na(Barbara.Corcoran.Investment.Amount)),
            Mark_Cuban_Deals = sum(!is.na(Mark.Cuban.Investment.Amount)),
            Lori_Greiner_Deals = sum(!is.na(Lori.Greiner.Investment.Amount)),
            Robert_Herjavec_Deals = sum(!is.na(Robert.Herjavec.Investment.Amount)),
            Daymond_John_Deals = sum(!is.na(Daymond.John.Investment.Amount)),
            Kevin_O_Leary_Deals = sum(!is.na(Kevin.O.Leary.Investment.Amount)),
            Guest_Deals = sum(!is.na(Guest.Investment.Amount))) %>% 
  arrange(Industry)

# Displays the total number of deals across all industries
View(total_deals_per_industry)

# ==============================================================================
#                       PROBABILITY OF JOINT PARTICIPATION 
# ==============================================================================

# Calculates the frequency and percentage of approved deals based on the number 
# of sharks involved
number_of_sharks_involved <- shark %>% 
  filter(Got.Deal == 1) %>% 
  group_by(Number.of.Sharks.in.Deal) %>% 
  summarize(Total_Deals = n()) %>%
  mutate(Percent = paste0(round(Total_Deals / sum(Total_Deals) * 100, 1),"%")) %>% 
  arrange(desc(Percent))


# Displays the counts and percentages of deals involving one or more sharks
View(number_of_sharks_involved)

# ==============================================================================
#                                 -  PLOT - 
#                       PROBABILITY OF JOINT PARTICIPATION
# ==============================================================================

# Creates a scatter plot to visualize the relationship between the number of
# sharks involved and the total number of deals
fig_13 <- ggplot(number_of_sharks_involved, aes(x= Number.of.Sharks.in.Deal, 
                                               y= Total_Deals)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(title = "PROBABILITY MODEL: MULTI-SHARK PARTICIPATION", 
       x= "Number of Sharks in Deal", y= "Total Deals")

# Displays the scatter plot visualizing the number of sharks involved in total 
# deals
fig_13

# ==============================================================================
#                 TOTAL AVERAGE EQUITY OBTAINED BY THE SHARKS
# ==============================================================================

# Prepares the data for analysis to determine the total average equity obtained
equity_per_shark <- shark %>% 
  select(Industry, Barbara.Corcoran.Investment.Equity, 
         Mark.Cuban.Investment.Equity, Lori.Greiner.Investment.Equity,
         Robert.Herjavec.Investment.Equity, Daymond.John.Investment.Equity,
         Kevin.O.Leary.Investment.Equity, Guest.Investment.Equity) %>% 
  rename(Barbara_Corcoran = Barbara.Corcoran.Investment.Equity,
         Mark_Cuban = Mark.Cuban.Investment.Equity,
         Lori_Greiner = Lori.Greiner.Investment.Equity,
         Robert_Herjavec = Robert.Herjavec.Investment.Equity,
         Daymond_John = Daymond.John.Investment.Equity,
         Kevin_O_Leary = Kevin.O.Leary.Investment.Equity,
         Guest = Guest.Investment.Equity) 

# Calculates the total average equity 
total_avg_equity <- equity_per_shark %>%  
  pivot_longer(cols = -Industry, names_to = "Shark", values_to = "Avg_Equity") %>% 
  summarize(Average_Equity = round(mean(Avg_Equity, na.rm = TRUE)))

# Note:
# Although entrepreneurs surrendered an average equity stake of 24% to secure deals, 
# the average equity received per individual shark was 18%. This gap arises 
# because several deals involved multiple sharks, who collectively split the equity offered. 
# In such cases, the total equity surrendered by the entrepreneur was divided among 
# participating sharks — resulting in a lower average stake per shark.

# Displays the total average equity 
View(total_avg_equity)

# ==============================================================================
#                     AVERAGE EQUITY OBTAINED PER SHARK    
# ==============================================================================

# Calculates the total average equity obtained by each sharks across all industries
avg_equity_per_shark <- equity_per_shark %>% 
  summarize(Barbara_Corcoran = round(mean(Barbara_Corcoran, na.rm = TRUE)),
            Mark_Cuban = round(mean(Mark_Cuban, na.rm = TRUE)),
            Lori_Greiner = round(mean(Lori_Greiner, na.rm = TRUE)),
            Robert_Herjavec = round(mean(Robert_Herjavec, na.rm = TRUE)),
            Daymond_John = round(mean(Daymond_John, na.rm = TRUE)),
            Kevin_O_Leary = round(mean(Kevin_O_Leary, na.rm = TRUE)),
            Guest = round(mean(Guest, na.rm = TRUE))) %>% 
  pivot_longer(cols = everything(), names_to = "Shark", values_to = "Avg_Equity") %>% 
  arrange(desc(Avg_Equity))

# Displays the total average equity per shark
View(avg_equity_per_shark)

# ==============================================================================
#                                 -  PLOT - 
#                      AVERAGE EQUITY OBTAINED PER SHARK
# ==============================================================================

# Creates a column chart to visualize the average equity obtained by each shark
fig_14 <- avg_equity_per_shark %>% 
  ggplot(aes(x= reorder(Shark, -Avg_Equity), y=Avg_Equity))+
  geom_col(fill="green") +
  geom_text(aes(label = Avg_Equity), vjust = 1, hjust = 0.5)+
  labs(title = "AVERAGE EQUITY PER SHARK", x= "Shark", y="Average Equity (%)")+
  geom_hline(yintercept = Total_Avg_Equity$Average_Equity, linetype ="solid", 
             color ="red")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  annotate("text", x =6, y= Total_Avg_Equity$Average_Equity +1,
           label = paste0("Average Equity: ", Total_Avg_Equity$Average_Equity, "%"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Displays the column chart showing the average equity obtained by each shark
fig_14

# ==============================================================================
#            AVERAGE EQUITY SECURED BY EACH SHARK ACROSS INDUSTRIES
# ==============================================================================

# Calculates the total average equity obtained by sharks in each industry
avg_equity_per_industry <- equity_per_shark %>% 
  group_by(Industry) %>% 
  summarize(
    Barbara_Corcoran_Avg_Equity = ifelse(
      is.nan(mean(Barbara_Corcoran, na.rm = TRUE)),
      NA, 
      paste0(round(mean(Barbara_Corcoran, na.rm = TRUE)), "%")
    ),
    Mark_Cuban_Avg_Equity = ifelse(
      is.nan(mean(Mark_Cuban, na.rm = TRUE)), 
      NA, 
      paste0(round(mean(Mark_Cuban, na.rm = TRUE)), "%")
    ),
    Lori_Greiner_Avg_Equity = ifelse(
      is.nan(mean(Lori_Greiner, na.rm = TRUE)),
      NA,
      paste0(round(mean(Lori_Greiner, na.rm = TRUE)), "%")
    ),
    Robert_Herjavec_Avg_Equity = ifelse(
      is.nan(mean(Robert_Herjavec, na.rm = TRUE)),
      NA,
      paste0(round(mean(Robert_Herjavec, na.rm = TRUE)), "%")
    ),
    Daymond_John_Avg_Equity = ifelse(
      is.nan(mean(Daymond_John, na.rm = TRUE)),
      NA,
      paste0(round(mean(Daymond_John, na.rm = TRUE)), "%")
    ),
    Kevin_O_Leary_Avg_Equity = ifelse(
      is.nan(mean(Kevin_O_Leary, na.rm = TRUE)),
      NA,
      paste0(round(mean(Kevin_O_Leary, na.rm = TRUE)), "%")
    ),
    Guest_Avg_Equity = ifelse(
      is.nan(mean(Guest, na.rm=TRUE)),
      NA,
      paste0(round(mean(Guest, na.rm = TRUE)), "%")
    )
  )

# Displays the average equity obtained by each shark across all industries
View(avg_equity_per_industry)

# ==============================================================================
#                                   SECTION 8
#                          ALTERNATIVE DEAL STRUCTURES
# ==============================================================================
#                             ROYALTY DEAL ANALYSIS
#===============================================================================

# Option #1

# Identifies the total number of royalty deals and displays the result to the 
# console
sum(!is.na(shark$Royalty.Deal))


# Option #2 

# Identifies the total number of royalty-based deals
total_royalty_deals <- shark %>% 
  select(Royalty.Deal) %>%
  filter(Royalty.Deal == 1) %>% 
  summarize(Total = n())

# Displays the total count of deals approved with a royalty condition 
View(total_royalty_deals)
  
# ==============================================================================
#                              LOAN DEAL ANALYSIS                
# ==============================================================================

# Option #1

# Counts the total number of deals involving loans
sum(!is.na(shark$Loan))


# Option #2

# Retrieves the lowest and highest values present in the "Loan" column
min(shark$Loan, na.rm = TRUE)
max(shark$Loan, na.rm = TRUE)

# Counts the total number of deals involving loans
total_loan_deals<- shark %>% 
  filter(Loan >= 50000, Loan <= 2000000) %>% 
  summarize(Total = n())

# Displays the total count of deals approved with a loan condition 
View(total_loan_deals)


