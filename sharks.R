# ==============================================================================
#                     INSTALL PACKAGES AND LOAD LIBRARIES`  
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
#                               LOAD DATASET    
# ==============================================================================

# Loads the Shark dataset for analysis
shark <- read.csv("C:\\Users\\rafae\\OneDrive\\Desktop\\Datasets\\Shark Tank3.csv", 
                  header = TRUE, stringsAsFactors = FALSE)

# Displays the contents of the "shark" data frame
View(shark)

# ==============================================================================
#                             METADATA OVERVIEW
# ==============================================================================

# Summarizes the data frame "shark", showing structure and data types
glimpse(shark)

# ==============================================================================
#                            CHANGING DATA TYPE     
# ==============================================================================

# Converts date-related columns to enable accurate analysis of timelines
shark$Original.Air.Date <- as.Date(shark$Original.Air.Date, format="%d-%b-%y")
shark$Season.Start <- as.Date(shark$Season.Start, format="%d-%b-%y")
shark$Season.End <- as.Date(shark$Season.End, format = "%d-%b-%y")

# Ensures the date conversions were successful
sapply(shark[, c("Original.Air.Date", "Season.Start", "Season.End")], class)

# Displays the contents of the "shark" data frame
View(shark)

# ==============================================================================
#                             REMOVING COLUMNS    
# ==============================================================================

# Streamlines the data frame by removing non-essential columns
shark <- shark %>% 
  select(-c("Company.Website", "Multiple.Entrepreneurs",
                 "Deal.Has.Conditions", "Advisory.Shares.Equity", 
                 "Pitchers.Average.Age", "Pitchers.City", "Pitchers.State"))

# Displays the contents of the "shark" data frame
View(shark)

# ==============================================================================
#                                   SECTION 1   
#                                SEASONS ANALYSIS
# ==============================================================================

# ==============================================================================
#                                 TOTAL SEASONS 
# ==============================================================================

# Determines the total number of unique seasons
total_seasons <- n_distinct(shark$Season.Number)

# Displays the total number of seasons 
View(total_seasons)

# ==============================================================================   
#                               DURATION OF SEASONS   
# ==============================================================================

# Extracts unique "Season.End" dates and organizes them into a data frame
season_end_dates <- unique(shark$Season.End) 
season_end_dates <- as.data.frame(season_end_dates)

# Displays the unique season end dates 
View(season_end_dates)

# Extracts unique "Season.Start" dates and organizes them into a data frame
season_start_dates <- unique(shark$Season.Start)
season_start_dates <-as.data.frame(season_start_dates)

# Displays the unique season start dates  
View(season_start_dates)

# Computes the duration of each season in days
duration <- season_end_dates - season_start_dates 

# Renames the "season_end_dates" column to "Days" for a clearer representation
duration <- duration %>% 
  rename(Days = season_end_dates)
  
# Converts the "Days" variable to numeric format to remove the character "days"
duration$Days <- as.numeric(duration$Days)

# Displays the duration of each season in days 
View(duration)

# Adds a "Months" column by converting days to months
duration$Months = round(duration$Days/30)

# Displays the duration of each season in days and months 
View(duration)

# ==============================================================================
#                         AVERAGE DURATION OF SEASONS  
# ==============================================================================

# Creates a data frame to summarize average season duration in days and months
average_duration <- data.frame(avg_in_days = round(mean(duration$Days)), 
                               avg_in_months = round(mean(duration$Months)))

# Displays the average season duration in days and months
View(average_duration)

# ==============================================================================
#                         TOTAL EPISODES PER SEASON
# ==============================================================================

# Determines the total number of episodes per season 
episodes_per_season <- shark %>%
  group_by(Season.Number) %>%
  summarise(Total_Episodes = n_distinct(Episode.Number)) %>% 
  select(Total_Episodes)

# Displays the total number of episodes per season
View(episodes_per_season)

# ==============================================================================
#                               TOTAL EPISODES    
# ==============================================================================

# Computes the total number of episodes aired
total_episodes <- sum(episodes_per_season$Total_Episodes)

# Displays the total number of episodes aired over 15 seasons
View(total_episodes)

# ==============================================================================
#                             MERGE DATA FRAMES
# ==============================================================================

# Merges data frames to create a comprehensive overview of season duration
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
  

# Displays the contents of the data frame
View(avg_viewership_per_season)

# ==============================================================================
#                       PLOT AVG_VIEWERSHIP_PER_SEASON
# ==============================================================================

# Creates a column chart to visualize average viewership per season
fig_1 <- avg_viewership_per_season %>% 
  ggplot(aes(x = Season.Number, y = Avg_Viewership_M, fill = Season.Number))+
  geom_col() +
  geom_text(aes(label = Avg_Viewership_M), vjust = -0.5, hjust = 0.5)+
  labs(title = "AVERAGE US VIEWERSHIP", x = "Season_Number", 
       y = "Avg_Viewership_M") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust =1)) +
  scale_x_continuous(breaks = seq(min(avg_viewership_per_season$Season.Number), 
                                  max(avg_viewership_per_season$Season.Number), 
                                  by = 1))

# Displays the column chart visualizing average viewership per season
fig_1

# ==============================================================================
#             SEASON NUMBER WITH THE HIGHEST AVERAGE VIEWERSHIP
# ==============================================================================

# Identifies the season with the highest average viewership
top_season <- avg_viewership_per_season %>% 
  arrange(desc(Avg_Viewership_M)) %>% 
  slice(1)

# Displays the season with the highest average viewership
View(top_season)

# ==============================================================================
#             SEASON NUMBER WITH THE LOWEST AVERAGE VIEWERSHIP
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

# Calculates the total number of pitches
total_pitches <- shark %>% 
  select(Got.Deal) %>% 
  summarize(Total = n()) %>% 
  pull(Total)

# Displays the total number of pitches (both approved and denied)
View(total_pitches)

# Option #2

# Calculates the total number of pitches and displays the result to the console
shark %>% 
  select(Got.Deal) %>% 
  summarize(Total = length(Got.Deal))

# ==============================================================================
#                       HANDLING MISSING (NULL) VALUES  
# ==============================================================================
# NOTE: The "Pitchers.Gender" column has 7 rows with missing (Null) values 

# Computes total pitches by gender 
total_pitches_by_gender <- shark %>% 
  group_by(Pitchers.Gender) %>% 
  summarize(Total = n())

# Displays the total pitches by gender, including 7 rows with missing values 
View(total_pitches_by_gender)

# Locates rows in the "Pitchers.Gender" column with missing values
missing_genders <- which(shark$Pitchers.Gender == "")

# Displays the indices of rows with empty strings to the console
missing_genders

# Accesses the specific indices in column 11, retrieving the names of the 
# entrepreneurs, and displaying the results to the console
shark[c(995,999,1001,1005,1071,1100,1122),11]

# Updates the specific indices in column 10 to input the value "Male"
shark[c(995, 999, 1122), 10] <- "Male"

# Updates the specific indices in column 10 to input the value "Mixed Team"
shark[c(1001, 1005, 1071, 1100), 10] <- "Mixed Team"

# Verifies gender corrections for the specific rows in the "shark" data frame
test_gender_correction <- shark %>% 
  select(Pitchers.Gender) %>% 
  slice(c(995,999,1001,1005,1071,1100,1122))

# Displays the corrected gender data for verification
View(test_gender_correction)

# Summarizes total pitches by gender from the updated "shark" data frame
total_pitches_by_gender_corrected <- shark %>% 
  group_by(Pitchers.Gender) %>% 
  summarize(Total = n()) %>% 
  arrange(desc(Total))
  
# Displays the total pitches by gender 
View(total_pitches_by_gender_corrected)

# ==============================================================================
#                         PITCH PERCENTAGES BY GENDER  
# ==============================================================================

# Adds a new column with the percentage of total pitches by gender
pitch_percentage_by_gender <- total_pitches_by_gender_corrected %>% 
  mutate(Percent = paste0(round((Total / nrow(shark) * 100),1),"%")) %>% 
  arrange(desc(Percent))

# Displays total pitches and their percentages by gender
View(pitch_percentage_by_gender)

# ==============================================================================
#                     PLOT PERCENTAGE OF PITCHES BY GENDER
# ==============================================================================

# Creates a pie chart illustrating pitch percentages by gender 
fig_2 <- plot_ly(pitch_percentage_by_gender, labels = ~Pitchers.Gender, 
        values = ~Total, type = "pie") %>% 
        layout(title = list(text = "PITCH BY GENDER", x = 0.5))

# Displays the pie chart illustrating total pitches and percentages by gender
fig_2

# ==============================================================================
#                           TOTAL SUCCESSFUL PITCHES   
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
  summarize(Totals_Approved_Denied = n()) %>% 
  mutate(Percent = paste0(round((Totals_Approved_Denied / total_pitches * 100),1)
                          ,"%")) %>% 
  arrange(desc(Percent))

# Displays a summary of total pitches and their percentages by approvals & denials
View(success_rate)

# ==============================================================================
#                             PLOT SUCCESS RATE   
# ==============================================================================

# Creates a donut chart illustrating success rates of pitches
fig_3 <- plot_ly(success_rate, labels = ~Got.Deal, values = ~Totals_Approved_Denied, 
                 type = 'pie', hole = 0.5) %>% 
          layout(title = "SUCCESS RATE")

# Displays the donut chart representing success rates of pitches
fig_3

# ==============================================================================
#                           TOTAL PITCHES BY INDUSTRY
# ==============================================================================

# Summarizes total pitches by industry
total_pitches_by_industry <-shark %>%  
  group_by(Industry) %>% 
  summarize(Total = n()) %>% 
  arrange(desc(Total)) 

# Displays a summary of total pitches by industry
View(total_pitches_by_industry)

# ==============================================================================
#                         PLOT TOTAL PITCHES BY INDUSTRY    
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
#                      PERCENT OF TOTAL PITCHES PER INDUSTRY  
# ==============================================================================

# Calculates and adds percentages to total pitches by industry
percent_of_total_pitches_per_industry <- total_pitches_by_industry %>% 
  mutate(Percent = paste0(round((Total/total_pitches * 100)),"%"))

# Displays total pitches and their percentages by industry 
View(percent_of_total_pitches_per_industry)

# ==============================================================================
#                     TOTAL APPROVED PITCHES BY INDUSTRY
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
#             PERCENTAGE OF TOTAL APPROVED PITCHES BY INDUSTRY
# ==============================================================================

# Calculates and adds percentages to total approved pitches by industry
percent_of_approved_pitches_per_industry <- total_approved_pitches_per_industry %>% 
  mutate(Percent = paste0(round(Total / pitches_approved *100),"%"))

# Displays total approved pitches and their percentages by industry 
View(percent_of_approved_pitches_per_industry)

# ==============================================================================
#                   PLOT TOTAL APPROVED PITCHES PER INDUSTRY  
# ==============================================================================

# Creates a column chart illustrating total approved pitches by industry
fig_5 <- total_approved_pitches_per_industry %>% 
  ggplot(aes(x = reorder(Industry,-Total), y = Total, fill = Industry))+
  geom_col()+
  geom_text(aes(label = Total), vjust = -0.5, hjust = 0.5)+
  labs(title = "APPROVED PITCHES BY INDUSTRY", x = "Industry", 
       y = "Approved Pitches")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust =1))

# Displays the column chart visualizing total approved pitches by industry
fig_5

# ==============================================================================
#                   PERCENTAGE SUCCESS RATE BY INDUSTRY  
# ==============================================================================

# Combines two data frames to calculate success rates by industry
percentage_success_rate_per_industry <- total_approved_pitches_per_industry %>% 
  left_join(total_pitches_by_industry, by="Industry") %>% 
  rename(Approved_Pitches = Total.x, Total_Pitches = Total.y) %>% 
  mutate(Percent_Success = round(Approved_Pitches / Total_Pitches *100,0)) %>% 
  arrange(desc(Percent_Success))

# NOTE: The values in the column "Percent_Success" are being left as raw data for
# plotting purposes

# Displays total pitches delivered, total pitches approved, & success percentages
View(percentage_success_rate_per_industry)

# ==============================================================================
#                 PLOT PERCENTAGE SUCCESS RATE BY INDUSTRY   
# ==============================================================================

# Computes the average success percentage across industries
average <- percentage_success_rate_per_industry %>% 
  select(Percent_Success) %>% 
  summarize(avg = round(mean(Percent_Success),2)) %>% 
  pull(avg)

# Displays the average success percentage across industries
View(average)

# Creates a column chart illustrating industry pitch success rates
fig_6 <- percentage_success_rate_per_industry %>% 
  ggplot(aes(x = reorder(Industry, -Percent_Success), y = Percent_Success, 
             fill= Percent_Success))+
  geom_col()+
  geom_text(aes(label = Percent_Success), vjust = -.05, hjust = 0.5)+
  geom_hline(yintercept = average, linetype = "solid", color ="red")+
  annotate("text", x=15, y=average +2, label=paste0("Average: ",average,"%"))+
  labs(title = "SUCCESS RATE BY INDUSTRY", x = "Industry", 
       y = "Approved_Pitches")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing pitch success rates by industry
fig_6

# ==============================================================================
#                                   SECTION 4
#                           INDUSTRY INVESTMENT ANALYSIS
# ==============================================================================

# ==============================================================================
#                     TOTAL AMOUNT INVESTED ON APPROVED DEALS   
# ==============================================================================
 
# Computes the total amount invested in approved deals 
total_invested <- shark %>% 
  filter(Got.Deal==1) %>% 
  summarize(Total = sum(Total.Deal.Amount)) %>% 
  pull(Total)

# Note: Keeping the value as raw numeric data for future use in other calculations

# Displays the cumulative total amount invested in approved deals by the sharks
View(total_invested)

# ==============================================================================
#                 TOTAL AMOUNT INVESTED IN THE TOP 3 INDUSTRIES 
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
#                     TOTAL INVESTMENTS BY INDUSTRY SECTOR
# ==============================================================================

# Calculates total investment by industry
total_investment_by_industry <- shark %>% 
  select(Industry, Total.Deal.Amount) %>% 
  group_by(Industry) %>% 
  summarize(Total_Shark_Investment = sum(Total.Deal.Amount, na.rm = TRUE)) %>% 
  arrange(desc(Total_Shark_Investment)) 

# Note: The values in the column "Total_Investment" are being left as raw data

# Displays total investment by industry
View(total_investment_by_industry)

# ==============================================================================
#                   PLOT TOTAL INVESTMENTS BY INDUSTRY SECTOR
# ==============================================================================

# Creates a horizontal column chart (bar) showing total investment by industry
fig_7 <- total_investment_by_industry %>% 
  ggplot(aes(x = Total_Shark_Investment, y = reorder(Industry, 
                                                     Total_Shark_Investment))) + 
  geom_col(fill='blue') + 
  geom_text(aes(label = scales::number(Total_Shark_Investment, scale = 1e-6, 
                                       suffix = "M", 
                                       accuracy = 0.01)), 
            hjust = -0.1) + 
  labs(title = "TOTAL INVESTED PER INDUSTRY", x = "Total Investment", 
       y = "Industry") + 
  scale_x_continuous(labels = scales::comma, expand = c(0, 0)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing total investment by industry
fig_7

# ==============================================================================                               
#               AVERAGE AMOUNT INVESTED PER DEAL BY INDUSTRY SECTOR
# ==============================================================================

# Joins total investments and approved deals by industry into a single data frame
avg_invested_per_deal <- merge(total_investment_by_industry, 
                               total_approved_pitches_per_industry)

# Enhances data presentation to ensure clarity while providing actionable insights
# into the average investment per deal
avg_invested_per_deal <- avg_invested_per_deal %>% 
  rename(Approved_Deals = Total) %>% 
  mutate(Average_Deal_Amount = format(Total_Shark_Investment / Approved_Deals, 
                                      scientific = FALSE)) %>% 
  arrange(desc(Average_Deal_Amount)) 

# Transforms the columns with "$" and "," for clarity and readability
avg_invested_per_deal <- avg_invested_per_deal %>%
  mutate(Average_Deal_Amount = dollar(as.numeric(Average_Deal_Amount)),
    Total_Shark_Investment = dollar(as.numeric(Total_Investment)))
  
# Displays total average investment amounts for approved deals across industry
View(avg_invested_per_deal)

# ==============================================================================
#                           HIGHEST INVESTMENT AMOUNT
# ==============================================================================

# Option #1

# Highlights the largest approved deal amount 
highest_deal_amount1 <- shark %>% 
  filter(Got.Deal==1) %>% 
  summarize(Highest_Deal = format(max(Total.Deal.Amount),scientific=FALSE))

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
  mutate(Total.Deal.Amount = format(Total.Deal.Amount,scientific = FALSE))

# Displays the largest investment made on a single approved deal 
View (highest_deal_amount3)

# ==============================================================================
#                                 SECTION 5
#                               DEAL ANALYSIS 
#                           (INVESTMENT AND EQUITY)
# ==============================================================================

# ==============================================================================
#                       TOTAL FAVORABLE INVESTMENT DEALS  
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

# Identifies the total number of favorable deals, where the participant received
# their requested or greater investment amounts
favorable_investment_deals <- total_deals %>% 
  filter(gain_loss <= 0) %>% 
  summarize(Total = n())

# Displays the count of favorable investment deals for the participants
View(favorable_investment_deals)

# ==============================================================================
#                 PERCENTAGE OF FAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the percentage of favorable deals to the entrepreneurs
percentage_of_favorable_investment_deals <- favorable_deals %>% 
summarize(Percent = round(Total / nrow(total_deals) * 100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
percentage_of_favorable_investment_deals$Percent <- 
  paste0(percentage_of_favorable_investment_deals$Percent,"%")

# Displays the percentage of favorable deals in favor of the participants
View(percentage_of_favorable_investment_deals)

# ==============================================================================
#                   TOTAL UNFAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the total number of unfavorable deals, where the participant received
# less than their asking investment amount
unfavorable_investment_deals <- total_deals %>% 
  filter(gain_loss >0) %>% 
  summarize(Total = n()) 

# Displays the count of unfavorable investment deals for the participants
View(unfavorable_investment_deals)

# ==============================================================================
#                PERCENTAGE OF UNFAVORABLE INVESTMENT DEALS
# ==============================================================================

# Calculates the percentage of unfavorable deals to the entrepreneurs
percentage_of_unfavorable_investment_deals <- unfavorable_deals %>% 
  summarize(Percent = round(Total / nrow(total_deal_amount)*100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
percentage_of_unfavorable_investment_deals$Percent <- 
  paste0(percentage_of_unfavorable_investment_deals$Percent,"%")

# Displays the percentage of unfavorable investment deals for entrepreneurs
View(percentage_of_unfavorable_investment_deals)

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

# Calculates the total number of favorable deals, where participants retained
# their original or greater equity share 
favorable_equity <- total_equity %>% 
  filter(gain_loss >= 0) %>% 
  summarize(Total = n())

# Displays the count of favorable equity deals for the participants
View(favorable_equity)

# ==============================================================================
#                   PERCENTAGE OF FAVORABLE EQUITY DEALS
# ==============================================================================

# Calculates the percentage of favorable equity deals to the entrepreneurs
favorable_equity_percentage <- favorable_equity %>% 
  summarize(Percent = round(Total / nrow(total_equity)*100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
favorable_equity_percentage$Percent <- paste0(favorable_equity_percentage$Percent,"%")

# Displays the percentage of favorable equity deals for the participants
View(favorable_equity_percentage)

# ==============================================================================
#                     TOTAL UNFAVORABLE EQUITY DEALS
# ==============================================================================

# Counts unfavorable equity deals where participants surrendered more than their 
# original offered equity
unfavorable_equity <- total_equity %>% 
  filter(gain_loss < 0) %>% 
  summarize(Total = n())

# Displays the count of unfavorable equity deals for the participants
View(unfavorable_equity)

# ==============================================================================
#                 PERCENTAGE OF UNFAVORABLE EQUITY DEALS
# ==============================================================================

# Calculates the percentage of unfavorable equity deals to the entrepreneurs
unfavorable_equity_percentage <- unfavorable_equity %>% 
  summarize(Percent = round(Total / nrow(total_equity)*100))

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
unfavorable_equity_percentage$Percent <- 
  paste0(unfavorable_equity_percentage$Percent,"%")

# Displays the percentage of unfavorable equity deals to the participants
View(unfavorable_equity_percentage)

# ==============================================================================
#                           TOTAL AVERAGE EQUITY
# ==============================================================================

# Calculates the average equity surrendered by participants across all approved 
# deals
total_average_equity <- shark %>% 
  select(Total.Deal.Equity) %>% 
  summarize(avg = round(mean(Total.Deal.Equity, na.rm = TRUE))) 

# Adds "%" symbol to the "Percent" value improving clarity and ease of readability
#total_average_equity$avg <- 
  #paste0(total_average_equity$avg,"%")

# Displays the average equity surrendered by participants over 824 approved deals
View(total_average_equity)

# ==============================================================================
#                       AVERAGE EQUITY ACROSS INDUSTRIES
# ==============================================================================

# Calculates the average equity surrendered by participants across all industries
avg_equity_per_industry <- shark %>% 
  group_by(Industry) %>% 
  summarize(avg = round(mean(Total.Deal.Equity, na.rm = TRUE))) %>% 
  arrange(desc(avg)) 

# Note: Maintaining the "avg" values as raw data for plotting purposes

# Displays the average equity surrendered by participants across all industries
View(avg_equity_per_industry)

# ==============================================================================
#                     PLOT AVERAGE EQUITY ACROSS INDUSTRIES
# ==============================================================================

# Creates a column chart visualizing average equity surrendered by participants 
# across all industries
fig_8 <- avg_equity_per_industry %>% 
  ggplot(aes(x = reorder(Industry, -avg), y = avg)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = avg), vjust = -0.5, hjust = 0.5) +
  geom_hline(yintercept = total_average_equity$avg, linetype = "solid", 
             color = "red") +
  annotate("text", x = 15, y = total_average_equity$avg + 2, 
           label = paste0("Average: ", total_average_equity$avg, "%")) +
  labs(title = "AVERAGE EQUITY PER INDUSTRY", x = "INDUSTRY", y = "AVERAGE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart visualizing average equity surrendered by participants 
# across industries. While Automotive and Travel industries stood out in prior 
# analyses, indicating advantages for participants, deals in these sectors tend 
# to involve higher equity stakes for entrepreneurs.
fig_8

# ==============================================================================
#                                 SECTION 6
#                            VALUATION ANALYSIS
# ==============================================================================

# The objective of this section is to compare valuations between approved versus 
# denied deals. By analyzing the differences in these valuations, the aim is to 
# determine whether discrepancies or potential misalignments in company valuations 
# influenced the decision-making process of the sharks.

# ==============================================================================
#                     AVERAGE VALUATION APPROVED BY SHARKS
# ==============================================================================

# Calculates the average valuation deemed acceptable by sharks in successful deals
avg_valuation_by_sharks <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Deal.Valuation) %>% 
  summarize(Average = mean(Deal.Valuation)) %>% 
  pull(Average)

# Note: Retaining the average value as raw data to enable further computation

# Displays the average valuation deemed acceptable by sharks  
View(avg_valuation_by_sharks)

# ==============================================================================
#                     AVERAGE VALUATION OF DENIED PITCHES
# ==============================================================================

# Calculates the average valuation presented by participants of denied pitches
avg_valuation_denied <- shark %>% 
  filter(Got.Deal == 0) %>% 
  select(Valuation.Requested) %>% 
  summarize(Average = mean(Valuation.Requested)) 

# Formats the average valuation as currency
avg_valuation_denied$Average <- dollar(avg_valuation_denied$Average)

# Displays the average valuation of denied deals
View(avg_valuation_denied)

# ==============================================================================
#           TOTAL DENIED PITCHES EXCEEDING SHARK VALUATION AVERAGE
# ==============================================================================

# Determines the count of denied pitches where the valuation presented exceeded
# the sharks' average
total_pitches_denied_exceeding_shark_valuation <- shark %>% 
  filter(Got.Deal==0, Valuation.Requested > avg_valuation_by_sharks ) %>% 
  summarize(Total = n())

# Displays the total count of denied pitches where the requested valuation 
# exceeded the sharks' average valuation
View(total_pitches_denied_exceeding_shark_valuation)

# ==============================================================================
#                    AVERAGE VALUATION OF APPROVED PITCHES
# ==============================================================================

# Calculates the average valuation presented by participants of approved pitches
avg_valuation_approved <- shark %>% 
  filter(Got.Deal == 1) %>% 
  select(Valuation.Requested) %>% 
  summarize(Average = mean(Valuation.Requested)) 

# Formats the average valuation as currency
avg_valuation_approved$Average <- dollar(avg_valuation_approved$Average)

# Displays the total count of approved pitches where the requested valuation 
# exceeded the sharks' average valuation
View(avg_valuation_approved)

# ==============================================================================
#         TOTAL APPROVED PITCHES EXCEEDING SHARK VALUATION AVERAGE
# ==============================================================================

# Determines the count of approved pitches where the valuation presented exceeded
# the sharks' average
total_pitches_approved_exceeding_shark_valuation <- shark %>% 
  filter(Got.Deal==1, Valuation.Requested > avg_valuation_by_sharks) %>% 
  summarize(Total = n())

# Displays the average valuation presented by participants of approved deals 
View(total_pitches_approved_exceeding_shark_valuation)

# ==============================================================================
#                                     SECTION 7
#                                  SHARK ANALYSIS
# ==============================================================================

# ==============================================================================
#                       NUMBER OF SHARKS INVOLVED PER DEAL
# ==============================================================================

# Calculates the total number of approved deals involving one or more sharks
number_of_sharks_involved <- shark %>% 
  filter(Got.Deal == 1) %>% 
  group_by(Number.of.Sharks.in.Deal) %>% 
  summarize(Total_Deals = n())

# Displays the total count of deals made that involved one or more sharks
View(number_of_sharks_involved)

# ==============================================================================
#                   PERCENTAGE DISTRIBUTION OF DEALS BY SHARK
# ==============================================================================

# Calculates the percentage distribution of deals involving different numbers of 
# sharks
percent_of_sharks_involved <- number_of_sharks_involved %>% 
  mutate(Percent = paste0(round((Total_Deals/pitches_approved * 100),1),"%"))

# Displays the number of sharks involved in approved deals along with the 
# probability of having multiple sharks participate in a single deal
View(percent_of_sharks_involved)


# ==============================================================================
#                  PLOT TOTAL SHARKS INVOLVED IN APPROVED DEALS
# ==============================================================================

# Creates a scatter plot to visualize the relationship between the number of
# sharks involved in a deal and the total number of deals
fig_9 <- ggplot(number_of_sharks_involved, aes(x= Number.of.Sharks.in.Deal, 
                                      y= Total_Deals)) +
  geom_point() +
  geom_smooth(method = "lm", se=FALSE, color = "blue") +
  labs(title = "DISTRIBUTION OF DEALS PER SHARK", x= "Number of Sharks in Deal", 
       y= "Total Deals")

# Displays the scatter plot visualizing the number of sharks involved in total 
# deals
fig_9

# ==============================================================================
#                          TOTAL INVESTMENTS BY SHARK     
# ==============================================================================

# Selects the investment amount columns for each shark, categorized by industry
investment_per_shark <- shark %>% 
  select(Industry, Barbara.Corcoran.Investment.Amount, Mark.Cuban.Investment.Amount,
         Lori.Greiner.Investment.Amount, Robert.Herjavec.Investment.Amount,
         Daymond.John.Investment.Amount, Kevin.O.Leary.Investment.Amount,
         Guest.Investment.Amount) 
  
# Displays individual investment totals by industry for each shark
View(investment_per_shark)

# Summarizes total investments by shark
Total_Investment_Per_Shark <- investment_per_shark %>% 
  summarize(Barbara.Corcoran = sum(Barbara.Corcoran.Investment.Amount, na.rm = TRUE),
            Mark.Cuban = sum(Mark.Cuban.Investment.Amount, na.rm = TRUE),
            Lori.Greiner = sum(Lori.Greiner.Investment.Amount, na.rm = TRUE),
            Robert.Herjavec = sum(Robert.Herjavec.Investment.Amount, na.rm = TRUE),
            Daymond.John = sum(Daymond.John.Investment.Amount, na.rm = TRUE),
            Kevin.O.Leary = sum(Kevin.O.Leary.Investment.Amount, na.rm = TRUE),
            Guest = sum(Guest.Investment.Amount, na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(), names_to = "Shark", values_to = "Amount_Invested") %>% 
  arrange(desc(Amount_Invested)) %>% 
  mutate(Percent = round(Amount_Invested / total_invested *100,1))

# Note: Retaining "Amount_Invested" and "Percent" as raw data for plotting

# Displays the summarized data of total investments and percentages for each shark
View(Total_Investment_Per_Shark) 

# ==============================================================================
#                       PLOT TOTAL INVESTMENTS BY SHARK     
# ==============================================================================

# Creates a column chart to visualize the total investments per shark
fig_10 <- Total_Investment_Per_Shark %>%  
  ggplot(aes(x= reorder(Shark, -Amount_Invested), y=Amount_Invested))+
  geom_col(fill="navy")+
  geom_text(aes(label = scales::dollar(Amount_Invested, scale = 1e-6, 
                                       suffix = "M", accuracy = 0.1)), 
            hjust = 0.5, vjust = 2, color="white")+
  labs(title = "TOTAL INVESTMENT PER SHARK", x = "Shark", y = "Amount Invested")+
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix ="M", 
                                                   accuracy = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Displays the column chart visualizing the total investments per shark
fig_10

# ==============================================================================
#                   PLOT TOTAL PERCENT INVESTED BY SHARK   
# ==============================================================================

# Creates a pie chart to visualize the percentages invested per shark
fig_11 <- plot_ly(Total_Investment_Per_Shark, labels = ~Shark, 
        values = ~Amount_Invested, type = 'pie') %>% 
  layout(title = "PERCENT INVESTED PER SHARK")

# Displays the pie chart visualizing the percentages invested per shark
fig_11

# ==============================================================================
#                 TOTAL INVESTED PER SHARK ACROSS INDUSTRIES
# ==============================================================================

# Summarizes investments by industry sector for each shark
Investment_per_shark_by_Industry_Sector <- investment_per_shark %>% 
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
View(Investment_per_shark_by_Industry_Sector)

# ==============================================================================
#                 PLOT INVESTMENTS BY SHARKS ACROSS INDUSTRIES
# ==============================================================================

# Reshapes the data to analyze individual shark investments by sector
pivoted_df <- Investment_per_shark_by_Industry_Sector %>% 
  pivot_longer(cols = where(is.numeric), names_to = "Shark", 
               values_to = "Investment")

# Creates a column chart to visualize shark investments across industries
fig_12 <- pivoted_df %>% 
  ggplot(aes(x=Industry, y=Investment, fill = Industry))+
  geom_col()+
  facet_wrap(~Shark, scales = "free_y", ncol=2)+
  scale_y_continuous(labels=scales::number_format(scale = 1e-6, suffix = "M"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title= "SHARK INVESTMENTS ACROSS INDUSTRIES", x = "Industry", 
       y = "Investment Amount")

# Displays the faceted column charts visualizing total shark investments by 
# industry sector 
fig_12

# ==============================================================================
#                               TOTAL DEALS PER SHARK
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

# ==============================================================================
#                                TOTAL DEALS MADE
# ==============================================================================

# Aggregates total deals made by all sharks 
total_deals_made <- total_deals_per_shark%>% 
  summarize(Total = sum(Total_Deals))
  
# Displays the aggregated total deals made by all sharks
View(total_deals_made)         

# ==============================================================================
#                           PLOT TOTAL DEALS PER SHARK
# ==============================================================================

# Creates a column chart to visualize the total number of deals made by each shark
fig_13 <- total_deals_per_shark %>% 
  ggplot(aes(x= reorder(Shark, -Total_Deals), y=Total_Deals))+
  geom_col(fill="blue") +
  geom_text(aes(label = Total_Deals), vjust = -0.5, hjust = 0.5)+
  labs(title = "Total Deals Per Shark", x= "Shark", y="Total Deals")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart showing the total number of deals made by each shark
fig_13

#===============================================================================
#                       TOTAL DEALS ACROSS INDUSTRIES 
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
#                 TOTAL AVERAGE EQUITY OBTAINED BY THE SHARKS
# ==============================================================================

# Prepares the data for analysis to determine the total average equity obtained
Avg_Equity_Per_Shark <- shark %>% 
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
Total_Avg_Equity <- Avg_Equity_Per_Shark %>%  
  pivot_longer(cols = -Industry, names_to = "Shark", values_to = "Avg_Equity") %>% 
  summarize(Average_Equity = round(mean(Avg_Equity, na.rm = TRUE)))

# Note: Retaining "Avg_Equity" as raw data for plotting
  
# Displays the total average equity 
View(Total_Avg_Equity)

# ==============================================================================
#                       AVERAGE EQUITY OBTAINED PER SHARK   
# ==============================================================================

# Calculates the total average equity obtained by each sharks across all industries
Avg_Equity_Per_Shark2 <-Avg_Equity_Per_Shark %>% 
  summarize(Barbara_Corcoran = round(mean(Barbara_Corcoran, na.rm = TRUE)),
            Mark_Cuban = round(mean(Mark_Cuban, na.rm = TRUE)),
            Lori_Greiner = round(mean(Lori_Greiner, na.rm = TRUE)),
            Robert_Herjavec = round(mean(Robert_Herjavec, na.rm = TRUE)),
            Daymond_John = round(mean(Daymond_John, na.rm = TRUE)),
            Kevin_O_Leary = round(mean(Kevin_O_Leary, na.rm = TRUE)),
            Guest = round(mean(Guest, na.rm = TRUE))) %>% 
  pivot_longer(cols = everything(), names_to = "Shark", values_to = "Avg_Equity")

# Displays the total average equity per shark
View(Avg_Equity_Per_Shark2)

# ==============================================================================
#                   PLOT AVERAGE EQUITY OBTAINED PER SHARK
# ==============================================================================

# Creates a column chart to visualize the average equity obtained by each shark
fig_14 <- Avg_Equity_Per_Shark2 %>% 
  ggplot(aes(x= reorder(Shark, -Avg_Equity), y=Avg_Equity))+
  geom_col(fill="green") +
  geom_text(aes(label = Avg_Equity), vjust = 1, hjust = 0.5)+
  labs(title = "AVERAGE EQUITY PER SHARK", x= "Shark", y="Average Equity")+
  geom_hline(yintercept = Total_Avg_Equity$Average_Equity, linetype ="solid", 
             color ="red")+
  annotate("text", x =6, y= Total_Avg_Equity$Average_Equity +1,
           label = paste0("Average: ", Total_Avg_Equity$Average_Equity, "%"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Displays the column chart showing the average equity obtained by each shark
fig_14

# ==============================================================================
#             AVERAGE EQUITY OBTAINED BY SHARK ACROSS INDUSTRIES
# ==============================================================================

# Calculates the total average equity obtained by sharks in each industry
Avg_equity_per_shark_per_industry <- Avg_Equity_Per_Shark %>% 
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
View(Avg_equity_per_shark_per_industry)

# ==============================================================================
#                                 SECTION 8
# ==============================================================================
#                            ROYALTY DEAL ANALYSIS
#===============================================================================

# Option #1

# Identifies the total number of royalty deals and displays the result to the 
# console
sum(!is.na(shark$Royalty.Deal))


# Option #2 

# Identifies the total number of royalty-based deals
total_royalty_deals <- shark %>% 
  select(Royalty.Deal) %>%
  filter(Royalty.Deal==1) %>% 
  summarize(Total = n())

# Displays the total count of deals approved with a royalty condition 
View(total_royalty_deals)
  
# ==============================================================================
#                             LOAN DEAL ANALYSIS                
# ==============================================================================

# Option #1

# Counts the total number of deals involving loans
sum(!is.na(shark$Loan))


# Option #2

# Retrieves the distinct values present in the loan column
unique(shark$Loan)

# Counts the total number of deals involving loans
total_loan_deals<- shark %>% 
  filter(Loan >=50000, Loan <=2000000) %>% 
  summarize(Total = n())

# Displays the total count of deals approved with a loan condition 
View(total_loan_deals)

# ==============================================================================
#                              SHARK METRIC TABLES
# ==============================================================================

# Combines multiple data frames to create a comprehensive metric table
metric_table <- total_investment_by_industry %>% 
  left_join(Investment_per_shark_by_Industry_Sector) %>% 
  left_join(total_deals_per_industry) %>% 
  left_join(Avg_equity_per_shark_per_industry, by = "Industry")

# Displays the contents of the metric table
View(metric_table)

# Creates a metric table summarizing Mark Cuban's industry-specific data 
mark_cuban_metric_table <- metric_table %>% 
  select(Industry, Total_Shark_Investment, Mark_Cuban_Investment, 
         Mark_Cuban_Percent, Mark_Cuban_Deals, Mark_Cuban_Avg_Equity)

# Displays the contents of Mark Cuban's metric table
View(mark_cuban_metric_table)

# Creates a metric table summarizing Lori Greiner's industry-specific data 
lori_greiner_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Lori_Greiner_Investment, 
         Lori_Greiner_Percent, Lori_Greiner_Deals, Lori_Greiner_Avg_Equity)

# Displays the contents of Lori Greiner's metric table  
View(lori_greiner_metric_table)

# Creates a metric table summarizing Robert Herjavec's industry-specific data
robert_herjavec_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Robert_Herjavec_Investment, 
         Robert_Herjavec_Percent, Robert_Herjavec_Deals, Robert_Herjavec_Avg_Equity)

# Displays the contents of Robert Herjavec's metric table
View(robert_herjavec_metric_table)

# Creates a metric table summarizing Kevin O Leary's industry-specific data
kevin_o_leary_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Kevin_O_Leary_Investment, 
         Kevin_O_Leary_Percent, Kevin_O_Leary_Deals, Kevin_O_Leary_Avg_Equity)

# Displays the contents of Kevin O Leary's metric table
View(kevin_o_leary_metric_table)

# Creates a metric table summarizing the Guest's industry-specific data
guest_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Guest_Investment, 
         Guest_Percent, Guest_Deals, Guest_Avg_Equity)

# Displays the contents of the Guest's metric table
View(guest_metric_table)

# Creates a metric table summarizing Barbara Corcoran's industry-specific data
barbara_corcoran_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Barbara_Corcoran_Investment, 
         Barbara_Corcoran_Percent, Barbara_Corcoran_Deals, Barbara_Corcoran_Avg_Equity)

# Displays the contents of Barbara Corcoran's metric table
View(barbara_corcoran_metric_table)

# Creates a metric table summarizing Daymond John's industry-specific data
daymond_john_metric_table <- metric_table %>%
  select(Industry, Total_Shark_Investment, Daymond_John_Investment, 
         Daymond_John_Percent, Daymond_John_Deals, Daymond_John_Avg_Equity)

# Displays the contents of Daymond John's metric table
View(daymond_john_metric_table)

