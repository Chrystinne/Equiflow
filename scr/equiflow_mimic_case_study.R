# Load necessary libraries
library(dplyr)
library(readr)
library(lubridate)
library(tableone)
library(htmlTable)
library(data.table)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(knitr)


# Set the working directory
setwd("~/Work/mit/bst209_2024/workshops/mit-flow-diagram")





# Initialize data structures (lists)
mimic_iv <- list()
dataframes <- list()


visualize_cohort <- function(dataframes) {
  if (length(dataframes) > 0) {
    
    # Initialize vectors to store the percentages for each race across all cohorts
    black_percentages <- c()
    asian_percentages <- c()
    hispanic_percentages <- c()
    unknown_percentages <- c()
    white_percentages <- c()
    
    # Loop through each cohort in the dataframes list
    for (i in 1:length(dataframes)) {
      cohort <- dataframes[[i]]
      
      # Calculate total patients in the cohort
      total_patients <- nrow(cohort)
      
      # Calculate the number and percentage of Black patients
      black_patients <- nrow(cohort %>% filter(race_ethnicity == "Black"))
      percent_black <- (black_patients / total_patients) * 100
      black_percentages <- c(black_percentages, percent_black)
      
      # Calculate the number and percentage of Asian patients
      asian_patients <- nrow(cohort %>% filter(race_ethnicity == "Asian"))
      percent_asian <- (asian_patients / total_patients) * 100
      asian_percentages <- c(asian_percentages, percent_asian)
      
      # Calculate the number and percentage of Hispanic patients
      hispanic_patients <- nrow(cohort %>% filter(race_ethnicity == "Hispanic OR Latino"))
      percent_hispanic <- (hispanic_patients / total_patients) * 100
      hispanic_percentages <- c(hispanic_percentages, percent_hispanic)
      
      # Calculate the number and percentage of White patients
      white_patients <- nrow(cohort %>% filter(race_ethnicity == "White"))
      percent_white <- (white_patients / total_patients) * 100
      white_percentages <- c(white_percentages, percent_white)
      
      # Calculate the number and percentage of Unknown patients
      unknown_patients <- nrow(cohort %>% filter(race_ethnicity == "Unknown"))
      percent_unknown <- (unknown_patients / total_patients) * 100
      unknown_percentages <- c(unknown_percentages, percent_unknown)
    }
    
    # Create the data frame similar to race_ethnicity_data
    race_ethnicity_data <- data.frame(
      Black = black_percentages,
      Asian = asian_percentages,
      Hispanic = hispanic_percentages,
      Unknown = unknown_percentages,
      White = white_percentages
    )
    
    # Print the race_ethnicity_data to check the result
    print(race_ethnicity_data)
    
    # Initialize vectors to store the percentages for male and female across all cohorts
    female_percentages <- c()
    male_percentages <- c()
    
    # Loop through each cohort in the dataframes list
    for (i in 1:length(dataframes)) {
      cohort <- dataframes[[i]]
      
      # Calculate total patients in the cohort
      total_patients <- nrow(cohort)
      
      # Calculate the number and percentage of female patients
      female_patients <- nrow(cohort %>% filter(sex_female == 1))
      percent_female <- (female_patients / total_patients) * 100
      female_percentages <- c(female_percentages, percent_female)
      
      # Calculate the number and percentage of male patients
      male_patients <- nrow(cohort %>% filter(sex_female == 0))
      percent_male <- (male_patients / total_patients) * 100
      male_percentages <- c(male_percentages, percent_male)
    }
    
    # Create the data frame similar to gender_data
    gender_data <- data.frame(
      female = female_percentages,
      male = male_percentages
    )
    
    # Print the gender_data to check the result
    print(gender_data)
    
    # Use a loop to generate long format data for race and gender
    combined_data <- bind_rows(lapply(1:length(dataframes), function(i) {
      racial_long <- race_ethnicity_data[i, ] %>%
        mutate(Group = paste('Race', paste0('Cohort ', i))) %>%
        pivot_longer(cols = -Group, names_to = 'Category', values_to = 'Value')
      
      gender_long <- gender_data[i, ] %>%
        mutate(Group = paste('Gender', paste0('Cohort ', i))) %>%
        pivot_longer(cols = -Group, names_to = 'Category', values_to = 'Value')
      
      bind_rows(racial_long, gender_long)
    }))
    
    # Define custom colors
    custom_colors <- c("Black" = "#1f78b4", "Asian" = "#33a02c", "Hispanic" = "#e31a1c",
                       "Unknown" = "#ff7f00", "White" = "#6a3d9a",
                       "female" = "#fdbf6f", "male" = "#b2df8a")
    
    # Plot
    ggplot(combined_data, aes(x = Group, y = Value, fill = Category)) +
      geom_bar(stat = "identity", position = "fill", width = 0.8) +  
      coord_flip() +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Group", y = "Proportion", fill = "Category") +
      scale_fill_manual(values = custom_colors) +
      ggtitle("Cohort Distribution") +
      theme_minimal(base_family = "Helvetica") +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 9),
        legend.position = "bottom",
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        panel.grid.major = element_line(size = 0.),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 12)
      )
    
  } else {
    print("The dataframes list is empty. Please load the data before running the analysis.")
  }
}



# Step 1: Get, preprocess and visualize cohort distribution for the first dataset: "Original"

# Step 1.1 Load Original dataset
mimic_iv_pat <- read_csv('./data/original.csv', show_col_types = FALSE)
head(mimic_iv_pat)

# Step 1.2 Preprocess Original dataset
mimic_iv_map <- read_csv('./data/definitions-mimic-iv.csv', show_col_types = FALSE)
head(mimic_iv_map)

# Map race_ethnicity values based on definitions
mapping_series <- setNames(mimic_iv_map$mapping, mimic_iv_map$original)
mimic_iv_pat$race_ethnicity <- mapping_series[mimic_iv_pat$race_ethnicity]

head(mimic_iv_pat)

# Group by subject_id and select the first record for each subject
mimic_iv_pat <- mimic_iv_pat %>% group_by(subject_id) %>% slice(1)

# Select relevant columns and add to dataframes list
dataframes[[1]] <- mimic_iv_pat %>% select(subject_id, race_ethnicity, sex_female, admission_age)
kable(head(dataframes[[1]]), format = "html")

visualize_cohort(dataframes)


# Step 2: Get, preprocess and visualize cohort distribution for the second dataset: "ABGs"

# Step 2.1 Load ABGs dataset
mimic_iv_abgs <- read_csv('./data/abgs.csv', show_col_types = FALSE)

# Step 2.2 preprocess ABGs dataset
mimic_iv_abgs <- mimic_iv_abgs %>%
  rename(charttime_abgs = charttime)

mimic_iv_abgs$charttime_abgs <- ymd_hms(mimic_iv_abgs$charttime_abgs)
mimic_iv[['abgs']] <- mimic_iv_abgs %>%
  mutate(across(c(subject_id, hadm_id, pH, pCO2, pO2, SaO2), as.numeric)) %>%
  filter(!is.na(subject_id) & !is.na(hadm_id) & !is.na(charttime_abgs) & !is.na(pH) & !is.na(pCO2) & !is.na(pO2) & !is.na(SaO2))

# Merge with patient data and group by subject_id
mimic_iv_pat_abgs <- left_join(mimic_iv[['abgs']], mimic_iv_pat, by = "hadm_id") %>%
  rename(subject_id = subject_id.x) %>%
  select(-subject_id.y) %>%
  group_by(subject_id) %>%
  slice(1)

# Add processed ABGs data to dataframes list
dataframes[[2]] <- mimic_iv_pat_abgs %>% select(subject_id, race_ethnicity, sex_female, admission_age)
head(dataframes[[2]])

# Step 2.3 Visualize cohort distribution for "ABGs" dataset

# Observe changes in cohort composition
visualize_cohort(dataframes)


# Step 3: Get, preprocess and visualize cohort distribution for the third dataset: "SPO2 paired"

# Step 3.1 Load Vitals dataset
mimic_iv_vitals <- read.csv('./data/vitals.csv')

# Step 3.2 preprocess Vitals dataset
mimic_iv_vitals <- mimic_iv_vitals %>%
  rename(charttime_vitals = charttime)

mimic_iv_vitals$charttime_vitals <- ymd_hms(mimic_iv_vitals$charttime_vitals)
mimic_iv$vitals <- mimic_iv_vitals %>%
  mutate(across(c(subject_id, hadm_id, stay_id), as.numeric)) %>%
  filter(!is.na(subject_id) & !is.na(hadm_id) & !is.na(charttime_vitals) & !is.na(O2.saturation.pulseoxymetry)) %>%
  rename(SpO2 = O2.saturation.pulseoxymetry)

# Filter vitals data with SpO2 and merge with "SPO2 paired" subject_ids
mimic_iv$vitals_withSpO2 <- mimic_iv$vitals %>%
  filter(!is.na(SpO2))

mimic_iv$vitals_withSpO2_SaO2 <- mimic_iv$vitals %>%
  filter(!is.na(SpO2) & subject_id %in% mimic_iv$abgs$subject_id)

# Convert datasets to data.table
mimic_iv$abgs <- as.data.table(mimic_iv$abgs)
mimic_iv$vitals_withSpO2_SaO2 <- as.data.table(mimic_iv$vitals_withSpO2_SaO2)

# Add start and end time columns
mimic_iv$abgs[, `:=`(start = charttime_abgs - minutes(5), end = charttime_abgs)]
mimic_iv$vitals_withSpO2_SaO2[, `:=`(start = charttime_vitals, end = charttime_vitals)]

# Set keys for data.tables
setkey(mimic_iv$abgs, hadm_id, start, end)
setkey(mimic_iv$vitals_withSpO2_SaO2, hadm_id, start, end)

# Perform asof join using foverlaps
mimic_iv_ABG_SpO2 <- foverlaps(
  mimic_iv$abgs,
  mimic_iv$vitals_withSpO2_SaO2,
  by.x = c("hadm_id", "start", "end"),
  by.y = c("hadm_id", "start", "end"),
  type = "any"
)

# Select and rename relevant columns
mimic_iv_ABG_SpO2 <- mimic_iv_ABG_SpO2[, .(
  subject_id = i.subject_id,
  hadm_id = hadm_id,
  stay_id = stay_id,
  SaO2_timestamp = charttime_abgs,
  SpO2_timestamp = charttime_vitals,
  pH = pH,
  pCO2 = pCO2,
  pO2 = pO2,
  SaO2 = SaO2,
  SpO2 = SpO2
)]

# Calculate the time difference between SpO2 and SaO2 measurements
mimic_iv_ABG_SpO2[, delta_SpO2 := as.numeric(difftime(SpO2_timestamp, SaO2_timestamp, units = "mins"))]

# Remove records without SpO2_timestamp
mimic_iv_ABG_SpO2 <- mimic_iv_ABG_SpO2[!is.na(SpO2_timestamp)]

# Merge resulting pairs with patient data and store specific columns in dataframes
mimic_iv_pairs_pats <- left_join(mimic_iv_ABG_SpO2, mimic_iv_pat, by = "subject_id")
mimic_iv_pairs_pats <- mimic_iv_pairs_pats %>% group_by(subject_id) %>% slice(1)

dataframes[[3]] <- mimic_iv_pairs_pats %>% select(subject_id, race_ethnicity, sex_female, admission_age)
head(dataframes[[3]])

# Step 3.3 Visualize cohort distribution for "SPO2 paired" dataset

visualize_cohort(dataframes)



# Step 4: Get, preprocess and visualize cohort distribution for the fourth dataset: "SpO2 >= 92"

# Step 4.1 Get SpO2 >= 92 dataset

# Filter pairs with SpO2 >= 92
mimic_iv_final <- mimic_iv_ABG_SpO2 %>% filter(SpO2 >= 92)

# Step 4.2 Preprocess SpO2 >= 92 dataset

# Merge with patient data and store specific columns in dataframes
mimic_iv_final_pats <- left_join(mimic_iv_final, mimic_iv_pat, by = "subject_id")
mimic_iv_final_pats <- mimic_iv_final_pats %>% group_by(subject_id) %>% slice(1)

dataframes[[4]] <- mimic_iv_final_pats %>% select(subject_id, race_ethnicity, sex_female, admission_age)
head(dataframes[[4]])

# Step 4.3 Visualize cohort distribution for SpO2 >= 92 dataset
visualize_cohort(dataframes)

# Step 4.4 Compare cohort distributions for "Original", "ABGs", "SPO2 paired", and "SpO2 >= 92" datasets

preprocess_cohorts <- function(dfs) {
  # Fill missing race_ethnicity values with 'Unknown'
  dfs$race_ethnicity[is.na(dfs$race_ethnicity)] <- 'Unknown'
  
  # Map race_ethnicity values to specific categories
  dfs$race_ethnicity <- recode(dfs$race_ethnicity,
                               'American Indian / Alaska Native' = 'Unknown',
                               'More Than One Race' = 'Unknown',
                               'Native Hawaiian / Pacific Islander' = 'Unknown',
                               'White' = 'White',
                               'Asian' = 'Asian',
                               'Hispanic OR Latino' = 'Hispanic OR Latino',
                               'Black' = 'Black',
                               'Unknown' = 'Unknown')
  
  # Filter DataFrame to ensure all necessary columns are not null
  dfs <- dfs %>%
    filter(!is.na(race_ethnicity) & !is.na(admission_age) & !is.na(sex_female)) %>%
    ungroup()
  return(dfs)
}

merge_datasets <- function(dataframes) {
  # Merge all dataframes
  dfs <- bind_rows(dataframes, .id = "cohort")
  
  # Preprocess final dataset All_cohorts
  dfs <- preprocess_cohorts(dfs)
  return(dfs)
}

dfs_cohort1_cohort2_cohort3_cohort4 <- merge_datasets(dataframes)

dfs_cohort1_cohort2_cohort3_cohort4 <- preprocess_cohorts(dfs_cohort1_cohort2_cohort3_cohort4)

### Generate TableOne

# Define function to generate TableOne
generate_tableone <- function(all_dfs, file_name) {
  # Define categorical and non-normal variables
  categ <- c('race_ethnicity', 'sex_female')
  nonnormal <- c('admission_age')
  
  # Generate TableOne
  table <- CreateTableOne(data = all_dfs, vars = c(categ, nonnormal), factorVars = categ, strata = 'cohort')
  
  # Convert the table to HTML format
  table_html <- print(table, quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  
  # Save the HTML table to a file
  html_table <- htmlTable::htmlTable(table_html)
  write(html_table, file = file_name)
  
  # Print the table using kableone
  kableone(table)
}

generate_tableone(dfs_cohort1_cohort2_cohort3_cohort4, './data/tableone_cohort1_cohort2_cohort3_cohort4.html')