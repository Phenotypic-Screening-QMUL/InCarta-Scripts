#Data Import and Wrangle

# Useful Packages
library("gplots")       # Graphical plots
library("RColorBrewer") # Color palettes
library("matrixStats")  # Efficient matrix computations
library("plyr")        # Data manipulation
library("dplyr")       # Data manipulation
library("data.table")  # High-performance data manipulation
library("stringr")     # String manipulation
library("ggplot2")     # Elegant plots
library("Rtsne")       # Dimensionality reduction
library("readxl")      # Read Excel files

# Import CSV File
Data <- fread("Summary Data.csv") # Fast CSV reading using fread function  - update with your file name

# List the variables you want to keep - make sure wavelength is correct!
Variable_Names <- c("Row", "Column", "Nuclei Nuclei Count wv1", "Nuclei Area wv1", "Cells Area wv2")

# Use your variables list to select the columns
Data_Tidy <- Data %>% 
  select(all_of(Variable_Names))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Tidy %>% 
  mutate(Treatment = case_when(Column == 1:6 ~ "DMSO", 
                               Column == 7:12 ~ "Palbo"))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Labelled %>% 
  mutate(Cells = case_when(Row %in% c("A","B","C","D") ~ "MB231", 
                           Row %in% c("E","F","G","H") ~ "SKMEL"))

#Filter and Split Data As Needed
Data_MB231 <- Data_Labelled %>%
  filter(Cells == "MB231")

Data_SKMEL <- Data_Labelled %>%
  filter(Cells == "SKMEL")

#Making and Saving Plots
# Make a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclei Nuclei Count wv1`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend())

ggsave("Nuclear Count Plot MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

# Make a Cell Area Plot - SKMEL
ggplot(Data_SKMEL, aes(x = Treatment, y = `Cells Area wv2`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "blue")) +
  labs(title = "Cell Area - SKMEL", x = "Treatment", y = expression("Cell Area (" * mu * "M"^"2" * ")")) +
  guides(fill = guide_legend())

ggsave("Cell Area Plot SKMEL1.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

## Adding Error Bars
# Calculate mean and standard deviation for each treatment group
summary_stats <- Data_MB231 %>%
  group_by(Treatment) %>%
  summarise(mean_count = mean(`Nuclei Nuclei Count wv1`), 
            sd_count = sd(`Nuclei Nuclei Count wv1`))

# Remake a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclei Nuclei Count wv1`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_errorbar(data = summary_stats, aes(y = mean_count, ymin = mean_count - sd_count, ymax = mean_count + sd_count),
                width = 0.4, position = position_dodge(width = 0.9)) +
  geom_point(position = position_dodge(width = .9), color = "blaCK") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend())

ggsave("Nuclear Count Plot MB231 with Error Bar.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

# Running a t-test

# Load ggpubr package for statistical testing
library(ggpubr)

# Conduct t-test
ttest <- t.test(`Nuclei Nuclei Count wv1` ~ Treatment, data = Data_MB231)

# Extract p-value from t-test result
p_value <- ttest$p.value

# Determine significance level
significance_level <- ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "")))

# Remake a Nuclear Count Plot - MB231
ggplot(Data_MB231, aes(x = Treatment, y = `Nuclei Nuclei Count wv1`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_errorbar(data = summary_stats, aes(y = mean_count, ymin = mean_count - sd_count, ymax = mean_count + sd_count),
                width = 0.4, position = position_dodge(width = 0.9)) +
  geom_point(position = position_dodge(width = .9), color = "black") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231", x = "Treatment", y = "Nuclei Count") +
  guides(fill = guide_legend()) +
  stat_compare_means(label = "p.signif", method = "t.test", comparisons = list(c("DMSO", "Palbo"))) +
  annotate("text", x = 1.5, y = max(Data_MB231$`Nuclei Nuclei Count wv1`), 
           label = significance_level,
           vjust = -1.5, hjust = 0.5, size = 6, color = "black")

ggsave("Nuclear Count Plot MB231 with Error Bar and Significance.png", plot = last_plot(), width = 6, height = 4, dpi = 300)