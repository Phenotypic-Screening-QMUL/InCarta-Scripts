# Importing Data and Wrangling

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
Data <- fread("Single Target Data.csv") # Fast CSV reading using fread function  - update with your file name

# List the variables you want to keep - make sure wavelength is correct!
Variable_Names <- c("Row", "Column", "Nuclei Area wv1", "Cells Area wv2")

# Use your variables list to select the columns
Data_Tidy <- Data %>% 
  select(all_of(Variable_Names))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Tidy %>% 
  mutate(Treatment = case_when(Column %in% c(1:6) ~ "DMSO", 
                               Column %in% c(7:12) ~ "Palbo"))

# Add some meaningful labels based on columns - here DMSO are in columns 1-6 and Palbociclib in columns 7-12
Data_Labelled <- Data_Labelled %>% 
  mutate(Cells = case_when(Row %in% c("A","B","C","D") ~ "MB231", 
                           Row %in% c("E","F","G","H") ~ "SKMEL"))

#Filter and Split Data As Needed
Data_MB231 <- Data_Labelled %>%
  filter(Cells == "MB231")

Data_SKMEL <- Data_Labelled %>%
  filter(Cells == "SKMEL")


# Histogram with frequency distribution
ggplot(Data_MB231, aes(x = `Nuclei Area wv1`, fill = Treatment)) + 
  geom_histogram(position = "dodge", alpha = 0.8, bins = 100) + 
  theme_bw() + 
  scale_fill_manual(values = c("#999999", "red")) + 
  labs(title = "Nuclear Area Historgram - MB231", x = "Nuclei Area", y = "Frequency") + 
  guides(fill = guide_legend()) + xlim (0,1000)

ggsave("Nuclear Area Historgram MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)


# Density plot 
ggplot(Data_SKMEL, aes(x = `Cells Area wv2`, fill = Treatment)) + 
  geom_density(alpha = 0.8) + 
  theme_bw() + 
  scale_fill_manual(values = c("#999999", "blue")) + 
  labs(title = "Cell Area Density Plot - SKMEL", x = expression("Cell Area (" * mu * "M"^"2" * ")"), y = "Density") +
  guides(fill = guide_legend())


# Plotting the percentage frequency polygon
ggplot(Data_MB231, aes(x = `Nuclei Area wv1`, colour = Treatment)) + 
  geom_freqpoly(aes(y = ..count../sum(..count..)*100), bins = 200, size = 1.2) + 
  theme_bw() + 
  scale_color_manual(values = c("#999999", "red")) + 
  labs(title = "Nuclear Area % Frquency Distribution - MB231", x = "Nuclei Area", y = "Percentage (%)") + 
  guides(color = guide_legend()) + xlim (0,1000)

ggsave("Nuclear Area % Frequency Distribution MB231.png", plot = last_plot(), width = 6, height = 4, dpi = 300)