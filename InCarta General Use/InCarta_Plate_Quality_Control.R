# Load required packages
library("gplots")
library("RColorBrewer")
library("matrixStats")
library("plyr")
library("dplyr")
library("data.table")
library("stringr")
library("ggplot2")
library("Rtsne")
library("readxl")
library(platetools)
library(stringr)



# Import CSV File
Data <- fread("Summary Data.csv")

# List the variables you want to keep
Variable_Names <- c("Row", "Column", "Nuclei Nuclei Count wv1", "Nuclei Area wv1", "Cells Area wv2")

# Use your variables list to select the columns
Data_Tidy <- Data %>% 
  select(all_of(Variable_Names))

# Add meaningful labels based on columns
Data_Labelled <- Data_Tidy %>% 
  mutate(Treatment = case_when(Column %in% 1:6 ~ "DMSO", 
                               Column %in% 7:12 ~ "Palbo"),
         Cells = case_when(Row %in% c("A", "B", "C", "D") ~ "MB231", 
                           Row %in% c("E", "F", "G", "H") ~ "SKMEL"))

# Filter and split data as needed
Data_MB231 <- Data_Labelled %>%
  filter(Cells == "MB231")

Data_SKMEL <- Data_Labelled %>%
  filter(Cells == "SKMEL")

# Make a Nuclear Count Plot of Columns - MB231
ggplot(Data_MB231, aes(x = Column, y = `Nuclei Nuclei Count wv1`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "black") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231 - Columns", x = "Column", y = "Nuclei Count") +
  guides(fill = guide_legend()) +
  scale_x_continuous(breaks = seq(min(Data_MB231$Column), max(Data_MB231$Column), by = 1))

# Save the plot
ggsave("Nuclear Count Plot MB231 - Columns.png", plot = last_plot(), width = 6, height = 4, dpi = 300)

# Convert Row to a factor
Data_MB231$Row <- factor(Data_MB231$Row, levels = c("A", "B", "C", "D"))

# Make a Nuclear Count Plot of Rows - MB231
ggplot(Data_MB231, aes(x = Row, y = `Nuclei Nuclei Count wv1`, fill = Treatment)) + 
  geom_bar(stat = "summary", fun = "mean", position = "dodge", alpha = 0.8) +
  geom_point(position = position_dodge(width = .9), color = "black") +
  theme_bw() +
  scale_fill_manual(values = c("#999999", "red")) +
  labs(title = "Nuclear Count - MB231 - Rows", x = "Row", y = "Nuclei Count") +
  guides(fill = guide_legend()) +
  scale_x_discrete(drop = FALSE)

# Save the plot
ggsave("Nuclear Count Plot MB231 - Rows.png", plot = last_plot(), width = 6, height = 4, dpi = 300)


#Update Data frame for heatmap
Data <- Data_Labelled %>%
  mutate(
    `NEW WELL LABEL` = str_c(Row, str_pad(Column, 2, pad = "0"))  # Combine Row and zero-padded Column
  )

# Generate heatmap using platetools
raw_map(data = Data$`Nuclei Nuclei Count wv1`,
        well = Data$`NEW WELL LABEL`,
        plate = 96)

# Save platetools heatmap
ggsave("Nuclei_Count_Heatmap_platetools.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# Generate a heatmap using ggplot2 for a more customized approach
Data$Column <- as.factor(Data$Column)
Data$Row <- factor(Data$Row, levels = rev(c("A", "B", "C", "D", "E", "F", "G", "H")))  # Reverse levels for ggplot

ggplot(Data, aes(x = Column, y = Row, fill = `Nuclei Nuclei Count wv1`)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", na.value = "white") +
  theme_minimal() +
  labs(title = "Nuclei Count Heatmap",
       x = "Column",
       y = "Row",
       fill = "Nuclei Count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Save ggplot2 heatmap
ggsave("Nuclei_Count_Heatmap_ggplot2.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

