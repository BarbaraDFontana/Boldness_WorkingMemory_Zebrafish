# Load necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggcorrplot)
library(tidyr)
library(car)  # For ANCOVA assumptions
library(multcomp)  # For Tukey's post hoc
library(gridExtra)
library(cowplot)  # For combining grids
library(emmeans)

# Set the working directory to the location of your CSV file
setwd("E:/Barbara/Postdoc_Denis(2024-2025)/FMP_Ymaze_Postdoc")

# Load the CSV file into a data frame
full_data <- read.csv("Data_full.csv", fileEncoding = "latin1")
ymaze <-read.csv("updated_ymaze_with_boldness_group.csv")


# Rename the levels of boldness_group to "Bold" and "Shy"
full_data$boldness_group <- factor(full_data$boldness_group, 
                                  levels = c("Bold", "Shy"))

# Ensure Group is a factor with the correct levels
full_data$Group <- factor(full_data$Group, levels = c("CTRL", "CAS"))

# Check for any missing values and ensure boldness_group is not NA
full_data <- full_data %>% filter(!is.na(boldness_group))

# Custom colors for bars
custom_colors <- c("Bold" =  "#00008B", "Shy" = "#FF8C00")
lighter_dot_colors <- c("Bold" = "#4169E1", "Shy" = "#ffbb78")  # Lighter shades for individual points

# Total Turns with individual points and mean + error bars
total_turns_plot  <- ggplot(full_data, aes(x=Group, y=num_turns, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", position="dodge", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=boldness_group), position=position_jitterdodge(jitter.width=0.1, dodge.width=0.7), size=2) +  # Individual points
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100), expand = c(0, 0)) +  # Remove gap and set custom y-axis
  scale_fill_manual(values=custom_colors, guide = "none") +  # Remove legend for fill
  scale_color_manual(values=lighter_dot_colors, guide = "none") +  # Lighter individual points, no legend
  labs(y="Number of Turns") +
  theme_classic()

# Alternations with individual points, custom y-axis scale, and mean + error bars
alternations_plot <- ggplot(full_data, aes(x=Group, y=alternations, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", position="dodge", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=boldness_group), position=position_jitterdodge(jitter.width=0.1, dodge.width=0.7), size=2) +  # Individual points
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10), expand = c(0, 0)) +  # Remove gap and set custom y-axis
  scale_fill_manual(values=custom_colors, guide = "none") +  # Remove legend for fill
  scale_color_manual(values=lighter_dot_colors, guide = "none") +  # Lighter individual points, no legend
  labs(y="Relative Alternations (%)") +
  theme_classic()


# Repetitions with individual points, custom y-axis scale, and mean + error bars
repetitions_plot <- ggplot(full_data, aes(x=Group, y=repetitions, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", position="dodge", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=boldness_group), position=position_jitterdodge(jitter.width=0.1, dodge.width=0.7), size=2) +  # Individual points
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 2.5), expand = c(0, 0)) +  # Remove gap and set custom y-axis
  scale_fill_manual(values=custom_colors, guide = "none") +  # Remove legend for fill
  scale_color_manual(values=lighter_dot_colors) +  # Lighter individual points, no legend
  labs(y="Relative Repetitions (%)") +
  theme_classic()

############################STATS#######################################################

# Ensure the factors are correctly set
full_data$Group <- factor(full_data$Group, levels = c("CTRL", "CAS"))
full_data$boldness_group <- factor(full_data$boldness_group, levels = c("Bold", "Shy"))
# Create an interaction term for Group and boldness_group
full_data$Group_boldness <- interaction(full_data$Group, full_data$boldness_group)

# ANCOVA for Alternations
ancova_alternations <- aov(alternations ~ boldness_group * Group + num_turns, data = full_data)
summary(ancova_alternations)

# ANCOVA for Repetitions
ancova_repetitions <- aov(repetitions ~ boldness_group * Group + num_turns, data = full_data)
summary(ancova_repetitions)

# ANOVA for Total Turns
anova_turns <- aov(num_turns ~ boldness_group * Group, data = full_data)
summary(anova_turns)

# Post-hoc for ANCOVA on Alternations (no p-value adjustment)
posthoc_alternations <- emmeans(ancova_alternations, pairwise ~ boldness_group * Group, adjust = "none")
summary(posthoc_alternations)
confint(posthoc_alternations)

# Post-hoc for ANCOVA on Repetitions (no p-value adjustment)
posthoc_repetitions <- emmeans(ancova_repetitions, pairwise ~ boldness_group * Group, adjust = "none")
summary(posthoc_repetitions)
confint(posthoc_repetitions)

# Post-hoc for ANOVA on Total Turns (no p-value adjustment)
posthoc_turns <- emmeans(anova_turns, pairwise ~ boldness_group * Group, adjust = "none")
summary(posthoc_turns)
confint(posthoc_turns)


#########################################LINE GRAPH###########################################
# Check the interaction levels to ensure they match the scale definitions
# Define custom colors to match the legend
custom_colors <- c("Bold + CTRL" = "#00008B",   # Dark blue
                   "Bold + CAS"  = "#FF8C00",   # Orange
                   "Shy + CTRL"  = "#4169E1",  # Light blue
                   "Shy + CAS"   = "#ffbb78")  # Light orange

# Define custom shapes to match the legend
custom_shapes <- c("Bold + CTRL" = 16,  # Circle
                   "Bold + CAS"  = 15,  # Square
                   "Shy + CTRL"  = 17,  # Triangle
                   "Shy + CAS"   = 18)  # Inverted triangle

# Create a new variable in the data to use these group labels
ymaze$group_label <- interaction(ymaze$boldness_group, ymaze$Group)
levels(ymaze$group_label) <- c("Bold + CTRL", "Bold + CAS", "Shy + CTRL", "Shy + CAS")

# Step 1: Manually aggregate the data for mean and standard error (SE)
summary_data <- ymaze %>%
  group_by(Trial, group_label) %>%
  summarize(mean_alternations = mean(alternations, na.rm = TRUE),
            se_alternations = sd(alternations, na.rm = TRUE) / sqrt(n()))

# Step 2: Plot using the aggregated data
alternations_line_plot <- ggplot(summary_data, aes(x=Trial, y=mean_alternations, 
                                                   group=group_label, 
                                                   color=group_label, 
                                                   shape=group_label)) +
  geom_line(size=1, alpha=0.8) +  # Line for mean values
  geom_point(size=4, alpha=0.8) +  # Points for mean values
  geom_errorbar(aes(ymin = mean_alternations - se_alternations, 
                    ymax = mean_alternations + se_alternations), 
                width=0.2, size=0.6, alpha=0.8) +  # Error bars using SE
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, by = 10), expand = c(0, 0)) +  # Y-axis limits and ticks
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +  # X-axis for Trials
  scale_color_manual(values = custom_colors, guide = "none") +  # Apply custom colors without legend labels
  scale_shape_manual(values = custom_shapes, guide = "none") +  # Apply custom shapes without legend labels
  labs(y="Relative alternations (%)", x="Time Bins", title="Alternations (lrlr + rlrl)") +
  theme_classic()

# Step 1: Manually aggregate the data for mean and standard error (SE) of repetitions
summary_data_repetitions <- ymaze %>%
  group_by(Trial, group_label) %>%
  summarize(mean_repetitions = mean(repetitions, na.rm = TRUE),
            se_repetitions = sd(repetitions, na.rm = TRUE) / sqrt(n()))

# Step 2: Plot using the aggregated data for repetitions
repetitions_line_plot <- ggplot(summary_data_repetitions, aes(x=Trial, y=mean_repetitions, 
                                                              group=group_label, 
                                                              color=group_label, 
                                                              shape=group_label)) +
  geom_line(size=1, alpha=0.8) +  # Line for mean values
  geom_point(size=4, alpha=0.8) +  # Points for mean values
  geom_errorbar(aes(ymin = mean_repetitions - se_repetitions, 
                    ymax = mean_repetitions + se_repetitions), 
                width=0.2, size=0.6, alpha=0.8) +  # Error bars using SE
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 5), expand = c(0, 0)) +  # Y-axis limits and ticks for repetitions
  scale_x_continuous(breaks = seq(1, 6, by = 1)) +  # X-axis for Trials
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = custom_shapes) +  # Apply custom shapes
  labs(y="Relative repetitions (%)", x="Time Bins", title="Repetitions (llll + rrrr)") +
  theme_classic()


#########################################STATS REPEATED MEASURE#############################################
# Convert group_label and Trial to factors if they’re not already
ymaze$group_label <- factor(ymaze$group_label)
ymaze$Trial <- factor(ymaze$Trial)

# Run repeated measures ANOVA with Animal_Number as the subject variable
anova_model <- aov(alternations ~ group_label * Trial + Error(Animal_Number/Trial), data = ymaze)
# Run repeated measures ANOVA without interaction
anova_main_effects <- aov(alternations ~ group_label + Trial + Error(Animal_Number/Trial), data = ymaze)
summary(anova_main_effects)

# Post-hoc tests using emmeans
posthoc <- emmeans(anova_model, ~ group_label | Trial)

# Pairwise comparisons
pairwise_comparisons <- contrast(posthoc, method = "pairwise", adjust = "none")

# Show results
summary(pairwise_comparisons)

# Run repeated measures ANOVA with Animal_Number as the subject variable for repetitions
anova_model_repetitions <- aov(repetitions ~ group_label * Trial + Error(Animal_Number/Trial), data = ymaze)
# Run repeated measures ANOVA without interaction for repetitions
anova_main_effects_repetitions <- aov(repetitions ~ group_label + Trial + Error(Animal_Number/Trial), data = ymaze)
summary(anova_main_effects_repetitions)

# Post-hoc tests using emmeans for repetitions
posthoc_repetitions <- emmeans(anova_model_repetitions, ~ group_label | Trial)

# Pairwise comparisons for repetitions without adjustment (less strict)
pairwise_comparisons_repetitions <- contrast(posthoc_repetitions, method = "pairwise", adjust = "none")

# Show results
summary(pairwise_comparisons_repetitions)

#########################GRID EXTRA############################################
# Assign more space to the last graph by adjusting the widths
top_grid <- grid.arrange(
  total_turns_plot, alternations_plot, repetitions_plot,  # First row: 3 plots
  widths = c(0.3, 0.3, 0.4),  # Last plot takes 40%, first two take 30% each
  ncol = 3  # Three columns in the top row
)


# Step 2: Create the bottom grid with the two line plots
bottom_grid <- grid.arrange(
  alternations_line_plot, repetitions_line_plot,  # Second row: 2 plots
  widths = c(0.46, 0.54),  # 55% width for the first plot, 45% for the second plot
  ncol = 2  # Two columns in the bottom row
)

# Step 3: Combine the top and bottom grids vertically
final_plot <- plot_grid(top_grid, bottom_grid, ncol = 1, rel_heights = c(1, 1))

# Step 4: Save the combined plot as a PDF
ggsave("grid_ymaze.pdf", plot = final_plot, width = 18, height = 10)


######################## NEW SECTION: BOLDNESS PARAMETERS AND T-TESTS ##############################
# Custom colors for bars
custom_colors <- c("Bold" =  "#00008B", "Shy" = "#FF8C00")
lighter_dot_colors <- c("Bold" = "#4169E1", "Shy" = "#ffbb78")  # Lighter shades for individual points

# 1. Create simplified for Distance..m., Max.speed..m.s., Absolute.turn.angle...., and Small...mean.distance.from..m.

# Distance Travelled plot (Distance..m.)
distance_travelled_plot <- ggplot(full_data, aes(x=boldness_group, y=`Distance..m.`, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  geom_jitter(aes(color=boldness_group), width=0.1, size=2) + 
  labs(y="Distance Travelled (m)", title="Distance Travelled by Boldness Group") +
  scale_fill_manual(values=custom_colors) +
  scale_color_manual(values=lighter_dot_colors) +
  theme_classic() + theme(legend.position = "none")  # Remove legend


# Max Speed plot (Max.speed..m.s.)
max_speed_plot <- ggplot(full_data, aes(x=boldness_group, y=`Max.speed..m.s.`, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  geom_jitter(aes(color=boldness_group), width=0.1, size=2) + 
  labs(y="Max Speed (m/s)", title="Max Speed by Boldness Group") +
  scale_fill_manual(values=custom_colors) +
  scale_color_manual(values=lighter_dot_colors) +
  theme_classic() +   theme(legend.position = "none")  # Remove legend

# Load the scales package
library(scales)
# Absolute Turn Angle plot (Absolute.turn.angle....)
turn_angle_plot <- ggplot(full_data, aes(x=boldness_group, y=`Absolute.turn.angle....`, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  geom_jitter(aes(color=boldness_group), width=0.1, size=2) + 
  labs(y="Absolute Turn Angle (°)", title="Absolute Turn Angle by Boldness Group") +
  scale_fill_manual(values=custom_colors) +
  scale_color_manual(values=lighter_dot_colors) +
  scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, by = 20000), labels = label_number()) +  # Format y-axis labels as plain numbers
  theme_classic() +
  theme(legend.position = "none")  # Remove legend


# Distance from Bottom plot (Small...mean.distance.from..m.)
distance_from_bottom_plot <- ggplot(full_data, aes(x=boldness_group, y=`Small...mean.distance.from..m.`, fill=boldness_group)) +
  geom_bar(stat="summary", fun="mean", width=0.7) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  geom_jitter(aes(color=boldness_group), width=0.1, size=2) + 
  labs(y="Distance from Bottom (m)", title="Distance from Bottom by Boldness Group") +
  scale_fill_manual(values=custom_colors) +
  scale_color_manual(values=lighter_dot_colors) +
  scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, by = 0.02)) +  # Manually scale y-axis
  theme_classic() +
  theme(legend.position = "none")  # Remove legend


# 2. Perform T-tests between Low boldness and High Boldness Groups

# Distance Travelled T-test (Student's t-test)
t_test_distance_travelled <- t.test(`Distance..m.` ~ boldness_group, data = full_data, var.equal = TRUE)
print(t_test_distance_travelled)

# Max Speed T-test (Student's t-test)
t_test_max_speed <- t.test(`Max.speed..m.s.` ~ boldness_group, data = full_data, var.equal = TRUE)
print(t_test_max_speed)

# Absolute Turn Angle T-test (Student's t-test)
t_test_turn_angle <- t.test(`Absolute.turn.angle....` ~ boldness_group, data = full_data, var.equal = TRUE)
print(t_test_turn_angle)

# Distance from Bottom T-test (Student's t-test)
t_test_distance_from_bottom <- t.test(`Small...mean.distance.from..m.` ~ boldness_group, data = full_data, var.equal = TRUE)
print(t_test_distance_from_bottom)


# Create a grid of the 4 plots
grid_plot <- grid.arrange(
  distance_travelled_plot, max_speed_plot, 
  turn_angle_plot, distance_from_bottom_plot, 
  ncol = 2  # Arrange the plots in 2 columns and 2 rows
)

# Save the combined grid as a PDF
ggsave("grid_boldness.pdf", plot = grid_plot, width = 12, height = 10)



####################################TETRAGRAMS####################################################

# Adjust this based on the actual column names
# Assuming 'Group' and 'boldness_group' are present and the columns 18-33 are your tetragrams
# Ensure Group and boldness_group are treated as factors
full_data$Group <- factor(full_data$Group)
full_data$boldness_group <- factor(full_data$boldness_group)

# Custom colors for Bold and Shy
custom_colors <- c("Bold" =  "#00008B", "Shy" = "#FF8C00")

# Extract only columns 18 to 33 for tetragrams along with Group and boldness_group
tetragram_data <- full_data[, c(36, 37, 18:33)]

# Melt the data for easier plotting
tetragram_data_melted <- melt(tetragram_data, id.vars = c("Group", "boldness_group"))

# Calculate mean and standard error for each tetragram, Group, and boldness_group
tetragram_summary <- tetragram_data_melted %>%
  group_by(Group, boldness_group, variable) %>%
  summarize(
    mean_value = mean(value, na.rm = TRUE),
    se_value = sd(value, na.rm = TRUE) / sqrt(n())
  )

# Plot the tetragrams for Ctrl and CAS, separated by Boldness Group with mean and error bars
tetragram_plot <- ggplot(tetragram_summary, aes(x=variable, y=mean_value, fill=boldness_group)) +
  geom_bar(stat="identity", position="dodge", width=0.7) +
  geom_errorbar(aes(ymin = mean_value - se_value, ymax = mean_value + se_value), 
                position=position_dodge(0.7), width=0.2) +
  facet_grid(. ~ Group) +  # Separate plots for CTRL and CAS
  scale_fill_manual(values=custom_colors) +
  labs(y="Frequency of Choice", x="Tetragram", title="Tetragram Choices by Group and Profile") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Save the plot as PDF
ggsave("tetragram_mean_se_plot.pdf", plot = tetragram_plot, width = 14, height = 5)


###########Stats tetragrams#############################
# Step 1: Subset the data by Group (CTRL and CAS)
ctrl_data <- subset(tetragram_data_melted, Group == "CTRL")
cas_data <- subset(tetragram_data_melted, Group == "CAS")

# Step 2: Run Two-Way ANOVA for CTRL group (with interaction term)
anova_ctrl <- aov(value ~ boldness_group * variable, data = ctrl_data)
summary(anova_ctrl)

# Step 3: Post hoc test for CTRL group (specific comparisons for each tetragram)
posthoc_ctrl <- emmeans(anova_ctrl, pairwise ~ boldness_group | variable)
summary(posthoc_ctrl)

# Step 4: Run Two-Way ANOVA for CAS group (with interaction term)
anova_cas <- aov(value ~ boldness_group * variable, data = cas_data)
summary(anova_cas)

# Step 5: Post hoc test for CAS group (specific comparisons for each tetragram)
posthoc_cas <- emmeans(anova_cas, pairwise ~ boldness_group | variable)
summary(posthoc_cas)



######################SCATTER PLOT#######################################
## Assuming you have a column named 'boldness_index' in your dataset
full_data$boldness_index <- 1 - full_data$boldness_index

# Ensure Sex is a factor
full_data$Sex <- factor(full_data$Sex, levels = c("F", "M"))

# Define a cutoff value for the boldness index to separate Bold and Shy groups
cutoff_value <- median(full_data$boldness_index)  # You can modify this based on your experiment

# Scatter plot for the boldness index with different shapes for females and males
boldness_index_plot <- ggplot(full_data, aes(x = boldness_group, y = boldness_index, color = boldness_group, shape = Sex)) +
  geom_point(position = position_jitter(width = 0.45), size = 5, alpha = 0.7) +  # Add jitter for visibility
  geom_hline(yintercept = cutoff_value, linetype = "dashed", color = "grey", size = 1) +  # Add cutoff line
  labs(title = "Distribution of Boldness Index", y = "Boldness Index", x = "Boldness Group") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +  # Set y-axis from 0 to 1 with intervals of 0.2
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  scale_shape_manual(values = c("F" = 16, "M" = 17)) +  # Different shapes: circles for females, triangles for males
  theme_classic()

# Show the plot
print(boldness_index_plot)

ggsave("boldnessindex_plot.pdf", plot = boldness_index_plot, width = 8, height = 8)




##################################SEX#######################################
# Count the number of observations by Sex and Boldness Group
n_counts <- full_data %>%
  group_by(Sex, boldness_group) %>%
  summarise(n = n())

# Print the counts
print(n_counts)

# Define custom colors for darker purple and green tones for bars and lighter tones for dots
fill_colors <- c("F" = "#4B0082", "M" = "#006400")  # Dark purple for female, dark green for male
dot_colors <- c("F" = "#9370DB", "M" = "#32CD32")   # Lighter purple for female, lighter green for male

# Distance Traveled plot (0 to 30 by 5)
distance_travelled_plot_sex_boldness <- ggplot(full_data, aes(x=boldness_group, y=`Distance..m.`, fill=Sex)) +
  geom_bar(stat="summary", fun="mean", position=position_dodge(width=0.7), width=0.6) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=Sex), position=position_jitterdodge(jitter.width=0.3, dodge.width=0.7), size=2) + 
  labs(y="Distance Travelled (m)", x="Boldness Group (Bold/Shy)", title="Distance Travelled by Sex and Boldness Group") +
  scale_fill_manual(values=fill_colors) +  
  scale_color_manual(values=dot_colors) +  
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5), expand = c(0, 0)) +  # Set scale from 0 to 30 by 5
  theme_classic()

# Max Speed plot (0.00 to 0.20 by 0.05)
max_speed_plot_sex_boldness <- ggplot(full_data, aes(x=boldness_group, y=`Max.speed..m.s.`, fill=Sex)) +
  geom_bar(stat="summary", fun="mean", position=position_dodge(width=0.7), width=0.6) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=Sex), position=position_jitterdodge(jitter.width=0.3, dodge.width=0.7), size=2) + 
  labs(y="Max Speed (m/s)", x="Boldness Group (Bold/Shy)", title="Max Speed by Sex and Boldness Group") +
  scale_fill_manual(values=fill_colors) +  
  scale_color_manual(values=dot_colors) +  
  scale_y_continuous(limits = c(0.00, 0.20), breaks = seq(0.00, 0.20, 0.05), expand = c(0, 0)) +  # Set scale from 0.00 to 0.20 by 0.05
  theme_classic()

# Absolute Turn Angle plot (0 to 100000 by 20000, with standard number formatting)
turn_angle_plot_sex_boldness <- ggplot(full_data, aes(x=boldness_group, y=`Absolute.turn.angle....`, fill=Sex)) +
  geom_bar(stat="summary", fun="mean", position=position_dodge(width=0.7), width=0.6) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=Sex), position=position_jitterdodge(jitter.width=0.3, dodge.width=0.7), size=2) + 
  labs(y="Absolute Turn Angle (degrees)", x="Boldness Group (Bold/Shy)", title="Absolute Turn Angle by Sex and Boldness Group") +
  scale_fill_manual(values=fill_colors) +  
  scale_color_manual(values=dot_colors) +  
  scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, 20000), labels = scales::comma, expand = c(0, 0)) +  # Use comma notation for y-axis labels
  theme_classic()


# Distance from Bottom plot (0.00 to 0.10 by 0.02)
distance_from_bottom_plot_sex_boldness <- ggplot(full_data, aes(x=boldness_group, y=`Small...mean.distance.from..m.`, fill=Sex)) +
  geom_bar(stat="summary", fun="mean", position=position_dodge(width=0.7), width=0.6) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=Sex), position=position_jitterdodge(jitter.width=0.3, dodge.width=0.7), size=2) + 
  labs(y="Distance from Bottom (m)", x="Boldness Group (Bold/Shy)", title="Distance from Bottom by Sex and boldness Group") +
  scale_fill_manual(values=fill_colors) +  
  scale_color_manual(values=dot_colors) +  
  scale_y_continuous(limits = c(0.00, 0.10), breaks = seq(0.00, 0.10, 0.02), expand = c(0, 0)) +  # Set scale from 0.00 to 0.10 by 0.02
  theme_classic()


# Arrange all plots in a 2x2 grid
sex_grid <- grid.arrange(distance_travelled_plot_sex_boldness, 
             max_speed_plot_sex_boldness, 
             turn_angle_plot_sex_boldness, 
             distance_from_bottom_plot_sex_boldness, 
             ncol = 2, nrow = 2)

ggsave("sexgrid.pdf", plot = sex_grid, width = 12, height = 8)

# Boldness Index plot by Sex
boldness_index_plot <- ggplot(full_data, aes(x=Sex, y=`boldness_index`, fill=Sex)) +
  geom_bar(stat="summary", fun="mean", position=position_dodge(width=0.7), width=0.6) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(0.7), width=0.2) +
  geom_jitter(aes(color=Sex), position=position_jitter(width=0.2), size=2) + 
  labs(y="Boldness Index", x="Sex", title="Boldness Index by Sex (Male vs. Female)") +
  scale_fill_manual(values=fill_colors) +  
  scale_color_manual(values=dot_colors) +  
  scale_y_continuous(expand = c(0, 0)) +  # Adjust y-axis to start at 0
  theme_classic()

# Print the plot
print(boldness_index_plot)

# Perform a t-test for Boldness Index between males and females
t_test_result <- t.test(boldness_index ~ Sex, data = full_data, var.equal = TRUE)

# Print the t-test results
print(t_test_result)

# ANOVA for Distance from Bottom with Sex and boldness Group
anova_distance_bottom_sex_boldness <- aov(`Small...mean.distance.from..m.` ~ boldness_group * Sex, data = full_data)
summary(anova_distance_bottom_sex_boldness)

# Post-hoc for Distance from Bottom
posthoc_distance_bottom <- emmeans(anova_distance_bottom_sex_boldness, pairwise ~ boldness_group * Sex, adjust = "none")
summary(posthoc_distance_bottom)

###################REPRESENTATION
# Repetitions vs Alternations with shapes for CAS and CTRL
repetitions_vs_alternations <- ggplot(full_data, aes(x = alternations, y = repetitions, color = boldness_group, shape = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Alternations vs Repetitions", x = "Alternations", y = "Repetitions") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = c("CTRL" = 16, "CAS" = 17)) +  # Customize shapes: 16 for circle, 17 for triangle
  theme_classic()

# Display the plot
print(repetitions_vs_alternations)

# Optionally save the plot as an image
ggsave("Repetitions_vs_Alternations.pdf", plot = repetitions_vs_alternations, width = 8, height = 6)

# Boldness Index vs Alternations with shapes for CAS and CTRL
boldness_vs_alternations <- ggplot(full_data, aes(x = boldness_index, y = alternations, color = boldness_group, shape = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Boldness Index vs Alternations", x = "Boldness Index", y = "Alternations") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = c("CTRL" = 16, "CAS" = 17)) +  # Customize shapes: 16 for circle, 17 for triangle
  theme_classic()

# Boldness Index vs Repetitions with shapes for CAS and CTRL
boldness_vs_repetitions <- ggplot(full_data, aes(x = boldness_index, y = repetitions, color = boldness_group, shape = Group)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Boldness Index vs Repetitions", x = "Boldness Index", y = "Repetitions") +
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = c("CTRL" = 16, "CAS" = 17)) +  # Customize shapes: 16 for circle, 17 for triangle
  theme_classic()

# Display the plots
print(boldness_vs_alternations)
print(boldness_vs_repetitions)

# Optionally save the plots as images
ggsave("Boldness_vs_Alternations.pdf", plot = boldness_vs_alternations, width = 5, height = 6)
ggsave("Boldness_vs_Repetitions.pdf", plot = boldness_vs_repetitions, width = 5, height = 6)

