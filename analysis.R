rm(list = ls())

setwd("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data")
setwd("E:/data/Statistics/Data")

# ----------------------------------------------------------------------------------------- #
#  ------------------------------------ loading packages ---------------------------------- #
# ----------------------------------------------------------------------------------------- #

# packages to load
packages_needed <- c('readxl', 'writexl', 'dplyr', 'ggplot2', "corrplot",
                     'tidyverse', 'tidyquant', 'ggdist', 'ggthemes',
                     'rstatix', 'lme4', 'fitdistrplus', 'lsr', 'pwrss',
                     'emmeans', 'performance', 'gridExtra', 'simr', 'pastecs',
                     'flexplot', 'influence.ME', 'MuMIn', 'gridGraphics', 
                     'DHARMa', 'lmerTest', 'sjPlot', 'ggeffects', 'cAIC4', 
                     'ggmosaic', 'modeest', 'apaTables', 'rempsyc', 'officer', 
                     'flextable')

lapply(packages_needed, FUN = require, character.only = T)


# Create a function to retrieve package information
get_package_info <- function(package_name) {
  if (requireNamespace(package_name, quietly = TRUE)) {
    pkg_desc <- packageDescription(package_name)
    
    package_info <- data.frame(
      Package = package_name,
      Description = ifelse("Title" %in% names(pkg_desc), pkg_desc[["Title"]], NA),
      Version = ifelse("Version" %in% names(pkg_desc), pkg_desc[["Version"]], NA)
    )
    return(package_info)
  } else {
    return(NULL)
  }
}

# Get information for each loaded package
package_info <- lapply(packages_needed, get_package_info)

# Filter out NULL values (packages that weren't loaded)
package_info <- Filter(Negate(is.null), package_info)

# Combine the information into a single data frame
package_info_df <- do.call(rbind, package_info)

# Make a table
writexl::write_xlsx(package_info_df, "package_info_slavikova.xlsx")
packages <- read_excel("package_info_slavikova.xlsx")

print(packages)
packages %>% as_tibble() %>% print(n = nrow(packages))

# ----------------------------------------------------------------------------------------- #
#  ------------------------------------  data import -------------------------------------- #
# ----------------------------------------------------------------------------------------- #

data <- readxl::read_excel("E:/data/Statistics/Data/slavikova_vysledky_update.xlsx", sheet = "raw")
data <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/slavikova_vysledky_update.xlsx", sheet = "raw")

View(data)
str(data)

#data <- data %>%
#  mutate_at(c("abd_right_pre", "abd_right_post", "abd_left_pre", "abd_left_post",
 #             "flex_right_pre", "flex_right_post", "flex_left_pre", "flex_left_post",
  #            "ext_right_pre", "ext_right_post", "ext_left_pre", "ext_left_post"), as.numeric)
str(data)


# ----------------------------------------------------------------------------------------- #
# -------------------------------------- data wrangling ----------------------------------- #
# ----------------------------------------------------------------------------------------- #

# --------------------------------------------------------- #
# ----------------- data extend abduction ----------------- #
# --------------------------------------------------------- #

# right
data_abd_right <- data[, c("ID", "abd_right_pre", "abd_right_post", 
                           "performance_level",
                           "height", "weight", 
                           "age", "length_of_practice", 
                           "dominance", "group")]
print(data_abd_right)

long_data_abd_right <- data_abd_right %>%
  pivot_longer(
    cols = c(abd_right_pre, abd_right_post),
    names_to = "pre_post",
    values_to = "abduction",
    names_prefix = "abd_right_"
  )

long_data_abd_right$pre_post <- setNames(c('PRE', 'POST'),
                                         c('pre', 'post'))[long_data_abd_right$pre_post]
View(long_data_abd_right)

# left
data_abd_left <- data[, c("ID", "abd_left_pre", "abd_left_post", "performance_level")]
print(data_abd_left)

long_data_abd_left <- data_abd_left %>%
  pivot_longer(
    cols = c(abd_left_pre, abd_left_post),
    names_to = "pre_post",
    values_to = "abduction",
    names_prefix = "abd_left_"
  )

long_data_abd_left$pre_post <- setNames(c('PRE', 'POST'),
                                        c('pre', 'post'))[long_data_abd_left$pre_post]
View(long_data_abd_left)

# --------------------------------------------------------- #
# ----------------- data extend flexion ------------------- #
# --------------------------------------------------------- #

data_flex_right <- data[, c("ID", "flex_right_pre", "flex_right_post", "performance_level")]
print(data_flex_right)

long_data_flex_right <- data_flex_right %>%
  pivot_longer(
    cols = c(flex_right_pre, flex_right_post),
    names_to = "pre_post",
    values_to = "flexion",
    names_prefix = "flex_right_"
  )

long_data_flex_right$pre_post <- setNames(c('PRE', 'POST'),
                                          c('pre', 'post'))[long_data_flex_right$pre_post]
View(long_data_flex_right)

# left
data_flex_left <- data[, c("ID", "flex_left_pre", "flex_left_post", "performance_level")]
print(data_flex_left)

long_data_flex_left <- data_flex_left %>%
  pivot_longer(
    cols = c(flex_left_pre, flex_left_post),
    names_to = "pre_post",
    values_to = "flexion",
    names_prefix = "flex_left_"
  )

long_data_flex_left$pre_post <- setNames(c('PRE', 'POST'),
                                         c('pre', 'post'))[long_data_flex_left$pre_post]
View(long_data_flex_left)

# --------------------------------------------------------- #
# ----------------- data extend extension ----------------- #
# --------------------------------------------------------- #

data_ext_right <- data[, c("ID", "ext_right_pre", "ext_right_post", "performance_level")]
print(data_ext_right)

long_data_ext_right <- data_ext_right %>%
  pivot_longer(
    cols = c(ext_right_pre, ext_right_post),
    names_to = "pre_post",
    values_to = "extension",
    names_prefix = "ext_right_"
  )

long_data_ext_right$pre_post <- setNames(c('PRE', 'POST'),
                                         c('pre', 'post'))[long_data_ext_right$pre_post]
View(long_data_ext_right)

# left
data_ext_left <- data[, c("ID", "ext_left_pre", "ext_left_post", "performance_level")]
print(data_ext_left)

long_data_ext_left <- data_ext_left %>%
  pivot_longer(
    cols = c(ext_left_pre, ext_left_post),
    names_to = "pre_post",
    values_to = "extension",
    names_prefix = "ext_left_"
  )

long_data_ext_left$pre_post <- setNames(c('PRE', 'POST'),
                                        c('pre', 'post'))[long_data_ext_left$pre_post]
View(long_data_ext_left)

# ------------------- joining the data -------------------- #

data_long_all <- cbind(long_data_abd_right,
      long_data_abd_left$abduction, 
      long_data_flex_right$flexion, 
      long_data_flex_left$flexion, 
      long_data_ext_right$extension, 
      long_data_ext_left$extension)

data_long_all <- data_long_all %>%
  rename("abduction_R" = "abduction", 
         "abduction_L" = "long_data_abd_left$abduction",
         "flexion_R" = "long_data_flex_right$flexion",
         "flexion_L" = "long_data_flex_left$flexion",
         "extension_R" = "long_data_ext_right$extension",
         "extension_L" = "long_data_ext_left$extension")

# --------------------------------------------------------- #
# ----------------- data extend included all -------------- #
# --------------------------------------------------------- #


data_long_all2 <- data %>%
  pivot_longer(cols = c(abd_right_pre, abd_right_post, abd_left_pre, abd_left_post, 
                        flex_right_pre, flex_right_post, flex_left_pre, flex_left_post,
                        ext_right_pre, ext_right_post, ext_left_pre, ext_left_post),
               names_to = c("test", "laterality", "pre_post"),
               names_pattern = "(abd|flex|ext)_(right|left)_(pre|post)",
               values_to = "value")

# Convert "pre_post" column to uppercase for consistency
data_long_all2$pre_post <- toupper(data_long_all2$pre_post)

# --------------------------------------------------------- #
# ---- data extend based on pre_post and laterality ------- #
# --------------------------------------------------------- #

data_long_all3 <- data %>%
  pivot_longer(cols = c(abd_right_pre, abd_right_post, abd_left_pre, abd_left_post, 
                        flex_right_pre, flex_right_post, flex_left_pre, flex_left_post,
                        ext_right_pre, ext_right_post, ext_left_pre, ext_left_post),
               names_to = c(".value", "laterality", "pre_post"),
               names_pattern = "(abd|flex|ext)_(right|left)_(pre|post)")

# Convert "pre_post" column to uppercase for consistency
data_long_all3$pre_post <- toupper(data_long_all3$pre_post)

print(data_long_all3)

data_long_all3 <- data_long_all3 %>%
  rename(
         "abduction" = "abd",
         "flexion" = "flex",
         "extension" = "ext"
  )

print(data_long_all3)

# --------------------------------------------------------- #
# ----------------- data save and reload ------------------ #
# --------------------------------------------------------- #

# write wrangled data

writexl::write_xlsx(long_data_abd_right, 'long_data_abd_right.xlsx')
writexl::write_xlsx(long_data_abd_left, 'long_data_abd_left.xlsx')
writexl::write_xlsx(long_data_flex_right, 'long_data_flex_right.xlsx')
writexl::write_xlsx(long_data_flex_left, 'long_data_flex_left.xlsx')
writexl::write_xlsx(long_data_ext_right, 'long_data_ext_right.xlsx')
writexl::write_xlsx(long_data_ext_left, 'long_data_ext_left.xlsx')
writexl::write_xlsx(data_long_all, 'data_long_all.xlsx')
writexl::write_xlsx(data_long_all2, 'data_long_all2.xlsx')
writexl::write_xlsx(data_long_all3, 'data_long_all3.xlsx')

# load wrangled data

rm(list = ls())

data_raw <- readxl::read_excel("E:/data/Statistics/Data/slavikova_vysledky_update.xlsx", sheet = "raw")
long_data_abd_right <- readxl::read_excel("E:/data/Statistics/Data/long_data_abd_right.xlsx")
long_data_abd_left <- readxl::read_excel("E:/data/Statistics/Data/long_data_abd_left.xlsx")
long_data_flex_right <- readxl::read_excel("E:/data/Statistics/Data/long_data_flex_right.xlsx")
long_data_flex_left <- readxl::read_excel("E:/data/Statistics/Data/long_data_flex_left.xlsx")
long_data_ext_right <- readxl::read_excel("E:/data/Statistics/Data/long_data_ext_right.xlsx")
long_data_ext_left <- readxl::read_excel("E:/data/Statistics/Data/long_data_ext_left.xlsx")
long_data_all <- readxl::read_excel("E:/data/Statistics/Data/data_long_all.xlsx")
long_data_all2 <- readxl::read_excel("E:/data/Statistics/Data/data_long_all2.xlsx")
long_data_all3 <- readxl::read_excel("E:/data/Statistics/Data/data_long_all3.xlsx")

# OR

data_raw <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/slavikova_vysledky_update.xlsx", sheet = "raw")
long_data_abd_right <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_abd_right.xlsx")
long_data_abd_left <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_abd_left.xlsx")
long_data_flex_right <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_flex_right.xlsx")
long_data_flex_left <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_flex_left.xlsx")
long_data_ext_right <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_ext_right.xlsx")
long_data_ext_left <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/long_data_ext_left.xlsx")
long_data_all <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/data_long_all.xlsx")
long_data_all2 <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/data_long_all2.xlsx")
long_data_all3 <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/data_long_all3.xlsx")

long_data_all$abduction_R <- as.numeric(long_data_all$abduction_R)
long_data_all$abduction_L <- as.numeric(long_data_all$abduction_L)
long_data_all$flexion_R <- as.numeric(long_data_all$flexion_R)
long_data_all$flexion_L <- as.numeric(long_data_all$flexion_L)
long_data_all$extension_R <- as.numeric(long_data_all$extension_R)
long_data_all$extension_L <- as.numeric(long_data_all$extension_L)

str(long_data_all3)

long_data_all3$abduction <- as.numeric(long_data_all3$abduction)
long_data_all3$flexion <- as.numeric(long_data_all3$flexion)
long_data_all3$extension <- as.numeric(long_data_all3$extension)

data_raw$abd_right_pre <- as.numeric(data_raw$abd_right_pre)
data_raw$abd_right_post <- as.numeric(data_raw$abd_right_post)
data_raw$abd_left_pre <- as.numeric(data_raw$abd_left_pre)
data_raw$abd_left_post <- as.numeric(data_raw$abd_left_post)

data_raw$flex_right_pre <- as.numeric(data_raw$flex_right_pre)
data_raw$flex_right_post <- as.numeric(data_raw$flex_right_post)
data_raw$flex_left_pre <- as.numeric(data_raw$flex_left_pre)
data_raw$flex_left_post <- as.numeric(data_raw$flex_left_post)

data_raw$ext_right_pre <- as.numeric(data_raw$ext_right_pre)
data_raw$ext_right_post <- as.numeric(data_raw$ext_right_post)
data_raw$ext_left_pre <- as.numeric(data_raw$ext_left_pre)
data_raw$ext_left_post <- as.numeric(data_raw$ext_left_post)

long_data_abd_right$abduction <- as.numeric(long_data_abd_right$abduction)
long_data_abd_left$abduction <- as.numeric(long_data_abd_left$abduction)
long_data_flex_right$flexion <- as.numeric(long_data_flex_right$flexion)
long_data_flex_left$flexion <- as.numeric(long_data_flex_left$flexion)
long_data_ext_right$extension <- as.numeric(long_data_ext_right$extension)
long_data_ext_left$extension <- as.numeric(long_data_ext_left$extension)

# ----------------------------------------------------------------------------------------- #
# -------------------------------------- descriptive stats -------------------------------- #
# ----------------------------------------------------------------------------------------- #

descriptive <- round(pastecs::stat.desc(data_raw[, c(2, 4:6, 8, 10:21)], p = 0.95, norm = T), 2)
descriptive <- as.data.frame(descriptive)

(descriptive_body <- descriptive[c(4:6, 8:10, 13), c(2:17)])
print(descriptive_body)

descriptive_body_normality <- descriptive_body %>%
  shapiro_test(
    height, weight, age, 
    length_of_practice, 
    abd_right_pre, 
    abd_right_post, 
    abd_left_pre, 
    abd_left_post,
    flex_right_pre, 
    flex_right_post, 
    flex_left_pre, 
    flex_left_post,
    ext_right_pre, 
    ext_right_post, 
    ext_left_pre, 
    ext_left_post
  )

print(descriptive_body_normality)

(descriptive_body <- as.data.frame(t(descriptive_body)))

descriptive_body$variable <- rownames(descriptive_body)
print(descriptive_body)

descriptive_normality_ordered <- descriptive_body_normality[match(descriptive_body$variable, 
                                                     descriptive_body_normality$variable), ]

print(descriptive_body)

descriptive_body <- cbind(descriptive_body[, c(8, 1:7)], descriptive_normality_ordered[, c("statistic","p")])

print(descriptive_body)
median(data_raw$height)

descriptive_body <- descriptive_body %>%
  mutate(variable = recode(variable, height = 'Height (cm)',
                           weight = 'Weight (kg)', 
                           age = 'Age (years)',
                           length_of_practice = 'Length of practice (years)',
                           abd_right_pre = 'Right abduction (°) pre-test',
                           abd_right_post = 'Right abduction (°) post-test',
                           abd_left_pre = 'Left abduction (°) pre-test',
                           abd_left_post = 'Left abduction (°) post-test',
                           flex_right_pre = 'Right flexion (°) pre-test',
                           flex_right_post = 'Right flexion (°) post-test',
                           flex_left_pre = 'Left flexion (°) pre-test',
                           flex_left_post = 'Left flexion (°) post-test',
                           ext_right_pre = 'Right extension (°) pre-test',
                           ext_right_post = 'Right extension (°) post-test',
                           ext_left_pre = 'Left extension (°) pre-test',
                           ext_left_post = 'Left extension (°) post-test'))

print(descriptive_body)

descriptive_body <- descriptive_body[, c(1, 6, 8, 2, 3, 9,10)]
print(descriptive_body)


# writexl::write_xlsx(descriptive_body, "descriptives_hip_joint.xlsx")

descriptive_body <- rempsyc::nice_table(descriptive_body)

print(descriptive_body)

class(descriptive_body)

doc <- read_docx() |> 
  body_add_flextable(value = descriptive_body) |> 
  body_add_par("", style = "Normal")

# Save the document
print(doc, target = "descriptive_body_hip_joint_apa.docx")

# ------------------------- Correlations among the measurements --------------------------- #

descriptive_table <- apaTables::apa.cor.table(data = data_raw[, c(10:21)], table.number = 1)
print(descriptive_table)

IQR(data_raw$height)

# ----------------------------------------------------------------------------------------- #
# ---------------------- descriptive stats - experience level and age --------------------- #
# ----------------------------------------------------------------------------------------- #
# library(ggmosaic)

# Proportions of age and performance levels of all participants.

ggplot2::ggplot(data = data_raw, aes(x = age,fill = performance_level)) + 
  geom_density(position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("#E6AB02", "#66A61E", "#7570B3", "#666666")) + 
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  labs(y = "Proportion", x = "Age", fill = "Performance Level") +
  theme_minimal()

table(data_raw$age)

data_mosaic <- data_raw %>%
  group_by(age, performance_level) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(prop = count / sum(count), total_age_count = sum(count)) %>%
  arrange(age, performance_level) %>%
  mutate(ymin = lag(cumsum(prop), default = 0), ymax = cumsum(prop))

ggplot(data_mosaic, aes(xmin = age - 0.5, xmax = age + 0.5, ymin = ymin, ymax = ymax, fill = performance_level)) +
  geom_rect(color = "white") +  
  geom_text(aes(x = age, y = (ymin + ymax) / 2, label = count), color = "white", fontface = "bold") + 
  scale_fill_manual(values = c("#E6AB02", "#66A61E", "#7570B3", "#666666")) +
  scale_x_continuous(breaks = unique(data_mosaic$age)) +  
  labs(x = "Age", y = "Proportion", fill = "Performance Level") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.title.y = element_blank()   
  )


ggplot2::ggplot(data = data_raw) +
  geom_mosaic(aes(x = product(age), fill = performance_level)) +
  theme_mosaic() + 
  theme(
    panel.grid = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),  
    axis.title.y = element_blank()   
  ) + labs(fill = "Performance Level") +
  scale_fill_manual(values = c("#4575B4", "#666666", "#FEE090", "#F46D43")) 


# --------------------------- This one for manuscript ---------------------------- #
p <- ggplot(data_raw) +
  geom_mosaic(aes(x = product(age), fill = performance_level), na.rm = TRUE, show.legend = F) +
  theme_mosaic() + 
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) + 
  labs(fill = "Performance Level") +
  scale_fill_manual(values = c("#4575B4", "#666666", "#FEE090", "#F46D43"))

# Extract tile positions and counts
gb <- ggplot_build(p)
tile_data <- gb$data[[1]]

# Prepare count labels (only if .wt > 0)
label_data <- tile_data %>%
  mutate(
    x = (xmin + xmax) / 2,
    y = (ymin + ymax) / 2,
    label = ifelse(.wt == 0, NA, as.character(.wt))  # raw counts only
  )

# Add count labels
plot_descr_1 <- p + geom_text(
  data = label_data,
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  size = 5  # increase size
)

print(plot_descr_1)

# ----------------------------------------------------------------------------------------- #

p2 <- ggplot(data_raw) +
  geom_mosaic(aes(x = product(group), fill = performance_level), na.rm = TRUE) +
  theme_mosaic() + 
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) + 
  labs(fill = "Performance Level") +
  scale_fill_manual(values = c("#4575B4", "#666666", "#FEE090", "#F46D43"))

# Extract tile positions and counts
g2 <- ggplot_build(p2)
tile_data <- g2$data[[1]]

# Prepare count labels (only if .wt > 0)
label_data <- tile_data %>%
  mutate(
    x = (xmin + xmax) / 2,
    y = (ymin + ymax) / 2,
    label = ifelse(.wt == 0, NA, as.character(.wt))  # raw counts only
  )

# Add count labels
plot_descr_2 <- p2 + geom_text(
  data = label_data,
  aes(x = x, y = y, label = label),
  inherit.aes = FALSE,
  size = 5  # increase size
)

print(plot_descr_2)


grid.arrange(plot_descr_1, plot_descr_2, ncol = 2)


# Note: The numbers in each rectangle show the number of participants.


table(data_raw$performance_level)
table(data_raw$laterality)

(table_height <- data_raw %>%
  group_by(performance_level) %>%
  summarise(
    mean_height = mean(height, na.rm = TRUE),
    sd_height = sd(height, na.rm = TRUE),
    min_height = min(height, na.rm = TRUE),
    max_height = max(height, na.rm = TRUE),
    n = n()
  ))

(table_weight <- data_raw %>%
  group_by(performance_level) %>%
  summarise(
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight = sd(weight, na.rm = TRUE),
    min_weight = min(weight, na.rm = TRUE),
    max_weight = max(weight, na.rm = TRUE),
    n = n()
  ))

# ----------------------------------------------------------------------------------------- #
# -------------------------------------- raw data density --------------------------------- #
# ----------------------------------------------------------------------------------------- #


# density of the data
dens_abd_right_pre <- ggplot(data = data_raw, aes(x = abd_right_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - right abduction ()", y = "Density")
dens_abd_right_post <- ggplot(data = data_raw, aes(x = abd_right_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - right abduction (?)", y = "Density")
dens_abd_left_pre <- ggplot(data = data_raw, aes(x = abd_left_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - left abduction (?)", y = "Density")
dens_abd_left_post <- ggplot(data = data_raw, aes(x = abd_left_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - left abduction (?)", y = "Density")
dens_flex_right_pre <- ggplot(data = data_raw, aes(x = flex_right_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - right flexion (?)", y = "Density")
dens_flex_right_post <- ggplot(data = data_raw, aes(x = flex_right_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - right flexion (?)", y = "Density")
dens_flex_left_pre <- ggplot(data = data_raw, aes(x = flex_left_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - left flexion (?)", y = "Density")
dens_flex_left_post <- ggplot(data = data_raw, aes(x = flex_left_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - left flexion (?)", y = "Density")
dens_ext_right_pre <- ggplot(data = data_raw, aes(x = ext_right_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - right extension (?)", y = "Density")
dens_ext_right_post <- ggplot(data = data_raw, aes(x = ext_right_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - right extension (?)", y = "Density")
dens_ext_left_pre <- ggplot(data = data_raw, aes(x = ext_left_pre)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="pre test - left extension (?)", y = "Density")
dens_ext_left_post <- ggplot(data = data_raw, aes(x = ext_left_post)) + geom_density(color="darkblue", fill="lightblue") + 
  theme_classic() + labs(x="post test - left extension (?)", y = "Density")

grid.arrange(dens_abd_right_pre, dens_ext_left_post, dens_abd_left_pre, dens_abd_left_post,
             dens_flex_right_pre, dens_flex_right_post, dens_flex_left_pre, dens_flex_left_post,
             dens_ext_right_pre, dens_ext_right_post, dens_ext_left_pre, dens_ext_left_post, ncol=4)

# ----------------------------------------------------------------------------------------- #
# ---------------------- data visualization - rain cloud plots ---------------------------- #
# ----------------------------------------------------------------------------------------- #
# ---------------------------------- rain cloud plots ------------------------------------- #

long_data_abd_right$pre_post <- factor(long_data_abd_right$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_abd_right_plot <- long_data_abd_right %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = abduction, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5, position = position_dodge(width = 2.5)
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("abduction right (°)") + 
  scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))


long_data_abd_left$pre_post <- factor(long_data_abd_left$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_abd_left_plot <- long_data_abd_left %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = abduction, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("abduction left (°)")+ 
    scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))


long_data_flex_right$pre_post <- factor(long_data_flex_right$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_flex_right_plot <- long_data_flex_right %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = flexion, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("flexion right (°)")+ 
    scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))

long_data_flex_left$pre_post <- factor(long_data_flex_left$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_flex_left_plot <- long_data_flex_left %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = flexion, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("flexion left (°)")+ 
    scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))

long_data_ext_right$pre_post <- factor(long_data_ext_right$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_ext_right_plot <- long_data_ext_right %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = extension, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("extension right (°)")+ 
    scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))

long_data_ext_left$pre_post <- factor(long_data_ext_left$pre_post , levels = c("PRE", "POST"), ordered = T)

(long_data_ext_left_plot <- long_data_ext_left %>% 
  filter(pre_post %in% c('PRE', 'POST')) %>% 
  ggplot(aes(x = factor(pre_post), y = extension, fill = factor(pre_post))) +
  # add half-violin from {ggdist} package
  stat_halfeye(
    # adjust bandwidth
    adjust = 1.0,
    # move to the right
    justification = 0.0,
    # remove the slub interval
    .width = 0.0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    # removing outliers
    outlier.color = NA,
    alpha = 0.5
  ) +
  stat_dots(
    # ploting on left side
    side = "left",
    # adjusting position
    justification = 1.1,
    # adjust grouping (binning) of observations
    binwidth = 0.35
  )+ theme(axis.line = element_line(colour = "black")) + theme_bw() + xlab("") + ylab("extension left (°)")+ 
    scale_fill_manual(values = c("PRE" = "#AF46B4", "POST" = "#33a02c")))


grid.arrange(long_data_abd_left_plot,
             long_data_abd_right_plot,
             long_data_ext_left_plot, 
             long_data_ext_right_plot,
             long_data_flex_left_plot,
             long_data_flex_right_plot, ncol=2)

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------ ICC and correlations ----------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------- #
# -------------------------------------- data correlation --------------------------------- #
# ----------------------------------------------------------------------------------------- #

cor_data_all <- corrplot(corr = cor(na.omit(data_raw[c(10:21)]),
                                    method = "spearman"), method = "number",
                         bg = "black",
                         addgrid.col = "gray50", 
                         tl.cex = 0.85,
                         order = "hclust", 
                         number.cex = 0.8,
                         addCoef.col = "black",
                         tl.col = "black",
                         tl.srt = 30,
                         sig.level = 0.05,
                         insig = "pch",
                         diag = T,
                         col = colorRampPalette(c("blue", "white", "red"))(100))

# ------------------------------ age and performance level correlation -------------------- #

data_raw <- data_raw %>%
  mutate(performance_level_num = case_when(
    performance_level == 'beginner' ~ 1,
    performance_level == 'preadvanced' ~ 2,
    performance_level == 'advanced' ~ 3,
    performance_level == 'intermediate' ~ 4
  ))

ggplot(data = data_raw, aes(x = performance_level_num, y = age)) + geom_point()
cor(data_raw$age, data_raw$performance_level_num, method = "spearman")

# ----------------------------------------------------------------------------------------- #
# -------------------------------------------- ICC ---------------------------------------- #
# ----------------------------------------------------------------------------------------- #

abduction_R_ICC <- irr::icc(data_raw[, c('abd_right_pre', 'abd_right_post')],
                            model = "twoway", type = "consistency", unit = "average")
abduction_R_ICC_summary <- cbind("ICC value" = round(abduction_R_ICC$value, 3),
                                 "Lower 95%CI" = round(abduction_R_ICC$lbound, 3),
                                 "Upper 95%CI" = round(abduction_R_ICC$ubound, 3),
                                 "p-value" = abduction_R_ICC$p.value)
rownames(abduction_R_ICC_summary) <- "abduction_R_ICC_summary"

abduction_L_ICC <- irr::icc(data_raw[, c('abd_left_pre', 'abd_left_post')],
                            model = "twoway", type = "consistency", unit = "average")
abduction_L_ICC_summary <- cbind("ICC value" = round(abduction_L_ICC$value, 3),
                                 "Lower 95%CI" = round(abduction_L_ICC$lbound, 3),
                                 "Upper 95%CI" = round(abduction_L_ICC$ubound, 3),
                                 "p-value" = abduction_L_ICC$p.value)
rownames(abduction_L_ICC_summary) <- "abduction_L_ICC_summary"

flexion_R_ICC <- irr::icc(data_raw[, c('flex_right_pre', 'flex_right_post')],
                            model = "twoway", type = "consistency", unit = "average")
flexion_R_ICC_summary <- cbind("ICC value" = round(flexion_R_ICC$value, 3),
                                 "Lower 95%CI" = round(flexion_R_ICC$lbound, 3),
                                 "Upper 95%CI" = round(flexion_R_ICC$ubound, 3),
                                 "p-value" = flexion_R_ICC$p.value)
rownames(flexion_R_ICC_summary) <- "flexion_R_ICC_summary"

flexion_L_ICC <- irr::icc(data_raw[, c('flex_left_pre', 'flex_left_post')],
                            model = "twoway", type = "consistency", unit = "average")
flexion_L_ICC_summary <- cbind("ICC value" = round(flexion_L_ICC$value, 3),
                                 "Lower 95%CI" = round(flexion_L_ICC$lbound, 3),
                                 "Upper 95%CI" = round(flexion_L_ICC$ubound, 3),
                                 "p-value" = flexion_L_ICC$p.value)
rownames(flexion_L_ICC_summary) <- "flexion_L_ICC_summary"

extension_R_ICC <- irr::icc(data_raw[, c('ext_right_pre', 'ext_right_post')],
                            model = "twoway", type = "consistency", unit = "average")
extension_R_ICC_summary <- cbind("ICC value" = round(extension_R_ICC$value, 3),
                                 "Lower 95%CI" = round(extension_R_ICC$lbound, 3),
                                 "Upper 95%CI" = round(extension_R_ICC$ubound, 3),
                                 "p-value" = extension_R_ICC$p.value)
rownames(extension_R_ICC_summary) <- "extension_R_ICC_summary"

extension_L_ICC <- irr::icc(data_raw[, c('ext_right_pre', 'ext_right_post')],
                            model = "twoway", type = "consistency", unit = "average")
extension_L_ICC_summary <- cbind("ICC value" = round(extension_L_ICC$value, 3),
                                 "Lower 95%CI" = round(extension_L_ICC$lbound, 3),
                                 "Upper 95%CI" = round(extension_L_ICC$ubound, 3),
                                 "p-value" = extension_L_ICC$p.value)
rownames(extension_L_ICC_summary) <- "extension_L_ICC_summary"


# All ICCs
ICC_all <- rbind(abduction_R_ICC_summary,
                 abduction_L_ICC_summary, 
                 flexion_R_ICC_summary, 
                 flexion_L_ICC_summary, 
                 extension_R_ICC_summary, 
                 extension_L_ICC_summary)

(ICC_all <- as.data.frame(ICC_all))

# round p values
ICC_all$`p-value` <- round(ICC_all$`p-value`, 4)
ICC_all$`Discernible` <- ifelse(ICC_all$`p-value` <= 0.0500, "YES", "NO")

ICC_all$Variable <- rownames(ICC_all)
ICC_all <- ICC_all[, c(6, 1:5)]
ICC_all$Variable <- gsub("_summary", "", ICC_all$Variable)
print(ICC_all)

ICC_all <- ICC_all %>%
  mutate(variable = recode(Variable, 
                           abduction_R_ICC = 'Right abduction',
                           abduction_L_ICC = 'Left abduction', 
                           flexion_R_ICC = 'Right flexion',
                           flexion_L_ICC = 'Left flexion',
                           extension_R_ICC = 'Right extension',
                           extension_L_ICC = 'Left extension'))

print(ICC_all)

ICC_all <- ICC_all[, c(7, 2, 3, 4, 5)]

print(ICC_all)

(ICC_all <- as.data.frame(ICC_all))

range(ICC_all$`ICC value`)

ICC_all <- rempsyc::nice_table(ICC_all)

print(ICC_all)

class(ICC_all)

doc <- read_docx() |> 
  body_add_flextable(value = ICC_all) |> 
  body_add_par("", style = "Normal")

# Save the document
print(doc, target = "ICC_all_hip_joint_apa.docx")


# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------ mixed effect models ------------------------------------------ #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- mixed abduction --------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------- Response variable distribution -------------------------------------- #

str(long_data_all3)

shapiro_test(long_data_all3$abduction)
ggplot(data = long_data_all3, aes(x = abduction)) + geom_density(fill = 'lightblue')

descdist(long_data_all3$abduction, boot = 1000)

# -------------------------------------------- Model settings and summary --------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# -------------------------- The simplest model '~ 1 + pre_post + (1|ID)' --------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

(model_simplest_abduction_lme4 <- lme4::lmer(abduction ~ 1 + pre_post + (1|ID), data = long_data_all3))
(model_simplest_abduction_lmerTest <- lmerTest::lmer(abduction ~ 1 + pre_post + (1|ID), data = long_data_all3))
summary(model_simplest_abduction_lmerTest)
summary(model_simplest_abduction_lme4)

anova(model_simplest_abduction_lmerTest, type = 3)

performance::icc(model = model_simplest_abduction_lme4)

confint(model_simplest_abduction_lmerTest)

confint(model_simplest_abduction_lmerTest, parm = "theta_", method = "profile")

broom.mixed::tidy(model_simplest_abduction_lmerTest)
broom.mixed::glance(model_simplest_abduction_lmerTest)

cAIC4::cAIC(model_simplest_abduction_lmerTest)


cohens_d(data = long_data_all3, formula = abduction ~ pre_post)
cohens_d(data = long_data_all3, formula = abduction ~ 1)

# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(abduction, flexion, extension), mean))

mean(long_data_all3$abduction)

# abduction
(abs(32.9 - 38.4)/((32.9 + 38.4)/2)) * 100


# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_simplest_abduction_lmerTest)

# variance estimation
VarCorr(model_simplest_abduction_lmerTest)

# flexplot estimates - the same as summary before
flexplot::estimates(model_simplest_abduction_lme4)
# 95%CIs 
confint(model_simplest_abduction_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_abd <- DHARMa::simulateResiduals(fittedModel = model_simplest_abduction_lmerTest, n = 10000)
residuals(res_abd, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_abd, quantileFunction = qnorm))

plot_res_abd <- DHARMa::plotQQunif(res_abd,
                               testUniformity = T,
                               testOutliers = T,
                               testDispersion = T, pch = 20)

plot_res1_abd <- DHARMa::plotResiduals(res_abd)

# ------------------------ Influential points - Cook's distance -------------- #

estex_abd <- influence.ME::influence(model = model_simplest_abduction_lmerTest, group = "ID")

cooks.distance.estex(estex_abd, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_abd, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_abd)


# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, flexplot packages)
sjPlot::plot_model(model_simplest_abduction_lmerTest, show.values = T, show.p = T)


# starting with PRE in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_abd <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_abd = mean(abduction, na.rm = TRUE),
            sterr = sd(abduction, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_abd - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_abd + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_abd_plot <- ggplot() +
  # 1. Individual participant data: gray points and connecting lines
  geom_line(data = long_data_all3, aes(x = pre_post, y = abduction, group = ID), 
            color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
  geom_point(data = long_data_all3, aes(x = pre_post, y = abduction), 
             size = 1, color = "gray") +  # Individual data points
  
  # 2. Mean values with 95% confidence intervals
  geom_point(data = summary_data_abd, aes(x = pre_post, y = mean_abd), 
             size = 3, color = "darkred") +  # Mean points
  geom_errorbar(data = summary_data_abd, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
  geom_line(data = summary_data_abd, aes(x = pre_post, y = mean_abd, group = 1), 
            color = "darkred", linewidth = 1.0) +  # Line connecting means
  
  # Labels and theme
  xlab("") + ylim(20, 50) +
  scale_x_discrete(expand = expansion(mult = c(0.3, 0.3)))+
  ylab("Leg Abduction (°)") +
  theme_minimal() +
  #theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
        axis.title.y = element_text(margin = margin(r = 15)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()))


# Base plot with flexplot
(model_abd_flexplot <- flexplot(abduction ~ pre_post,
                                      data = long_data_all3,
                                      model = model_simplest_abduction_lmerTest, spread = "sterr",
                                      jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg Abduction (°)"))


# Add individual participant lines and means with connecting line
(model_abd_flexplot <- model_abd_flexplot +
  geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
  geom_point(size = 0.1, color = "gray") +  # Individual data points
  geom_line(data = summary_data_abd, aes(x = pre_post, y = mean_abd, group = 1), 
            color = "red", linewidth = 1.5) +  # Line connecting mean values
  geom_point(data = summary_data_abd, aes(x = pre_post, y = mean_abd), 
             color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_abd <- ggpredict(model_simplest_abduction_lmerTest, terms = c("pre_post"))
print(pred_model_abd)

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------- The step wise model - the lowest AIC ---------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# cAIC4 package
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

model_abd_lme4_a <- lme4::lmer(abduction ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                               data = long_data_all3)

isSingular(model_abd_lme4_a)
summary(model_abd_lme4_a)

# cAIC4::cAIC(model_abd_final_right_lme4) - backward step wise of random effects
(step_model_abd_final_stepcAIC <- cAIC4::stepcAIC(model_abd_lme4_a, direction = "backward", trace = TRUE, 
                                            data = long_data_all3))
summary(step_model_abd_final_stepcAIC)

# -------------------------------------------- final model settings --------------------------------------------- #

model_abd_final_lme4_a <- lme4::lmer(abduction ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                                     data = long_data_all3)
model_abd_final_lme4_a_lmerTest <- lmerTest::lmer(abduction ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                                                  data = long_data_all3)


performance::icc(model = model_abd_final_lme4_a_lmerTest)

confint(model_abd_final_lme4_a_lmerTest)
cAIC4::cAIC(model_abd_final_lme4_a_lmerTest)


cohens_d(data = long_data_all3, formula = abduction ~ pre_post)
cohens_d(data = long_data_all3, formula = abduction ~ 1)

# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(abduction, flexion, extension), mean))

mean(long_data_all3$abduction)

# abduction
(abs(32.9 - 38.4)/((32.9 + 38.4)/2)) * 100


# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_abd_final_lme4_a_lmerTest)

# variance estimation
VarCorr(model_abd_final_lme4_a_lmerTest)

# flexplot estimates - the same as summary before
flexplot::estimates(model_abd_final_lme4_a_lmerTest)
# 95%CIs 
confint(model_abd_final_lme4_a_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_abd <- DHARMa::simulateResiduals(fittedModel = model_abd_final_lme4_a_lmerTest, n = 10000)
residuals(res_abd, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_abd, quantileFunction = qnorm))

plot_res_abd <- DHARMa::plotQQunif(res_abd,
                                   testUniformity = T,
                                   testOutliers = T,
                                   testDispersion = T, pch = 20)

plot_res1_abd <- DHARMa::plotResiduals(res_abd)

# ------------------------ Influential points - Cook's distance -------------- #

estex_abd <- influence.ME::influence(model = model_abd_final_lme4_a_lmerTest, group = "ID")

cooks.distance.estex(estex_abd, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_abd, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_abd)


# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, flexplot packages)
sjPlot::plot_model(model_abd_final_lme4_a_lmerTest, show.values = T, show.p = T)


# starting with PRE in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_abd <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_abd = mean(abduction, na.rm = TRUE),
            sterr = sd(abduction, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_abd - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_abd + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_abd_final_plot <- ggplot() +
    # 1. Individual participant data: gray points and connecting lines
    geom_line(data = long_data_all3, aes(x = pre_post, y = abduction, group = ID), 
              color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
    geom_point(data = long_data_all3, aes(x = pre_post, y = abduction), 
               size = 1, color = "gray") +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_abd, aes(x = pre_post, y = mean_abd), 
               size = 3, color = "darkred") +  # Mean points
    geom_errorbar(data = summary_data_abd, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
    geom_line(data = summary_data_abd, aes(x = pre_post, y = mean_abd, group = 1), 
              color = "darkred", linewidth = 1.0) +  # Line connecting means
    
    # Labels and theme
    xlab("Pre-test and Post-test") +
    ylab("Leg Abduction (°)") +
    theme_minimal() +
    #theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))


# Base plot with flexplot
(model_abd_final_flexplot <- flexplot(abduction ~ pre_post,
                                data = long_data_all3,
                                model = model_abd_final_lme4_a_lmerTest, spread = "sterr",
                                jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg Abduction (°)"))


# Add individual participant lines and means with connecting line
(model_abd_final_flexplot <- model_abd_final_flexplot +
    geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
    geom_point(size = 0.1, color = "gray") +  # Individual data points
    geom_line(data = summary_data_abd, aes(x = pre_post, y = mean_abd, group = 1), 
              color = "red", linewidth = 1.5) +  # Line connecting mean values
    geom_point(data = summary_data_abd, aes(x = pre_post, y = mean_abd), 
               color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_abd <- ggpredict(model_abd_final_lme4_a_lmerTest, terms = c("pre_post"))
print(pred_model_abd)


# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- mixed flexion ----------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------- Response variable distribution -------------------------------------- #

str(long_data_all3)

shapiro_test(long_data_all3$flexion)
ggplot(data = long_data_all3, aes(x = flexion)) + geom_density(fill = 'lightblue')

descdist(long_data_all3$flexion, boot = 1000)

# -------------------------------------------- Model settings and summary --------------------------------------- #


# -------------------------- The simplest model '~ 1 + pre_post + (1|ID)' ---------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

(model_simplest_flexion_lme4 <- lme4::lmer(flexion ~ 1 + pre_post + (1|ID), data = long_data_all3))
(model_simplest_flexion_lmerTest <- lmerTest::lmer(flexion ~ 1 + pre_post + (1|ID), data = long_data_all3))
summary(model_simplest_flexion_lmerTest)

anova(model_simplest_flexion_lmerTest, type = 3)

performance::icc(model = model_simplest_flexion_lme4)

confint(model_simplest_flexion_lmerTest)

confint(model_simplest_flexion_lmerTest, parm = "theta_", method = "profile")

cAIC4::cAIC(model_simplest_flexion_lmerTest)

cohens_d(data = long_data_all3, formula = flexion ~ pre_post)
cohens_d(data = long_data_all3, formula = flexion ~ 1)


broom.mixed::tidy(model_simplest_flexion_lmerTest)
broom.mixed::glance(model_simplest_flexion_lmerTest)



# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(abduction, flexion, extension), mean))

mean(long_data_all3$abduction)


# flexion
(abs(40.1 - 44.3)/((40.1 + 44.3)/2)) * 100


# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_simplest_flexion_lmerTest)

# variance estimation
VarCorr(model_simplest_flexion_lmerTest)

# flexplot estimates - the same as summary before
flexplot::estimates(model_simplest_flexion_lme4)

# 95%CIs 
confint(model_simplest_flexion_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_flex <- DHARMa::simulateResiduals(fittedModel = model_simplest_flexion_lmerTest, n = 10000)
residuals(res_flex, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_flex, quantileFunction = qnorm))

plot_res_flex_left <- DHARMa::plotQQunif(res_flex,
                               testUniformity = T,
                               testOutliers = T,
                               testDispersion = T, pch = 20)

plot_res1_flex_left <- DHARMa::plotResiduals(res_flex)

# ------------------------ Influential points - Cook's distance -------------- #

estex_flex <- influence.ME::influence(model = model_simplest_flexion_lmerTest, group = "ID")

cooks.distance.estex(estex_flex, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_flex, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_flex)

# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, flexplot packages)
sjPlot::plot_model(model_simplest_flexion_lmerTest, show.values = T, show.p = T)


# starting with PRe in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_flex <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_flex = mean(flexion, na.rm = TRUE),
            sterr = sd(flexion, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_flex - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_flex + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_flex_plot <- ggplot() +
    # 1. Individual participant data: gray points and connecting lines
    geom_line(data = long_data_all3, aes(x = pre_post, y = flexion, group = ID), 
              color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
    geom_point(data = long_data_all3, aes(x = pre_post, y = flexion), 
               size = 1, color = "gray") +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_flex, aes(x = pre_post, y = mean_flex), 
               size = 3, color = "darkred") +  # Mean points
    geom_errorbar(data = summary_data_flex, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
    geom_line(data = summary_data_flex, aes(x = pre_post, y = mean_flex, group = 1), 
              color = "darkred", linewidth = 1.0) +  # Line connecting means
    
    # Labels and theme
    xlab("") + ylim(20, 50) +
    scale_x_discrete(expand = expansion(mult = c(0.3, 0.3)))+
    ylab("Leg Flexion (°)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.title.y = element_text(margin = margin(r = 15)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))


# Base plot with flexplot
(model_flex_flexplot <- flexplot(flexion ~ pre_post,
                                       data = long_data_all3,
                                       model = model_simplest_flexion_lmerTest, spread = "sterr",
                                       jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg Flexion (°)"))

# Add individual participant lines and means with connecting line
(model_flex_flexplot <- model_flex_flexplot +
    geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
    geom_point(size = 0.1, color = "gray") +  # Individual data points
    geom_line(data = summary_data_flex, aes(x = pre_post, y = mean_flex, group = 1), 
              color = "red", linewidth = 1.5) +  # Line connecting mean values
    geom_point(data = summary_data_flex, aes(x = pre_post, y = mean_flex), 
               color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_flex <- ggpredict(model_simplest_flexion_lmerTest, terms = c("pre_post"))
print(pred_model_flex)


# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------- The step wise model - the lowest AIC ---------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

# cAIC4 package
model_flex_lme4_a <- lme4::lmer(flexion ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                               data = long_data_all3)

isSingular(model_flex_lme4_a)
summary(model_flex_lme4_a)

# cAIC4::cAIC(model_flex_final_right_lme4) - backward step wise of random effects
(step_model_flex_final_stepcAIC <- cAIC4::stepcAIC(model_flex_lme4_a, direction = "backward", trace = TRUE, 
                                                  data = long_data_all3))
summary(step_model_flex_final_stepcAIC)

# -------------------------------------------- final model settings --------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

model_flex_final_lme4_a <- lme4::lmer(flexion ~ 1 + pre_post + (1|ID) + (1|age) + (1|laterality),
                                     data = long_data_all3)
model_flex_final_lme4_a_lmerTest <- lmerTest::lmer(flexion ~ 1 + pre_post + (1|ID) + (1|age) + (1|laterality),
                                                  data = long_data_all3)


performance::icc(model = model_flex_final_lme4_a_lmerTest)

confint(model_flex_final_lme4_a_lmerTest)
cAIC4::cAIC(model_flex_final_lme4_a_lmerTest)


cohens_d(data = long_data_all3, formula = flexion ~ pre_post)
cohens_d(data = long_data_all3, formula = flexion ~ 1)
?cohens_d
# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(flexion, flexion, extension), mean))

mean(long_data_all3$flexion)

# flexion
(abs(32.9 - 38.4)/((32.9 + 38.4)/2)) * 100


# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_flex_final_lme4_a_lmerTest)

# variance estimation
VarCorr(model_flex_final_lme4_a_lmerTest)

# flexplot estimates - the same as summary before
flexplot::estimates(model_flex_final_lme4_a_lmerTest)
# 95%CIs 
confint(model_flex_final_lme4_a_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_flex <- DHARMa::simulateResiduals(fittedModel = model_flex_final_lme4_a_lmerTest, n = 10000)
residuals(res_flex, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_flex, quantileFunction = qnorm))

plot_res_flex <- DHARMa::plotQQunif(res_flex,
                                   testUniformity = T,
                                   testOutliers = T,
                                   testDispersion = T, pch = 20)

plot_res1_flex <- DHARMa::plotResiduals(res_flex)

# ------------------------ Influential points - Cook's distance -------------- #

estex_flex <- influence.ME::influence(model = model_flex_final_lme4_a_lmerTest, group = "ID")

cooks.distance.estex(estex_flex, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_flex, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_flex)


# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, flexplot packages)
sjPlot::plot_model(model_flex_final_lme4_a_lmerTest, show.values = T, show.p = T)


# starting with PRE in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_flex <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_flex = mean(flexion, na.rm = TRUE),
            sterr = sd(flexion, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_flex - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_flex + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_flex_final_plot <- ggplot() +
    # 1. Individual participant data: gray points and connecting lines
    geom_line(data = long_data_all3, aes(x = pre_post, y = flexion, group = ID), 
              color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
    geom_point(data = long_data_all3, aes(x = pre_post, y = flexion), 
               size = 1, color = "gray") +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_flex, aes(x = pre_post, y = mean_flex), 
               size = 3, color = "darkred") +  # Mean points
    geom_errorbar(data = summary_data_flex, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
    geom_line(data = summary_data_flex, aes(x = pre_post, y = mean_flex, group = 1), 
              color = "darkred", linewidth = 1.0) +  # Line connecting means
    
    # Labels and theme
    xlab("Pre-test and Post-test") +
    ylab("Leg flexion (°)") +
    theme_minimal() +
    #theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))


# Base plot with flexplot
(model_flex_final_flexplot <- flexplot(flexion ~ pre_post,
                                      data = long_data_all3,
                                      model = model_flex_final_lme4_a_lmerTest, spread = "sterr",
                                      jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg flexion (°)"))


# Add individual participant lines and means with connecting line
(model_flex_final_flexplot <- model_flex_final_flexplot +
    geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
    geom_point(size = 0.1, color = "gray") +  # Individual data points
    geom_line(data = summary_data_flex, aes(x = pre_post, y = mean_flex, group = 1), 
              color = "red", linewidth = 1.5) +  # Line connecting mean values
    geom_point(data = summary_data_flex, aes(x = pre_post, y = mean_flex), 
               color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_flex <- ggpredict(model_flex_final_lme4_a_lmerTest, terms = c("pre_post"))
print(pred_model_flex)



# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------- mixed extension --------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# ----------------------------------------- Response variable distribution -------------------------------------- #
str(long_data_all3)

shapiro_test(long_data_all3$extension)
ggplot(data = long_data_all3, aes(x = extension)) + geom_density(fill = 'lightblue')

descdist(long_data_all3$extension, boot = 1000)


# --------------------------------------------------------------------------------------------------------------- #
# -------------------------- The simplest model '~ 1 + pre_post + (1|ID)' --------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

# Analysis of Variance Table with Satterthwaite's method
(model_simplest_extension_lme4 <- lme4::lmer(extension ~ 1 + pre_post + (1|ID), data = long_data_all3))
(model_simplest_extension_lmerTest <- lmerTest::lmer(extension ~ 1 + pre_post + (1|ID), data = long_data_all3))
summary(model_simplest_extension_lmerTest)
summary(model_simplest_extension_lme4)

performance::icc(model = model_simplest_extension_lme4)

anova(model_simplest_extension_lmerTest)

confint(model_simplest_extension_lmerTest)

cAIC4::cAIC(model_simplest_extension_lmerTest)

cohens_d(data = long_data_all3, formula = extension ~ pre_post)
cohens_d(data = long_data_all3, formula = extension ~ 1)

# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(abduction, flexion, extension), mean))

# extension
(abs(35.0 - 39.8)/((35.0 + 39.8)/2)) * 100

# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_simplest_extension_lmerTest)

# variance estimation
VarCorr(model_simplest_extension_lmerTest)

# flexplot estimates - the same as summary before
flexplot::estimates(model_simplest_extension_lme4)

# 95%CIs 
confint(model_simplest_extension_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_ext_right <- DHARMa::simulateResiduals(fittedModel = model_simplest_extension_lmerTest, n = 10000)
residuals(res_ext_right, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_ext_right, quantileFunction = qnorm))

plot_res_ext_right <- DHARMa::plotQQunif(res_ext_right,
                               testUniformity = T,
                               testOutliers = T,
                               testDispersion = T, pch = 20)

plot_res1_ext_right <- DHARMa::plotResiduals(res_ext_right)

# ------------------------ Influential points - Cook's distance -------------- #

estex_ext_right <- influence.ME::influence(model = model_simplest_extension_lmerTest, group = "ID")

cooks.distance.estex(estex_ext_right, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_ext_right, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_ext_right)


# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, flexplot packages)
sjPlot::plot_model(model_simplest_extension_lmerTest, show.values = T, show.p = T)


# starting with PRe in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_ext <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_ext = mean(extension, na.rm = TRUE),
            sterr = sd(extension, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_ext - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_ext + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_ext_plot <- ggplot() +
    # 1. Individual participant data: gray points and connecting lines
    geom_line(data = long_data_all3, aes(x = pre_post, y = extension, group = ID), 
              color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
    geom_point(data = long_data_all3, aes(x = pre_post, y = extension), 
               size = 1, color = "gray") +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_ext, aes(x = pre_post, y = mean_ext), 
               size = 3, color = "darkred") +  # Mean points
    geom_errorbar(data = summary_data_ext, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
    geom_line(data = summary_data_ext, aes(x = pre_post, y = mean_ext, group = 1), 
              color = "darkred", linewidth = 1.0) +  # Line connecting means
    
    # Labels and theme
    xlab("") + ylim(20, 50) +
    scale_x_discrete(expand = expansion(mult = c(0.3, 0.3))) +
    ylab("Leg Extension (°)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5), 
          axis.title.y = element_text(margin = margin(r = 15)),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))


# Base plot with flexplot
(model_ext_flexplot <- flexplot(extension ~ pre_post,
                                      data = long_data_all3,
                                      model = model_simplest_extension_lmerTest, spread = "sterr",
                                      jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg Extension (°)"))

# Add individual participant lines and means with connecting line
(model_ext_flexplot <- model_ext_flexplot +
    geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
    geom_point(size = 0.1, color = "gray") +  # Individual data points
    geom_line(data = summary_data_ext, aes(x = pre_post, y = mean_ext, group = 1), 
              color = "red", linewidth = 1.5) +  # Line connecting mean values
    geom_point(data = summary_data_ext, aes(x = pre_post, y = mean_ext), 
               color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_ext <- ggpredict(model_simplest_extension_lmerTest, terms = c("pre_post"))
print(pred_model_ext)

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------- The step wise model - the lowest AIC ---------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

# cAIC4 package
model_ext_lme4_a <- lme4::lmer(extension ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                                data = long_data_all3)

isSingular(model_ext_lme4_a)
summary(model_ext_lme4_a)

# cAIC4::cAIC(model_ext_final_right_lme4) - backward step wise of random effects
(step_model_ext_final_stepcAIC <- cAIC4::stepcAIC(model_ext_lme4_a, direction = "backward", trace = TRUE, 
                                                   data = long_data_all3))
summary(step_model_ext_final_stepcAIC)

# -------------------------------------------- final model settings --------------------------------------------- #
long_data_all3$pre_post <- as.character(long_data_all3$pre_post)

model_ext_final_lme4_a <- lme4::lmer(extension ~ 1 + pre_post + (1|ID) + (1|age) + (1|laterality),
                                      data = long_data_all3)
model_ext_final_lme4_a_lmerTest <- lmerTest::lmer(extension ~ 1 + pre_post + (1|ID) + (1|age) + (1|laterality),
                                                   data = long_data_all3)


performance::icc(model = model_ext_final_lme4_a_lmerTest)

confint(model_ext_final_lme4_a_lmerTest)
cAIC4::cAIC(model_ext_final_lme4_a_lmerTest)


cohens_d(data = long_data_all3, formula = extension ~ pre_post)
cohens_d(data = long_data_all3, formula = extension ~ 1)

# --------------------- % change --------------------- #

long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(across(c(extension, extension, extension), mean))

mean(long_data_all3$extension)

# extension
(abs(32.9 - 38.4)/((32.9 + 38.4)/2)) * 100


# R2M and R2C
#library(MuMIn)
r.squaredGLMM(model_ext_final_lme4_a_lmerTest)

# variance estimation
VarCorr(model_ext_final_lme4_a_lmerTest)

# extplot estimates - the same as summary before
extplot::estimates(model_ext_final_lme4_a_lmerTest)
# 95%CIs 
confint(model_ext_final_lme4_a_lmerTest)

# ------------------------ Homoscedasticity ---------------------------------- #

res_ext <- DHARMa::simulateResiduals(fittedModel = model_ext_final_lme4_a_lmerTest, n = 10000)
residuals(res_ext, quantileFunction = qnorm)

# normality test of residuals
shapiro.test(residuals(res_ext, quantileFunction = qnorm))

plot_res_ext <- DHARMa::plotQQunif(res_ext,
                                    testUniformity = T,
                                    testOutliers = T,
                                    testDispersion = T, pch = 20)

plot_res1_ext <- DHARMa::plotResiduals(res_ext)

# ------------------------ Influential points - Cook's distance -------------- #

estex_ext <- influence.ME::influence(model = model_ext_final_lme4_a_lmerTest, group = "ID")

cooks.distance.estex(estex_ext, sort=TRUE)

(cutoff_model <- 4/(nrow(data_raw)))

plot(estex_ext, which="cook",
     cutoff = cutoff_model, sort=TRUE,
     xlab = "Cook's distance",
     ylab = "Participant's ID", cex.axis = 0.5)

sigtest(estex = estex_ext)


# --------------- visualization of the model --------------------------------- #

# Visualize the model (sjPlot, extplot packages)
sjPlot::plot_model(model_ext_final_lme4_a_lmerTest, show.values = T, show.p = T)


# starting with PRE in the plot
long_data_all3$pre_post <- factor(long_data_all3$pre_post, levels = c("PRE", "POST"), ordered = T)

# Calculate mean values for each pre_post condition
summary_data_ext <- long_data_all3 %>%
  group_by(pre_post) %>%
  summarise(mean_ext = mean(extension, na.rm = TRUE),
            sterr = sd(extension, na.rm = TRUE) / sqrt(n()),
            lower_ci = mean_ext - qt(0.975, df = n() - 1) * sterr,
            upper_ci = mean_ext + qt(0.975, df = n() - 1) * sterr)


long_data_all3 <- long_data_all3 %>%
  mutate(ID = as.factor(ID))  # Convert ID to factor for correct grouping

# Create the plot
(model_ext_final_plot <- ggplot() +
    # 1. Individual participant data: gray points and connecting lines
    geom_line(data = long_data_all3, aes(x = pre_post, y = extension, group = ID), 
              color = "gray", alpha = 0.5) +  # Connect pre/post for each participant
    geom_point(data = long_data_all3, aes(x = pre_post, y = extension), 
               size = 1, color = "gray") +  # Individual data points
    
    # 2. Mean values with 95% confidence intervals
    geom_point(data = summary_data_ext, aes(x = pre_post, y = mean_ext), 
               size = 3, color = "darkred") +  # Mean points
    geom_errorbar(data = summary_data_ext, aes(x = pre_post, ymin = lower_ci, ymax = upper_ci), 
                  width = 0.1, color = "darkred", linewidth = 1.0) +  # 95% CI error bars
    geom_line(data = summary_data_ext, aes(x = pre_post, y = mean_ext, group = 1), 
              color = "darkred", linewidth = 1.0) +  # Line connecting means
    
    # Labels and theme
    xlab("Pre-test and Post-test") +
    ylab("Leg extension (°)") +
    theme_minimal() +
    #theme_bw(base_size = 12) +
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()))


# Base plot with extplot
(model_ext_final_extplot <- extplot(extension ~ pre_post,
                                       data = long_data_all3,
                                       model = model_ext_final_lme4_a_lmerTest, spread = "sterr",
                                       jitter = FALSE) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) + 
    xlab("Pre-test and Post-test") + 
    ylab("Leg extension (°)"))


# Add individual participant lines and means with connecting line
(model_ext_final_extplot <- model_ext_final_extplot +
    geom_line(aes(group = ID), color = "gray", alpha = 0.5) +  # Connect individual pre/post points
    geom_point(size = 0.1, color = "gray") +  # Individual data points
    geom_line(data = summary_data_ext, aes(x = pre_post, y = mean_ext, group = 1), 
              color = "red", linewidth = 1.5) +  # Line connecting mean values
    geom_point(data = summary_data_ext, aes(x = pre_post, y = mean_ext), 
               color = "red", size = 3))  # Mean points


# ?pretty data frame? of predicted values for the response variable and its confidence interval
# library(ggeffects)
pred_model_ext <- ggpredict(model_ext_final_lme4_a_lmerTest, terms = c("pre_post"))
print(pred_model_ext)


# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ----------------------------------------------- grid of plots ------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

grid.arrange(model_abd_plot,
             model_flex_plot,
             model_ext_plot, ncol=3, bottom = textGrob("Condition"))


# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------------------- power simr -------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

str(long_data_all3)
# simulated_dataset_PAPE_ballistic$time_point <- as.numeric(simulated_dataset_PAPE_ballistic$time_point)

model_extension_1_simr <- lme4::lmer(extension ~ 1 + pre_post + (1|ID) + (1|age) + (1|performance_level) + (1|laterality),
                               data = long_data_all3)

power_model_extension_1 <- powerSim(fit = model_extension_1_simr, fixed('pre_post'), nsim = 5000)

print(power_model_extension_1)
summary(power_model_extension_1)

extended_model_extension_1 <- simr::extend(model_extension_1_simr, along = "ID", n = 100)

pc_extended_model_extension_1 <- powerCurve(extended_model_extension_1,
                                    fixed('pre_post'),
                                    along = 'ID',
                                    nsim = 5000, 
                                    alpha = 0.05, 
                                    progress = T)

print(summary(pc_extended_model_extension_1))
plot(pc_extended_model_extension_1)
abline(v = 29, col = "red")

# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# ------------------------------------- ESD based on the Thomas et al. (2018) ----------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #
# --------------------------------------------------------------------------------------------------------------- #

# --------------------------------------------------------------------------------------------------------------- #
# ------------------------ mean, median, and mode of sample size from Thomas et al. (2018) ---------------------- #
# --------------------------------------------------------------------------------------------------------------- #

sample_sizes <- data.frame(sample_N = c(35,76,18,19,18,14,13,13,58,12,14,9,11,9,22,28,38,33,14,19,30,18,19,18))

mean(sample_sizes$sample_N)
median(sample_sizes$sample_N)

modeest::mlv(sample_sizes$sample_N, method = "mfv")

# Function to estimate SD from 95% CI and sample size

estimate_sd_from_CI <- function(ci_lower, ci_upper, n) {
  half_width <- (ci_upper - ci_lower) / 2 # CI half-width
  df <- n - 1 # Degrees of freedom
  t_critical <- qt(0.975, df) # t-critical value for 95% CI
  sd_estimate <- (half_width / t_critical) * sqrt(n) # Estimated SD
  return(sd_estimate)
}

# Sainz de Baranda P, Ayala F. Chronic flexibility improvement after 
# 12 week of stretching program utilizing the ACSM recommendations: 
# Hamstring flexibility. Int J Sports Med 2010; 31: 389–396

estimate_sd_from_CI(82.27, 90.51, 28) # mean = 88.5
estimate_sd_from_CI(86.37, 94.88, 38) # mean = 89.82
estimate_sd_from_CI(84.01, 91.38, 33) # mean = 87.9
estimate_sd_from_CI(78.45, 84.90, 14) # mean = 84.2
estimate_sd_from_CI(80.37, 87.98, 19) # mean = 85.4

estimate_sd_from_CI(102.18, 110.74, 28) # mean = 88.5
estimate_sd_from_CI(99.28, 166.23, 38) # mean = 89.82
estimate_sd_from_CI(98.86, 106.24, 33) # mean = 87.9
estimate_sd_from_CI(95.77, 109.39, 14) # mean = 84.2
estimate_sd_from_CI(95.67, 105.93, 19) # mean = 85.4

ESD_data <- readxl::read_excel("E:/data/Statistics/Data/thomas_2018_table.xlsx")
ESD_data <- readxl::read_excel("C:/Users/malir/OneDrive - Univerzita Karlova/Plocha/Statistics/Data/thomas_2018_table.xlsx")

str(ESD_data)

ESD_data$pooled_SD <- sqrt((ESD_data$ROM_Pre_SD^2 + ESD_data$ROM_Post_SD^2) / 2)
ESD_data$Cohens_d <- abs((ESD_data$ROM_Post_mean - ESD_data$ROM_Pre_mean) / ESD_data$pooled_SD)
ESD_data$Hedges_g <- ESD_data$Cohens_d * (1 - (3 / (4 * ESD_data$Sample_size - 9)))

print(ESD_data$Hedges_g)

quantile(ESD_data$Hedges_g)

ecdf_function <- ecdf(ESD_data$Hedges_g)

percentile_abd <- ecdf_function(1.24)
percentile_flex <- ecdf_function(1.37)
percentile_ext <- ecdf_function(0.98)

percentile_abd*100
percentile_flex*100
percentile_ext*100


# ---------------------------------- distribution visualization ------------------------------------------------ #

(ESD_plot <- ESD_data %>% 
    ggplot(aes(x = Hedges_g)) +
    
    geom_density(
      adjust = .8,
      justification = 0.0,  # Centered, avoids negatives
      .width = 0.0,
      point_colour = NA,
      color = "darkred", linewidth = 0.82,
      fill = "white"
    ) +
    geom_boxplot(
      width = 0.15,
      outlier.color = NA,
      #alpha = 0.5,
      fill = "white", linewidth = 0.7
    ) +
    stat_dots(
      side = "left",
      justification = 1.15,
      binwidth = 0.15,
      #color = "", 
      fill = "gray", dotsize = 0.8, position = "dodgejust",
    ) +
    theme_bw(base_size = 10) + 
    theme(axis.line = element_line(colour = "black"),
          panel.grid = element_blank(), 
          panel.border = element_blank(),
          axis.title.x = element_text(margin = margin(t = 18)), 
          axis.title.y = element_text(margin = margin(r = 18))) + 
    xlab("Effect size (Hedges g)") +
    ylab("Density") + scale_x_continuous(breaks = seq(0.0, 6.0, by = 0.5)) +
    coord_cartesian(ylim = c(-0.5, 1)) + scale_y_continuous(breaks = seq(0, 1, by = 0.5)))

ggsave("E:/data/Statistics/Plots/ESD_plot.png", plot = ESD_plot, 
       width = 5.0, height = 3, dpi = 600)
