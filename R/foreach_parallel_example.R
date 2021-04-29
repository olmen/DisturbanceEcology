# ==============================================================================
#
# Unregistered height normalization using only ALS DTM (unregistered)
#
# ==============================================================================
#
# Author: Sean Reilly, sean.reilly66@gmail.com
#
# Created: 15 March 2021
# Last commit: 22 April 2021
#
# ==============================================================================
#
# Description:
#
# This example script is taken from a point cloud processing work flow. It is 
# designed to height normalize multiple las point clouds in parallel based on a
# DTM. This is done by first adjusting for vertical offset then subtracting the
# terrain heights from the las points
#
# The first part of the script plots the raw data to view the offset.
#
# Lines 73-125: Parallel processing example of foreach. This part involves writing
# an intermediate output to file and then combining output dataframes containing
# results from the processing of each point cloud.
#
# The rest of the script visualizes the output.
#
# The data to go along with this example is available at https://bit.ly/3ugKSkA
# Unzip the folder and put its location in line 46
# If unable to access, email me directly.
#
# ==============================================================================

library(lidR)
library(tidyverse)
library(glue)
library(ggpubr)
library(lemon)

library(doParallel)

# ================================= User inputs ================================

zone <- c(1:3)

folder_location <- 'foreach_parallel_example/'

las_file <- 'foreach_example_las_z{z}.las'

dtm_file <- 'foreach_example_dtm_z{z}.tif'

height_normalized_las_output <- 'output/foreach_example_las_z{z}_height_normalized.las'


# ================= Plot example of terrain and offset from DTM ================

# Change this z value to visualize different zones (1 - 3)
z = 2

glue(folder_location, las_file) %>%
  readLAS(select = '') %>%
  plot() %>%
  add_dtm3d(
    raster(
      glue(folder_location, dtm_file)
      )
  )

# The gray DTM surface and the bottom of the colored LAS point cloud should 
# align. However, due to a vertical datum error, we need to correct for the shift
# before we can use the DTM to height normalize the las dataset


# ==============================================================================
# =========================== Height normalization =============================
# ==============================================================================

# -------------------------- Setup cluster processing -------------------------- 

cl <- makeCluster(3)
registerDoParallel(cl)

ground_data <- foreach (
  z = zone,
  .combine = 'rbind',
  .packages = c('lidR', 'tidyverse', 'glue', 'sf')
) %dopar% {
  
  dtm <- glue(folder_location, dtm_file) %>%
    raster()
  
  las <- glue(folder_location, las_file) %>%
    readLAS(select = 'c')
  
  # ------ Regression correction for offset between ground points and DTM ------
  
  # This is not the most robust way to do it but it works well enough to 
  # illustrate the effectiveness of the method. In this case we are shifting the
  # DTM upwards based on the linear model relationship between ground points in 
  # the las file and the corresponding elevation in the DTM

  las_ground_points <- las %>%
    filter_ground() %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z')

  model <- lm(Z ~ dtm_z,
              data = las_ground_points@data)

  dtm = dtm*model$coefficients[2] + model$coefficients[1]
  
  # ---- Extract las ground points and  adjusted dtm Z to confirm alignment ---- 
  
  adjusted_ground_points <- las %>%
    filter_ground() %>%
    merge_spatial(source = dtm,
                  attribute = 'dtm_z')

  # ---------------------- DTM based height normalization ----------------------

  las <- normalize_height(las, dtm, na.rm = TRUE)

  writeLAS(las, glue(folder_location, height_normalized_las_output))

  # ----------- Foreach returns the final variable that is assigned ------------ 
  
  adjusted_ground_points <- adjusted_ground_points@data %>%
    rename(uas_z = Z) %>%
    select(uas_z, dtm_z) %>%
    add_column(zone = z)
  
}

stopCluster(cl)


# ==============================================================================
# ================================ Plot results ================================ 
# ==============================================================================

# ====================== Plot height normalized las files ====================== 

# Change z values to view different zones (1 - 3)
z = 2

las <- glue(folder_location, height_normalized_las_output) %>%
  readLAS(select = '')

plot(las)

# ================= Plot las ground points to dtm relationship ================= 

# Now that we have adjusted for the offset, these values should be as close to 
# y = x as possible. 

# ------------------------------ Set ggplot theme ------------------------------ 

theme_set(
  theme(
    text = element_text(family = 'serif', face = 'plain'),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    line = element_line(size = 1),
    axis.line = element_line(),
    panel.background = element_rect(color = 'white'),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key = element_blank(),
    legend.spacing = unit(0, "cm"),
    legend.margin = margin(0,0,0,0)
  )
)

# ------------------------------- Generate plot --------------------------------

ggplot(data = ground_data %>%
               sample_frac(.001),
             mapping = aes(
               x = uas_z,
               y = dtm_z
             )) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x, color = 'firebrick') +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  stat_cor(label.y = 420) +
  stat_regline_equation(label.y = 365) +
  labs(
    x = 'LAS ground points elevation (m)',
    y = 'DTM elevation (m)'
  ) +
  facet_rep_wrap(~ zone,
                 ncol = 1) +
  theme(
    strip.text.x = element_text(size = 13, hjust = 0),
    strip.background = element_blank(),
    axis.line = element_line(),
    plot.caption = element_text(size = 14, hjust = 0)
  )
  
# ==============================================================================