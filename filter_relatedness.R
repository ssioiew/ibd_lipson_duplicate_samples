# ------------------------------------------------------
# File: filter_relatedness.R
# Purpose: Process PLINK IBD results to identify and create a removal list
#          for duplicate and related individuals based on specific criteria.
# Author: Pirada Naewkam
# Date: 2025-08-22
#
# Description:
#   This script reads the .genome file from a PLINK IBD analysis.
#   1. It identifies duplicate pairs (PI_HAT > 0.9) and selects the most
#      recent sample based on publication year from a metadata file.
#   2. It identifies related individuals (0.125 < PI_HAT < 0.9) and
#      systematically selects one individual from each pair for removal.
#   3. It compiles a final list of individuals to remove and saves it
#      in a format compatible with PLINK's --remove flag.
#
# Input:
#   - lipson_ibd_on_duplicates.genome (from PLINK)
#   - SK _ PL 31 July 2025 v62.0_HO_public.xlsx (Metadata file)
#   - modern_lipson_ancient.fam (PLINK .fam file)
#
# Output:
#   - samples_to_remove.txt (A list of FID/IID for PLINK's --remove flag)
# ------------------------------------------------------


# Step 0: Setup All Environments ------------------------------------------

# Load All Libraries
library(tidyverse)
library(readxl)

# Load All Path Files
input_file_path <- "results/lipson_ibd_on_duplicates.genome"
metadata_file_path <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/redundance_samples/Lipson_duplicates_report.xlsx"
output_dir <- "results"


# Step 1: Retrieve the Data -----------------------------------------------

# สร้าง metadata dataframe
df <- read_xlsx(metadata_file_path) %>% 
  select("IID" = `Genetic ID`, "Study_Date" = `Year data`) %>% 
  mutate(Study_Date = as.numeric(Study_Date))   # เปลี่ยนให้ Study_Date เป็นตัวเลข เพื่อให้สามารถเปรียบเทียบได้
nrow(df)  # 204 rows

# สร้าง dataframe ของ genome file
relateness_df <- read.table(input_file_path, header = TRUE)
nrow(relateness_df) # 155


# Step 2: Data Wraglling --------------------------------------------------

# กรองเอาแค่ความสัมพันธ์ 1-3 degree
related_pairs <- relateness_df %>% 
  filter(PI_HAT > 0.125) %>%  # 
  arrange(desc(PI_HAT))






# บันทึกไฟล์สำหรับใช้กับ PLINK
write.table(
  plink_keep_list,
  file = file.path(output_file_path, "samples_to_keep.txt"), # *** เปลี่ยนชื่อไฟล์เป็น keep.txt
  quote = FALSE,
  row.names = FALSE,
  col.names = FALSE,
  sep = "\t"
)
