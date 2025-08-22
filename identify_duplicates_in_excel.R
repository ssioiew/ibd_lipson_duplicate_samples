# ------------------------------------------------------
# File: identify_duplicates_in_excel.R
# Purpose: Identify and flag duplicate entries based on Master ID and Genetic ID.
# Author: Pirada Naekam
# Date: 2025-08-15
#
# Description:
#   This script reads data from a specified Excel file and performs data cleaning
#   to find duplicate entries. It creates flags for both Master ID and Genetic ID
#   and filters the dataset to show all rows containing at least one type of duplicate
#
# Input: The Excel file "SK _ PL 31 July 2025 v62.0_HO_public.xlsx"
# Output: A data frame containing all identified duplicate entries and a
#   console output showing the total number of duplicate rows found
# ------------------------------------------------------


# Step 0: Setup All Environments ------------------------------------------

# Load All Libraries
library(tidyverse)
library(readxl)
library(writexl)

# Load All Path Files
input_file_path <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/redundance_samples/"
plink_file_path <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/ibd_lipson_duplicate_samples/" # หา path ที่มี fam file
output_file_path <- "C:/Users/patch/Desktop/"


# Step 1: Retrieve the Data -----------------------------------------------

df <- read_excel(file.path(input_file_path, "SK _ PL 31 July 2025 v62.0_HO_public.xlsx"),
                 sheet = "Lipson samples") # nrow = 430 = excel
nrow(df) # 430

fam_df <- read.table(file.path(plink_file_path, "modern_lipson_ancient.fam"),
                     header = FALSE,
                     col.names = c("FID", "IID", "PathernalID", "MothernalID", "Sex", "Phenotype"),
                     stringsAsFactors = FALSE)


# Step 2: Data Wraggling --------------------------------------------------

# Find all duplicates in a single operation
all_duplicates <- df %>% 
  mutate(Genetic_ID_Base = str_remove(`Genetic ID`, "\\.[A-Z]{2}$")) %>% 
  
  # สร้าง Flag (ติดป้าย) สำหรับ Master ID ที่ซ้ำกัน
  group_by(`Master ID`) %>% 
    mutate(is_master_dup = n() > 1) %>%   # ถ้าตรงเงื่อนไขจะเป็น TRUE ถ้าไม่ตรงจะเป็น FALSE
    ungroup() %>% 
  
  # สร้าง Flag สำหรับ Genetic ID ที่ซ้ำกัน
  group_by(Genetic_ID_Base) %>% 
    mutate(is_genetic_dup = n() > 1) %>% 
    ungroup() %>% 
  
  # กรองเอาเฉพาะที่เข้าข่ายอย่างน้อย 1 เงื่อนไข
  filter(is_master_dup | is_genetic_dup) %>%  # ที่เป็น TRUE เท่านั้น
  
  # ลบคอลัมน์ที่ไม่ได้ใช้ออกไป
  select(-Genetic_ID_Base, -is_master_dup, -is_genetic_dup) %>% 
  arrange(`Master ID`, `Genetic ID`)

cat("--- ตารางที่รวม duplicate ทั้งหมดมี", nrow(all_duplicates), "แถว ---\n")

# บันทึกไฟล์
# write_xlsx(all_duplicates, path = file.path(output_file_path, "Lipson_duplicates_report.xlsx")) # excel


# Step 3: Create a PLINK File (.fam) for Keeped. --------------------------

# นำ Master ID ที่ได้จาก excel ไปหา FID และ IID คู่กันใน .fam
plink_filter_list <- inner_join(
  fam_df,
  all_duplicates,
  by = c("IID" = "Genetic ID")   # บอกให้ R รู้ว่า IID ของ .fam ตรงกับ Master ID ของ excel
) %>% 
  select(FID, IID)  # เลือกแค่ 2 columns (fam file) ที่ PLINK program ต้องการ

# บันทึกเป็น fam file (.txt)
write.table(
  plink_filter_list,
  file = file.path(output_file_path, "suspected_duplicates.txt"),
  quote = FALSE,          # ไม่ต้องมีเครื่องหมายคำพูด
  sep = "\t",             # คั่นด้วย tab (หรือ space ก็ได้)
  row.names = FALSE,      # ไม่ต้องมีเลขแถว
  col.names = FALSE       # ไม่ต้องมี header (FID, IID)
)

