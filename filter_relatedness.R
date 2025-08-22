# ------------------------------------------------------
# File: filter_relatedness.R
# Purpose: Process PLINK IBD results to filter duplicate and related samples.
# Author: Pirada Naewkam
# Date: 2025-08-22
# Description:
#   Identifies and removes duplicates (PI_HAT > 0.9) and related individuals
#   (0.125 < PI_HAT < 0.9) using metadata criteria.
# Input: .genome, metadata file, .fam file.
# Output: samples_to_remove.txt
# ------------------------------------------------------


# Step 0: Setup All Environments ------------------------------------------

# Load All Libraries
library(tidyverse)
library(readxl)
library(writexl)

# Load All Path Files
input_file_path <- "results/lipson_duplicates_ibd.genome"
metadata_file_path <- "C:/Old Volume/Work/Project Work/Poseidon/Poseidon Project/redundance_samples/Lipson_duplicates_report.xlsx"
new_fam_file_path <- "scripts/suspected_duplicates.txt"
old_fam_file_path <- "full_data/modern_lipson_ancient.fam"
output_dir <- "results"


# Step 1: Retrieve the Data -----------------------------------------------

# สร้าง metadata dataframe
metadata_df <- read_xlsx(metadata_file_path) %>% 
  select("IID" = `Genetic ID`, "Study_Date" = `Year data`) %>% 
  mutate(Study_Date = as.numeric(Study_Date))   # เปลี่ยนให้ Study_Date เป็นตัวเลข เพื่อให้สามารถเปรียบเทียบได้
nrow(metadata_df)  # 204 rows

# สร้าง dataframe ของ genome file
genome_df <- read.table(input_file_path, header = TRUE)
nrow(genome_df) # 139 rows

# สร้าง fam file อันใหม่ที่ได้มาจาก duplicates และ fam file อันเก่าของ lipson sample ทั้งหมด
new_fam <- read.table(new_fam_file_path)
old_fam <- read.table(old_fam_file_path)
colnames(old_fam) <- c("FID", "IID", "PaternalID", "MaternalID", "Sex", "Phenotype")


# Step 2: Data Wraglling --------------------------------------------------

# เรียงลำดับ PI_HAT มากไปน้อย
related_pairs <- genome_df %>% 
  arrange(desc(PI_HAT))

# สร้าง vector ว่าง ที่จะลบ IID
samples_to_remove <- c()

# วนลูปเพื่อตัดสินใจว่าจะลบตัวไหนในแต่ละคู่ PI_HAT
for (i in 1:nrow(related_pairs)) {
  
  pair <- related_pairs[i, ]
  iid1 <- pair$IID1
  iid2 <- pair$IID2
  
  # ถ้าใครคนหนึ่งในคู่นี้อยู่ใน list ที่ลบไปแล้ว ให้ข้ามไปเลย
  if (iid1 %in% samples_to_remove || iid2 %in% samples_to_remove) {
    next
  }
  
  # ดึงข้อมูลของ metadata ทั้งสองตัวอย่าง
  meta1 <- metadata_df %>% filter(IID == iid1)
  meta2 <- metadata_df %>% filter(IID == iid2)
  
  # สร้างเกณฑ์ตัดสินใจ เก็บ Year data ที่ใหม่กว่าไว้
  if (meta1$Study_Date >= meta2$Study_Date) {
    samples_to_remove <- c(samples_to_remove, iid2) # meta1 ใหม่กว่าหรือเท่ากัน เก็บ meta1 ไว้ ลบ meta2
  } else {
    samples_to_remove <- c(samples_to_remove, iid1) # meta2 ใหม่กว่าก็ให้เก็บ meta2 ลบ meta1
  }
}

# ได้รายชื่อทั้งหมดที่จะลบ
final_remove_list <- unique(samples_to_remove)

# บันทึกไฟล์สำหรับใช้กับ PLINK
plink_removal_df_file <- old_fam %>% 
  filter(IID %in% final_remove_list) %>% 
  select(FID, IID)

# บันทึกเป็นไฟล์ .txt
write.table(
  plink_removal_df_file,
  file.path(output_dir, "samples_to_remove.txt"),
  quote = FALSE,
  sep = "\t",
  row.names = FALSE,
  col.names = FALSE
)

