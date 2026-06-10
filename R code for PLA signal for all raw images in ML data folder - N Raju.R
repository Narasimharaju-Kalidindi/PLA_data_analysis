rm(list = ls())

# select your ML Data folder

setwd(choose.dir())   

# Getting data from MatLab raw data CSV files 

all_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Identifying cell and puncta files
cell_files <- all_files[grepl("Cell Label", basename(all_files), ignore.case = TRUE)]
puncta_files <- all_files[grepl("Puncta_statistics", basename(all_files), ignore.case = TRUE)]

# Extracting raw image name as everything BEFORE the keyword

get_raw_name_cell <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("(?i)\\s*Cell Label.*$", "", x, perl = TRUE)
  trimws(x)
}

get_raw_name_puncta <- function(x) {
  x <- tools::file_path_sans_ext(basename(x))
  x <- sub("(?i)\\s*Puncta_statistics.*$", "", x, perl = TRUE)
  trimws(x)
}

cell_names <- sapply(cell_files, get_raw_name_cell)
puncta_names <- sapply(puncta_files, get_raw_name_puncta)

# Showing extracted names for checking

cat("Cell files and extracted names:\n")
print(data.frame(file = basename(cell_files), raw_name = cell_names))

cat("\nPuncta files and extracted names:\n")
print(data.frame(file = basename(puncta_files), raw_name = puncta_names))

# Keeping only exact matches

common_names <- intersect(cell_names, puncta_names)

if (length(common_names) == 0) {
  stop("Still no matches. Check the printed extracted names above.")
}

# Output table
df1 <- data.frame(
  Raw_Image_Name = common_names,
  PLA_Signal = rep(NA_real_, length(common_names)),
  stringsAsFactors = FALSE
)

# calculations 
for (i in seq_along(common_names)) {
  
  current_name <- common_names[i]
  
  cell_path <- cell_files[which(cell_names == current_name)[1]]
  puncta_path <- puncta_files[which(puncta_names == current_name)[1]]
  
  Cell_Label <- read.csv(cell_path, header = TRUE)
  Puncta_Label <- read.csv(puncta_path, header = TRUE)
  
  # Counting total cells numbers in each image
  Cell_count <- nrow(Cell_Label)
  
  # Calculating each Punctum intensity in an image
  Punctum_intensity <- Puncta_Label$Area.Px. * Puncta_Label$MeanIntensity
  
  # Calculating Puncta intensity of each image
  Puncta_intensity <- sum(Punctum_intensity, na.rm = TRUE)
  
  # Calculating PLA signal of each image
  PLA_Signal <- Puncta_intensity / Cell_count
  
  df1$PLA_Signal[i] <- PLA_Signal
}

print(df1)

dir.create("Results", FALSE)
write.csv(df1, "Results/PLA_Output.csv", row.names = FALSE)