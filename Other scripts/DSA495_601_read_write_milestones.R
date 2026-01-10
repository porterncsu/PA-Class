# Install packages if not already installed and load libraries
required_packages <- c("yaml", "dplyr")
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!pkg %in% installed_packages) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}


# Set path with data files to download folder on my mac
data_path <- "~/Downloads/"

# Set subdirectory
subdir <- "Milestone6/"

# Find the single yml file in the subdirectory and load
yml_file <- list.files(path = paste0(data_path, subdir), pattern = "\\.yml$", full.names = TRUE)
data <- yaml::read_yaml(yml_file)


# Get all the top-level keys (PDF filenames)
pdf_files <- names(data)

# Extract all student IDs
student_ids <- c()

for (pdf in pdf_files) {
  # Extract the student ID from the YAML entry
  sid_full <- data[[pdf]]$`:submitters`[[1]]$`:email`
  
  # Extract just the username part (before the colon)
  student_id <- strsplit(sid_full, "@")[[1]][1]
  
  student_ids <- c(student_ids, student_id)
  
  # Print the PDF and corresponding student ID
  cat("PDF:", pdf, "-> Student ID:", student_id, "\n")

  # save pdf file to new name with student ID
  file.rename(from = paste0(data_path, subdir, pdf), to = paste0(data_path, subdir, "DSA495_601_f25_Milestone6_",student_id, ".pdf"))
}

# Print all student IDs
cat("\nAll student IDs:\n")
print(student_ids)