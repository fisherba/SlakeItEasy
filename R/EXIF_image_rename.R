# Load required packages
install.packages("exifr")  # If not already installed
library(exifr)

# Set the directory containing the images
image_dir <- "~/Documents/r/Slake/CSC/Summer2024"  # Change this to your actual folder path


# Get the list of all .jpg files in the root directory and subdirectories
image_files <- list.files(image_dir, pattern = "\\.jpg$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

#view the list of all .jpg files found
image_files


# Check if any images are found
if (length(image_files) == 0) {
  cat("No images found in the specified directory.\n")
} else {
  # Loop through each image file and rename based on CreateDate
  for (image_path in image_files) {

    # Extract EXIF metadata
    metadata <- read_exif(image_path, tags = "CreateDate")

    # Check if CreateDate exists
    if (!is.na(metadata$CreateDate)) {

      # Format CreateDate into IMG_YYYYMMDD_HHMMSS.jpg
      new_name <- format(as.POSIXct(metadata$CreateDate, format="%Y:%m:%d %H:%M:%S"), "IMG_%Y%m%d_%H%M%S.jpg")

      # Keep the file in the same directory
      image_dir <- dirname(image_path)  # Get the current folder of the image
      new_path <- file.path(image_dir, new_name)  # Define new full path

      # Rename the file if the new name doesn't already exist
      if (!file.exists(new_path)) {
        file.rename(image_path, new_path)
        cat("Renamed:", image_path, "->", new_name, "\n")
      } else {
        cat("Skipped:", image_path, "(Target filename already exists)\n")
      }
    } else {
      cat("No CreateDate found for:", image_path, "\n")
    }
  }
}
