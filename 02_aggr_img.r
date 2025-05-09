library("R.matlab")

# List matlab image files to merge
img_files <- list.files(path = "./data/raw/rocksizes",
												pattern = "\\.mat$",
												full.names = TRUE)

# Loop over each matlab file, select variables of interest
# create a dataframe with the image name & variables
df_img <- mapply(file = img_files, FUN = function(file) {
		data <- readMat(file)
		img <- sub(".mat$", "", basename(file))
		D50 <- data$D50
		D65 <- data$D65
		D84 <- data$D84
		PS <- data$PS
		return(data.frame(img, D50, D65, D84, PS))
}, SIMPLIFY = FALSE)
df_img <- do.call(rbind, df_img)

# Save results to csv for downstream analysis
write.csv(df_img, file = "./data/df_img.csv", row.names = TRUE, quote = FALSE)