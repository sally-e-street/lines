### setup ###

# please note - aspects of this R script are not fully reproducible as they require access to images that cannot be shared online for copyright reasons. Some code also contains file paths that are specific to the author's computer which would need to be amended by other users 

# clear workspace
rm(list=ls()) 

### trace lines ###

## full sample (n=162 images, all manually traced by first author SS)

# load tracing function
source("https://raw.githubusercontent.com/sally-e-street/lines/refs/heads/main/tracing") 

# set directory to folder where the traced line coordinates will be written
setwd("~/Dropbox/Sim_graphics_project/Coords")

# list the files containing images for tracing
image_files<-list.files("~/Dropbox/Sim_graphics_project/Images")

# manually trace lines and save coordinate files to the working directory
for (i in 1:length(image_files)){
	rLineTrace(paste("~/Dropbox/Sim_graphics_project/Images", image_files[i], sep="/"))
}

# plot selected images with traced lines overlaid
coords_files<-list.files("~/Dropbox/Sim_graphics_project/Coords")
par(mfrow=c(1,4)) 
for (i in 1:4){ # select first four images to plot
	image<-paste("~/Dropbox/Sim_graphics_project/Images", image_files[i], sep="/")
	plot(imager::load.image(image))
	coords<-read.csv(paste("~/Dropbox/Sim_graphics_project/Coords", coords_files[i], sep="/"))
	segments(coords$x1, coords$y1, coords$x2, coords$y2, col="green", lwd=2)
}

## retraced random sample (n=30 images, manually re-traced by first author SS)

# identify a stratified sample of 30 images from the 3 empirical reference samples for retracing 
set.seed(123)
retracing_sample<-c(sample(image_files[grepl("I_", files)], size=10), 		
			sample(image_files[grepl("B_", files)], size=10), 
			sample(image_files[grepl("G_", files)], size=10))

# shuffle the order of images to break up reference sample blocks
retracing_sample<-sample(retracing_sample, size=30) 

# set a different working directory for re-traced line coordinates to be saved
setwd("~/Dropbox/Sim_graphics_project/Coords_SS2")

# re-trace lines and save log files
for (i in 1:length(retracing_sample)){
	rLineTrace(paste("~/Dropbox/Sim_graphics_project/Images", retracing_sample[i], sep="/"))
}

## retraced SignBase images where photos (of sufficient quality) available (n=26)

# set a different working directory for re-traced line coordinates to be saved
setwd("~/Dropbox/Sim_graphics_project/Coords_SB_photos")

# list the files containing images for tracing
SB_photos<-list.files("~/Dropbox/Sim_graphics_project/SB_original_photos")

# re-trace lines and save log files
for (i in 1:length(SB_photos)){
	rLineTrace(paste("~/Dropbox/Sim_graphics_project/SB_original_photos", SB_photos[i], sep="/"))
}

### create simulated lines ###

# load line sim function
source("https://raw.githubusercontent.com/sally-e-street/lines/refs/heads/main/simulated_lines") 

# set directory to location for coordinate files to be written
setwd("~/Dropbox/Sim_graphics_project/Coords")

# get distribution of n lines from the empirical reference samples
empirical_coords<-coords_files[!grepl("S_", coords_files)] # exclude simulated coords
empirical_n_lines<-NA
	for (i in 1:length(empirical_coords)){
		empirical_n_lines[i]<-nrow(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords", empirical_coords[i], sep="/")))
	}
	
# get list of convex hulls from the empirical reference samples (note - these will be flipped upside down relative to original images due to imager package using a flipped y axis but this does not make any difference to extracted geometric/spatial features)
empirical_hulls<-list()
for (i in 1:length(empirical_coords)){
	hull_coords<-read.csv(paste("~/Dropbox/Sim_graphics_project/Coords", empirical_coords[i], sep="/")) # create a copy of coordinates to make the convex hull
	colnames(hull_coords)<-c("x", "x", "y", "y") # change the column names to allow rbind to work (reformat to 2 col matrix)
	hull_coords_combined<-rbind(hull_coords[,c(1,3)], hull_coords[,c(2,4)]) # reformat hull coords
	conv_hull<-chull(hull_coords_combined) # get convex hull
	empirical_hulls[[i]] <-hull_coords_combined[conv_hull,] # get coords of hull edges and add to empirical_hulls list
}

# simulate 50 line sets
rLineSim(n=50, n_lines=empirical_n_lines, hulls=empirical_hulls)


### feature extraction ###

## full dataset

# load function
source("https://raw.githubusercontent.com/sally-e-street/lines/refs/heads/main/features") 

# update coords_files after simulating lines
coords_files<-list.files("~/Dropbox/Sim_graphics_project/Coords") 

# create dataset for analysis (using default 10 bins for entropy functions)
data<-as.data.frame(t(sapply(coords_files, FUN=function(x){rLineFeatures(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords", x, sep="/")))})))

# check data structure
str(data) # 212 rows, 15 columns
sum(is.na(data)) # no missing values

# create in the data column for filenames
data$filename<-rownames(data) 

# add variable for sample
data$sample<-NA
data[grepl("_S_", data$filename),]$sample<-"random"
data[grepl("_I_", data$filename),]$sample<-"intentional"
data[grepl("_B_", data$filename),]$sample<-"incidental"
data[grepl("_G_", data$filename),]$sample<-"geology"
data[grepl("_Q_", data$filename),]$sample<-"query"
data$sample<-factor(data$sample, levels=c("intentional", "incidental", "geology", "random", "query"))

# add variable for source figure (unique for all line sets apart from geological sample)
data$figure<-data$filename
data[grepl("Berger", data$filename),]$figure<-"Berger"
data[grepl("Herries", data$filename),]$figure<-"Herries"
data[grepl("Martinon24_Fig1b", data$filename),]$figure<-"Martinon1b"
data[grepl("Martinon24_Fig1c", data$filename),]$figure<-"Martinon1c"
data[grepl("Martinon24_Fig1d", data$filename),]$figure<-"Martinon1d"
data[grepl("Rodriguez", data$filename),]$figure<-"Rodriguez"

## re-traced datasets

# SS re-traced lines
coords_SS2_files<-list.files("~/Dropbox/Sim_graphics_project/Coords_SS2")
SS2_data<-as.data.frame(t(sapply(coords_SS2_files, FUN=function(x){rLineFeatures(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords_SS2", x, sep="/")))})))
str(SS2_data) # 30 rows, 15 columns
SS2_data$filename<-rownames(SS2_data) # create column for filenames

# SE re-traced lines
coords_SE_files<-list.files("~/Dropbox/Sim_graphics_project/Coords_SE")
SE_data<-as.data.frame(t(sapply(coords_SE_files, FUN=function(x){rLineFeatures(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords_SE", x, sep="/")))})))
str(SE_data) # 30 rows, 15 columns
SE_data$filename<-rownames(SE_data) # create column for filenames

# GPT remarked dataset
coords_GPT_files<-list.files("~/Dropbox/Sim_graphics_project/Coords_GPT")
GPT_data<-as.data.frame(t(sapply(coords_GPT_files, FUN=function(x){rLineFeatures(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords_GPT", x, sep="/")))})))
str(GPT_data) # 28 rows, 15 columns (two images dropped due to tracing quality issues)
GPT_data$filename<-rownames(GPT_data) # create column for filenames

# SB original photos
coords_SB_photos<-list.files("~/Dropbox/Sim_graphics_project/Coords_SB_photos")
SB_photo_data<-as.data.frame(t(sapply(coords_SB_photos, FUN=function(x){rLineFeatures(read.csv(paste("~/Dropbox/Sim_graphics_project/Coords_SB_photos", x, sep="/")))})))
str(SB_photo_data) # 26 rows, 15 columns 
SB_photo_data$filename<-rownames(SB_photo_data) # create column for filenames

# write data to files in working directory
setwd("~/Dropbox/Sim_graphics_project")
write.csv(data, file="full_dataset.csv", row.names=F)
write.csv(SS2_data, file="SS_retraced_dataset.csv", row.names=F)
write.csv(SE_data, file="SE_retraced_dataset.csv", row.names=F)
write.csv(GPT_data, file="GPT_retraced_dataset.csv", row.names=F)
write.csv(SB_photo_data, file="SB_photos_retraced_dataset.csv", row.names=F)

