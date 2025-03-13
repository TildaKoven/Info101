library(ggplot2)
library(marinecs100b)


# Questionable organization choices ---------------------------------------

# P1 Call the function dir() at the console. This lists the files in your
# project's directory. Do you see woa.csv in the list? (If you don't, move it to
# the right place before proceeding.)
#yes

# P2 Critique the organization of woa.csv according to the characteristics of
# tidy data.
#It does not have consistent names and formats, there is spaces in the column
#names and there is no units for the depths. It is also not rectangular.
#It is confusing the computer and me.

# Importing data ----------------------------------------------------------

# P3 Call read.csv() on woa.csv. What error message do you get? What do you
# think that means?
read.csv("woa.csv")
#It says there is an error in read.table. Because the data is not tidy and R is
#confused.

# P4 Re-write the call to read.csv() to avoid the error in P3.
better_woa.csv <- read.csv("woa.csv", skip = 1)
#row 1 was confusing the computer
# Fix the column names ----------------------------------------------------

# P5 Fill in the blanks below to create a vector of the depth values.

depths <- c(
  seq(0, 100, by = 5),
  seq(125, 500, by = 25),
  seq(550, 2000, by = 50),
  seq(2100, 5500, by = 100)
)


# P6 Create a vector called woa_colnames with clean names for all 104 columns.
# Make them the column names of your WOA data frame.
woa_colnames <- c("latitude", "longitude", paste0("Depth", depths))
colnames(better_woa.csv) <- woa_colnames
View(better_woa.csv)
# Analyzing wide-format data ----------------------------------------------

# P7 What is the mean water temperature globally in the twilight zone (200-1000m
# depth)?
woa <- read.csv("woa.csv", skip = 1)
woa_twilight <- woa[ , 27:49]
sum_woa_twilight <- sum(woa_twilight, na.rm = TRUE)
mean_temps_twilight <- (sum_woa_twilight)/((47-25)*(40566-3))
mean_temps_twilight
#6.718066

mean_temp_twilight <- (sum_woa_twilight)/(nrow(woa_twilight)*ncol(woa_twilight))
mean_temp_twilight
#6.425818

woa <- read.csv("woa.csv", skip = 1)
woa_twilight <- woa[ , 27:49]
sum_woa_twilight <- sum(woa_twilight, na.rm = TRUE)
num_measurements <- (sum(!is.na(woa[, 27:49])))
sum_woa_twilight/num_measurements

nrow(woa_twilight)*ncol(woa_twilight)
(47-25)*(40566-3)
# Analyzing long-format data ----------------------------------------------

# P8 Using woa_long, find the mean water temperature globally in the twilight zone.
View(woa_long)
mean("temp_c"[1,])
twilight_temps <- woa_long[woa_long$depth_m >= 200 & woa_long$depth_m <= 1000, 4]
mean(twilight_temps)
#6.573227

# P9 Compare and contrast your solutions to P8 and P7.
# It was easier to find the mean in part 8 because of the long format and
#because there are no blank cells we need to account for. In part 7, the wide
#format makes it hard to know how many cells have data in them. We used two
#different methods to determine that and got two slightly different answers which
#were both a bit off from the correct answer we got in Part 8. But overall,
#all of our answers were pretty close together.

# P10 Create a variable called mariana_temps. Filter woa_long to the rows in the
# location nearest to the coordinates listed in the in-class instructions.

mariana_lat <- woa_long[woa_long$latitude == 11.5, ]
mariana_long <- woa_long[woa_long$longitude == 142.5, ]
mariana_lat
mariana_long
mariana_temps <- woa_long[woa_long$latitude == 11.5 & woa_long$longitude == 142.5, ]
mariana_temps

ggplot(mariana_temps, aes(temp_c, depth_m)) +
  geom_path() +
  scale_y_reverse()
ggsave("mariana_temp_depth.png")
# P11 Interpret your temperature-depth profile. What's the temperature at the
#surface? How about in the deepest parts? Over what depth range does temperature
#change the most?
#The temperature at the surface is 28.65 Celsius. In the deepest parts, the
#temperature is 1.53 Celsius. The temperature changes the most in the top 450 ft.
#This makes sense because when I scuba dive, the water gets really cold quite fast.4

# ggplot is a tool for making figures, you'll learn its details in COMM101
ggplot(mariana_temps, aes(temp_c, depth_m)) +
  geom_path() +
  scale_y_reverse()
