######FROM HERE ON OUT Keuchel
# Print the first 6 rows of ff_dt
head(sl_dt_DK)
#format game dates in this vector
# Create game_date in ff_dt
sl_dt_DK$game_date <- as.Date(row.names(sl_dt_DK), "%Y-%m-%d")

# Rename the first column
colnames(sl_dt_DK)[1] <- "SL_spin_rate"

# Remove row names
row.names(sl_dt_DK) <- NULL

# View head of ff_dt
head(sl_dt_DK)
#plot game by game slider spin rate
plot(sl_dt_DK$SL_spin_rate ~ sl_dt_DK$game_date, lwd = 4, type = "l", ylim = c(1500, 2800),
     main = "CK Slider Spin Rate",
     xlab = "Date", ylab = "Spin Rate")
# Add jittered points to the plot
points(AllFX17_DK_SL$spin_rate ~ jitter(as.numeric(AllFX17_DK_SL$game_date)),
       pch = 16, col = "#99004450")
#PITCH MIX table
# Subset the data to remove pitch types "IN" and "EP"
AllFX17_DK <- subset(AllFX17_DK, pitch_type != "IN" & pitch_type != "EP")

# Drop the levels from pitch_type
#AllFX17_DK$pitch_type <- droplevels(AllFX17_DK$pitch_type)
#character class, rather than factor so above function doesn't do anything
# Create type_tab
type_tab <- table(AllFX17_DK$pitch_type, AllFX17_DK$PS_All)

# Print type_tab
type_tab
# Create type_prop table
type_prop <- round(prop.table(type_tab, margin = 2), 3)

# Print type_prop
type_prop

#compare spin rate to slider usage overall, to see if less spin -> slider usage
# Create ff_prop
SL_prop <- type_prop[5,]
# Print ff_prop
SL_prop
#Spin rate split out for postseason
SL_sr_month <- tapply(AllFX17_DK$spin_rate, AllFX17_DK$PS_All, mean)

type_prop <- as.data.frame.matrix(type_prop)
Pitch <- rownames(type_prop)
type_prop <- cbind(Pitch, type_prop)
colnames(type_prop) <- paste(c("Pitch", "RegSeason"))#include "PS" as third column when that is added in
# Print type_prop
type_prop
# Create the Difference column
type_prop$Difference <- (type_prop$PS - type_prop$RegSeason)/type_prop$RegSeason

# Plot a barplot
barplot(type_prop$Difference, names.arg = type_prop$Pitch, 
        main = "Pitch Usage in PS vs. Reg Season", 
        ylab = "Percentage Change in PS", 
        ylim = c(-0.3, 0.3))
#what type of pitches used in different ball strike frequencies
# Create bs_table
bs_table <- table(greinke$balls, greinke$strikes)

# Create bs_prop_table
bs_prop_table <- round(prop.table(bs_table), 3)

# Print bs_prop_table
bs_prop_table
#skip at 2.7 ball strike point, will come back once i get the DB working, will look at locational changes now
#############

#pitch selection fro innfing 5 on, = Create the late_in_game column
AllFX17_DK$late_in_game <- ifelse(AllFX17_DK$inning > 5, 1, 0)
# Convert late_in_game
AllFX17_DK$late_in_game <- factor(AllFX17_DK$late_in_game)
# Create type_late
type_late_prop <- table(AllFX17_DK$pitch_type, AllFX17_DK$late_in_game)
# Create type_late_prop
type_late_prop <- round(prop.table(type_late_prop, margin = 2), 3)
# Print type_late_prop
type_late_prop
# Create t_type_late
t_type_late_prop <- t(type_late_prop)
# Print dimensions of t_type_late
dim(t_type_late_prop)
# Print dimensions of type_late
dim(type_late_prop)
# Change row names
rownames(t_type_late_prop) <- c("Early", "Late")
# Make barplot using t_type_late
barplot(t_type_late_prop, beside = TRUE, col = c("red", "blue"), 
        main = "Early vs. Late In Game Pitch Selection", 
        ylab = "Pitch Selection Proportion", 
        legend = rownames(t_type_late_prop))

#Pitch selection dependent on men being on base
AllFX17_DK$late_in_game <- ifelse(AllFX17_DK$inning > 5, 1, 0)
# Convert late_in_game
AllFX17_DK$late_in_game <- factor(AllFX17_DK$late_in_game)
# Create type_late
type_late_prop <- table(AllFX17_DK$pitch_type, AllFX17_DK$late_in_game)
# Create type_late_prop
type_late_prop <- round(prop.table(type_late_prop, margin = 2), 3)
# Print type_late_prop
type_late_prop
# Create t_type_late
t_type_late_prop <- t(type_late_prop)
# Print dimensions of t_type_late
dim(t_type_late_prop)
# Print dimensions of type_late
dim(type_late_prop)
# Change row names
rownames(t_type_late_prop) <- c("Early", "Late")
# Make barplot using t_type_late
barplot(t_type_late_prop, beside = TRUE, col = c("red", "blue"), 
        main = "Early vs. Late In Game Pitch Selection", 
        ylab = "Pitch Selection Proportion", 
        legend = rownames(t_type_late_prop))

#pitch mix with men on base
AllFX17_DK$men_on_base <- ifelse(complete.cases(AllFX17_DK$on_1b), 1, 0)
AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_2b) & !complete.cases(AllFX17_DK$on_3b)] <- 2
AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_3b)] <- 3
AllFX17_DK$men_on_base <- factor(AllFX17_DK$men_on_base)

#AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_1b) & complete.cases(AllFX17_DK$on_2b)] <- '1st and 2nd'
#AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_3b) & complete.cases(AllFX17_DK$on_2b)] <-
#                                      "2nd and 3nd"
#AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_1b) & complete.cases(AllFX17_DK$on_3b)] <-
#                                                             "1st and 3rd"
#AllFX17_DK$men_on_base[complete.cases(AllFX17_DK$on_3b) & complete.cases(AllFX17_DK$on_2b)
#                                                             & complete.cases(AllFX17_DK$on_1b)] <- "Bases Loaded"
#AllFX17_DK$men_on_base[AllFX17_DK$men_on_base == 0] <- 'Bases Empty'
unique(AllFX17_DK$men_on_base)
#AllFX17_DK$man_on_3rd <- ifelse(complete.cases(AllFX17_DK$on_3b), 1, 0)
#AllFX17_DK$man_on_1st <- factor(AllFX17_DK$man_on_1st)
#AllFX17_DK$man_on_2nd <- factor(AllFX17_DK$man_on_2nd)
#AllFX17_DK$man_on_3rd <- factor(AllFX17_DK$man_on_3rd)
type_OB <- table(AllFX17_DK$pitch_type, AllFX17_DK$men_on_base)
type_OB
#ordered <- c("Bases Empty","1st","2nd","3rd","1st and 2nd","1st and 3rd","2nd and 3rd", "Bases Loaded") 
#type_OB <- type_OB[,c(7,1,4,6,2,3,5,8)] 
OB_prop <- round(prop.table(type_OB, margin = 2), 3)
OB_prop
OB_prop<- t(OB_prop)
dim(OB_prop)
dim(type_OB)
# Change row names
rownames(OB_prop) <- c("Bases Empty","1st","2nd","3rd")
# Make barplot using OB_prop
#might put more colors, or just do man on base
barplot(OB_prop, beside = TRUE, col = c("green", "yellow", "orange", "red"),
        main = "Situational Pitch Type by Farthest Runner", 
        ylab = "Pitch Selection Proportion", 
        legend = rownames(OB_prop), args.legend = list(x = "topleft"))
#still need to put locational values

#horizontal and vertical location variables: px and pz
# Calculate average pitch height in inches (multiply by 12) in July vs. other months
tapply(AllFX17_DK$pz, AllFX17_DK$PS_All, mean) * 12
# Create AllFX17_DK_lhb
AllFX17_DK_lhb <- subset(AllFX17_DK, batter_stand == "L")
# Create AllFX17_DK_rhb
AllFX17_DK_rhb <- subset(AllFX17_DK, batter_stand == "R")
# Compute average px location for LHB
tapply(AllFX17_DK_lhb$px, AllFX17_DK_lhb$PS_All, mean) * 12
# Compute average px location for RHB
tapply(AllFX17_DK_rhb$px, AllFX17_DK_rhb$PS_All, mean) * 12

# Plot location of all pitches
plot(AllFX17_DK$pz ~ AllFX17_DK$px,
     col = factor(AllFX17_DK_lhb$PS_All),
     xlim = c(-3, 3))
# Formatting code, don't change this
par(mfrow = c(1, 2))
# Plot the pitch loctions for July
plot(pz ~ px, data = AllFX17_DK[AllFX17_DK$PS_All != "other"],
     col = "red", pch = 16,
     xlim = c(-3, 3), ylim = c(-1, 6),
     main = "Postseason")
# Plot the pitch locations for other months
plot(pz ~ px, data = AllFX17_DK[AllFX17_DK$PS_All == "other"],
     col = "black", pch = 16,
     xlim = c(-3, 3), ylim = c(-1, 6),
     main = "Regular Season")

#subsetting the data to exclude any pitch well outside the strike zone, define "well outside the strike zone" as any pitch 
#more than 2 feet inside/outside from the center of the plate, below 0 feet (i.e. bouncing), or above 5 feet
# Create greinke_sub
AllFX17_DK$zone_px <- (ceiling(AllFX17_DK$px) + floor(AllFX17_DK$px)) / 2
AllFX17_DK$zone_pz <- (ceiling(AllFX17_DK$pz) + floor(AllFX17_DK$pz)) / 2

AllFX17_DK_sub <- subset(AllFX17_DK, px>-2 & px<2 & pz>0 & pz<5)

# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Kershaw Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")
# Add the grid lines
grid(lty = "solid", col = "black")
#20 possibilities for the zone variable, numbered 1 through 20. Each classification tells us about the location of the 
#given pitch, binned as a grid across the strike zone and just outside the strike zone. There is also a zone_px and zone_pz 
#variable that identify the middle of each of these locational bins

# Create table
AllFX17_DK_table <- table(AllFX17_DK_sub$zone)

# Create zone_prop
zone_prop <- round(prop.table(AllFX17_DK_table), 3)

# Plot strike zone grid, don't change this
# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Kershaw Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")
# Add the grid lines
grid(lty = "solid", col = "black")
# Add text from zone_prop[1]
text(zone_prop[1], x=-1.5, y=4.5, cex=1.5)

#use a for loop to plot the proportions for each zone in the grid. This prevents you from having to individually plot the 
#text() for each zone onto the grid with its own line of code.
# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Kershaw Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")
# Add the grid lines
grid(lty = "solid", col = "black")
# Plot text using for loop
for(i in 1:20) {
  text(mean(AllFX17_DK_sub$zone_px[AllFX17_DK_sub$zone == i]),
       mean(AllFX17_DK_sub$zone_pz[AllFX17_DK_sub$zone == i]),
       zone_prop[i], cex = 1.5)
}

#zone location proportion differences from Reg Season vs Postseason
# Create zone_prop_PS
zone_prop_PS <- round(
  table(AllFX17_DK_sub$zone[AllFX17_DK_sub$PS_All != "other"]) /
    nrow(subset(AllFX17_DK_sub, PS_All != "other")), 3)
# Create zone_prop_other
zone_prop_other <- round(
  table(AllFX17_DK_sub$zone[AllFX17_DK_sub$PS_All == "other"]) /
    nrow(subset(AllFX17_DK_sub, PS_All == "other")), 3)
# Print zone_prop_PS
zone_prop_PS
# Print zone_prop_other
zone_prop_other
########fix this here 
## Fix zone_prop_july vector, don't change this
# This line is necessary b/c Greinke didn't pitch to zone 4 at all. As a result, zone_prop_july is one observation shorter 
#than zone_prop_other, need to adjust accordingly for CK
# Fix zone_prop_july vector, don't change this
zone_prop_july2 <- c(zone_prop_july[1:3], 0.00, zone_prop_july[4:19])
names(zone_prop_july2) <- c(1:20)
# Create zone_prop_diff
zone_prop_diff <- zone_prop_july2 - zone_prop_other
# Print zone_prop_diff
zone_prop_diff
##ok resume with plug n play

#zone proportion differences
# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Kershaw Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")
# Add the grid lines
grid(lty = "solid", col = "black")
# Create for loop
for(i in 1:20) {
  text(mean(AllFX17_DK_sub$zone_px[AllFX17_DK_sub$zone == i]),
       mean(AllFX17_DK_sub$zone_pz[AllFX17_DK_sub$zone == i]),
       zone_prop_diff[i], cex = 1.5)
}

#propensity to throw to each zone location depending on the count (i.e. number of balls and strikes)
#you will put together a table of location proportions for each ball-strike count situation.
#need to add new variable for ball strike
AllFX17_DK_sub$bs_count <-
  # Create greinke_zone_tab
  CK_zone_tab <- table(AllFX17_DK_sub$zone, AllFX17_DK_sub$bs_count)
# Create zone_count_prop
zone_count_prop <- round(prop.table(CK_zone_tab, margin = 2), 3)
# Print zone_count_prop
zone_count_prop

#create a table of differences for just the 0-2 and 3-0 counts
# Create zone_count_diff
zone_count_diff <- zone_count_prop[, 3] - zone_count_prop[, 10]
#those subsets depends on which columns have to do with the count
# Print the table
zone_count_diff

# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Kershaw Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")
# Add the grid lines
grid(lty = "solid", col = "black")
# Add text to the figure for location differences
for(i in 1:20) {
  text(mean(AllFX17_DK_sub$zone_px[AllFX17_DK_sub$zone == i]),
       mean(AllFX17_DK_sub$zone_pz[AllFX17_DK_sub$zone == i]),
       zone_count_diff[i], cex = 1.5)
}

#exploring batted ball outcomes
#mpact of spin rate, spin dir on the likelihood that a pitch is missed by the batter
#look at distinct outcomes in des of gameday.pitches table, all 20 outcomes listed below

#In play, no out, In play, out(s), Called Strike, Ball, Foul, Swinging Strike, In play, run(s), Ball In Dirt, 
#Foul Tip, Swinging Strike (Blocked), Hit By Pitch, Foul (Runner Going), 
#Intent Ball, Foul Bunt, Missed Bunt, Pitchout, Swinging Pitchout, Automatic Ball, Automatic Strike, Foul Pitchout

# Create batter_swing
no_swing <- c("Ball", "Called Strike", "Ball In Dirt", "Hit By Pitch", "Intent Ball", "Pitchout", "Automatic Ball")
AllFX17_DK_SL$batter_swing <- ifelse(AllFX17_DK_SL$pitch_result %in% no_swing, 0, 1)
# Create swing_SL
swing_sl <- subset(AllFX17_DK_SL, AllFX17_DK_SL$batter_swing == 1)
# Create the contact variable
no_contact <- c("Swinging Strike", "Missed Bunt", "Swinging Strike (Blocked)", "Swinging Pitchout")
swing_sl$contact <- ifelse(swing_sl$pitch_result %in% no_contact, 0, 1)
# Create spinr_bin: add one line for "Spin Rate high", put the edge at 1900, 2300 but might change based on Kershaw mean, quarts
swing_sl$spinr_bin <- ifelse(swing_sl$spin_rate < 1900, "Low", NA)
swing_sl$spinr_bin <- ifelse(swing_sl$spin_rate >= 1900 & swing_sl$spin_rate < 2400, 
                             "Medium", swing_sl$spinr_bin)
swing_sl$spinr_bin <- ifelse(swing_sl$spin_rate >= 2400, 
                             "High", swing_sl$spinr_bin)
# Aggregate contact rate by spin rate bin
tapply(swing_sl$contact, swing_sl$spinr_bin, mean)

#relationship between pitch_type and contact rate
#average spin_rate varies by pitch_type, reconfigure your spinr_bin variable, structure this into 3 groups for each pitch
#with each group the within-pitch start_speed
# Create the swings dataset, which includes only pitches at which a batter has swung; can use variables from above
no_swing <- c("Ball", "Called Strike", "Ball In Dirt", "Hit By Pitch", "Intent Ball", "Pitchout", "Automatic Ball")
AllFX17_DK$batter_swing <- ifelse(AllFX17_DK$pitch_result %in% no_swing, 0, 1)
swings <- subset(AllFX17_DK, AllFX17_DK$batter_swing == 1)

# Create a contact variable
no_contact <- c("Swinging Strike", "Missed Bunt", "Swinging Strike (Blocked)", "Swinging Pitchout")
swings$contact <- ifelse(swings$pitch_result %in% no_contact, 0, 1)

# Create a new function called bin_pitch_spinr() for use in calculating spinr_bin.
bin_pitch_spinr <- function(x) {
  cut(x, breaks = quantile(x, probs = c(0,1/3,2/3,1)), labels = FALSE)
}
###########################################################

# Create the subsets for each pitch type
swing_ff <- subset(swings, pitch_type == "FF")
swing_ch <- subset(swings, pitch_type == "CH")
swing_cu <- subset(swings, pitch_type == "CU")
swing_ft <- subset(swings, pitch_type == "FT")
swing_sl <- subset(swings, pitch_type == "SL")

# Make spinr_bin_pitch variable for each subset
swing_ff$spinr_bin <- bin_pitch_rate(swing_ff$spin_rate)
swing_ch$spinr_bin  <- bin_pitch_rate(swing_ch$spin_rate)
swing_cu$spinr_bin  <- bin_pitch_rate(swing_cu$spin_rate)
swing_ft$spinr_bin  <- bin_pitch_rate(swing_ft$spin_rate)
swing_sl$spinr_bin  <- bin_pitch_rate(swing_sl$spin_rate)

# Print quantile levels for each pitch
thirds <- c(0, 1/3, 2/3, 1)
quantile(swing_sl$spin_rate, probs = thirds)

quantile(swing_ff$spin_rate, probs = thirds)

quantile(swing_ch$spin_rate, probs = thirds)

quantile(swing_cu$spin_rate, probs = thirds)

quantile(swing_ft$spin_rate, probs = thirds)

head(swing_sl)

# Calculate contact rate by spin_rate for swing_sl
tapply(swing_sl$contact, swing_sl$spinr_bin, mean)

# Calculate contact rate by spin_rate for swing_ff
tapply(swing_ff$contact, swing_ff$spinr_bin, mean)

# Calculate contact rate by spin_rate for swing_ch
tapply(swing_ch  $contact, swing_ch$spinr_bin, mean)

# Calculate contact rate by spin_rate for swing_cu
tapply(swing_cu$contact, swing_cu$spinr_bin, mean)

# Calculate contact rate by spin_rate for swing_ft
tapply(swing_ft$contact, swing_ft$spinr_bin, mean)

head(AllFX17_DK,30)