# Import data
data = read.csv("C:/Users/User/OneDrive/Desktop/UC Merced/Senior Year/Fall 2026/Math 180/project/Latest data as of 11.20.csv", stringsAsFactors = TRUE, na.strings = c("", "NA", "#N/A"))

# Remove players who were traded mid season
data <- data[data$Team != "Two Teams", ]
data <- data[data$Team != "Three Teams", ]

# Remove undrafted players
data <- data[!is.na(data$Draft.Year) & !is.na(data$Draft..), ]

# Extract response variable
y <- data$All.Star.this.season.

# Create ID to identify row
id <- data[, c(
  "Player",
  "Season"
)]

# Process data into desired predictors
df <- data[, c(
  "Conference",
  "Team",
  "Average.Points.per.game",
  "True.Rebound..",
  "Assist..",
  "Games.Started",
  "USA.INT",
  "Age",
  "Height..CM.",
  "Draft.Year",
  "Draft..",
  "Has.been.All.Star.before.",
  "Position",
  "Avg.min.played"
)]

# Combine ID, response, and predictors
clean_data <- cbind(id, AllStar = y, df)

# Export cleaned dataset
write.csv(clean_data, "NBA_Processed_Data.csv", row.names = FALSE)
