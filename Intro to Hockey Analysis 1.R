# Simple data frame creation in R with data for the 2017-18 Canucks

# Top 5 Point scorers on the 17-18 Canucks
# Top 5 point scorers goal totals
goals <- c(29, 23, 3, 22, 17)

# Top 5 point scorers assist totals
assists <- c(26, 32, 47, 22, 24)

# The names of the top 5 point scorers
names <- c("Brock Boeser", "Daniel Sedin", "Henrik Sedin", "Bo Horvat", "Thomas Vanek")

# Points for the top 5 point scorers
points <- goals + assists

# Make sure we got the correct values when adding goals and assists
tPoints <- c(55, 55, 50, 44, 41)
points == tPoints

# Create a Data Frame for players and their goals, assists, and points
player_stats <- data.frame(names, goals, assists, points)

# Viewing the data
player_stats
View(player_stats)

# Let's do the same, but with the 3 goalies who appeared for the team

# Games played for each goalie
played <- c(60, 27, 1)

# Win totals for the goalies
wins <- c(23, 7, 1)

# Loss totals
loss <- c(26, 14, 0)

# Overtime loss totals
overtime <- c(7, 4, 0)

# The names for the goalies
goalies <- c("Jacob Markstrom", "Anders Nilsson", "Thatcher Demko")

# Creating and viewing the goalie data frame
goalie_stats <- data.frame(goalies, played, wins, loss, overtime)
goalie_stats
View(goalie_stats)