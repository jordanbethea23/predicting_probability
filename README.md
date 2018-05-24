# predicting_probability

Teams <- c("Toronto Raptors", "Boston Celtics", "Cleveland Cavaliers", "Washington Wizards","Indiana Pacers", "Milwaukee Bucks", "Philadelphia 76ers", "Miami Heat", "Detroit Pistons","Charlotte Hornets", "New York Knicks", "Chicago Bulls", "Brooklyn Nets", "Orlando Magic","Atlanta Hawks", "Houston Rockets", "Golden State Warriors", "San Antonio Spurs", "Minnesota Timberwolves","Oklahoma City Thunder", "Denver Nuggets", "Portland Trail Blazers", "New Orleans Pelicans", "Los Angeles Clippers","Utah Jazz", "Los Angeles Lakers", "Memphis Grizzlies", "Sacramento Kings", "Dallas Mavericks", "Phoenix Suns")
NumTeams <- length(Teams)
PlayoffAppearance <- data.frame(matrix(0, NumTeams, 1))
colnames(PlayoffAppearance) <- "PlayoffAppearances"
rownames(PlayoffAppearance) <- Teams
PlayoffAppearance$TeamID <- 1:NumTeams

##### Load the Data #####
PlayoffRun <- function(){
  nba <- read.csv("NBA Data.csv")
  nba$X <- NULL
  nba$X.1<- NULL
  nba$Attend. <- NULL
  
  ##### Summary #####
  summary(nba)
  
  ##### Build a Games Played Table #####
  nba$GameID <- 1:nrow(nba)
  game <- 1
  
  idx <- which(nba$GameID == game)
  temp <- nba[idx,]
  saverow <- temp[1,]
  
  num.games <- max(nba$GameID)
  for (game in 2:num.games){
    idx <- which(nba$GameID == game)
    temp <- nba[idx,]
    saverow <- rbind(saverow, temp[1,])
  }
  games.played.table <- saverow
  
  ##### Assign Teams Individual Team IDs #####
  Teams <- c("Toronto Raptors", "Boston Celtics", "Cleveland Cavaliers", "Washington Wizards","Indiana Pacers", "Milwaukee Bucks", "Philadelphia 76ers", "Miami Heat", "Detroit Pistons","Charlotte Hornets", "New York Knicks", "Chicago Bulls", "Brooklyn Nets", "Orlando Magic","Atlanta Hawks", "Houston Rockets", "Golden State Warriors", "San Antonio Spurs", "Minnesota Timberwolves","Oklahoma City Thunder", "Denver Nuggets", "Portland Trail Blazers", "New Orleans Pelicans", "Los Angeles Clippers","Utah Jazz", "Los Angeles Lakers", "Memphis Grizzlies", "Sacramento Kings", "Dallas Mavericks", "Phoenix Suns")
  Home.Team <- factor(nba$Home.Neutral, levels = Teams)
  Visitor.Team <- factor(nba$Visitor.Neutral, levels = Teams)
  nba$HomeTeamID <- as.numeric(Home.Team)
  nba$VisitorTeamID <- as.numeric(Visitor.Team)
  games.played.table$HomeTeamID <- as.numeric(Home.Team)
  games.played.table$VisitorTeamID <- as.numeric(Visitor.Team)
  
  ##### Convert all Pts to 0 after March 12th (row 1004) #####
  p <- 1
  num.games <- max(nrow(nba))
  for(p in 1:num.games){
    if(games.played.table$GameID[p] > 1003){
      games.played.table$PTS[p] <- 0
      games.played.table$PTS.1[p] <- 0
    } 
  }
  
  ##### New dataframe Record for Wins and Losses #####
  NumTeams <- length(Teams)
  
  record <- data.frame(matrix(0, NumTeams, 2))
  colnames(record) <- c("W", "L")
  rownames(record) <- Teams
  record
  
  ##### Fill in Home Result Column#####
  games.played.table$HomeResult <- 0
  game <- 1
  num.games <- max(games.played.table$GameID)
  for (game in 1:num.games){
    if(games.played.table$PTS.1[game] > games.played.table$PTS[game]){
      games.played.table$HomeResult[game] <- "W"
    } else if (games.played.table$PTS.1[game] < games.played.table$PTS[game]){
      games.played.table$HomeResult[game] <- "L" 
    } else {
      NULL
    }
  }
  
  #### Fill in Record table #####
  g <- 1
  for (g in 1:num.games){
    if (games.played.table$HomeResult[g] == 'L'){ 
      record$L[games.played.table$HomeTeamID[g]] = record$L[games.played.table$HomeTeamID[g]] + 1
      record$W[games.played.table$VisitorTeamID[g]] = record$W[games.played.table$VisitorTeamID[g]] + 1
    } else if (games.played.table$HomeResult[g] == 'W'){
      record$W[games.played.table$HomeTeamID[g]] = record$W[games.played.table$HomeTeamID[g]] + 1
      record$L[games.played.table$VisitorTeamID[g]] = record$L[games.played.table$VisitorTeamID[g]] + 1
    } else{
      NULL
    }
  }
  record
  
  ##### Calculating Win Percentage #####
  record$WP <- (record$W/(record$W + record$L))*100
  record
  
  record$TeamID <- 1:max(nrow(record))
  
  ##### Rank based on WP #####
  record$rank <- rank(-record$WP, ties.method = "first")
  record
  
  ##### Subset the data #####
  remaining.games <- subset(games.played.table, games.played.table$GameID > 1003, select = c(1:10))
  
  ##### Add each team's WP to new DF #####
  
  names(remaining.games)[names(remaining.games) == 'HomeTeamID'] <- 'TeamID'
  
  WIP <- merge(remaining.games, record, by.x = "TeamID", by.y = "TeamID")
  WIP$W <- NULL
  WIP$L <- NULL
  WIP$rank <- NULL
  
  names(WIP)[names(WIP) == 'TeamID'] <- 'HomeTeamID'
  names(WIP)[names(WIP) == 'WP'] <- 'HomeTeamWP'
  names(WIP)[names(WIP) == 'VisitorTeamID'] <- 'TeamID'
  
  WIP.2 <- merge(WIP, record, by.x = "TeamID", by.y = "TeamID")
  WIP.2$W <- NULL
  WIP.2$L <- NULL
  WIP.2$rank <- NULL
  names(WIP.2)[names(WIP.2) == 'WP'] <- 'VisitorTeamWP'
  names(WIP.2)[names(WIP.2) == 'TeamID'] <- 'VisitorTeamID'
  
  remaining.games <- WIP.2[order(WIP.2$GameID),]
  remaining.games <- remaining.games[, c(9,3,4,5,1,6,7,2,8,10,11,12)]
  
  ##### Calculating Home and Visitor Probability of winning each game #####
  
  remaining.games$HomeTeamProb <- (remaining.games$HomeTeamWP)/(remaining.games$HomeTeamWP + remaining.games$VisitorTeamWP)
  remaining.games$VisitorTeamProb <- 1 - remaining.games$HomeTeamProb
  
  ##### Create Simulated record for last 227 games #####
  simulated.record <- data.frame(matrix(0, NumTeams, 2)) 
  colnames(simulated.record) <- c("W", "L")
  rownames(simulated.record) <- Teams
  simulated.record$TeamID <-record$TeamID
  simulated.record
  
  ##### Simulate Games, Outcomes, season standings #####
  r <- min(remaining.games$GameID)
  num.games.2 <- max(nrow(remaining.games))
  
  for (r in 1:num.games.2){
    remaining.games$RanNum[r] <- runif(1,0,1)
    if(remaining.games$HomeTeamProb[r] > remaining.games$RanNum[r]){
      remaining.games$HomeResult[r] <- "W"
    } else { 
      remaining.games$HomeResult[r] <- "L" 
    }
    if (remaining.games$HomeResult[r] == 'L'){ 
      simulated.record$L[remaining.games$HomeTeamID[r]] = simulated.record$L[remaining.games$HomeTeamID[r]] + 1
      simulated.record$W[remaining.games$VisitorTeamID[r]] = simulated.record$W[remaining.games$VisitorTeamID[r]] + 1
    } else if (remaining.games$HomeResult[r] == 'W'){
      simulated.record$W[remaining.games$HomeTeamID[r]] = simulated.record$W[remaining.games$HomeTeamID[r]] + 1
      simulated.record$L[remaining.games$VisitorTeamID[r]] = simulated.record$L[remaining.games$VisitorTeamID[r]] + 1
    } else{
      NULL
    }
  }
  
  ##### Look at simulated record #####
  simulated.record
  
  
  ##### Combine actual record and simulated record together #####
  final.record <- merge(record, simulated.record, by.x = "TeamID", by.y = "TeamID")
  rownames(final.record) <- Teams
  final.record$WP <- NULL
  
  final.record$TotalWins <- final.record$W.x + final.record$W.y
  final.record$TotalLosses <- final.record$L.x + final.record$L.y
  final.record <- final.record[, -c(2:6)]
  final.record$WP <- (final.record$TotalWins)/(final.record$TotalLosses+final.record$TotalWins)
  
  
  ##### Subset Western and Eastern Conference ######
  EasternConference <- subset(final.record, final.record$TeamID < 16, select = c(1:4))
  EasternConference$rank <- rank(-EasternConference$WP, ties.method = "first")
  
  WesternConference <- subset(final.record, final.record$TeamID > 15, select = c(1:4))
  WesternConference$rank <- rank(-WesternConference$WP, ties.method = "first")
  
  ##### Rank each conference and subset top 8 teams and recombine for entire playoffs #####
  EasternConferencePlayoffs <- subset(EasternConference, EasternConference$rank < 9, select = c(1:5))
  
  WesternConferencePlayoffs <- subset(WesternConference, WesternConference$rank < 9, select = c(1:5))
  
  Playoff <- rbind(EasternConferencePlayoffs, WesternConferencePlayoffs)
}

##### Manually clicked source #####

Teams <- c("Toronto Raptors", "Boston Celtics", "Cleveland Cavaliers", "Washington Wizards","Indiana Pacers", "Milwaukee Bucks", "Philadelphia 76ers", "Miami Heat", "Detroit Pistons","Charlotte Hornets", "New York Knicks", "Chicago Bulls", "Brooklyn Nets", "Orlando Magic","Atlanta Hawks", "Houston Rockets", "Golden State Warriors", "San Antonio Spurs", "Minnesota Timberwolves","Oklahoma City Thunder", "Denver Nuggets", "Portland Trail Blazers", "New Orleans Pelicans", "Los Angeles Clippers","Utah Jazz", "Los Angeles Lakers", "Memphis Grizzlies", "Sacramento Kings", "Dallas Mavericks", "Phoenix Suns")
NumTeams <- length(Teams)
PlayoffAppearance <- data.frame(matrix(0, NumTeams, 1))
colnames(PlayoffAppearance) <- "PlayoffAppearances"
rownames(PlayoffAppearance) <- Teams
PlayoffAppearance$TeamID <- 1:NumTeams
num.teams <- nrow(PlayoffAppearance)
iterations <- 1000


for (i in 1:iterations){
  Playoff = PlayoffRun() 
   PlayoffAppearance[Playoff$TeamID,"PlayoffAppearances"] = PlayoffAppearance[Playoff$TeamID,"PlayoffAppearances"] + 1
}



PlayoffAppearance$PlayoffProbability <- (PlayoffAppearance$PlayoffAppearances/iterations)*100
PlayoffAppearance
 ### after 1000 iterations, the Utah Jazz have a 59.6% chance of making the playoffs
