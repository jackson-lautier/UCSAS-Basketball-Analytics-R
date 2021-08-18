#########################################################
#########################################################
# R Code to accompany the 'basketball analytics w R'
# workshop, 2021 UCSAS workshop
# Jackson P. Lautier
#########################################################
#########################################################
#########################################################

#install the necessary package & load to library
install.packages('BasketballAnalyzeR')
library('BasketballAnalyzeR')

#If you want to reproduce the figures contained in the book of
#Zuccolotto and Manisera (2020) and
#if the version of your R machine is >= 3.6.0, you need to type
#RNGkind(sample.kind = "Rounding")
#at the beginning of your working session
RNGkind(sample.kind = "Rounding")

#see available data
data(package="BasketballAnalyzeR")

#nbastatR still in development; must be installed from git site
#not yet available on cran
#see: https://rdrr.io/github/abresler/nbastatR/f/README.md
install.packages("remotes")
remotes::install_github("abresler/nbastatR")
library('nbastatR')

#The NBA's Game ID, 0021400001, is a 10-digit code: XXXYYGGGGG, 
#where XXX refers to a season prefix, 
#YY is the season year (e.g. 14 for 2014-15), 
#and GGGGG refers to the game number 
#(1-1230 for a full 30-team regular season).

#001 : Pre Season
#002 : Regular Season
#003 : All-Star
#004 : Post Season

#########################################################
#########################################################
#########################################################
#get team data
#########################################################
#########################################################
#########################################################

#possible lengthy run-time
#note nbastatR uses a slightly different gameid code
#G1: 22000001
#G1080: 22001080
#20-21 had 72 games; (72 * 30) / 2 = 1080
first_game = 22000001
last_game = 22001080
#get first game data
box_scores(game_ids = first_game, 
           box_score_types = c("Traditional"), 
           result_types = c("team"), 
           join_data = TRUE, 
           assign_to_environment = TRUE, 
           return_message = TRUE)
master_df = dataBoxScoreTeamNBA

for (i in c((first_game + 1):last_game)) {
  box_scores(game_ids = i, 
             box_score_types = c("Traditional"), 
             result_types = c("team"), 
             join_data = TRUE, 
             assign_to_environment = TRUE, 
             return_message = TRUE)
  cur_dat = dataBoxScoreTeamNBA
  master_df = rbind(master_df, cur_dat)
  write.csv(master_df, "team_dat.csv")
  print(paste(i-22000000, " of 1080 complete.",sep=""))
}

#########################################################
#########################################################
#########################################################
#get player box score data
#########################################################
#########################################################
#########################################################

#possible lengthy run-time
#note nbastatR uses a slightly different gameid code
#G1: 22000001
#G1080: 22001080
#20-21 had 72 games; (72 * 30) / 2 = 1080
last_game = 22001080
#get first game data
box_scores(game_ids = 22000001, 
           box_score_types = c("Traditional"), 
           result_types = c("player"), 
           join_data = TRUE, 
           assign_to_environment = TRUE, 
           return_message = TRUE)
master_df = dataBoxScorePlayerNBA

for (i in c(22000002:last_game)) {
  box_scores(game_ids = i, 
             box_score_types = c("Traditional"), 
             result_types = c("player"), 
             join_data = TRUE, 
             assign_to_environment = TRUE, 
             return_message = TRUE)
  cur_dat = dataBoxScorePlayerNBA
  master_df = rbind(master_df, cur_dat)
  write.csv(master_df, "player_dat.csv")
  print(paste(i-22000000, " of 1080 complete.",sep=""))
}

#########################################################
#########################################################
#########################################################
#organize box score data
#########################################################
#########################################################
#########################################################

#import data
path = "./2021/data/team_dat.csv"
team_box <- read.csv(path)
team_box$w <- ifelse(team_box$plusminus > 0, 1, 0)
team_box$l <- ifelse(team_box$plusminus < 0, 1, 0)
team_box$team_name_full = paste(team_box$cityTeam, team_box$teamName)
team_box$gp <- rep(1,nrow(team_box))

df <- data.frame(team_box$ast,
                 team_box$blk,
                 team_box$dreb,
                 team_box$fta,
                 team_box$ftm,
                 team_box$pctFT,
                 team_box$gp,
                 team_box$l,
                 team_box$minExact,
                 team_box$oreb,
                 team_box$fg2a,
                 team_box$fg2m,
                 team_box$fg3a,
                 team_box$fg3m,
                 team_box$pf,
                 team_box$plusminus,
                 team_box$pts,
                 team_box$stl,
                 team_box$team_name_full,
                 team_box$tov,
                 team_box$w)

df = aggregate(. ~ team_box.team_name_full,data=df, FUN=sum)
cnames = c("Team", sort(names(Tbox))[1:12], 
           sort(names(Tbox))[14:15],
           sort(names(Tbox))[17:20],
           sort(names(Tbox))[22:23])
names(df) = cnames
df$P2p = (df$P2M / df$P2A)
df$P3p = (df$P3M / df$P3A)

#get opponent box scores

df <- data.frame(team_box$ast,
                 team_box$blk,
                 team_box$dreb,
                 team_box$fta,
                 team_box$ftm,
                 team_box$pctFT,
                 team_box$gp,
                 team_box$l,
                 team_box$minExact,
                 team_box$oreb,
                 team_box$fg2a,
                 team_box$fg2m,
                 team_box$fg3a,
                 team_box$fg3m,
                 team_box$pf,
                 team_box$plusminus,
                 team_box$pts,
                 team_box$stl,
                 team_box$team_name_full,
                 team_box$tov,
                 team_box$w)

opp_dat <- data.frame(Team = vector(),
                      AST = vector(), BLK = vector(),
                      DREB = vector(), FTA = vector(),
                      FTM = vector(), FTp = vector(),
                      GP = vector(), L = vector(), MIN = vector(),
                      OREB = vector(), P2A = vector(),
                      P2M = vector(), P3A = vector(), P3M = vector(),
                      PF = vector(), PM = vector(),
                      PTS = vector(), STL = vector(), TOV = vector(),
                      W = vector())

teams = sort(unique(team_box$team_name_full))

for (i in c(1:30)) {
  cur_team_games <- sort(unique(team_box[team_box$team_name_full == teams[i],]$idGame))
  cur_row_ind = (team_box$idGame %in% cur_team_games) & (team_box$team_name_full != teams[i])
  
  cur_ast = sum(cur_row_ind * team_box$ast)
  cur_blk = sum(cur_row_ind * team_box$blk)
  cur_dreb = sum(cur_row_ind * team_box$dreb)
  cur_fta = sum(cur_row_ind * team_box$fta)
  cur_ftm = sum(cur_row_ind * team_box$ftm)
  cur_pctFT = sum(cur_row_ind * team_box$pctFT)
  cur_gp = sum(cur_row_ind * team_box$gp)
  cur_l = sum(cur_row_ind * team_box$l)
  cur_minExact = sum(cur_row_ind * team_box$minExact)
  cur_oreb = sum(cur_row_ind * team_box$oreb)
  cur_fg2a = sum(cur_row_ind * team_box$fg2a)
  cur_fg2m = sum(cur_row_ind * team_box$fg2m)
  cur_fg3a = sum(cur_row_ind * team_box$fg3a)
  cur_fg3m = sum(cur_row_ind * team_box$fg3m)
  cur_pf = sum(cur_row_ind * team_box$pf)
  cur_plusminus = sum(cur_row_ind * team_box$plusminus)
  cur_pts = sum(cur_row_ind * team_box$pts)
  cur_stl = sum(cur_row_ind * team_box$stl)
  cur_tov = sum(cur_row_ind * team_box$tov)
  cur_w = sum(cur_row_ind * team_box$w)
  
  cur_row <- data.frame(Team = teams[i],
                        AST = cur_ast, BLK = cur_blk,
                        DREB = cur_dreb, FTA = cur_fta,
                        FTM = cur_ftm, FTp = cur_pctFT,
                        GP = cur_gp, L = cur_l, MIN = cur_minExact,
                        OREB = cur_oreb, P2A = cur_fg2a,
                        P2M = cur_fg2m, P3A = cur_fg3a, P3M = cur_fg3m,
                        PF = cur_pf, PM = cur_plusminus,
                        PTS = cur_pts, STL = cur_stl, TOV = cur_tov,
                        W = cur_w)
  
  opp_dat = rbind(opp_dat, cur_row)
}

opp_dat$P2p = (opp_dat$P2M / opp_dat$P2A)
opp_dat$P3p = (opp_dat$P3M / opp_dat$P3A)

#########################################################
#########################################################
#########################################################
#update displays with new data
#########################################################
#########################################################
#########################################################
Tbox2021 = read.csv("./2021/data/Tbox2021.csv") 
Obox2021 = read.csv("./2021/data/Obox2021.csv")
FF <- fourfactors(Tbox2021, Obox2021)
listPlots <- plot(FF) #four plots available
listPlots[2] #view plot of interest
listPlots[1] #pace of play also interesting

#########################################################
#########################################################
#########################################################
#cluster analysis
#########################################################
#########################################################
#########################################################

FF <- fourfactors(Tbox2021, Obox2021)
OD.Rtg <- FF$ORtg/FF$DRtg
F1.r <- FF$F1.Off/FF$F1.Def
F2.r <- FF$F2.Def/FF$F2.Off
F3.Off <- FF$F3.Off
F3.Def <- FF$F3.Def
P3M <- Tbox2021$P3M
STL.r <- Tbox2021$STL/Obox2021$STL
data <- data.frame(OD.Rtg, F1.r, F2.r, F3.Off, F3.Def,
                   P3M, STL.r)

set.seed(29)
kclu2 <- kclustering(data, labels=Tbox2021$Team, k=5)

cluster <- as.factor(kclu2$Subjects$Cluster)
Xbubble <- data.frame(Team=Tbox2021$Team, PTS=Tbox2021$PTS,
                      PTS.Opp=Obox2021$PTS, cluster,
                      W=Tbox2021$W)
labs <- c("PTS", "PTS.Opp", "cluster", "Wins")
bubbleplot(Xbubble, id="Team", x="PTS", y="PTS.Opp",
           col="cluster", size="W", labels=labs)
kclu2$ClusterList

#########################################################
#########################################################
#########################################################
# 'best shot' in basketball
#########################################################
#########################################################
#########################################################

#must buy play-by-play data!
playBplay <- read.csv("add play-by-play data file here!")

pBp <- playBplay[playBplay$data_set == "2020-21 Regular Season",]
mil_games <- sort(unique(pBp[pBp$team == "MIL",]$game_id))
pBp_MIL <- pBp[pBp$game_id %in% mil_games,]
phx_games <- sort(unique(pBp[pBp$team == "PHX",]$game_id))
pBp_PHX <- pBp[pBp$game_id %in% phx_games,]

PbP_finals = rbind(pBp_MIL, pBp_PHX)
write.csv(PbP_finals, "nba_finals_pbp.csv")
PbP.sd <- subset(PbP_finals, shot_distance < 35)
pl <- c("Chris Paul", "Devin Booker", "Deandre Ayton",
        "Giannis Antetokounmpo", "Jrue Holiday", "Khris Middleton")

mypal <- colorRampPalette(c("red", "green"))
expectedpts(data=PbP.sd, players=pl,
            col.team="gray", palette=mypal,
            col.hline="gray")

#########################################################
#########################################################
#########################################################
# player similarities
#########################################################
#########################################################
#########################################################

d1 <- read.csv('./2021/data/player_dat_1.csv')
d2 <- read.csv('./2021/data/player_dat_2.csv')
df <- rbind(d1,d2)
df <- data.frame(df$pts, df$fg3m, df$fg2m, df$treb,
                 df$ast, df$tov, df$stl, df$blk,
                 df$minExact, df$namePlayer)
df = aggregate(. ~ df.namePlayer,data=df, FUN=sum)
cnames = c("NAME", "PTS", "P3M", "P2M", "REB", "AST", "TOV", "STL", "BLK","MIN")
names(df) = cnames

df = df[df$MIN >= 1500,]
id <- df$NAME[df$MIN >= 1500]
df <- df[,2:10]

#buddy hield vs. duncan robinson
#clint capela vs. rudy gobert
#julius randle contract
mds <- MDSmap(df)
plot(mds, labels=id, #subset=which(id=="Rudy Gobert" | id=="Clint Capela" |
                                    #id == "Buddy Hield" | id == "Duncan Robinson"),
     col.subset = "tomato")

#########################################################
#########################################################
#########################################################
# shot chart
#########################################################
#########################################################
#########################################################

#must buy play-by-play data!
playBplay <- read.csv("add play-by-play data file here!")

pBp <- playBplay[playBplay$data_set == "2020-21 Regular Season",]

phi_games <- sort(unique(pBp[pBp$team == "PHI",]$game_id))
pBp_PHI <- pBp[pBp$game_id %in% phi_games,]

PbP <- PbPmanipulation(pBp_PHI) #prepare dataset

#prepare data of interest
subdata <- subset(PbP, (player=="Ben Simmons") & (result != ""))
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75
subdata$result = as.factor(subdata$result)


shotchart(data=subdata, x="xx", y="yy", z = "result",
          scatter=TRUE, pt.col = 'red', bg.col = 'white',
          result='result')


phx_games <- sort(unique(pBp[pBp$team == "PHX",]$game_id))
pBp_PHX <- pBp[pBp$game_id %in% phx_games,]

PbP <- PbPmanipulation(pBp_PHX) #prepare dataset

#prepare data of interest
subdata <- subset(PbP, (player=="Devin Booker") & (result != ""))
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75
subdata$result = as.factor(subdata$result)


shotchart(data=subdata, x="xx", y="yy", z = "result",
          scatter=TRUE, pt.col = 'red', bg.col = 'white',
          result='result')


bkn_games <- sort(unique(pBp[pBp$team == "BKN",]$game_id))
pBp_BKN <- pBp[pBp$game_id %in% bkn_games,]

PbP <- PbPmanipulation(pBp_BKN) #prepare dataset

#prepare data of interest
subdata <- subset(PbP, (player=="James Harden") & (result != ""))
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75
subdata$result = as.factor(subdata$result)


shotchart(data=subdata, x="xx", y="yy", z = "result",
          scatter=TRUE, pt.col = 'red', bg.col = 'white',
          result='result')


#########################################################
#########################################################
#########################################################
# assist network
#########################################################
#########################################################
#########################################################

#must buy play-by-play data!
PbP.MIA <- subset(pBp, team=="MIA")
write.csv(PbP.MIA, "pbp_mia.csv")
netdata <- assistnet(PbP.MIA)
set.seed(7)
plot(netdata, layout="circle", edge.thr=20)

PbP.DAL <- subset(pBp, team=="DAL")
write.csv(PbP.DAL, "pbp_dal.csv")
netdata <- assistnet(PbP.DAL)
set.seed(7)
plot(netdata, layout="circle", edge.thr=20)


