#########################################################
#########################################################
# R Code to accompany the 'basketball analytics w R'
# workshop, 2020 UCSAS workshop
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

#offensive and defensive rating of the set of teams
FF <- fourfactors(Tbox, Obox)
listPlots <- plot(FF) #four plots available
listPlots[2] #view plot of interest

#########################################################
#########################################################
#########################################################
#shot charts
#########################################################
#########################################################
#########################################################

PbP <- PbPmanipulation(PbP.BDB) #prepare dataset

#prepare data of interest
subdata <- subset(PbP, player=="Kevin Durant")
subdata$xx <- subdata$original_x/10
subdata$yy <- subdata$original_y/10-41.75

shotchart(data=subdata, x="xx", y="yy", z = 'result',
          scatter=TRUE, pt.col = 'red', bg.col = 'white',
          result='result')

shotchart(data=subdata, x="xx", y="yy", z="playlength",
          num.sect=5, type="sectors", scatter=FALSE,
          result = "result")

#########################################################
#########################################################
#########################################################
#player similarities
#########################################################
#########################################################
#########################################################

attach(Pbox)
#original set of variables to consider
data <- data.frame(PTS, P3M, P2M, REB=OREB+DREB,
                     AST, TOV, STL, BLK)

detach(Pbox)
data <- subset(data, Pbox$MIN>=1500) #1500 minutes or more
id <- Pbox$Player[Pbox$MIN>=1500]

mds <- MDSmap(data)
plot(mds, labels=id, subset=which(id=="Kyle Lowry" | id=="Stephen Curry" ),
     col.subset = "tomato")

#########################################################
#########################################################
#########################################################
#assist network
#########################################################
#########################################################
#########################################################

# Warriors' offense youtube link
# https://www.youtube.com/watch?v=1xu8W10vymo

PbP.GSW <- subset(PbP, team=="GSW")
netdata <- assistnet(PbP.GSW)
set.seed(7)
plot(netdata, layout="circle", edge.thr=20)

#########################################################
#########################################################
#########################################################
#cluster analysis
#########################################################
#########################################################
#########################################################

FF <- fourfactors(Tbox, Obox)
OD.Rtg <- FF$ORtg/FF$DRtg
F1.r <- FF$F1.Off/FF$F1.Def
F2.r <- FF$F2.Def/FF$F2.Off
F3.Off <- FF$F3.Off
F3.Def <- FF$F3.Def
P3M <- Tbox$P3M
STL.r <- Tbox$STL/Obox$STL
data <- data.frame(OD.Rtg, F1.r, F2.r, F3.Off, F3.Def,
                   P3M, STL.r)

set.seed(29)
kclu2 <- kclustering(data, labels=Tbox$Team, k=5)

cluster <- as.factor(kclu2$Subjects$Cluster)
Xbubble <- data.frame(Team=Tbox$Team, PTS=Tbox$PTS,
                      PTS.Opp=Obox$PTS, cluster,
                      W=Tbox$W)
labs <- c("PTS", "PTS.Opp", "cluster", "Wins")
bubbleplot(Xbubble, id="Team", x="PTS", y="PTS.Opp",
           col="cluster", size="W", labels=labs)

#########################################################
#########################################################
#########################################################
#mid-range shots
#########################################################
#########################################################
#########################################################

#jordan game winner
# https://www.youtube.com/watch?v=vdPQ3QxDZ1s

PbP.GSW.sd <- subset(PbP.GSW, shot_distance < 40)
Pbox.GSW <- subset(Pbox, PTS>=500 &
                     Team=="Golden State Warriors")
pl <- c("Klay Thompson", "Kevin Durant", "Draymond Green", "Stephen Curry")

mypal <- colorRampPalette(c("red","green"))
expectedpts(data=PbP.GSW.sd, players=pl,
            col.team="gray", palette=mypal,
            col.hline="gray")
