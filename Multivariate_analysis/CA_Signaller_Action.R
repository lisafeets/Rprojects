##Signaller x Action, only modelling 10x10 rows and columns
##all other rows and columns set to supplimentary


allsgca=read.table("All SGCA.txt",h=T)
attach(allsgca)
str(allsgca)

#'data.frame':	443 obs. of  5 variables:
# $ Signaller: Factor w/ 17 levels "Brian","Claudine",..: 5 5 8 6 8 8 5 17 14 6 ...
# $ Recipient: Factor w/ 16 levels "Brian","Claudine",..: 6 6 7 5 5 7 16 1 5 11 ...
# $ Context  : Factor w/ 9 levels "Affiliation",..: 1 8 8 8 8 8 8 5 9 8 ...
# $ Gesture  : Factor w/ 38 levels "ArmRaise","ArmSwing",..: 1 1 1 1 1 1 1 1 2 2 ...
# $ Action   : Factor w/ 16 levels "ClimbOnMe","FollowAhead",..: 15 11 11 11 11 11 11 7 16 11 ...



dim(table(Signaller,Action))
#[1] 17 16

snames=sort(unique(Signaller))
anames=sort(unique(Action))

samatrix=matrix(table(Signaller,Action),17,16)
row.names(samatrix)=snames
colnames(samatrix)=anames

##include only signallers and actions with at least 10 observations each
signallertotals=cbind(snames, apply(samatrix,1,sum))
actiontotals=cbind(anames,apply(samatrix,2,sum))

getsup=function(x){
	suplist=vector()
	y=1
	for(i in x[,2]){
		if(i<10){
			suplist=c(suplist,y)
			y=y+1}
			else(y=y+1)}
	return(suplist)

}

sigsup=getsup(signallertotals)
actsup=getsup(actiontotals)



##run correspondence analysis setting signaller and actions with less than 10 observations as supplimentary points
##also included 4 other supp points because they were outliers
##by default CA plots dimensions 1 and 2 with rows and columns plotted in the same plot which is pretty busy
##supplimentary rows/columns are plotted with different color and in italics
##saved as "Rplot_CA.jpg"
library(FactoMineR)
ca2=CA(samatrix, row.sup = c(sigsup,16), col.sup = c(actsup, 6,7,13))

## ca package has a nice scree plot for visualizing dimensional contirbution to total inertia
library(ca)
ca3=ca(samatrix, suprow = c(sigsup,16), supcol = c(actsup, 6,7,13))
summary(ca3, scree=TRUE)

#Principal inertias (eigenvalues):
#
 #dim    value      %   cum%   scree plot               
 #1      0.281700  31.2  31.2  *************************
 #2      0.224628  24.8  56.0  ********************     
 #3      0.164367  18.2  74.2  **************           
 #4      0.114191  12.6  86.8  **********               
 #5      0.056008   6.2  93.0  ****                     
 #6      0.039918   4.4  97.4  ***                      
 #7      0.014316   1.6  99.0  *                        
 #8      0.008826   1.0 100.0                           
 #       -------- -----                                 
 #Total: 0.903953 100.0   
 

##create an assymetric graph where only the signallers are plotted and the actions are used to describe the axes
##this plot created in excel


ca2$col

#$coord
#                       Dim 1      Dim 2       Dim 3      Dim 4       Dim 5
#ClimbOnMe         0.32087179 -0.5684078 -0.13376225  0.1830649  0.02864457
#FollowAhead       1.59258899  1.1669097 -0.32976755 -0.4863576 -0.06240599
#GGRubStart       -0.01327535  0.3090410 -0.49300337  0.1692029  0.18328490
#GiveAffiliation   0.32794853  0.4624179  1.53549010  0.4406463 -0.06017753
#MoveAway          0.93733279  0.3625761  0.09196581 -0.6339474  0.30620044
#MoveIntoPosition -0.57285006  0.6752765 -0.19587538  0.3342005  0.06330792
#PlayStartConact  -0.42210611 -0.1118810  0.07853573 -0.3408752 -0.16033586
#PlayStartChase   -0.13671324 -0.5854922  0.24986433  0.1211402  0.51417503
#StraddleMe        0.86293858 -0.1173624 -0.29490760  0.5358554 -0.70015213

#$contrib
#                        Dim 1      Dim 2      Dim 3      Dim 4      Dim 5
#ClimbOnMe         7.821796177 30.7812271  2.3295986  6.2807165  0.3135203
#FollowAhead      24.523711023 16.5110899  1.8020429  5.6421656  0.1893952
#GGRubStart        0.005842299  3.9705135 13.8090013  2.3413341  5.6012237
#GiveAffiliation   1.931236447  4.8152171 72.5585580  8.6012166  0.3270623
#MoveAway         18.203697553  3.4158036  0.3003276 20.5415887  9.7705993
#MoveIntoPosition 16.317933216 28.4360305  3.2697417 13.7010247  1.0023904
#PlayStartConact  19.442455107  1.7129435  1.1534877 31.2790946 14.1093413
#PlayStartChase    0.438885097 10.0947203  2.5125152  0.8500841 31.2240474
#StraddleMe       11.314443080  0.2624544  2.2647271 10.7627753 37.4624203

##list of actions that contribute to dims 1 and 2 more than average, 100/9= 11.11
##polarity of actions given by dimensional coordinate
##dim1 = FollowAhead(+), MoveAway(+), MoveIntoPosition(-), PlayStartContact(-), StraddleMe(+)
##dim2 = ClimbOnMe(-), MoveIntoPosition(+), FollowAhead(+)


##get coordinates for signallers and supplimenatry signallers for plotting
ca2$row$coord
ca2$row.sup$coord


##how well do the first two dimensions describe each of the signallers 
##dimensions 1 + 2 for supplimentary signallers
rowSums(ca2$row.sup$cos2[,1:2])
#     Brian    Makanza      Murph      Ricky      Viaje       Zomi 
#0.68202473 0.04385938 0.12286266 0.52044202 0.33147878 0.58745868 

##dimensions 1 + 2 for siganllers contributing to model
rowSums(ca2$row$cos2[,1:2])
#  Claudine     Deidre     Elikia      Faith     Hannah         K2     Kitoko 
#0.83179561 0.57470621 0.71618832 0.81993968 0.81286578 0.05696615 0.34013579 
#     Laura       Lody      Tamia       Zuri 
#0.65818730 0.61325908 0.57648934 0.36324984 
