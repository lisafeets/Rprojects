##Inter-observer reliability between 2 coders of gesture data 
##Cohen's Kappa for directedness, attention and gesture type and function
##recoded categories numerically

gesture=read.table("gesture",h=T)
gesture
#                lisa lisanum               cat catnum
#1     HangUpsideDown       1    HangUpsideDown      1
#2           LimpHand       3          LimpHand      3
#3      SuspendedHand       4     SuspendedHand      4
#4           LimpHand       3          LimpHand      3 etc...


library(irr)

kappa2(cbind(gesture[2],gesture[4]))
# Cohen's Kappa for 2 Raters (Weights: unweighted)
#
# Subjects = 50 
#   Raters = 2 
#    Kappa = 0.875 
#
#        z = 30.9 
#  p-value = 0 
#  

directed=read.table("directed",h=T)
kappa2(cbind(directed[2],directed[4]))
# Cohen's Kappa for 2 Raters (Weights: unweighted)
#
# Subjects = 50 
#   Raters = 2 
#    Kappa = NaN 
#
#        z = NaN 
#  p-value = NaN 
#
#
#  

attention=read.table("attention",h=T)
kappa2(cbind(attention[2],attention[5]))
# Cohens Kappa for 2 Raters (Weights: unweighted)
#
# Subjects = 50 
#   Raters = 2 
#    Kappa = 0.886 
#
#        z = 9.02 
#  p-value = 0 
#




##kappa for function
func=read.table("function.txt",h=T)
kappa2(cbind(func[2],func[4]))
# Cohen's Kappa for 2 Raters (Weights: unweighted)
#
# Subjects = 50 
#   Raters = 2 
#    Kappa = 0.933 
#
#        z = 19.7 
#  p-value = 0 
 