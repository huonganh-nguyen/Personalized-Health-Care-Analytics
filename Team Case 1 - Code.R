### Question 1

# Stacked Bar Plot with Colors and Legend
counts <- table(DATA$smoke, DATA$ed.col)

#Proportional Stacked Bar Plot 
prop = prop.table(counts, margin = 2)
barplot(prop, main="Smoking and College Education",
        names.arg=c("No or Some College", "Completed College"), col=c("darkblue","red"),
        legend = c("Nonsmoking", "Smoking"))

###Question 2

#First create vector of where pvals are significant based on the traditional method, 
#conservative method, and fdr method
sig05 <- pvals < .05
sigcon <- pvals < (.05/45)
sigfdr <- pvals < fdr_cut(pvals, .001)

#Next find the differences between the traditional method and the other methods below

#The addition below will place a 1 where the two vectors are different
sig05andsigcon <- sig05 + sigcon
sig05andsigfdr <- sig05 + sigfdr

#By examining the vectors for where the 1 is, we can find the differences 
sig05andsigcon
sig05andsigfdr

#In the fourth, fifth, and thirty-third indices, there is a difference in significance
ListLabels[4]
sig05[4]
sigcon[4]
sigfdr[4]
ListLabels[5]
sig05[5]
sigcon[5]
sigfdr[5]
ListLabels[33]
sig05[33]
sigcon[33]
sigfdr[33]

### Question 3

# Find the correlation between each variable and weight.
cor(DATA)

# Check if correction between each variable and weight is significant.
cor.test(DATA$black, DATA$weight)
cor.test(DATA$married, DATA$weight)
cor.test(DATA$boy, DATA$weight)
cor.test(DATA$tri1, DATA$weight)
cor.test(DATA$tri2, DATA$weight)
cor.test(DATA$tri3, DATA$weight)
cor.test(DATA$ed.hs, DATA$weight)
cor.test(DATA$ed.smcol, DATA$weight)
cor.test(DATA$ed.col, DATA$weight)
cor.test(DATA$mom.age, DATA$weight)
cor.test(DATA$smoke, DATA$weight)
cor.test(DATA$cigsper, DATA$weight)
cor.test(DATA$m.wtgain, DATA$weight)
cor.test(DATA$mom.age2, DATA$weight)

### Question 4

# Create a multiple regression with 14 variables against weight
reg<-glm(weight ~ black + married + boy +tri1+tri2+tri3+ed.hs+ed.smcol+ed.col+mom.age+smoke+cigsper+m.wtgain+mom.age2, data=DATA)
summary(reg)

# Apply the 0.05 cut-off
pvals<-summary(reg)$coef[,4]
sigvar<-which( pvals <.05)
sigvar

# Apply the conservative cut-off (alpha/n)
cons_cutoff<-0.05/14
cons_cutoff
conservative_sigvar<-which( pvals <cons_cutoff)
conservative_sigvar

# Apply the FDR cut-off of 0.001
fdr_cutoff<-fdr_cut(pvals,0.001)

# Find the significant variables
fdr_significant_var<-which( pvals<fdr_cutoff)
fdr_significant_var
