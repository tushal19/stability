install.packages("metan")
library(metan)
library(ggplot2)
options(max.print = 10000)
getwd()
library(readxl)
BSK <- read_excel("BSK.xlsx")
View(BSK)
class(BSK)
str(BSK)
BSK$ENV <- factor(BSK$ENV, levels = unique(BSK$ENV))
BSK$GEN <- factor(BSK$GEN, levels = unique(BSK$GEN))
BSK$REP <- factor(BSK$REP, levels = unique(BSK$REP))
str(BSK)
inspect(BSK, plot = TRUE)
find_outliers(BSK, var = Height, plots = TRUE)
find_outliers(BSK, var = BD, plots = TRUE)
find_outliers(BSK, var = DBH, plots = TRUE)
find_outliers(BSK, var = VI, plots = TRUE)
find_outliers(BSK, var = CBH, plots = TRUE)
find_outliers(BSK, var = PB, plots = TRUE)
find_outliers(BSK, var = BA, plots = TRUE)
find_outliers(BSK, var = Nodes, plots = TRUE)
find_outliers(BSK, var = INL, plots = TRUE)
###DATA ANALYSIS########3
desc_stat(BSK)
desc_stat(BSK, stats = "all")
ds <- desc_stat(BSK, stats = "all")
ds
View(ds)
library(writexl)
write_xlsx(ds, "ds.xlsx")
mg <- means_by(BSK, GEN)
mg
write_xlsx(mg, "mg.xlsx")
View(mg)
me <- means_by(BSK, ENV)
me
write_xlsx(me, "me.xlsx")
View(me)
mge <- BSK %>% 
  group_by(ENV, GEN) %>%
  desc_stat(Height, BD, DBH, VI, CBH, PB, BA, Nodes, INL, stats = "mean")
mge
View(mge)
write_xlsx(mge, "mge.xlsx")
pht <- ge_plot(BSK, ENV, GEN, Height)
pht
PHT2 <-ge_plot(BSK, ENV, GEN, Height, type = 2)
PHT2
pbd <- ge_plot(BSK, ENV, GEN, BD)
pbd
pbd2 <-ge_plot(BSK, ENV, GEN, BD, type = 2)
pbd2
pdbh <- ge_plot(BSK, ENV, GEN, DBH)
pdbh
pdbh2 <-ge_plot(BSK, ENV, GEN, DBH, type = 2)
pdbh2
pvi <- ge_plot(BSK, ENV, GEN, VI)
pvi
pvi2 <-ge_plot(BSK, ENV, GEN, VI, type = 2)
pvi2
pcbh <- ge_plot(BSK, ENV, GEN, CBH)
pcbh
pcbh2 <-ge_plot(BSK, ENV, GEN, CBH, type = 2)
pcbh2
ppb <- ge_plot(BSK, ENV, GEN, PB)
ppb
ppb2 <-ge_plot(BSK, ENV, GEN, PB, type = 2)
ppb2
pba <- ge_plot(BSK, ENV, GEN, BA)
pba
pba2 <-ge_plot(BSK, ENV, GEN, BA, type = 2)
pba2
pnodes <- ge_plot(BSK, ENV, GEN, Nodes)
pnodes
pnodes2 <- ge_plot(BSK, ENV, GEN, Nodes, type = 2)
pnodes2
pinl <- ge_plot(BSK, ENV, GEN, INL)
pinl
pinl2 <- ge_plot(BSK, ENV, GEN, INL, type = 2)
pinl2
win <- ge_winners(BSK, ENV, GEN, resp = everything())
win
View(win)
write_xlsx(win, "win.xlsx")
ranks <- ge_winners(BSK, ENV, GEN, resp =everything(), type = "ranks")
View(ranks)
write_xlsx(ranks, "ranks.xlsx")
ge_details(BSK, ENV, GEN, resp = (everything()))
indav <- anova_ind(BSK, ENV, GEN, REP, resp = c(Height, BD, DBH, VI, CBH, PB, BA, Nodes, INL))
indav$Height$individual
iaht <- indav$Height$individual
write_xlsx(iaht, "ht anova.xlsx")
indav$BD$individual
iabd <- indav$BD$individual
write_xlsx(iabd, "bd anova.xlsx")
indav$DBH$individual
iadbh <- indav$DBH$individual
write_xlsx(iadbh, "dbh anova.xlsx")
indav$VI$individual
iavi <- indav$VI$individual
write_xlsx(iavi, "vi anova.xlsx")
indav$CBH$individual
iacbh <- indav$CBH$individual
write_xlsx(iacbh, "cbh anova.xlsx")
indav$PB$individual
iapb <- indav$PB$individual
write_xlsx(iapb, "pb anova.xlsx")
indav$BA$individual
iaba <- indav$BA$individual
write_xlsx(iaba, "ba anova.xlsx")
indav$Nodes$individual
ianodes <- indav$Nodes$individual
write_xlsx(ianodes, "nodes anova.xlsx")
indav$INL$individual
iainl <- indav$INL$individual
write_xlsx(iainl, "inl anova.xlsx")
?anova_ind
######### pooled anova ########
panv1 <- anova_joint(BSK, ENV, GEN, REP, Height)
pavt1 <-panv1$Height$anova
write_xlsx(pavt1, "pld ht anova.xlsx")
panv2 <- anova_joint(BSK, ENV, GEN, REP, BD)
pavt2 <- panv2$BD$anova
write_xlsx(pavt2, "pld bd anova.xlsx")
panv3 <- anova_joint(BSK, ENV, GEN, REP, DBH)
pavt3 <- panv3$DBH$anova
write_xlsx(pavt3, "pld dbh anova.xlsx")
panv4 <- anova_joint(BSK, ENV, GEN, REP, VI)
pavt4 <- panv4$VI$anova
write_xlsx(pavt4, "pld vi anova.xlsx")
panv5 <- anova_joint(BSK, ENV, GEN, REP, CBH)
pavt5 <- panv5$CBH$anova
write_xlsx(pavt5, "pld cbh anova.xlsx")
panv6 <- anova_joint(BSK, ENV, GEN, REP, PB)
pavt6 <- panv6$PB$anova
write_xlsx(pavt6, "pld pb anova.xlsx")
panv7 <- anova_joint(BSK, ENV, GEN, REP, BA)
pavt7 <- panv7$BA$anova
write_xlsx(pavt7, "pld ba anova.xlsx")
panv8 <- anova_joint(BSK, ENV, GEN, REP, Nodes)
pavt8 <- panv8$Nodes$anova
write_xlsx(pavt8, "pld nodes anova.xlsx")
panv9 <- anova_joint(BSK, ENV, GEN, REP, INL)
pavt9 <- panv9$INL$anova
write_xlsx(pavt9, "pld inl anova.xlsx")
########annichiarico env index#########
ann1 <- Annicchiarico(BSK, ENV, GEN, REP, Height)
print(ann1)
View(ann1$Height$environments)
ann2 <-Annicchiarico(BSK, ENV, GEN, REP, BD)
print(ann2)
View(ann2$BD$environments)
ann3 <- Annicchiarico(BSK, ENV, GEN, REP, DBH)
print (ann3)
View(ann3$DBH$environments)
ann4 <- Annicchiarico(BSK, ENV, GEN, REP, VI)
print(ann4)
View(ann4$VI$environments)
ann5 <- Annicchiarico(BSK, ENV, GEN, REP, CBH)
print(ann5)
View(ann5$CBH$environments)
ann6 <- Annicchiarico(BSK, ENV, GEN, REP, PB)
print(ann6)
View(ann6$PB$environments)
ann7 <- Annicchiarico(BSK, ENV, GEN, REP, BA)
print(ann7)
View(ann7$BA$environments)
ann8 <- Annicchiarico(BSK, ENV, GEN, REP, Nodes)
print(ann8)
View(ann8$Nodes$environments)
ann9 <- Annicchiarico(BSK, ENV, GEN, REP, INL)
print(ann9)
View(ann9$INL$environments)
########## ecovalence
eco1 <- ecovalence(BSK,ENV, GEN, REP, Height)
eco1
View(eco1$Height)
eco2 <- ecovalence(BSK,ENV, GEN, REP, BD)
eco2
View(eco2$BD)
eco3 <- ecovalence(BSK,ENV, GEN, REP, DBH)
eco3
View(eco3$DBH)
eco4 <- ecovalence(BSK,ENV, GEN, REP, VI)
eco4
View(eco4$VI)
eco5 <- ecovalence(BSK,ENV, GEN, REP, CBH)
eco5
View(eco5$CBH)
eco6 <- ecovalence(BSK,ENV, GEN, REP, PB)
eco6
View(eco6$PB)
eco7 <- ecovalence(BSK,ENV, GEN, REP, BA)
eco7
View(eco7$BA)
eco8 <- ecovalence(BSK,ENV, GEN, REP, Nodes)
eco8
View(eco8$Nodes)
eco9 <- ecovalence(BSK,ENV, GEN, REP, INL)
eco9
View(eco9$INL)
######## regression based method########
reg1 <- ge_reg(BSK, ENV, GEN, REP, Height)
reg1
View(reg1$Height$regression)
View(reg1$Height$anova)
write_xlsx(reg1$Height$regression, "height regression.xlsx")
reg2 <- ge_reg(BSK, ENV, GEN, REP, BD)
reg2
View(reg2$BD$regression)
write_xlsx(reg2$BD$regression, "bd regression.xlsx")
reg3 <- ge_reg(BSK, ENV, GEN, REP, DBH)
reg3
write_xlsx(reg3$DBH$regression, "dbh regression.xlsx")
reg4 <- ge_reg(BSK, ENV, GEN, REP, VI)
reg4
View(reg4$VI$regression)
write_xlsx(reg4$VI$regression, "vi regression.xlsx")
reg5 <- ge_reg(BSK, ENV, GEN, REP, CBH)
reg5
View(reg5$CBH$regression)
write_xlsx(reg5$CBH$regression, "cbh regression.xlsx")
reg6 <- ge_reg(BSK, ENV, GEN, REP, PB)
reg6
write_xlsx(reg6$PB$regression, "pb regression.xlsx")
reg7 <- ge_reg(BSK, ENV, GEN, REP, BA)
reg7
write_xlsx(reg7$BA$regression, "ba regression.xlsx")
reg8 <- ge_reg(BSK, ENV, GEN, REP, Nodes)
reg8
write_xlsx(reg8$Nodes$regression, "nodes regression.xlsx")
reg9 <- ge_reg(BSK, ENV, GEN, REP, INL)
reg9
write_xlsx(reg9$INL$regression, "inl regression.xlsx")
#####MTSII
waas(BSK, ENV, GEN, REP, resp = Height)
mod <- waas(BSK, ENV, GEN, REP, Height)
write_xlsx(mod$Height$ANOVA, "ht ammmi anova.xlsx")
View(mod$Height$ANOVA)
plot_scores(mod, type = 3)
waas(BSK, ENV, GEN, REP, resp = BD)
mod1 <- waas(BSK, ENV, GEN, REP, BD)
write_xlsx(mod1$BD$ANOVA, "bd ammi anova.xlsx")
plot_scores(mod1, type = 3)
waas(BSK, ENV, GEN, REP, resp = DBH)
mod2 <- waas(BSK, ENV, GEN, REP, DBH)
write_xlsx(mod2$DBH$ANOVA, "dbh ammi anova.xlsx")
plot_scores(mod2, type = 3)
waas(BSK, ENV, GEN, REP, resp = VI)
mod3 <- waas(BSK, ENV, GEN, REP, VI)
write_xlsx(mod3$VI$ANOVA, "vi ammi anova.xlsx")
plot_scores(mod3, type = 3)
waas(BSK, ENV, GEN, REP, resp = CBH)
mod4 <- waas(BSK, ENV, GEN, REP, CBH)
write_xlsx(mod4$CBH$ANOVA, "cbh ammi anova.xlsx")
plot_scores(mod4, type = 3)
waas(BSK, ENV, GEN, REP, resp = PB)
mod5 <- waas(BSK, ENV, GEN, REP, PB)
write_xlsx(mod5$PB$ANOVA, "pb ammi anova.xlsx")
plot_scores(mod5, type = 3)
waas(BSK, ENV, GEN, REP, resp = BA)
mod6 <- waas(BSK, ENV, GEN, REP, BA)
write_xlsx(mod6$BA$ANOVA, "ba ammi anova.xlsx")
plot_scores(mod6, type = 3)
waas(BSK, ENV, GEN, REP, resp = Nodes)
mod7 <- waas(BSK, ENV, GEN, REP, Nodes)
write_xlsx(mod7$Nodes$ANOVA, "nodes ammi anova.xlsx")
plot_scores(mod7, type = 3)
waas(BSK, ENV, GEN, REP, resp = INL)
mod8 <- waas(BSK, ENV, GEN, REP, INL)
write_xlsx(mod8$INL$ANOVA, "INL ammi anova.xlsx")
plot_scores(mod8, type = 3)

#########
mtsi_mod <- BSK %>% waas(ENV, GEN, REP, everything()) %>%mtsi()
print(mtsi_mod$sel_dif_stab)
View(mtsi_mod$sel_dif_stab)
write_xlsx(mtsi_mod$sel_dif_stab, "mtsi sd value.xlsx")
print(mtsi_mod$sel_dif_mps)
View(mtsi_mod$sel_dif_mps)
print(mtsi_mod$data)
print(mtsi_mod$PCA)
print(mtsi_mod$sel_dif_trait)
View(mtsi_mod$sel_dif_trait)
print(mtsi_mod$stat_dif_trait)
print(mtsi_mod$sel_dif_stab)
print(mtsi_mod$stat_dif_trait)
print(mtsi_mod$sel_dif_mps)
write_xlsx(mtsi_mod$MTSI, "mtsi value.xlsx")
write_xlsx(mtsi_mod$sel_dif_mps, "mtsi x0 xs value.xlsx")
View(mtsi_mod$MTSI)

?mtsi
plot(mtsi_mod)
anova_ind(BSK, ENV, GEN, REP, everything())
########
wsmp_mod <- BSK %>%waas(ENV, GEN, REP, Height)%>% wsmp()
plot(wsmp_mod)
######### ammi 
amod1 <-performs_ammi(BSK, ENV, GEN, REP, Height)
print(amod1)
plot(amod1)
plot_scores(amod1)
View(amod1$Height$ANOVA)
write_xlsx(amod1$Height$ANOVA, "ht ammi anova.xlsx")
amod2 <-performs_ammi(BSK, ENV, GEN, REP, BD)
print(amod2)
plot(amod2)
plot_scores(amod2)
write_xlsx(amod2$BD$ANOVA, "bd ammi anova.xlsx")
amod3 <- performs_ammi(BSK, ENV, GEN, REP, DBH)
print(amod3)
plot(amod3)
plot_scores(amod3)
write_xlsx(amod3$DBH$ANOVA, "dbh ammi anova.xlsx")
amod4 <- performs_ammi(BSK, ENV, GEN, REP, VI)
print(amod4)
plot(amod4)
plot_scores(amod4)
write_xlsx(amod4$VI$ANOVA, "vi ammi anova.xlsx")
amod5 <- performs_ammi(BSK, ENV, GEN, REP, CBH)
print(amod5)
plot(amod5)
plot_scores(amod5)
write_xlsx(amod5$CBH$ANOVA, "cbh ammi anova.xlsx")
amod6 <-performs_ammi(BSK, ENV, GEN, REP, PB)
print(amod6)
plot(amod6)
plot_scores(amod6)
write_xlsx(amod6$PB$ANOVA, "pb ammi anova.xlsx")
amod7 <-performs_ammi(BSK, ENV, GEN, REP, BA)
print(amod7)
plot(amod7)
plot_scores(amod7)
write_xlsx(amod7$BA$ANOVA, "ba ammi anova.xlsx")
amod8 <- performs_ammi(BSK, ENV, GEN, REP, Nodes)
print(amod8)
plot(amod8)
plot_scores(amod8)
write_xlsx(amod8$Nodes$ANOVA, "nodes ammi anova.xlsx")
amod9 <- performs_ammi(BSK, ENV, GEN, REP, INL)
print(amod9)
plot(amod9)
plot_scores(amod9)
write_xlsx(amod9$INL$ANOVA)
######## significance of ipca ################
get_model_data(amod1, "ipca_pval")
############ ammi biplot ###############
a1 <- plot_scores(amod1)
a1
a1 <- plot_scores(amod1, x.lab = "Height")
a1
b1 <- plot_scores(amod1, type = 2)
b1
b1 <- plot_scores(amod1, type = 2, polygon = TRUE)
b1
?plot_scores
c1 <- plot_scores(amod1, type = 4)
c1
?plot_scores
arrange_ggplot(a1, b1, tag_levels = "a", nrow = 1)
######## BD ###########3
a2 <- plot_scores(amod2)
a2
a2 <- plot_scores(amod2, x.lab = "Basal Diameter")
a2
b2 <- plot_scores(amod2, type = 2)
b2
b2 <- plot_scores(amod2, type = 2, polygon = TRUE)
b2
arrange_ggplot(a2, b2, tag_levels = "a", nrow = 1)
########## DBH ############
a3 <- plot_scores(amod3)
a3
a3 <- plot_scores(amod3, x.lab = "Diameter at Breast Height")
a3
b3 <- plot_scores(amod3, type = 2)
b3
b3 <- plot_scores(amod3, type = 2, polygon = TRUE)
b3
arrange_ggplot(a3, b3, tag_levels = "a", nrow = 1)
########## VI #############
a4 <- plot_scores(amod4)
a4
a4 <- plot_scores(amod4, x.lab = "Volume Index")
a4
b4 <- plot_scores(amod4, type = 2)
b4
b4 <- plot_scores(amod4, type = 2, polygon = TRUE)
b4
arrange_ggplot(a4, b4, tag_levels = "a", nrow = 1)
a5 <- plot_scores(amod5)
a5
a5 <- plot_scores(amod5, x.lab = "Clear Bole Height")
a5
b5 <- plot_scores(amod5, type = 2)
b5
b5 <- plot_scores(amod5, type = 2, polygon = TRUE)
b5
arrange_ggplot(a5, b5, tag_levels = "a", nrow = 1)
a6 <- plot_scores(amod6)
a6
a6 <- plot_scores(amod6, x.lab = "Primary Branches")
a6
b6 <- plot_scores(amod6, type = 2)
b6
b6 <- plot_scores(amod6, type = 2, polygon = TRUE)
b6
arrange_ggplot(a6, b6, tag_levels = "a", nrow = 1)
a7 <- plot_scores(amod7)
a7
a7 <- plot_scores(amod7, x.lab = "Branch Angle")
a7
b7 <- plot_scores(amod7, type = 2)
b7
b7 <- plot_scores(amod7, type = 2, polygon = TRUE)
b7
arrange_ggplot(a7, b7, tag_levels = "a", nrow = 1)
a8 <- plot_scores(amod8)
a8
b8 <- plot_scores(amod8, type = 2)
b8
b8 <- plot_scores(amod8, type = 2, polygon = TRUE)
b8
arrange_ggplot(a8, b8, tag_levels = "a", nrow = 1)
a9 <- plot_scores(amod9)
a9
a9 <- plot_scores(amod9, x.lab = "Internodal Length")
a9
b9 <- plot_scores(amod9, type = 2)
b9
b9 <- plot_scores(amod9, type = 2, polygon = TRUE)
b9
arrange_ggplot(a9, b9, tag_levels = "a", nrow = 1)

################################### ammi based stability statistics ########################################################
abs1 <- ammi_indexes(amod1)
print(abs1)
View(abs1$Height)
###########gge biplot ################
gge_model1 <- gge(BSK, ENV, GEN, Height)
predict(gge_model1)
pgge1 <- predict(gge_model1)
View(pgge1$Height)
gge_model2 <- gge(BSK, ENV, GEN, BD)
predict(gge_model2)
pgge2 <- predict(gge_model2)
gge_model3 <- gge(BSK, ENV, GEN, DBH)
predict(gge_model3)
pgg3 <- predict(gge_model3)
gge_model4 <- gge(BSK, ENV, GEN, VI)
predict(gge_model4)
pgg4 <- predict(gge_model4)
gge_model5 <- gge(BSK, ENV, GEN, CBH)
predict(gge_model5)
pgg5 <- predict(gge_model5)
gge_model6 <- gge(BSK, ENV, GEN, PB)
predict(gge_model6)
pgg6 <- predict(gge_model6)
gge_model7 <- gge(BSK, ENV, GEN, BA)
predict(gge_model7)
pgg7 <- predict(gge_model7)
gge_model8 <- gge(BSK, ENV, GEN, Nodes)
predict(gge_model8)
pgg8 <- predict(gge_model8)
gge_model9 <- gge(BSK, ENV, GEN, INL)
predict(gge_model9)
pgg9 <- predict(gge_model9)

# basic biplot
bbp1 <- plot(gge_model1)
bbp1
bbp2 <- plot(gge_model2)
bbp2
bbp3 <- plot(gge_model3)
bbp3
bbp4 <- plot(gge_model4)
bbp4
bbp5 <- plot(gge_model5)
bbp5
bbp6 <- plot(gge_model6)
bbp6
bbp7 <- plot(gge_model7)
bbp7
bbp8 <- plot(gge_model8)
bbp8
bbp9 <- plot(gge_model9)
bbp9
############ svp= environment############
########## descriminativeness vs representiveness#######
dvr  <- plot(gge_model1, type = 4)
dvr
dvr1  <- plot(gge_model2, type = 4)
dvr1
dvr2  <- plot(gge_model3, type = 4)
dvr2
dvr3  <- plot(gge_model4, type = 4)
dvr3
dvr4  <- plot(gge_model5, type = 4)
dvr4
dvr5  <- plot(gge_model6, type = 4)
dvr5
dvr6  <- plot(gge_model7, type = 4)
dvr6
dvr7  <- plot(gge_model8, type = 4)
dvr7
dvr8  <- plot(gge_model9, type = 4)
dvr8
######### svp = genotype
#######mean performance vs stability
gpg1 <- gge(BSK, ENV, GEN, Height, svp = "genotype")
pgpg1 <- predict(gpg1)
View(pgpg1$Height)
gpg2 <- gge(BSK, ENV, GEN, BD, svp = "genotype")
pgpg2 <- predict(gpg2)
View(pgpg2$BD)
gpg3 <- gge(BSK, ENV, GEN, DBH, svp = "genotype")
pgpg3 <- predict(gpg3)
View(pgpg3$DBH)
gpg4 <- gge(BSK, ENV, GEN, VI, svp = "genotype")
pgpg4 <- predict(gpg4)
View(pgpg4$VI)
gpg5 <- gge(BSK, ENV, GEN, CBH, svp = "genotype")
pgpg5 <- predict(gpg5)
View(pgpg5$CBH)
gpg6 <- gge(BSK, ENV, GEN, PB, svp = "genotype")
pgpg6 <- predict(gpg6)
View(pgpg6$PB)
gpg7 <- gge(BSK, ENV, GEN, BA, svp = "genotype")
pgpg7 <- predict(gpg7)
View(pgpg7$BA)
gpg8 <- gge(BSK, ENV, GEN, Nodes, svp = "genotype")
pgpg8 <- predict(gpg8)
View(pgpg8$Nodes)
gpg9 <- gge(BSK, ENV, GEN, INL, svp = "genotype")
pgpg9 <- predict(gpg9)
View(pgpg9$INL)
mvs1 <- plot(gpg1, type = 2)
mvs1
mvs2 <- plot(gpg2, type = 2)
mvs2
mvs3 <- plot(gpg3, type = 2)
mvs3
mvs4 <- plot(gpg4, type = 2)
mvs4
mvs5 <- plot(gpg5, type = 2)
mvs5
mvs6 <- plot(gpg6, type = 2)
mvs6
mvs7 <- plot(gpg7, type = 2)
mvs7
mvs8 <- plot(gpg8, type = 2)
mvs8
mvs9 <- plot(gpg9, type = 2)
mvs9
########## svp = symmetrical
gps1 <- gge(BSK, ENV, GEN, Height, svp = "symmetrical")
pgps1 <- predict(gps1)
View(pgps1$Height)
gps2 <- gge(BSK, ENV, GEN, BD, svp = "symmetrical")
pgps2 <- predict(gps2)
gps3 <- gge(BSK, ENV, GEN, DBH, svp = "symmetrical")
pgps3 <- predict(gps3)
gps4 <- gge(BSK, ENV, GEN, VI, svp = "symmetrical")
pgps4 <- predict(gps4)
gps5 <- gge(BSK, ENV, GEN, CBH, svp = "symmetrical")
pgps5 <- predict(gps5)
gps6 <- gge(BSK, ENV, GEN, PB, svp = "symmetrical")
pgps6 <- predict(gps6)
gps7 <- gge(BSK, ENV, GEN, BA, svp = "symmetrical")
pgps7 <- predict(gps7)
gps8 <- gge(BSK, ENV, GEN, Nodes, svp = "symmetrical")
pgps8 <- predict(gps8)
gps9 <- gge(BSK, ENV, GEN, INL, svp = "symmetrical")
pgps9 <- predict(gps9)
##### which won where
www1 <- plot(gps1, type = 3)
www1
www2 <- plot(gps2, type = 3)
www2
www3 <- plot(gps3, type = 3)
www3
www4 <- plot(gps4, type = 3)
www4
www5 <- plot(gps5, type = 3)
www5
www6 <- plot(gps6, type = 3)
www6
www7 <- plot(gps7, type = 3)
www7
www8 <- plot(gps8, type = 3)
www8
www9 <- plot(gps9, type = 3)
www9
###### correlation and covarience
de1 <- subset(BSK, ENV=="E1")
View(de1)
cr_cv <- covcor_design(de1, GEN, REP, resp = c(Height, BD, DBH, VI, CBH, PB, BA, Nodes, INL),design = "RCBD")
cr_cv
View(cr_cv$geno_cor)
class(cr_cv$geno_cor)
e1gencor <-as.data.frame(cr_cv$geno_cor)
write_xlsx(e1gencor, "e1 gen cor.xlsx")
View(cr_cv$phen_cor)
e1phecor <-as.data.frame(cr_cv$phen_cor)
write_xlsx(e1phecor, "e1 phe cor.xlsx")
View(cr_cv$geno_cov)
View(cr_cv$phen_cov)
View(cr_cv$resi_cov)
View(cr_cv$resi_cor)
View(cr_cv$means)
################
de2 <- subset(BSK, ENV=="E2")
View(de2)
cr_cv <- covcor_design(de2, GEN, REP, resp = c(Height, BD, DBH, VI, CBH, PB, BA, Nodes, INL),design = "RCBD")
cr_cv
View(cr_cv$geno_cor)
class(cr_cv$geno_cor)
e2gencor <-as.data.frame(cr_cv$geno_cor)
write_xlsx(e2gencor, "e2 gen cor.xlsx")
View(cr_cv$phen_cor)
e2phecor <-as.data.frame(cr_cv$phen_cor)
write_xlsx(e2phecor, "e2 phe cor.xlsx")
##################
de3 <- subset(BSK, ENV=="E3")
View(de3)
cr_cv <- covcor_design(de3, GEN, REP, resp = c(Height, BD, DBH, VI, CBH, PB, BA, Nodes, INL),design = "RCBD")
cr_cv
View(cr_cv$geno_cor)
class(cr_cv$geno_cor)
e3gencor <-as.data.frame(cr_cv$geno_cor)
write_xlsx(e3gencor, "e3 gen cor.xlsx")
View(cr_cv$phen_cor)
e3phecor <-as.data.frame(cr_cv$phen_cor)
write_xlsx(e3phecor, "e3 phe cor.xlsx")
########################3
View(cr_cv$geno_cov)
View(cr_cv$phen_cov)
View(cr_cv$resi_cov)
View(cr_cv$resi_cor)
View(cr_cv$means)



















































