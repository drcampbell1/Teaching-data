# European Social Survey Cumulative File

#Populist right, Norway: Fremskrittspartiet
#                Sweden : Sverigedemokraterna
#                Poland : Kukiz '15

ess_full <- read_csv("/Users/rosscampbell/Desktop/ess_cum.csv")

ess <- ess_full %>% 
  select(cntry, essround, ppltrst, polintr, 
               trstprl,  trstlgl,  trstplc, trstplt,  
               trstprt,  trstep, imsmetn, imdfetn, vote, contplt, 
               wrkprty, wrkorg, badge, sgnptit, pbldmn,
               bctprd, stfdem, stfgov, stfeco, prtvede2, prtvtbgb,
               prtvtgnl, prtvtbno, prtvtbse, prtvtgch, prtvtdfr, prtvtdpl, 
               gndr, agea, eduyrs, uemp12m)
library(naniar)

ess <- ess %>% 
  replace_with_na_at(.vars = c("trstprl","trstlgl","trstplc", 
                               "trstplt", "trstprt", "trstep"),
                     condition = ~ .x >10)
ess <- ess %>% 
  replace_with_na_at(.vars = c("vote","contplt","wrkprty", 
                               "wrkorg", "badge", "sngptit",
                               "pbldmn", "bctprd"),
                     condition = ~ .x >2)


# Make participation variables factors

ess$vote <- factor(ess$vote, levels = c(1,2), labels= c("voted", "did not vote"))
ess$contplt <- factor(ess$contplt, levels = c(1,2), labels= c("contacted", "not contacted"))
ess$wrkprty <- factor(ess$wrkprty, levels = c(1,2), labels= c("worked", "not worked"))
ess$wrkorg <- factor(ess$wrkorg, levels = c(1,2), labels= c("worked", "not worked"))
ess$badge <- factor(ess$badge, levels = c(1,2), labels= c("worn", "not worn"))
ess$sngptit <- factor(ess$sgnptit, levels = c(1,2), labels= c("signed", "not signed"))
ess$pbldmn <- factor(ess$pbldmn, levels = c(1,2), labels= c("demonstrated", "not demonstrated"))
ess$bctprd <- factor(ess$bctprd, levels = c(1,2), labels= c("boycotted", "not boycotted"))

# Add Year variable 
ess <- ess %>% 
  mutate(year = 2000 + 2 * essround)

# Add Country variable

ess <- ess %>% 
  mutate(country = fct_recode(cntry, "Switzerland" = 'CH', 
                         "Germany" = 'DE', 
                         "France" = 'FR', 
                         "UK" = 'GB', 
                         "Netherlands" = 'NL', 
                         "Norway" = 'NO', 
                         "Poland" = 'PL', 
                         "Sweden" = 'SE'))
library(memisc)
ess$agecat <- recode(ess$agea, 0 <- 14:17, 1 <- 18:29, 2 <- 30:44, 3 <- 45:59, 4 <- 60:74, 5 <- 75:123)
ess$agecat <- factor(ess$agecat, levels = c(0,1,2,3,4,5), 
                     labels= c("14-17", "18-29", "30-44", "45-59", "60-74", "75+"))

ess$gender <- factor(ess$gndr, 
                     levels = c(1,2), labels = c("male", "female"))


ess$educat <- recode(ess$eduyrs, 0 <- 12, 1 <- 13:14, 2 <- 15:18, 3 <- 19:56)
ess$educat <- factor(ess$educat, levels = c(0,1,2,3), 
                     labels = c("Basic", "HS", "Uni", "Postgrad"))

ess$unemp <- factor(ess$uemp12m, levels = c(1,2), 
                    labels = c("yes", "no"))

ess$pinterest <- factor(ess$polintr, levels = c(1,2,3,4), 
                    labels = c("very", "quite", "hardly", "none"))

# Radical Right Parties
ess$afd <- recode(ess$prtvede2, 1 <- 6, 0 <- 1:5, 0 <- 7:9)
ess$ukip <- recode(ess$prtvtbgb, 1 <- 7, 0 <- 1:6, 0 <- 8:18)
ess$pvv <- recode(ess$prtvtgnl, 1 <- 3, 0 <- 1:2, 0 <- 4:17)
ess$fkk <- recode(ess$prtvtbno, 1 <- 8, 0 <- 1:7, 0 <- 9:11)
ess$swd <- recode(ess$prtvtbse, 1 <- 10, 0 <- 1:9, 0 <- 11)
ess$spp <- recode(ess$prtvtgch, 1 <- 1, 0 <- 2:16)
ess$kuk <- recode(ess$prtvtdpl, 1 <- 2, 0 <- 1, 0 <- 3:9)
ess$fnn <- recode(ess$prtvtdfr, 1 <- 11, 0 <- 2:10, 0 <- 12)



ess <- ess %>% 
  mutate(rright = case_when(afd ==1 | ukip==1 | pvv ==1 | fkk == 1 | swd == 1 | spp == 1 | kuk== 1 | fnn == 1 ~ "1", 
                                  afd == 0 |  ukip == 0 | pvv==0 | fkk ==0 | swd ==0 | spp ==0 | kuk ==0 | fnn ==0 ~"0", 
                                  TRUE ~ "NA"))
ess$rright <- factor(ess$rright,
                     levels = c(0,1), labels = c("did not vote", "voted"))

ess$ptrust <- recode(ess$trstplc, 0 <- 0:3, 1 <- 4:6, 2 <- 7:10)
ess$ptrust <- factor(ess$ptrust, levels = c(0,1,2), labels = c("low", "medium", "high"))

ess$strust <- recode(ess$ppltrst, 0 <- 0:3, 1 <- 4:6, 2 <- 7:10)
ess$strust <- factor(ess$strust, levels = c(0,1,2), labels = c("low", "medium", "high"))
ess$satecon <- recode(ess$stfeco, 0 <- 0:3, 1 <- 4:6, 2 <- 7:10)
ess$satecon <- factor(ess$satecon, levels = c(0,1,2), labels = c("low", "medium", "high"))
ess$satgov <- recode(ess$stfgov, 0 <- 0:3, 1 <- 4:6, 2 <- 7:10)
ess$satgov <- factor(ess$satgov, levels = c(0,1,2), labels = c("low", "medium", "high"))


ess <- ess %>% 
  replace_with_na_at(.vars = c("imsmetn","imdfetn"),
                     condition = ~ .x > 4)

ess$immseth <- factor(ess$imsmetn, levels = c(1,2,3,4), 
                      labels = c("many", "some", "few", "none"))

ess$immdeth <- factor(ess$imdfetn, levels = c(1,2,3,4), 
                      labels = c("many", "some", "few", "none"))

ess_new <- ess %>% select(vote, contplt, wrkprty, badge, 
                      pbldmn, bctprd, sngptit, pinterest, 
                      year, country, agea, agecat, gender, educat,
                      rright, unemp, ptrust, strust, immseth,
                      immdeth, satecon, satgov)

ess_new <- ess_new %>% rename(contact = contplt, 
                   party = wrkprty, 
                   demo = pbldmn, 
                   boyct = bctprd, 
                   petit = sngptit, 
                   age = agea, 
                   right = rright)

ess_new <- ess_new %>% 
  relocate(country, .before = vote)

ess_new <- ess_new %>% 
  relocate(year, .after = country)

