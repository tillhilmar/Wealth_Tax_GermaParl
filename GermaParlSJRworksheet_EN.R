
install.packages("polmineR")
library(polmineR)
use("GermaParl")

library(data.table)
library(magrittr)
library(xts)
library(lubridate)
library(ggplot2)
library(ggpubr)


junk <- c(tm::stopwords("de"), c(".", "''", ",", "``", ")", "(", "-", "!", "[", "]"), "000", "?", ":", "--")


# Preliminary Analysis ----

count("GERMAPARL", query = "Vermögensteuer")

# Only using the speech by PARLIAMENTARY GROUPS:
FraktionenParl <- partition("GERMAPARL", parliamentary_group = c("SPD", "CDU/CSU", "GRUENE", "FDP", "PDS", "LINKE"), interjection = FALSE)

count(FraktionenParl, query = c("Vermögensteuer", "Vermögenssteuer", "Vermögensbesteuerung", "Vermögensabgabe", "Schenkungsteuer", "Millionärsteuer", "Reichensteuer", "Erbschaftsteuer"))
count(FraktionenParl, query = c("Mehrwertsteuer", "Tabaksteuer", "Alkoholsteuer", "Mineralölsteuer", "Kfz-Steuer", "Kraftfahrzeugsteuer"))

count("GERMAPARL", query = c("Vermögensteuer", "Vermögenssteuer", "Vermögensbesteuerung", "Vermögensabgabe", "Schenkungsteuer", "Millionärsteuer", "Reichensteuer", "Erbschaftsteuer"))
count("GERMAPARL", query = c("Mehrwertsteuer", "Tabaksteuer", "Alkoholsteuer", "Mineralölsteuer", "Kfz-Steuer", "Kraftfahrzeugsteuer"))

# some interesting KWICs  ----

#Is there a difference between an CQP and non CQP search for this term?
count("GERMAPARL", query = "Vermögensteuer")
count("GERMAPARL", query = '"Vermögensteuer"', cqp = TRUE)
#no difference between CQP an non-CQP for this term

#is there an adjective usage of Vermögen in the material?
count("GERMAPARL", query = c('"Vermögensteuer"', '"Vermögensteuer*" %c'), cqp = TRUE)
count("GERMAPARL", query = '"vermögensteuer*"', cqp = TRUE)
#no hits for adjectives like "vermögensteuerlich", no hits for lower-case

#getting a sense of how the stem "Vermögens*" behaves here:
count("GERMAPARL", query = '"Vermögen.*"', cqp = TRUE, breakdown = TRUE) %>% head(n = 15)
count("GERMAPARL", query = '"[Vv]ermögen.*"', cqp = TRUE, breakdown = TRUE) %>% head(n = 15)
#finding: Adjektive Abwandlungen sind hier relativ irrelevant. Wir können uns also auf das Substantiv konzentrieren.

#how about "Reiche.*"?
count("GERMAPARL", query = '"Reiche.*"', cqp = TRUE, breakdown = TRUE) %>% head(n = 15)
count(FraktionenParl, query = '"Reiche.*"', cqp = TRUE, breakdown = TRUE) %>% head(n = 15)

# "Vermögensversteuerung" doesn't exist here as a word
count("GERMAPARL", query = '"Vermögens(be|ver)steuerung.*"', breakdown = TRUE) %>% head()

# Important: To identify relevant passages, not just the term itself 
kwic("GERMAPARL", query = '"Vermögen.*" []{0,5} ".*[Ss]teuer.*"', cqp = TRUE)
kwic("GERMAPARL", query = '".*[Ss]teuer.*" []{0,5} "Vermögen.*"', cpq = TRUE)
kwic("GERMAPARL", query = "Vermögensteuer") # erzielt 2,212 Einträge während die anderen jeweils nur etwas mehr als 800 erzielen
# Some interesting KWICS: ----
kwic("GERMAPARL", query = '"Vermögen.*" []{0,5} ".*[Ss]teuer.*"', cqp = TRUE)
kwic("GERMAPARL", query = '"Vermögen.*" []{0,5} ".*(be|ver)[Ss]teuer.*"', cqp = TRUE, left = 15, right = 15, s_attributes = c("party", "date"))
kwic("GERMAPARL", query = '"Vermögen.*" []{0,5} ".*(be|ver)[Ss]teuer.*"', cqp = TRUE, left = 25, right = 25, s_attributes = c("party", "date"))

count("GERMAPARL", query = '".*[Ss]teuer.*"', breakdown = TRUE) %>% head(n = 30)
# "Vermögensteuer" (wealth tax) comes directly after "Ökosteuer" (ecological tax) (4982) and "Mehrwertsteuer" (value-added tax) (2420) at (2212).
count(FraktionenParl, query = '".*[Ss]teuer.*"', breakdown = TRUE) %>% head(n = 30)
count("GERMAPARL", query = '".*[Ss]teuer.*"', breakdown = TRUE) %>% tail(n = 30)
count("GERMAPARL", query = '".*[Ss]teuer.*"', breakdown = TRUE) %>% tail(n = 90)

# Taxation -- in which combination does this formulation appear?
count("GERMAPARL", query = '"Besteuerung" [] [pos = "NN"]', cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))
# Turns out, "Besteuerung von Vermögen" (taxation of wealth) ranks second/third - tied with "Besteuerung der Alterseinkünfte" (taxation of retirement income) at 20)
count(FraktionenParl, query = '"Besteuerung" [] [pos = "NN"]', cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count("GERMAPARL", query = '"Besteuerung" "von" [pos = "NN"]', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Besteuerung" "von" [pos = "NN"][][]', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Steuern" "vom" [pos = "NN"][][][]', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Millionärsteuer"', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Vermögensteuer"', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count("GERMAPARL", query = '"Steuern" "auf" [pos = "NN"]', 
      cqp = T, breakdown = T) %>% head(n = 15) %>% 
  subset(select = c("match", "count", "share"))
count(FraktionenParl, query = '"Steuern" "auf" [pos = "NN"]', 
      cqp = T, breakdown = T) %>% head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Steuern" "vom" "Einkommen" "und" "vom" "Vermögen"', 
      cqp = T, breakdown = T) %>% head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Vermögensabgabe"', 
      cqp = T, breakdown = T) %>% head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count(FraktionenParl, query = '"Vermögensbesteuerung"', 
      cqp = T, breakdown = T) %>% head(n = 15) %>% 
  subset(select = c("match", "count", "share"))

count("GERMAPARL", query = '"Steuer.*" [] [] "*.Vermögen.*"', cqp = T, breakdown = T) %>% head(n = 15) %>% subset(select = c("match", "count", "share"))
count(FraktionenParl, query = '"Steuer.*" [] [] "*.Vermögen.*"', cqp = T, breakdown = T) %>% head(n = 15) %>% subset(select = c("match", "count", "share"))
# FINDING: Die Formulierung "Steuern vom Einkommen und vom Vermögen" kommt 157 mal vor! Ist also wichtig!
# Die Formulierung "Besteuerung von Vermögen" zumindest 20 Mal
count("GERMAPARL", query = '".*[Ss]teuer.*" []{0,5} "[Vv]ermögen.*"', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share")
  )

count(FraktionenParl, query = '".*[Ss]teuer.*" []{0,5} "[Vv]ermögen.*"', 
      cqp = T, breakdown = T) %>% 
  head(n = 15) %>% 
  subset(select = c("match", "count", "share")
  )

count("GERMAPARL", query = '"[Vv]ermögen.*" []{0,5} ".*[Ss]teuer.*"', 
      cqp = T, breakdown = T) %>% 
  head(n = 10) %>% 
  subset(select = c("match", "count", "share")
  )

count(FraktionenParl, query = '"[Vv]ermögen.*" []{0,5} ".*[Ss]teuer.*"', 
      cqp = T, breakdown = T) %>% 
  head(n = 10) %>% 
  subset(select = c("match", "count", "share")
  )


count("GERMAPARL", query = '".*[Ee]ntlast.*"', breakdown = TRUE) %>% head(n = 30)
count("GERMAPARL", query = '".*[Ee]ntlast.*" [] [pos = "NN"]', cqp = T, breakdown = T) %>% 
  head(n = 30) %>% 
  subset(select = c("match", "count", "share"))

count("GERMAPARL", query = '"Doppelbesteuerung"', cqp = TRUE, breakdown = TRUE) %>% head(n = 15)

Q <- '("[Vv]ermögen.*" []{0,9} ".*[Ss]teuer.*" | ".*[Ss]teuer.*" []{0,9} "[Vv]ermögen.*")'
Y <- count("GERMAPARL", query = Q, cqp = TRUE)
Y[, "count"]
# ein Count als Ergebnis: 2392 - ist tendenziell nur im Vergleich aussagekräftig?
Q <- '("[Vv]ermögen.*" []{0,9} ".*[Ee]ntlast.*" | ".*[Ee]ntlast.*" []{0,9} "[Vv]ermögen.*")'
Y <- count("GERMAPARL", query = Q, cqp = TRUE)
Y[, "count"]

K <- kwic("GERMAPARL", query = "Vermögensteuer")
read(K, i = 1)

kwic("GERMAPARL", query = '"Vermögen.*" []{0,5} ".*[Ss]teuer.*"', cqp = TRUE)
kwic("GERMAPARL", query = '".*[Ss]teuer.*" []{0,5} "Vermögen.*"', cpq = TRUE)

kwic("GERMAPARL", query = '"Doppelbesteuerung" []{0,5} "Vermögen.*"', cpq = TRUE)

# can use CQP Queries for cooccurrences, too:
cooccurrences("GERMAPARL", query = '"(Vermögen.*|.*[Ss]teuer.*)"', cqp = TRUE) %>%
  as.data.table() %>% 
  subset(rank_ll < 30) %>% 
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``")) %>%
  DT::datatable() 
cooccurrences("GERMAPARL", query = '"(.*(be|ver)[Ss]teuer.*|Vermögen.*)"', cqp = TRUE) %>%
  as.data.table() %>% subset(rank_ll < 10) %>% DT::datatable()
# not all that different from the semantic neighborhood of the "Vermögensteuer" term, but interesting nonetheless


# Main visualization ----

# VISUALIZING Fraktionen: Parliamentary Groups only

par(mar = c(4,2,2,2))
dt <- dispersion(FraktionenParl, query = "Vermögensteuer", s_attribute = "date", interjection = FALSE)
dt <- dt[!is.na(as.Date(dt[["date"]]))]
ts <- xts(x = dt[["count"]], order.by = as.Date(dt[["date"]]))
plot(ts)

#ts_week <- aggregate(ts, {a <- lubridate::ymd(paste(lubridate::year(index(ts)), 1, 1, sep = "-")); lubridate::week(a) <- lubridate::week(index(ts)); a})
ts_month <- aggregate(ts, as.Date(as.yearmon(index(ts))))
#ts_qtr <- aggregate(ts, as.Date(as.yearqtr(index(ts))))
#ts_year <- aggregate(ts, as.Date(sprintf("%s-01-01", gsub("^(\\d{4})-.*?$", "\\1", index(ts)))))
#par(mfrow = c(2,2), mar = c(2,2,3,1))
#plot(as.xts(ts_week), main = "Aggregation: Woche")
plot(as.xts(ts_month), 
     main = "'wealth tax' by month",
     ylab="",
     type="l",
     col="darkblue",
     )

?plot

par(mar = c(4,2,2,2))
dt <- dispersion(FraktionenParl, query = "Einkommensteuer", s_attribute = "date", interjection = FALSE)
dt <- dt[!is.na(as.Date(dt[["date"]]))]
ts <- xts(x = dt[["count"]], order.by = as.Date(dt[["date"]]))
plot(ts)
?plot

#ts_week <- aggregate(ts, {a <- lubridate::ymd(paste(lubridate::year(index(ts)), 1, 1, sep = "-")); lubridate::week(a) <- lubridate::week(index(ts)); a})
ts_month <- aggregate(ts, as.Date(as.yearmon(index(ts))))
#ts_qtr <- aggregate(ts, as.Date(as.yearqtr(index(ts))))
#ts_year <- aggregate(ts, as.Date(sprintf("%s-01-01", gsub("^(\\d{4})-.*?$", "\\1", index(ts)))))
#par(mfrow = c(2,2), mar = c(2,2,3,1))
#plot(as.xts(ts_week), main = "Aggregation: Woche")
plot(as.xts(ts_month), 
     main = "'wealth tax' by month",
     ylab="",
     type="l",
     col="darkblue")

# optional: design ggplot
p <- ggplot(dt, aes(as.yearmon(date), count)) +
  geom_col() + 
  xlab("'Wealth tax' as mentioned by CDU/CSU and SPD") +
  theme_minimal()
p

wealthtaxdata <- read.csv("wealthtaxdata.csv", header = T, sep = ";", dec = ",")
wealthtaxdata$Year <- as.numeric(wealthtaxdata$Year)
wealthtaxdata$Percent_of_GDP <- as.numeric(wealthtaxdata$Percent_of_GDP)

str(wealthtaxdata)


plot(wealthtaxdata,
     main = "",
     ylab="Percent of GDP",
     #xaxp = c(1950,2014,5),
     xaxt = "n",
     #axis(1, seq(1950,2014,5)),
     type="l",
     col="darkblue",
     panel.first = grid())
axis(side = 1, at = c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2014))


?plot

wd_year <- aggregate(wealthtaxdata, as.Date(as.yearmon(index(wealthtaxdata))))

plot(as.xts(wealthtaxdata), 
     main = "'wealth tax' by month",
     ylab="",
     type="l",
     col="darkblue")
>
# Creating partitions by phase ----
  
CDU_CSU_Phase1_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", year = 1996:2001, interjection = FALSE)
CDU_CSU_Phase2_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", year = 2002:2007, interjection = FALSE)
CDU_CSU_Phase3_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", year = 2008:2016, interjection = FALSE)
SPD_Phase1_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", year = 1996:2001, interjection = FALSE)
SPD_Phase2_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", year = 2002:2007, interjection = FALSE)
SPD_Phase3_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", year = 2008:2016, interjection = FALSE)
CDU_CSU_MAIN <- "GERMAPARL" %>%
partition(parliamentary_group = "CDU/CSU", year = 1996:2016, interjection = FALSE)
SPD_MAIN_partition <- "GERMAPARL" %>%
partition(parliamentary_group = "SPD", year = 1996:2016, interjection = FALSE)

CDU_CSU_Phase1 <- "GERMAPARL" %>%
  subset(parliamentary_group = "CDU/CSU", year = 1996:2001, interjection = FALSE)
CDU_CSU_Phase2 <- "GERMAPARL" %>%
  subset(parliamentary_group = "CDU/CSU", year = 2002:2007, interjection = FALSE)
CDU_CSU_Phase3 <- "GERMAPARL" %>%
  subset(parliamentary_group = "CDU/CSU", year = 2008:2016, interjection = FALSE)
SPD_Phase1 <- "GERMAPARL" %>%
  subset(parliamentary_group = "SPD", year = 1996:2001, interjection = FALSE)
SPD_Phase2 <- "GERMAPARL" %>%
  subset(parliamentary_group = "SPD", year = 2002:2007, interjection = FALSE)
SPD_Phase3 <- "GERMAPARL" %>%
  subset(parliamentary_group = "SPD", year = 2008:2016, interjection = FALSE)
CDU_CSU_MAIN <- "GERMAPARL" %>%
  subset(parliamentary_group = "CDU/CSU", year = 1996:2016, interjection = FALSE)
SPD_MAIN <- "GERMAPARL" %>%
  subset(parliamentary_group = "SPD", year = 1996:2016, interjection = FALSE)

count(CDU_CSU_Phase1, query = '"Vermögensteuer"', cqp = TRUE)
count(CDU_CSU_Phase2, query = '"Vermögensteuer"', cqp = TRUE)
count(CDU_CSU_Phase3, query = '"Vermögensteuer"', cqp = TRUE)
count(CDU_CSU_Phase3, query = '"Vermögensteuer"', cqp = TRUE)
count(SPD_Phase1, query = '"Vermögensteuer"', cqp = TRUE)
count(SPD_Phase2, query = '"Vermögensteuer"', cqp = TRUE)
count(SPD_Phase3, query = '"Vermögensteuer"', cqp = TRUE)

#subsetting by party - for comparative purposes
party_CDU_Phase1 <- "GERMAPARL" %>%
  subset(party = "CDU", year = 1996:2001, interjection = FALSE)
party_CSU_Phase1 <- "GERMAPARL" %>%
  subset(party = "CSU", year = 1996:2001, interjection = FALSE)
party_CDU_Phase2 <- "GERMAPARL" %>%
  subset(party = "CDU", year = 2002:2007, interjection = FALSE)
party_CSU_Phase2 <- "GERMAPARL" %>%
  subset(party = "CSU", year = 2002:2007, interjection = FALSE)
party_CDU_Phase3 <- "GERMAPARL" %>%
  subset(party = "CDU", year = 2007:2016, interjection = FALSE)
party_CSU_Phase3 <- "GERMAPARL" %>%
  subset(party = "CSU", year = 2007:2016, interjection = FALSE)

party_SPD_Phase1 <- "GERMAPARL" %>%
  subset(party = "SPD", year = 1996:2001, interjection = FALSE)
party_SPD_Phase2 <- "GERMAPARL" %>%
  subset(party = "SPD", year = 2002:2007, interjection = FALSE)
party_SPD_Phase3 <- "GERMAPARL" %>%
  subset(party = "SPD", year = 2008:2016, interjection = FALSE)

count(CDU_CSU_Phase1, query = '"Vermögensteuer"', cqp = TRUE) # 278
count(CDU_CSU_Phase2, query = '"Vermögensteuer"', cqp = TRUE) # 159
count(CDU_CSU_Phase3, query = '"Vermögensteuer"', cqp = TRUE) # 176

count(party_CSU_Phase1, query = '"Vermögensteuer"', cqp = TRUE) # 171
count(party_CDU_Phase1, query = '"Vermögensteuer"', cqp = TRUE) # 248 - that's a LOT more... more than 400!
count(party_CDU_Phase2, query = '"Vermögensteuer"', cqp = TRUE) # 127
count(party_CSU_Phase2, query = '"Vermögensteuer"', cqp = TRUE) # 33 - here, the numbers ALMOST add up
count(party_CDU_Phase3, query = '"Vermögensteuer"', cqp = TRUE) # 150 
count(party_CSU_Phase3, query = '"Vermögensteuer"', cqp = TRUE) # 33 - a little more than

count(SPD_Phase1, query = '"Vermögensteuer"', cqp = TRUE) # 267
count(party_SPD_Phase1, query = '"Vermögensteuer"', cqp = TRUE) #312

count(SPD_Phase2, query = '"Vermögensteuer"', cqp = TRUE) # 60 
count(party_SPD_Phase2, query = '"Vermögensteuer"', cqp = TRUE) # 85

count(SPD_Phase3, query = '"Vermögensteuer"', cqp = TRUE) # 106
count(party_SPD_Phase3, query = '"Vermögensteuer"', cqp = TRUE) # 116

# the parliamentary group corpus is smaller than the party corpus. WHY?

fraktionslos_Phase1 <- "GERMAPARL" %>%
  subset(party = "fraktionslos", year = 1996:2001, interjection = FALSE)

s_attributes("GERMAPARL")
s_attributes("GERMAPARL", s_attribute = "party")
s_attributes("GERMAPARL", s_attribute = "parliamentary_group")

fraktionslos_Phase1 <- "GERMAPARL" %>%
  subset(parliamentary_group = "fraktionslos", year = 1996:2001, interjection = FALSE)
count(fraktionslos_Phase1, query = '"Vermögensteuer"', cqp = TRUE)


cooccurrences(CDU_CSU_Phase1, query = "Vermögensteuer", p_attribute = c("word", "pos")) %>%
#subset(ll >= 11.83) %>%
#subset(!tolower(word) %in% tm::stopwords("de")) %>%
#subset(!word %in% c("''", ",", "``"))
#subset(pos %in% c("NN", "ADJA")) %>%
subset(pos %in% c("ADJA")) %>%
as.data.table() %>%DT::datatable()

cooccurrences(CDU_CSU_Phase1, query = "Vermögensteuer", p_attribute = c("word", "pos")) %>%
subset(!word %in% junk) %>%
subset(pos %in% c("ADJA")) %>%
dotplot()

#different types of verbs that appear together with the term
cooccurrences(SPD_Phase1, query = "Vermögensteuer", p_attribute = c("word", "pos")) %>%
#subset(ll >= 11.83) %>%
#subset(!tolower(word) %in% tm::stopwords("de")) %>%
#subset(!word %in% junk) %>%
#subset(pos %in% c("NN", "ADJA")) %>%
subset(pos %in% c("VVFIN", "VVIMP", "VVINF", "VVIZU", "VVPP", "VAFIN")) %>%
as.data.table() %>%DT::datatable()

#Considering only co-occuring adjectives:
cooccurrences(CDU_CSU_Phase1, query = '"(Vermögen.*|.*[Ss]teuer.*)"', cqp = TRUE, p_attribute = c("word", "pos")) %>%
#subset(ll >= 11.83) %>%
#subset(!tolower(word) %in% tm::stopwords("de")) %>%
#subset(!word %in% c("''", ",", "``"))
#subset(pos %in% c("NN", "ADJA")) %>%
subset(pos %in% c("ADJA")) %>%
as.data.table() %>%DT::datatable()

# This KWIC shows that there's something meaningful (in terms of a pattern of argumentation) going on with "substanzverzehrend" and vermögensteure among conservatives in 1996, specifically
kwic("GERMAPARL", query = '("[Vv]ermögensteuer.*" []{0,30} "substanz.*" | "substanz.*" []{0,30} "[Vv]ermögensteuer.*")', cqp = TRUE, left = 25, right = 25, s_attributes = c("date", "party"))

kwic(CDU_CSU_MAIN, query = "wachstumsschädlich", left = 25, right = 25, s_attributes = c("date"))
kwic(CDU_CSU_MAIN, query = "'wachstumsschädlich.*'", cqp = TRUE, left = 25, right = 25, s_attributes = c("date"))

kwic("GERMAPARL", query = "Vermögensteuer", positivelist = "'wachstumsschädlich.*'", regex = T, cqp = T)
Q <- '("[Vv]ermögensteuer.*" []{30,30} "wachstumsschädlich.*" | "wachstumsschädlich.*" []{30,30} "[Vv]ermögensteuer.*")'
Y <- count("GERMAPARL", query = Q, cqp = TRUE)
Y[, "count"]

kwic(CDU_CSU_MAIN, query = "'substanzverzehrenden.*'", cqp = TRUE, left = 25, right = 25, s_attributes = c("date"))
Q <- '("[Vv]ermögensteuer.*" []{0,30} "substanzverzehrenden.*" | "substanzverzehrenden.*" []{0,30} "[Vv]ermögensteuer.*")'
Y <- count("GERMAPARL", query = Q, cqp = TRUE)
Y[, "count"]

# actually, quite interesting, discourse of "leistungsfrei".. maybe compare that in other contexts?
kwic("GERMAPARL", query = "'leistungsfrei.*'", cqp = TRUE, left = 25, right = 25, s_attributes = c("date", "party"))
kwic("GERMAPARL", query = "'leistungs.*'", cqp = TRUE, left = 25, right = 25, s_attributes = c("date", "party"))
kwic("GERMAPARL", query = "'unverdient.*'", cqp = TRUE, left = 25, right = 25, s_attributes = c("date", "party"))


# Constructing the sub-corpora ----

# Corpus Phase 1:
# Finding the most active speakers and the most active dates by phase 1
CDU_CSU_Phase1_byspeaker <- partition_bundle(CDU_CSU_Phase1_partition, s_attribute = "speaker")
SprecherCDU_CSU_Phase1 <- count(CDU_CSU_Phase1_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_Phase1_bydate <- partition_bundle(CDU_CSU_Phase1_partition, s_attribute = "date")
DatumCDU_CSU_Phase1 <- count(CDU_CSU_Phase1_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase1_byspeaker <- partition_bundle(SPD_Phase1_partition, s_attribute = "speaker")
SprecherSPD_Phase1 <- count(SPD_Phase1_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase1_bydate <- partition_bundle(SPD_Phase1_partition, s_attribute = "date")
DatumSPD_Phase1 <- count(SPD_Phase1_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_Phase1$TOTAL <- NULL
SprecherSPD_Phase1$TOTAL <- NULL
DatumCDU_CSU_Phase1$TOTAL <- NULL
DatumSPD_Phase1$TOTAL <- NULL
DT::datatable(SprecherCDU_CSU_Phase1)
DT::datatable(SprecherSPD_Phase1)
DT::datatable(DatumCDU_CSU_Phase1)
DT::datatable(DatumSPD_Phase1)

# using this information to construct a corpus object for Phase 1:
d_1CDUCSU_p <- partition(CDU_CSU_Phase1_partition, date = c("1996-11-07","1996-06-14", "1996-12-12", "1996-10-17", "1996-09-11", "1999-11-11", "1996-04-26", "1996-10-10", "1997-02-20", "1996-02-29", "1996-11-27", "1996-11-29", "1996-05-23", "1996-09-10", "1996-11-08", "2000-05-10", "1996-10-30", "1998-05-28", "1999-10-28", "1996-06-28"))
#size(d_1CDUCSU_p)
s_d_1CDUCSU_p <- partition(d_1CDUCSU_p, speaker = c("Hans-Peter Repnik","Gerda Hasselfeldt", "Hans Michelbach", "Heiner Geißler", "Michael Glos", "Hansgeorg Hauser", "Gunnar Uldall", "Hansjürgen Doss", "Peter Rauen", "Elke Wülfing", "Friedrich Merz", "Wolfgang Steiger", "Dietrich Austermann", "Ernst Hinsken", "Gerhard Schulz", "Leo Dautzenberg", "Wolfgang Schäuble"))
#size(s_d_1CDUCSU_p)
size(s_d_1CDUCSU_p)
d_1SPD_p <- partition(SPD_Phase1_partition, date = c("1996-11-07","1996-12-12", "1996-02-29", "1996-09-10", "1996-10-17", "1996-11-26", "1996-06-14", "1998-03-27", "1997-04-25", "1996-05-22", "1996-05-23", "1996-09-12", "1998-04-02", "1996-09-11", "1997-03-21", "2000-05-10", "1999-10-28", "1996-08-29", "1996-10-10", "1996-11-27", "1997-11-27"))
#size(d_1SPD_full_p)
s_d_1SPD_p <- partition(d_1SPD_p, speaker = c("Ingrid Matthäus-Maier","Joachim Poß", "Barbara Hendricks", "Ottmar Schreiner", "Peter Struck", "Rudolf Scharping", "Jörg-Otto Spiller", "Siegmar Mosdorf", "Detlev von Larcher", "Karl Diller", "Anke Fuchs", "Erika Lotz", "Klaus Hagemann", "Rudolf Dreßler", "Lothar Binding", "Ludwig Eich", "Wolfgang Ilte"))
size(s_d_1SPD_p)

# Corpus Phase 2:
CDU_CSU_Phase2_byspeaker <- partition_bundle(CDU_CSU_Phase2_partition, s_attribute = "speaker")
SprecherCDU_CSU_Phase2 <- count(CDU_CSU_Phase2_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_Phase2_bydate <- partition_bundle(CDU_CSU_Phase2_partition, s_attribute = "date")
DatumCDU_CSU_Phase2 <- count(CDU_CSU_Phase2_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase2_byspeaker <- partition_bundle(SPD_Phase2_partition, s_attribute = "speaker")
SprecherSPD_Phase2 <- count(SPD_Phase2_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase2_bydate <- partition_bundle(SPD_Phase2_partition, s_attribute = "date")
DatumSPD_Phase2 <- count(SPD_Phase2_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_Phase2$TOTAL <- NULL
SprecherSPD_Phase2$TOTAL <- NULL
DatumCDU_CSU_Phase2$TOTAL <- NULL
DatumSPD_Phase2$TOTAL <- NULL
DT::datatable(SprecherCDU_CSU_Phase2)
DT::datatable(SprecherSPD_Phase2)
DT::datatable(DatumCDU_CSU_Phase2)
DT::datatable(DatumSPD_Phase2)

d_2CDUCSU_p <- partition(CDU_CSU_Phase2_partition, date = c("2002-12-19","2002-12-18", "2003-04-03", "2002-12-03", "2003-01-31", "2002-12-04", "2003-11-27", "2004-02-12")) # 8 dates with at least 4 mentions
s_d_2CDUCSU_p <- partition(d_2CDUCSU_p, speaker = c("Michael Meister","Stefan Müller", "Friedrich Merz", "Hans Michelbach", "Angela Merkel", "Heinz Seiffert", "Jochen-Konrad Fromme", "Klaus Lippold", "Norbert Schindler", "Elke Wülfing", "Maria Böhmer", "Michael Fuchs", "Peter Rzepka")) # 13 names
size(s_d_2CDUCSU_p)
d_2SPD_p <- partition(SPD_Phase2_partition, date = c("2003-04-03","2002-12-19", "2007-02-01", "2006-03-16", "2006-09-29")) # only 5 dates here with at least 4 mentions
s_d_2SPD_p <- partition(d_2SPD_p, speaker = c("Bernd Scheelen","Florian Pronold", "Lydia Westrich", "Rainer Wend", "Gabriele Frechen", "Joachim Poß")) # 6 names with at least 3
size(s_d_2SPD_p)

# Corpus Phase 3:
CDU_CSU_Phase3_byspeaker <- partition_bundle(CDU_CSU_Phase3_partition, s_attribute = "speaker")
SprecherCDU_CSU_Phase3 <- count(CDU_CSU_Phase3_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_Phase3_bydate <- partition_bundle(CDU_CSU_Phase3_partition, s_attribute = "date")
DatumCDU_CSU_Phase3 <- count(CDU_CSU_Phase3_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase3_byspeaker <- partition_bundle(SPD_Phase3_partition, s_attribute = "speaker")
SprecherSPD_Phase3 <- count(SPD_Phase3_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_Phase3_bydate <- partition_bundle(SPD_Phase3_partition, s_attribute = "date")
DatumSPD_Phase3 <- count(SPD_Phase3_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_Phase3$TOTAL <- NULL
SprecherSPD_Phase3$TOTAL <- NULL
DatumCDU_CSU_Phase3$TOTAL <- NULL
DatumSPD_Phase3$TOTAL <- NULL
DT::datatable(DatumCDU_CSU_Phase3)

d_3CDUCSU_p <- partition(CDU_CSU_Phase3_partition, date = c("2012-11-28","2012-03-23", "2010-01-29", "2013-01-17", "2012-09-27", "2011-11-11", "2011-03-25", "2013-04-19", "2013-04-25", "2013-06-27", "2012-10-18","2013-03-21", "2013-06-06", "2010-10-07", "2013-04-26")) # 15 dates with at least 4 mentions
s_d_3CDUCSU_p <- partition(d_3CDUCSU_p, speaker = c("Christian von Stetten","Mathias Middelberg", "Olav Gutting", "Norbert Schindler", "Hans Michelbach", "Volker Kauder", "Gerda Hasselfeldt", "Klaus-Peter Flosbach", "Norbert Barthle", "Peter Aumer", "Dieter Jasper", "Georg Nüßlein", "Frank Steffel", "Gero Storjohann", "Michael Fuchs", "Ralph Brinkhaus")) # 16 names
size(s_d_3CDUCSU_p)
d_3SPD_p <- partition(SPD_Phase3_partition, date = c("2011-03-25","2012-03-23", "2012-11-28", "2012-09-27", "2013-06-27", "2008-11-27", "2010-01-29")) # 7 dates here with at least 4 mentions
s_d_3SPD_p <- partition(d_3SPD_p, speaker = c("Manfred Zöllmer","Carsten Sieling", "Joachim Poß", "Lothar Binding", "Florian Pronold", "Bernd Scheelen", "Sigmar Gabriel", "Nicolette Kressl", "Ernst Dieter Rossmann", "Gabriele Hiller-Ohm")) # 10 names with at least 3
size(s_d_3SPD_p)

s_d_1CDUCSU_p <- enrich(s_d_1CDUCSU_p, p_attribute = c("word", "pos"))
s_d_1SPD_p <- enrich(s_d_1SPD_p, p_attribute = c("word", "pos"))
s_d_2CDUCSU_p <- enrich(s_d_2CDUCSU_p, p_attribute = c("word", "pos"))
s_d_2SPD_p <- enrich(s_d_2SPD_p, p_attribute = c("word", "pos"))
s_d_3CDUCSU_p <- enrich(s_d_3CDUCSU_p, p_attribute = c("word", "pos"))
s_d_3SPD_p <- enrich(s_d_3SPD_p, p_attribute = c("word", "pos"))


# Features Analysis ----

# Phase 1---- 
polmineR::features(s_d_1CDUCSU_p, s_d_1SPD_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
as.data.table() %>%DT::datatable()

CDUCSU1_freq <- polmineR::features (s_d_1CDUCSU_p, s_d_1SPD_p) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  #subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  #subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  # subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()

kwic(CDU_CSU_Phase1, query = "'Lafontaine.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase1, query = "'Arbeitsplätze.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase1, query = "'Veränderungen.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase1, query = "'Weg.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase1, query = "'Mittelstandsoffensive.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase1, query = "'investiert.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

FraktionenParlPhase1 <- partition(FraktionenParl, year = 1996:2001)
phase1_full <- partition(FraktionenParlPhase1, interjection = FALSE)
phase1_full <- enrich(phase1_full, p_attribute = c("word", "pos"))
CDU_CSU1_compared_to_full <- polmineR::features(s_d_1CDUCSU_p, phase1_full) %>%
subset(count_coi >= 5) %>%
subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
dotplot()

polmineR::features(s_d_1SPD_p, s_d_1CDUCSU_p) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  # subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  as.data.table() %>%DT::datatable()

SPD1_freq <- polmineR::features (s_d_1SPD_p, s_d_1CDUCSU_p) %>%
 subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
 subset(!word %in% junk) %>%
#subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
#subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
dotplot()


kwic(SPD_Phase1, query = "'Bundesregierung.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase1, query = "'F.D.P.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase1, query = "'Familien*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase1, query = "'Ostdeutschland.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase1, query = "'Arbeitnehmer.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase1, query = "'Arbeitslosigkeit.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

polmineR::features(s_d_1SPD_p, phase1_full) %>%
  subset(count_coi >= 5) %>%
  subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("ADJA")) %>%
  as.data.table() %>%DT::datatable()

# Phase 2 -----


polmineR::features(s_d_2CDUCSU_p, s_d_2SPD_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
as.data.table() %>%DT::datatable()

kwic(CDU_CSU_Phase2, query = "'Deutschland.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase2, query = "'Bundeskanzler.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

CDU_CSU2_freq <- polmineR::features (s_d_2CDUCSU_p, s_d_2SPD_p) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  #subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()

FraktionenParlPhase2 <- partition(FraktionenParl, year = 2002:2007)
phase2_full <- partition(FraktionenParlPhase2, interjection = FALSE)
phase2_full <- enrich(phase2_full, p_attribute = c("word", "pos"))
polmineR::features(s_d_2CDUCSU_p, phase2_full) %>%
subset(count_coi >= 5) %>%
subset(chisquare >= 10.83) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("ADJA")) %>%
as.data.table() %>%DT::datatable()

polmineR::features (x = s_d_2SPD_p, y = s_d_2CDUCSU_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
as.data.table() %>%DT::datatable()

kwic(SPD_Phase2, query = "'Gesetzesentwurf.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase2, query = "'Einkünfte.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase2, query = "'Gewerbesteuer.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase2, query = "'Vermögen.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase2, query = "'Lafontaine.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase2, query = "'Koalition.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

SPD2_freq <- polmineR::features (x = s_d_2SPD_p, y = s_d_2CDUCSU_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
#subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
dotplot()

polmineR::features(s_d_2SPD_p, phase2_full) %>%
  subset(count_coi >= 5) %>%
  subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("ADJA")) %>%
  as.data.table() %>%DT::datatable()

# Phase 3 ----

polmineR::features(s_d_3CDUCSU_p, s_d_3SPD_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
subset(!pos %in% "CARD") %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
as.data.table() %>%DT::datatable()

kwic(CDU_CSU_Phase3, query = "'Europa.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase3, query = "'Banken.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase3, query = "'Weg.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(CDU_CSU_Phase3, query = "'Opposition.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

CDU_CSU3_freq <- polmineR::features (s_d_3CDUCSU_p, s_d_3SPD_p) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  subset(!pos %in% "CARD") %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  #subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()

FraktionenParlPhase3 <- partition(FraktionenParl, year = 2008:2016)
phase3_full <- partition(FraktionenParlPhase3, interjection = FALSE)
phase3_full <- enrich(phase3_full, p_attribute = c("word", "pos"))
polmineR::features(s_d_3CDUCSU_p, phase3_full) %>%
  subset(count_coi >= 5) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

polmineR::features(s_d_3SPD_p, s_d_3CDUCSU_p) %>%
subset(count_coi >= 5) %>%
# subset(chisquare >= 10.83) %>%
subset(!word %in% junk) %>%
# subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
# subset(pos %in% c("NN", "ADJA")) %>%
# subset(pos %in% c("NN")) %>%
# subset(pos %in% c("ADJA")) %>%
# subset(pos %in% c("VVFIN")) %>%
as.data.table() %>%DT::datatable()

kwic(SPD_Phase3, query = "'Sozialdemokraten*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'Gerechtigkeit.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'FDP.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'Schwarz-Gelb.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'Michelbach.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'Gesellschaft.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")
kwic(SPD_Phase3, query = "'Wirklichkeit.*'", cqp = TRUE, left = 100, right = 100, s_attributes = c("date", "speaker"), positivelist = "Vermögensteuer")

SPD3_freq <- polmineR::features (s_d_3SPD_p, s_d_3CDUCSU_p) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  #subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()

polmineR::features(s_d_3SPD_p, phase3_full) %>%
subset(count_coi >= 5) %>%
subset(chisquare >= 10.83) %>%
as.data.table() %>%DT::datatable()


# testing the data. Creating corpora by legislative period ----

# Testing: creating partitions by legislative period
s_attributes("GERMAPARL", s_attribute = "lp")
# We have six legislative periods in the corpus, so we'll create a corpus for each of them, and of for the fractions

# LP1 (13) -> 1994 to 1998 # truncated: only two years in this data
# LP2 (14) -> 1998 to 2002
# LP3 (15) -> 2002 to 2005
# LP4 (16) -> 2005 to 2009
# LP5 (17) -> 2009 to 2013
# LP6 (18) -> 2013 to 2017 # truncated: only three years in this data

CDU_CSU_lp1_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 13, interjection = FALSE)
CDU_CSU_lp2_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 14, interjection = FALSE)
CDU_CSU_lp3_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 15, interjection = FALSE)
CDU_CSU_lp4_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 16, interjection = FALSE)
CDU_CSU_lp5_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 17, interjection = FALSE)
CDU_CSU_lp6_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "CDU/CSU", lp = 18, interjection = FALSE)
SPD_lp1_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 13, interjection = FALSE)
SPD_lp2_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 14, interjection = FALSE)
SPD_lp3_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 15, interjection = FALSE)
SPD_lp4_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 16, interjection = FALSE)
SPD_lp5_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 17, interjection = FALSE)
SPD_lp6_partition <- "GERMAPARL" %>%
  partition(parliamentary_group = "SPD", lp = 18, interjection = FALSE)

# LP1
CDU_CSU_lp1_byspeaker <- partition_bundle(CDU_CSU_lp1_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp1 <- count(CDU_CSU_lp1_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp1_bydate <- partition_bundle(CDU_CSU_lp1_partition, s_attribute = "date")
DatumCDU_CSU_lp1 <- count(CDU_CSU_lp1_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp1_byspeaker <- partition_bundle(SPD_lp1_partition, s_attribute = "speaker")
SprecherSPD_lp1 <- count(SPD_lp1_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp1_bydate <- partition_bundle(SPD_lp1_partition, s_attribute = "date")
DatumSPD_lp1 <- count(SPD_lp1_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp1$TOTAL <- NULL
SprecherSPD_lp1$TOTAL <- NULL
DatumCDU_CSU_lp1$TOTAL <- NULL
DatumSPD_lp1$TOTAL <- NULL
DT::datatable(SprecherCDU_CSU_lp1)
DT::datatable(SprecherSPD_lp1)
DT::datatable(DatumCDU_CSU_lp1)
DT::datatable(DatumSPD_lp1)

DatumCDU_CSU_lp1_a <- DatumCDU_CSU_lp1[order(-Vermögensteuer)] 
DatumCDU_CSU_lp1_a_b <- DatumCDU_CSU_lp1_a[Vermögensteuer >= 4] # 18 top dates in this early LP
SprecherCDU_CSU_lp1_a <- SprecherCDU_CSU_lp1[order(-Vermögensteuer)]
SprecherCDU_CSU_lp1_a_b <- SprecherCDU_CSU_lp1[Vermögensteuer >= 3] # 14 speakers in this early LP

d_1CDUCSU_lp1 <- partition(CDU_CSU_lp1_partition, date = DatumCDU_CSU_lp1_a_b$partition)
size(d_1CDUCSU_lp1)
s_d_1CDUCSU_lp1 <- partition(d_1CDUCSU_lp1, speaker = SprecherCDU_CSU_lp1_a_b$partition)
size(s_d_1CDUCSU_lp1)

DatumSPD_lp1_a <- DatumSPD_lp1[order(-Vermögensteuer)] 
DatumSPD_lp1_a_b <- DatumSPD_lp1_a[Vermögensteuer >= 4] # 19 top dates in this early LP
SprecherSPD_lp1_a <- SprecherSPD_lp1[order(-Vermögensteuer)]
SprecherSPD_lp1_a_b <- SprecherSPD_lp1[Vermögensteuer >= 3] # 16 speakers in this early LP

d_1SPD_lp1 <- partition(SPD_lp1_partition, date = DatumSPD_lp1_a_b$partition)
size(d_1SPD_lp1)
s_d_1SPD_lp1 <- partition(d_1SPD_lp1, speaker = SprecherSPD_lp1_a_b$partition)
size(s_d_1SPD_lp1)

# LP2
CDU_CSU_lp2_byspeaker <- partition_bundle(CDU_CSU_lp2_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp2 <- count(CDU_CSU_lp2_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp2_bydate <- partition_bundle(CDU_CSU_lp2_partition, s_attribute = "date")
DatumCDU_CSU_lp2 <- count(CDU_CSU_lp2_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp2_byspeaker <- partition_bundle(SPD_lp2_partition, s_attribute = "speaker")
SprecherSPD_lp2 <- count(SPD_lp2_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp2_bydate <- partition_bundle(SPD_lp2_partition, s_attribute = "date")
DatumSPD_lp2 <- count(SPD_lp2_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp2$TOTAL <- NULL
SprecherSPD_lp2$TOTAL <- NULL
DatumCDU_CSU_lp2$TOTAL <- NULL
DatumSPD_lp2$TOTAL <- NULL

DatumCDU_CSU_lp2_a <- DatumCDU_CSU_lp2[order(-Vermögensteuer)] 
DatumCDU_CSU_lp2_a_b <- DatumCDU_CSU_lp2_a[Vermögensteuer >= 4] # 3 top dates 
SprecherCDU_CSU_lp2_a <- SprecherCDU_CSU_lp2[order(-Vermögensteuer)]
SprecherCDU_CSU_lp2_a_b <- SprecherCDU_CSU_lp2[Vermögensteuer >= 3] # 3 speakers

d_2CDUCSU_lp2 <- partition(CDU_CSU_lp2_partition, date = DatumCDU_CSU_lp2_a_b$partition)
size(d_2CDUCSU_lp2)
s_d_2CDUCSU_lp2 <- partition(d_2CDUCSU_lp2, speaker = SprecherCDU_CSU_lp2_a_b$partition)
size(s_d_2CDUCSU_lp2) # very small in this second LP

DatumSPD_lp2_a <- DatumSPD_lp2[order(-Vermögensteuer)] 
DatumSPD_lp2_a_b <- DatumSPD_lp2_a[Vermögensteuer >= 4] # 1 top date 
SprecherSPD_lp2_a <- SprecherSPD_lp2[order(-Vermögensteuer)]
SprecherSPD_lp2_a_b <- SprecherSPD_lp2[Vermögensteuer >= 3] # 2 speakers

d_2SPD_lp2 <- partition(SPD_lp2_partition, date = DatumSPD_lp2_a_b$partition)
size(d_2SPD_lp2)
s_d_2SPD_lp2 <- partition(d_2SPD_lp2, speaker = SprecherSPD_lp2_a_b$partition)
size(s_d_2SPD_lp2) # tiny corpus in this second LP

# LP3
CDU_CSU_lp3_byspeaker <- partition_bundle(CDU_CSU_lp3_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp3 <- count(CDU_CSU_lp3_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp3_bydate <- partition_bundle(CDU_CSU_lp3_partition, s_attribute = "date")
DatumCDU_CSU_lp3 <- count(CDU_CSU_lp3_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp3_byspeaker <- partition_bundle(SPD_lp3_partition, s_attribute = "speaker")
SprecherSPD_lp3 <- count(SPD_lp3_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp3_bydate <- partition_bundle(SPD_lp3_partition, s_attribute = "date")
DatumSPD_lp3 <- count(SPD_lp3_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp3$TOTAL <- NULL
SprecherSPD_lp3$TOTAL <- NULL
DatumCDU_CSU_lp3$TOTAL <- NULL
DatumSPD_lp3$TOTAL <- NULL

DatumCDU_CSU_lp3_a <- DatumCDU_CSU_lp3[order(-Vermögensteuer)] 
DatumCDU_CSU_lp3_a_b <- DatumCDU_CSU_lp3_a[Vermögensteuer >= 4] # 8 top dates 
SprecherCDU_CSU_lp3_a <- SprecherCDU_CSU_lp3[order(-Vermögensteuer)]
SprecherCDU_CSU_lp3_a_b <- SprecherCDU_CSU_lp3[Vermögensteuer >= 3] # 12 top speakers

d_3CDUCSU_lp3 <- partition(CDU_CSU_lp3_partition, date = DatumCDU_CSU_lp3_a_b$partition)
size(d_3CDUCSU_lp3)
s_d_3CDUCSU_lp3 <- partition(d_3CDUCSU_lp3, speaker = SprecherCDU_CSU_lp3_a_b$partition)
size(s_d_3CDUCSU_lp3)

DatumSPD_lp3_a <- DatumSPD_lp3[order(-Vermögensteuer)] 
DatumSPD_lp3_a_b <- DatumSPD_lp3_a[Vermögensteuer >= 4] #  only 2 top dates
SprecherSPD_lp3_a <- SprecherSPD_lp3[order(-Vermögensteuer)]
SprecherSPD_lp3_a_b <- SprecherSPD_lp3[Vermögensteuer >= 3] # only 2 top speakers

d_3SPD_lp3 <- partition(SPD_lp3_partition, date = DatumSPD_lp3_a_b$partition)
size(d_3SPD_lp3)
s_d_3SPD_lp3 <- partition(d_3SPD_lp3, speaker = SprecherSPD_lp3_a_b$partition)
size(s_d_3SPD_lp3) # interesting how the SPD corpus is dramatically smaller than CDU/CSU in this third LP

# LP4
CDU_CSU_lp4_byspeaker <- partition_bundle(CDU_CSU_lp4_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp4 <- count(CDU_CSU_lp4_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp4_bydate <- partition_bundle(CDU_CSU_lp4_partition, s_attribute = "date")
DatumCDU_CSU_lp4 <- count(CDU_CSU_lp4_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp4_byspeaker <- partition_bundle(SPD_lp4_partition, s_attribute = "speaker")
SprecherSPD_lp4 <- count(SPD_lp4_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp4_bydate <- partition_bundle(SPD_lp4_partition, s_attribute = "date")
DatumSPD_lp4 <- count(SPD_lp4_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp4$TOTAL <- NULL
SprecherSPD_lp4$TOTAL <- NULL
DatumCDU_CSU_lp4$TOTAL <- NULL
DatumSPD_lp4$TOTAL <- NULL

DatumCDU_CSU_lp4_a <- DatumCDU_CSU_lp4[order(-Vermögensteuer)] 
DatumCDU_CSU_lp4_a_b <- DatumCDU_CSU_lp4_a[Vermögensteuer >= 4] # 0 dates 
SprecherCDU_CSU_lp4_a <- SprecherCDU_CSU_lp4[order(-Vermögensteuer)]
SprecherCDU_CSU_lp4_a_b <- SprecherCDU_CSU_lp4[Vermögensteuer >= 3] # 0 top speakers

d_4CDUCSU_lp4 <- partition(CDU_CSU_lp4_partition, date = DatumCDU_CSU_lp4_a_b$partition)
size(d_4CDUCSU_lp4) # 0
s_d_4CDUCSU_lp4 <- partition(d_4CDUCSU_lp4, speaker = SprecherCDU_CSU_lp4_a_b$partition)
size(s_d_4CDUCSU_lp4) # 0

DatumSPD_lp4_a <- DatumSPD_lp4[order(-Vermögensteuer)] 
DatumSPD_lp4_a_b <- DatumSPD_lp4_a[Vermögensteuer >= 4] #  4 top dates
SprecherSPD_lp4_a <- SprecherSPD_lp4[order(-Vermögensteuer)]
SprecherSPD_lp4_a_b <- SprecherSPD_lp4[Vermögensteuer >= 3] # 6 top speakers

d_4SPD_lp4 <- partition(SPD_lp4_partition, date = DatumSPD_lp4_a_b$partition)
size(d_4SPD_lp4)
s_d_4SPD_lp4 <- partition(d_4SPD_lp4, speaker = SprecherSPD_lp4_a_b$partition)
size(s_d_4SPD_lp4) # LP 4, SPD is returning - in fact, CDU-CSU is zero in this LP

# LP5
CDU_CSU_lp5_byspeaker <- partition_bundle(CDU_CSU_lp5_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp5 <- count(CDU_CSU_lp5_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp5_bydate <- partition_bundle(CDU_CSU_lp5_partition, s_attribute = "date")
DatumCDU_CSU_lp5 <- count(CDU_CSU_lp5_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp5_byspeaker <- partition_bundle(SPD_lp5_partition, s_attribute = "speaker")
SprecherSPD_lp5 <- count(SPD_lp5_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp5_bydate <- partition_bundle(SPD_lp5_partition, s_attribute = "date")
DatumSPD_lp5 <- count(SPD_lp5_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp5$TOTAL <- NULL
SprecherSPD_lp5$TOTAL <- NULL
DatumCDU_CSU_lp5$TOTAL <- NULL
DatumSPD_lp5$TOTAL <- NULL

DatumCDU_CSU_lp5_a <- DatumCDU_CSU_lp5[order(-Vermögensteuer)] 
DatumCDU_CSU_lp5_a_b <- DatumCDU_CSU_lp5_a[Vermögensteuer >= 4] # 15 top dates - LP is the return of the topic
SprecherCDU_CSU_lp5_a <- SprecherCDU_CSU_lp5[order(-Vermögensteuer)]
SprecherCDU_CSU_lp5_a_b <- SprecherCDU_CSU_lp5[Vermögensteuer >= 3] # 14 top speakers

d_5CDUCSU_lp5 <- partition(CDU_CSU_lp5_partition, date = DatumCDU_CSU_lp5_a_b$partition)
size(d_5CDUCSU_lp5)
s_d_5CDUCSU_lp5 <- partition(d_5CDUCSU_lp5, speaker = SprecherCDU_CSU_lp5_a_b$partition)
size(s_d_5CDUCSU_lp5) # back to a respectable size

DatumSPD_lp5_a <- DatumSPD_lp5[order(-Vermögensteuer)] 
DatumSPD_lp5_a_b <- DatumSPD_lp5_a[Vermögensteuer >= 4] #  6 top dates
SprecherSPD_lp5_a <- SprecherSPD_lp5[order(-Vermögensteuer)]
SprecherSPD_lp5_a_b <- SprecherSPD_lp5[Vermögensteuer >= 3] # 9 top speakers

d_5SPD_lp5 <- partition(SPD_lp5_partition, date = DatumSPD_lp5_a_b$partition)
size(d_5SPD_lp5)
s_d_5SPD_lp5 <- partition(d_5SPD_lp5, speaker = SprecherSPD_lp5_a_b$partition)
size(s_d_5SPD_lp5) # LP 5, here again, SPD isn't as prominent (at least not as dominant with a handfull of speakers)

# LP6
CDU_CSU_lp6_byspeaker <- partition_bundle(CDU_CSU_lp6_partition, s_attribute = "speaker")
SprecherCDU_CSU_lp6 <- count(CDU_CSU_lp6_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
CDU_CSU_lp6_bydate <- partition_bundle(CDU_CSU_lp6_partition, s_attribute = "date")
DatumCDU_CSU_lp6 <- count(CDU_CSU_lp6_bydate, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp6_byspeaker <- partition_bundle(SPD_lp6_partition, s_attribute = "speaker")
SprecherSPD_lp6 <- count(SPD_lp6_byspeaker, query = "Vermögensteuer", breakdown = TRUE)
SPD_lp6_bydate <- partition_bundle(SPD_lp6_partition, s_attribute = "date")
DatumSPD_lp6 <- count(SPD_lp6_bydate, query = "Vermögensteuer", breakdown = TRUE)
SprecherCDU_CSU_lp6$TOTAL <- NULL
SprecherSPD_lp6$TOTAL <- NULL
DatumCDU_CSU_lp6$TOTAL <- NULL
DatumSPD_lp6$TOTAL <- NULL

DatumCDU_CSU_lp6_a <- DatumCDU_CSU_lp6[order(-Vermögensteuer)] 
DatumCDU_CSU_lp6_a_b <- DatumCDU_CSU_lp6_a[Vermögensteuer >= 4] # 0 dates
SprecherCDU_CSU_lp6_a <- SprecherCDU_CSU_lp6[order(-Vermögensteuer)]
SprecherCDU_CSU_lp6_a_b <- SprecherCDU_CSU_lp6[Vermögensteuer >= 3] # 0 speakers

d_6CDUCSU_lp6 <- partition(CDU_CSU_lp6_partition, date = DatumCDU_CSU_lp6_a_b$partition)
size(d_6CDUCSU_lp6)
s_d_6CDUCSU_lp6 <- partition(d_6CDUCSU_lp6, speaker = SprecherCDU_CSU_lp6_a_b$partition)
size(s_d_6CDUCSU_lp6) # 0 for CDU_CSU in LP 6

DatumSPD_lp6_a <- DatumSPD_lp6[order(-Vermögensteuer)] 
DatumSPD_lp6_a_b <- DatumSPD_lp6_a[Vermögensteuer >= 4] #  0 dates
SprecherSPD_lp6_a <- SprecherSPD_lp6[order(-Vermögensteuer)]
SprecherSPD_lp6_a_b <- SprecherSPD_lp6[Vermögensteuer >= 3] # 1 top speaker

d_6SPD_lp6 <- partition(SPD_lp6_partition, date = DatumSPD_lp6_a_b$partition)
size(d_6SPD_lp6)
s_d_6SPD_lp6 <- partition(d_6SPD_lp6, speaker = SprecherSPD_lp6_a_b$partition)
size(s_d_6SPD_lp6) # LP 6 - one speaker here who doesnt appear because there are no dates for him


#actually, the structure by LP allows me to say how the use of this word is DISTRIBUTED among participants! Like, doe MORE people among SPD talk about than from CDU/CSU?

# CDU LP1: 263 speakers, 14 meet our criteria - 5%
# CDU LP2: 248:3 - 1%
# CDU LP3: 251:12 - 5%
# CDU LP4: 224:0 - 0%
# CDU LP5: 231:14 - 6%
# CDU LP6: 302:0 - 0%

# SPD LP1: 245:16 - 7%
# SPD LP2: 298:2 - 1%
# SPD LP3: 239:2 - 1%
# SPD LP4: 214:6 - 3%
# SPD LP5: 153:9 - 6%
# SPD LP6: 198:1 - 0%


s_d_1CDUCSU_lp1 <- enrich(s_d_1CDUCSU_lp1, p_attribute = c("word", "pos"))
s_d_1SPD_lp1 <- enrich(s_d_1SPD_lp1, p_attribute = c("word", "pos"))
s_d_2CDUCSU_lp2 <- enrich(s_d_2CDUCSU_lp2, p_attribute = c("word", "pos"))
s_d_2SPD_lp2 <- enrich(s_d_2SPD_lp2, p_attribute = c("word", "pos"))
s_d_3CDUCSU_lp3 <- enrich(s_d_3CDUCSU_lp3, p_attribute = c("word", "pos"))
s_d_3SPD_lp3 <- enrich(s_d_3SPD_lp3, p_attribute = c("word", "pos"))
s_d_4CDUCSU_lp4 <- enrich(s_d_4CDUCSU_lp4, p_attribute = c("word", "pos")) # 0
s_d_4SPD_lp4 <- enrich(s_d_4SPD_lp4, p_attribute = c("word", "pos"))
s_d_5CDUCSU_lp5 <- enrich(s_d_5CDUCSU_lp5, p_attribute = c("word", "pos"))
s_d_5SPD_lp5 <- enrich(s_d_5SPD_lp5, p_attribute = c("word", "pos"))
s_d_6CDUCSU_lp6 <- enrich(s_d_6CDUCSU_lp6, p_attribute = c("word", "pos")) # 0
s_d_6SPD_lp6 <- enrich(s_d_6SPD_lp6, p_attribute = c("word", "pos")) # 0

lp1_full <- partition("GERMAPARL", lp  = 13, interjection = FALSE)
lp1_full <- enrich(lp1_full, p_attribute = c("word", "pos"))
lp2_full <- partition("GERMAPARL", lp  = 14, interjection = FALSE)
lp2_full <- enrich(lp2_full, p_attribute = c("word", "pos"))
lp3_full <- partition("GERMAPARL", lp  = 15, interjection = FALSE)
lp3_full <- enrich(lp3_full, p_attribute = c("word", "pos"))
lp4_full <- partition("GERMAPARL", lp  = 16, interjection = FALSE)
lp4_full <- enrich(lp4_full, p_attribute = c("word", "pos"))
lp5_full <- partition("GERMAPARL", lp  = 17, interjection = FALSE)
lp5_full <- enrich(lp5_full, p_attribute = c("word", "pos"))
lp6_full <- partition("GERMAPARL", lp  = 18, interjection = FALSE)
lp6_full <- enrich(lp6_full, p_attribute = c("word", "pos"))

# LP1 (13) -> 1994 to 1998 # This is essentially Phase 1
# LP2 (14) -> 1998 to 2002 # almost no activity in this phase
# LP3 (15) -> 2002 to 2005 # in this phase, CDU/CSU is active, SPD is not - but, surprisingly, there is no distinct vocab for CDU/CSU here
# LP4 (16) -> 2005 to 2009 # zero activity in this phase
# LP5 (17) -> 2009 to 2013 # This is essentially the same as Phase 3 - EUROPA for CDU-CSU is even more central
# LP6 (18) -> 2013 to 2017 # truncated: only three years in this data

polmineR::features(s_d_1CDUCSU_lp1, s_d_1SPD_lp1) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

# The CDU/CSU result for LP1 looks very similar to the one from Phase 1

CDU_CSU_freq <- polmineR::features(s_d_1CDUCSU_lp1, s_d_1SPD_lp1) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  #subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()

# very similar results as for Phase 1 - check differences (if any) in detail

polmineR::features(s_d_1CDUCSU_lp1, lp1_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

polmineR::features(s_d_1SPD_lp1, s_d_1CDUCSU_lp1) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

# SPD words for LP 1 again look very similar - this is essentially Phase 1 right here

SPD_freq <- polmineR::features(s_d_1SPD_lp1, s_d_1CDUCSU_lp1) %>%
  subset(count_coi >= 5) %>%
  # subset(chisquare >= 10.83) %>%
  subset(!word %in% junk) %>%
  # subset(pos %in% c("NN", "ADJA", "VVFIN")) %>%
  # subset(pos %in% c("NN", "ADJA")) %>%
  # subset(pos %in% c("NN")) %>%
  #subset(pos %in% c("ADJA")) %>%
  # subset(pos %in% c("VVFIN")) %>%
  dotplot()
polmineR::features(s_d_1SPD_lp1, lp1_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

# LP 2 has no distinct vocab for CDU-CDU compared to SPD at all - but slightly so in comparison to the full corp
polmineR::features(s_d_2CDUCSU_lp2, s_d_2SPD_lp2) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_2CDUCSU_lp2, lp2_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
# LP 2 has just a bit of distinct vocab for SPD
polmineR::features(s_d_2SPD_lp2, s_d_2CDUCSU_lp2) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_2SPD_lp2, lp2_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

# LP 3 no specific vocab for CDU_CSU - this is surprising - because they talk a lot here.
# Likely this is the case because there isn't really any substantive talk
# but instead just mentioning this term along with others
polmineR::features(s_d_3CDUCSU_lp3, s_d_3SPD_lp3) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_3CDUCSU_lp3, lp3_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
# LP 3 has quite a lot of distinct vocab for SPD compared to CDU-CSU, even if much smaller corpus
polmineR::features(s_d_3SPD_lp3, s_d_3CDUCSU_lp3) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_3SPD_lp3, lp3_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

#LP4 - object doesn't exist for CDU-CSU
polmineR::features(s_d_4SPD_lp4, lp4_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

#LP5 # ONLY Europa as distinct vocab here
polmineR::features(s_d_5CDUCSU_lp5, s_d_5SPD_lp5) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_5CDUCSU_lp5, lp5_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
# LP 5 reveals some interesting vocab here, like "Finanztransaktionssteuer"
polmineR::features(s_d_5SPD_lp5, s_d_5CDUCSU_lp5) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()
polmineR::features(s_d_5SPD_lp5, lp5_full) %>%
  subset(count_coi >= 5) %>%
  subset(!word %in% junk) %>%
  subset(chisquare >= 10.83) %>%
  as.data.table() %>%DT::datatable()

# this is much like phase 3 in our earlier analysis

