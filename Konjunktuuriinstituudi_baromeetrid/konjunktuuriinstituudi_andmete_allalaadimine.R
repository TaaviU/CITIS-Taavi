# Koodi sisu: Kood laeb alla Eesti Konjunktuuriinstituudi (KI) lehel olevad andmefailid ja viib need masinloetavale kujule
# Autorid: Andres Võrk ja Taavi Unt
# TÜ CITIS


# Väljundi kirjeldus ------------------------------------------------------

# Koodi väljundiks on andmetabel (data frame), milles on kokku pandud järgnevad KI lehel olevad tabelid
# (http://www.ki.ee/baromeetrid/)

### Tööstusbaromeeter (industry)
### Ehitusbaromeeter (ehitus)
### Kaubandusbaromeeter (kaubandus)
### Teenindusbaromeeter (teenind)
### Tarbijabaromeeter (tarbija)
### Majandususaldusindeks (majandus)

# Väljundandmestikus olevad tunnused:

### KI_andmestik - näitab, millise baromeetriga (KI tabeliga) on tegemist
### indikaator -  indikaatori nimetus vastavas KI tabelis
### kpv - kuupäev; aasta ja kuu formaadile on lisatud juurde päeva tähis (YYYY-mm-01), et tulemusi mugavam käsitleda oleks
### vaatus - indikaatori väärtus vastaval kuul

# NB! Väljundis olevad andmed on alates kuupäevast jaanuar 2003 (kuigi KI tabelites esineb ka varasemaid vaatlusi)!

# Vajalikud paketid -------------------------------------------------------

library(readxl)
library(readr)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(XML)
library(lettercase)


# Funktsiooni defineerimine -----------------------------------------------

download_KI_data <- function() {
  ########################################
  # 1. Konjunktuuriinstituudi lehelt Exceli failid, 
  # NB! Exceli ridade nimed erinevad eri failides
  ########################################
  
  failialgused <- c("industry", "ehitus", "kaubandus", "teenind", "tarbija", "majandus") 
  # "majandus" - selle saab ka eurostatist
  
  # Vaatame, mis xls failid juba kaustas olemas on. Neid, mis olemas, enam uuesti ei tõmba
  failid_kaustas <- list.files(pattern = "xls")
  failid_kaustas <-  failid_kaustas[unlist(lapply(failialgused, function(x) grep(x, failid_kaustas)))]
  failid_kaustas_df = data.frame( failid_kaustas =  failid_kaustas, 
                                  valdkond = substr(failid_kaustas, 1, regexpr("_",failid_kaustas)-1), 
                                  versioon = readr::parse_number( failid_kaustas), stringsAsFactors = FALSE)
  failid_kaustas_df <- failid_kaustas_df %>% 
    group_by(valdkond) %>% 
    top_n(1,  versioon)
  
  #leht kus failidele lingid
  url <- "http://www.ki.ee/baromeetrid/baromeetrid.htm"
  doc <- tryCatch({htmlParse(url)},
                  error = function(e) return(NULL))    
  #otsime lehelt kõik lingid
  if (!is.null(doc)) {
    links <- xpathSApply(doc, "//a/@href")
    free(doc)
    rm(doc)
    # Otsime linkidest kõik xls ja xlsx lõpuga failid
    wanted <- links[grepl("*(.xls |.xlsx)", links)]
    # Jätame alles ainult need, mis meile huvi pakuvad
    wanted <-  wanted[sapply(failialgused, function(x) grep(x, wanted))]
  } else{ 
    wanted <- failid_kaustas_df$failid_kaustas
    if(nrow(failid_kaustas_df) < length(failialgused)){
      stop("KI lehelt ei saa andmeid alla laadida ja töökataloogis pole kõiki vajaminevaid faile.\n Lisa lehelt http://www.ki.ee/baromeetrid/baromeetrid.htm uuendatud failid käsitsi töökataloogi.")
    } else{
      veateade = function() warning(cat("KI lehelt ei saa andmeid alla laadida! Kasutatakse järgmisi andmetabeleid:\n ",failid_kaustas_df$failid_kaustas,
                                        "\n Kui mõni neist on aegunud, lisa lehelt http://www.ki.ee/baromeetrid/baromeetrid.htm uuendatud failid käsitsi töökataloogi."),
                                    immediate. = TRUE, call. = FALSE)
      veateade()}
  }
  
  
  
  files_to_download <- wanted[!(wanted %in% failid_kaustas)]
  GetMe <- ""
  # Laeme alla need failid, mida veel ei ole
  if (length(files_to_download) > 0) {
    GetMe <- paste("http://www.ki.ee/baromeetrid/", files_to_download, sep = "")
    #Laeme alla
    tmp <- lapply(seq_along(GetMe), 
           function(x) download.file(GetMe[x], files_to_download[x], mode = "wb"))
  }
  
  #Loeme sisse soovitud Exceli failid
  nimetused <- data.frame(str_split_fixed(wanted, "_", n = 2), stringsAsFactors = F)
  nimetused$end_date <- as.Date(paste0(readr::parse_number(wanted),"01"), format = "%y%m%d")
  
  for (x in 1:length(wanted)) {
    failinimi <-  wanted[x]
    nimetus <- nimetused[x,1]
    if (!(nimetus %in% failialgused)) {stop("Viga andmete sisselugemisel")}
    temp_df <- read_excel(failinimi)
    assign(nimetus, temp_df)
  } 
  rm(temp_df)
  
  #Ükshaaval failide puhastamine
  #jäta alles vajalikud read ja veerud alates 2003. aastast.
  #NB! seda peab käsitsi kontrollima aeg-ajalt, et Konjunktuuriinstituut ei oleks muutnud veerge ega ridu
  
  #Tööstuse fail
  industry <- industry[c(2:8,10:24), c(1,56:ncol(industry))]
  
  #Ehituse fail
  ehitus <- ehitus[c(2,4:15), c(1,44:ncol(ehitus))]
  
  #Kaubanduse fail
  kaubandus <- kaubandus[c(2:8), c(1,44:ncol(kaubandus))]
  
  #Teenindus
  teenind <- teenind[c(2:7, 9:15), c(1,11:ncol(teenind))]
  
  #Tarbija 
  tarbija <- as.data.frame(tarbija[2:22, c(1,56:ncol(tarbija))])
  colnames(tarbija)[1] <- "indikaator"
  tarbija$indikaator2 <- tarbija$indikaator
  tarbija$indikaator2[!is_upper_case(tarbija$indikaator2)] = NA
  tarbija <- tarbija %>% tidyr::fill(indikaator2, .direction = "down")
  tarbija <- tarbija %>% 
    mutate(indikaator = ifelse(indikaator == indikaator2, 
                                                    indikaator, 
                                                    paste0(indikaator2,": ",indikaator)),
                                indikaator2 = NULL) %>% 
    filter(!(indikaator %in% c("PEREKONNA MAJANDUSLIK OLUKORD","RIIGI MAJANDUSLIK OLUKORD",
                               "PÜSIKAUPADE OSTUD" ,"HINNAD","SÄÄSTUD")))
  
  #Majandus (saab ka eurostatist)
  majandus <- majandus[c(2:7), c(1,48:ncol(majandus))]
        

  # Tsükliga teisendame
  #Veergudele õigete kuude nimede panemine
  start = as.Date("2003-01-01")
  
  koondandmestik = data.frame(stringsAsFactors = F)
  for (x in 1:nrow(nimetused)) {
    data = get(nimetused[x,1])
    end = nimetused$end_date[x]
    #paneme kuud veerunimedeks
    colnames(data) <- c("indikaator", as.character(seq.Date(start,end,"month")[1:(ncol(data) - 1)]))
    #Keerame teistpidi
    data <- gather(data, key = "variable", value = "vaartus", -indikaator)
    #Teeme kuupäeva
    data$kpv <- as.Date(as.character(data$variable))
    options(warn = -1)
    data$vaartus <- as.numeric(data$vaartus)
    options(warn = 0)
    data$variable <- NULL
    #tabeli nimi
    data$sektor <- nimetused[x,1]
    #Puhastame ja täpsutame näitajate nimesid
    data$indikaator <- gsub("[\n]"," ", data$indikaator)
    data$indikaator <- gsub("[\r]"," ", data$indikaator)
    data$indikaator <- stri_trim_both(str_replace_all(data$indikaator, "\\s+", " "))
    data$indikaator <- gsub("[*]","", data$indikaator)
    data$indikaator[is_lowercase(substr(data$indikaator, 1,1))] <- paste0("Piirab praegu: ", data$indikaator[is_lowercase(substr(data$indikaator, 1,1))] )
    data <- data[!is.na(data$vaartus),]
    #Salvestame sama nime alla, mis on Konjunktuuriinstituudi failid
    assign(nimetused[x,1],data)
    
    koondandmestik = rbind(koondandmestik,data)
  } 
  
  #kustutame üleliigsed objektid
  rm(data, wanted, url, links, GetMe, failinimi, failid_kaustas)
  
  koondandmestik <- select(koondandmestik, KI_andmestik = sektor, indikaator, kpv, vaartus)
  return(koondandmestik)
}  


# Funktsiooni rakendamine (andmete allalaadimine) -------------------------

#KI_data <- download_KI_data()
