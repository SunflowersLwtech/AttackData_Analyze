library(readr)
library(dplyr)

raw_data_1 <- read_csv("C:\\Users\\User\\Downloads\\AssignmentDatasets\\HackingData_Part1.csv")
raw_data_2 <- read_csv("C:\\Users\\User\\Downloads\\AssignmentDatasets\\HackingData_Part2.csv")
raw_data_3 <- read_tsv("C:\\Users\\User\\Downloads\\AssignmentDatasets\\HackingData_Part3.txt")


# INSPECT STR
str(raw_data_1)
str(raw_data_2)
str(raw_data_3)

raw_data_1 <- raw_data_1 %>% distinct()
raw_data_1 <- raw_data_1 %>%
  mutate(Country = na_if(Country, "UNKNOWN"))

# _________COUNTRY__________
# Part 1 Data

# INSPECT COUNTRY
table(raw_data_1$Country)
raw_data_1 %>% distinct(Country) %>% print(n=50)
raw_data_1[tolower(raw_data_1$Country) == "united states", ]

# CLEAN COUNTRY
raw_data_1 <- raw_data_1 %>%
  mutate(Country = toupper(Country))

raw_data_1$Country <- sub("\"$", "", raw_data_1$Country)

raw_data_1$Country <- sub(",.*", "", raw_data_1$Country)
raw_data_1$Country <- trimws(raw_data_1$Country)

raw_data_1 <- raw_data_1 %>%
  mutate(Country = case_when(
    
    Country %in% c("UNITED STATE","AMERICA") ~ "UNITED STATES",
    Country %in% c("VIET NAM", "VIETNAM") ~ "VIETNAM",
    Country %in% c("KOREA", "KOREA, REPUB", "SOUTH KOREA") ~ "SOUTH KOREA",
    Country %in% c("SYRIAN ARAB", "SYRIAN ARAB REPUBLIC") ~ "SYRIA",
    Country %in% c("RUSSIAN FEDE", "RUSSIAN FEDERATION") ~ "RUSSIA",
    Country %in% c("VIRGIN ISLAN", "VIRGINISLANDS(U.S.)") ~ "VIRGIN ISLANDS",
    
    Country == "UNITED KINGD" ~ "UNITED KINGDOM",
    Country == "SAUDIARABIA" ~ "SAUDI ARABIA",
    Country == "NEWZEALAND" ~ "NEW ZEALAND",
    Country == "SRILANKA" ~ "SRI LANKA",
    Country == "BURKINAFASO" ~ "BURKINA FASO",
    Country == "ELSALVADOR" ~ "EL SALVADOR",
    Country == "COSTARICA" ~ "COSTA RICA",
    Country == "UNITED ARABEMIRATES" ~ "UNITED ARAB EMIRATES",
    Country == "CZECH REPUBL" ~ "CZECH REPUBLIC",
    Country == "OSEANIA" ~ "OCEANIA",
    Country == "VATICANCITYSTATE" ~ "VATICAN CITY",
    
    TRUE ~ Country
  ))

# Part 2 Data

# INSPECT COUNTRY
table(raw_data_2$Country)
raw_data_2 %>% distinct(Country) %>% print(n=50)

#CLEAN COUNTRY
raw_data_2 <- raw_data_2 %>%
  mutate(Country = toupper(Country))

raw_data_2 <- raw_data_2 %>%
  mutate(Country = case_when(
    
    Country == "UK" ~ "UNITED KINGDOM",
    Country == "USA" ~ "UNITED STATES",
    
    TRUE ~ Country
  ))


# Part 3 Data

# INSPECT COUNTRY
table(raw_data_3$Country)
raw_data_3 %>% distinct(Country) %>% print(n=50)

#CLEAN COUNTRY
raw_data_3 <- raw_data_3 %>%
  mutate(Country = toupper(Country))

raw_data_3 <- raw_data_3 %>%
  mutate(Country = case_when(
    
    Country == "UK" ~ "UNITED KINGDOM",
    Country == "USA" ~ "UNITED STATES",
    
    TRUE ~ Country
  ))



# _________WEBSERVER__________
# Part 1 Data

# INSPECT WEBSERVER
table(raw_data_1$WebServer)
raw_data_1 %>% distinct(WebServer) %>% print(n=50)

# CLEAN WEBSERVER
raw_data_1 <- raw_data_1 %>%
  mutate(WebServer = case_when(
    grepl("IIS|Microsoft|PWS", WebServer, ignore.case = TRUE) ~ "MICROSOFT",
    grepl("nginx|openresty", WebServer, ignore.case = TRUE) ~ "NGINX",
    grepl("LiteSpeed", WebServer, ignore.case = TRUE) ~ "LITESPEED",
    grepl("Apache|IBM_HTTP|Oracle-HTTP|Red Hat|WEBSERV", WebServer, ignore.case = TRUE) ~ "APACHE",
    grepl("ghs|gws|GSE", WebServer, ignore.case = TRUE) ~ "GOOGLE",
    grepl("YTS", WebServer, ignore.case = TRUE) ~ "YAHOO",
    grepl("Varnish", WebServer, ignore.case = TRUE) ~ "VARNISH",
    grepl("lighttpd", WebServer, ignore.case = TRUE) ~ "LIGHTTPD",
    grepl("Zeus", WebServer, ignore.case = TRUE) ~ "ZEUS",
    grepl("^$|Unknown|&quot;|\\*|\\+|^[0-9.]+$", WebServer, ignore.case = TRUE) ~ NA_character_,
    
    TRUE ~ "OTHER"
  ))


# Part 2 Data

# INSPECT WEBSERVER
table(raw_data_2$WebServer)
raw_data_2 %>% distinct(WebServer) %>% print(n=50)

# CLEAN WEBSERVER
raw_data_2 <- raw_data_2 %>%
  mutate(WebServer = case_when(
    grepl("Microsoft|IIS", WebServer, ignore.case = TRUE) ~ "MICROSOFT",
    grepl("OpenResty", WebServer, ignore.case = TRUE) ~ "OPENRESTY",
    grepl("Nginx", WebServer, ignore.case = TRUE) ~ "NGINX",
    grepl("LiteSpeed", WebServer, ignore.case = TRUE) ~ "LITESPEED",
    grepl("Apache", WebServer, ignore.case = TRUE) ~ "APACHE",
    
    TRUE ~ "OTHER"
  ))


# Part 3 Data

# INSPECT WEBSERVER
table(raw_data_3$WebServer)
raw_data_3 %>% distinct(WebServer) %>% print(n=50)

# CLEAN WEBSERVER
raw_data_3 <- raw_data_3 %>%
  mutate(WebServer = case_when(
    grepl("Microsoft|IIS", WebServer, ignore.case = TRUE) ~ "MICROSOFT",
    grepl("OpenResty", WebServer, ignore.case = TRUE) ~ "OPENRESTY",
    grepl("Nginx", WebServer, ignore.case = TRUE) ~ "NGINX",
    grepl("LiteSpeed", WebServer, ignore.case = TRUE) ~ "LITESPEED",
    grepl("Apache", WebServer, ignore.case = TRUE) ~ "APACHE",
    grepl("<script>|alert", WebServer, ignore.case = TRUE) ~ NA_character_,
    
    TRUE ~ "UNKNOWN"
  ))


# __________ENCODING__________
# Part 1 Data

# INSPECT ENCODING
table(raw_data_1$Encoding)
raw_data_1 %>% distinct(Encoding) %>% print(n=50)

# CLEAN ENCODING
raw_data_1 <- raw_data_1 %>%
  mutate(Encoding = toupper(Encoding))

raw_data_1 <- raw_data_1 %>%
  mutate(Encoding = case_when(
    Encoding == "N" ~ NA_character_,
    Encoding == "NULL" ~ NA_character_,
    TRUE ~ Encoding 
  ))


# Part 2 Data

# INSPECT ENCODING
table(raw_data_2$Encoding)
raw_data_2 %>% distinct(Encoding) %>% print(n=50)

# CLEAN ENCODING
raw_data_2 <- raw_data_2 %>%
  mutate(Encoding = toupper(Encoding))

# Part 3 Data

# INSPECT ENCODING
table(raw_data_3$Encoding)
raw_data_3 %>% distinct(Encoding) %>% print(n=50)

# CLEAN ENCODING
raw_data_3 <- raw_data_3 %>%
  mutate(Encoding = case_when(
    Encoding == "UTF_7" ~ "UTF-7",
    Encoding == "UTF8\\x00" ~ "UTF-8",
    
    Encoding %in% c("\\xFF\\xFE", "Unknown-Encoding") ~ NA_character_,
    
    TRUE ~ Encoding
  ))
raw_data_3 <- raw_data_3 %>%
  mutate(Encoding = toupper(Encoding))


# ____________DATE_____________
# Part 1 Data

# TRANSFORM DATE
raw_data_1$Date <- as.Date(raw_data_1$Date, format = "%d/%m/%Y")

# CHECK INVALID DATE
summary(raw_data_1$Date)
raw_data_1 %>%
  filter(is.na(Date)) %>%
  print(n=10)


# Part 2 Data

# TRANSFORM DATE
raw_data_2$Date <- as.Date(raw_data_2$Date, format = "%d/%m/%Y")

# CHECK INVALID DATE
summary(raw_data_2$Date)
raw_data_2 %>%
  filter(is.na(Date)) %>%
  print(n=10)

# Part 3 Data

# TRANSFORM DATE
raw_data_3$Date <- as.Date(raw_data_3$Date, format = "%d/%m/%Y")

# CHECK INVALID DATE
summary(raw_data_3$Date)
raw_data_3 %>%
  filter(is.na(Date)) %>%
  print(n=10)

# PART 3 DATA TRANSFORM DATA TYPE
# _______RANSOM________
# INSPECT RANSOM
failed_rows <- raw_data_3 %>%
  select(Ransom) %>%
  mutate(parsed_value = parse_number(Ransom)) %>%
  filter(is.na(parsed_value))
print(failed_rows[!is.na(failed_rows$Ransom),],n=500)

# CLEAN RANSOM
raw_data_3 <- raw_data_3 %>%
  mutate(Ransom = case_when(
    grepl("ten thousand", Ransom, ignore.case = TRUE) ~ "10000",
    grepl("unknown|N/A|NA", Ransom, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ Ransom
  )) %>%
  mutate(Ransom = parse_number(Ransom))

# ________DOWNTIME________
# INSPECT DOWNTIME
failed_rows <- raw_data_3 %>%
  select(DownTime) %>%
  mutate(parsed_value = parse_number(DownTime)) %>%
  filter(is.na(parsed_value))
print(failed_rows[!is.na(failed_rows$DownTime),],n=500)

# CLEAN DOWNTIME
raw_data_3 <- raw_data_3 %>%
  mutate(DownTime = case_when(
    grepl("two", DownTime, ignore.case = TRUE) ~ "2",
    grepl("unknown|N/A|NA", DownTime, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ DownTime
  )) %>%
  mutate(DownTime = parse_number(DownTime))



# ________LOSS_________
# INSPECT LOSS
failed_rows <- raw_data_3 %>%
  select(Loss) %>%
  mutate(parsed_value = parse_number(Loss)) %>%
  filter(is.na(parsed_value))
print(failed_rows[!is.na(failed_rows$Loss),],n=500)

# CLEAN LOSS
raw_data_3 <- raw_data_3 %>%
  mutate(Loss = case_when(
    grepl("one million", Loss, ignore.case = TRUE) ~ "1000000",
    grepl("unknown|N/A|NA|null|USD", Loss, ignore.case = TRUE) ~ NA_character_,
    TRUE ~ Loss
  )) %>%
  mutate(Loss = parse_number(Loss))
