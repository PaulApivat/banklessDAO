
# load libraries
library(tidyverse)
library(tidytext)
library(lubridate)  # Working with date: what year did you enter the space?

# load data ----
df <- read_csv("./DAO_Creator_Report_Sep4.csv")

# Rename: short column names to be manageable ----
names(df)

df1 <- df %>%
    rename(
        timestamp = "Timestamp",
        age = "How old are you? (in years)",
        gender = "What is your gender?",
        wallet = "What wallet do you use?",
        blockchain = "What blockchain(s) do you use?",
        eth_scaling = "What Ethereum Scaling solutions do you use?",
        primary_dao = "What is the primary DAO you work for?",
        num_member = "How many \"members\" does your DAO have?",
        type_dao = "What type of DAO is this DAO?",
        fav_apps = "What are your favorite blockchain-enabled apps? (Please separate your answers with a comma; comma-seperated ex: coinbase, uniswap, gnosis safe)",
        x11 = "X11",
        email = "What is your email address?",
        twitter = "What is your twitter username?",
        family_rec = "On a scale of 1 to 10, 10 being the most emphatic and 1 being the least, how strongly would you recommend to a friend or family member that they should work for a DAO?",
        why_dao = "Why are you involved in DAOs?",
        advice = "What advice would you have for someone who  is going down the DAO Rabbit-hole?",
        other = "Is there anything we didnt ask you want to tell us?",
        feature = "This question is only for those who want to be FEATURED in the report (and is totally optional).   ------------------------------------------------------  Tell us the story of how you became involved",
        country = "In what country do you reside? (Paul comment: see drop-down option below)",
        dao_tool = "Is your DAO built upon any of these tools?",
        dao_funded = "How is this DAO Funded?",
        country2 = "In what country do you reside?",
        year_enter = "What year did you get into the blockchain space?",
        age2 = "Please indicate your age."
    )

# CHARTS ----
# Charts to start with:

# What type of DAO is this DAO? ----
df1$type_dao

type_dao_v <- as.vector(df1$type_dao)
type_dao_tbl <- tibble(line = 1:256, text = type_dao_v)

# separate by comma
# unnest
# remove white space
# group, tally
# filter
type_dao_tbl2 <- type_dao_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1)

# visualize
type_dao_tbl2 %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What type of DAO is this DAO?",
        subtitle = "Total Survey Response (n = 256)",
        y = "",
        x = "Number of Responses"
    )




# How many members does your DAO have? ----
df1$num_member

num_member_v <- as.vector(df1$num_member)
num_member_tbl <- tibble(line = 1:256, text = num_member_v)

# group, tally
# save as new df
num_member_tbl2 <- num_member_tbl %>%
    count(text, sort = TRUE)

# manually reformat to make understandable
num_member_tbl2$text[1] <- "0 - 50"
num_member_tbl2$text[2] <- "50 - 100"
num_member_tbl2$text[3] <- "100 - 500"
num_member_tbl2$text[4] <- "1000 - 5000"
num_member_tbl2$text[5] <- "5000+"
num_member_tbl2$text[6] <- "500 - 1000"

# manually add items from same category
num_member_tbl2$n[1] <- 97
num_member_tbl2$n[2] <- 50
num_member_tbl2$n[3] <- 44

# reorder & select desired rows with slice
# set factor for visualization
# visualize
num_member_tbl3 <- num_member_tbl2 %>%
    slice(1:3, 6, 4:5) 

num_member_tbl3 %>%
    # set factor orders so categories show up in order with as_factor()
    mutate(text_factor = as_factor(text)) %>%
    ggplot(aes(x = text_factor, y = n, fill = text)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#525252", "#737373", "#969696")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How many 'members' does your DAO have?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Membership Numbers",
        y = "Number of Responses"
    )



# How is this DAO funded? ----
df1$dao_funded

dao_funded_v <- as.vector(df1$dao_funded)
dao_funded_tbl <- tibble(line = 1:256, text = dao_funded_v)

# split by comma, unnest, 
# get rid of white space
# group, tally
# add new column, save as new df
dao_funded_tbl2 <- dao_funded_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE) %>%
    mutate(
        phrase2 = NA
    )

# fill in new column, consolidate misc answers
dao_funded_tbl2$phrase2[1] <- "Token Sale"
dao_funded_tbl2$phrase2[2] <- "Investors"
dao_funded_tbl2$phrase2[3] <- "NFTs"
dao_funded_tbl2$phrase2[4] <- "Services"
dao_funded_tbl2$phrase2[5] <- "Member Dues"
dao_funded_tbl2$phrase2[6] <- "Not Funded"

dao_funded_tbl2$phrase2[11] <- "Token Sale"
dao_funded_tbl2$phrase2[12] <- "Donations"
dao_funded_tbl2$phrase2[13] <- "Donations"
dao_funded_tbl2$phrase2[14] <- "Grants"
dao_funded_tbl2$phrase2[17] <- "Grants"
dao_funded_tbl2$phrase2[18] <- "Grants"
dao_funded_tbl2$phrase2[19] <- "Grants"
dao_funded_tbl2$phrase2[20] <- "Grants"
dao_funded_tbl2$phrase2[21] <- "Grants"
dao_funded_tbl2$phrase2[22] <- "Donations"
dao_funded_tbl2$phrase2[24] <- "Member Dues"
dao_funded_tbl2$phrase2[25] <- "Member Dues"
dao_funded_tbl2$phrase2[26] <- "Merchandise"
dao_funded_tbl2$phrase2[32] <- "Merchandise"
dao_funded_tbl2$phrase2[36] <- "Grants"


# manually add same categories
dao_funded_tbl2$n[1] <- 106   # Token Sale
dao_funded_tbl2$phrase[7] <- "Grants"
dao_funded_tbl2$n[7] <- 7
dao_funded_tbl2$phrase[8] <- "Donations"
dao_funded_tbl2$n[8] <- 3
dao_funded_tbl2$n[5] <- 43     # Member Dues
dao_funded_tbl2$phrase[9] <- "Merchandise"
dao_funded_tbl2$n[9] <- 2


# select desired rows, then visualize
dao_funded_tbl2 %>%
    slice(1:9) %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#bdbdbd", "#9ecae1",
                                 "#737373", "#deebf7", "#525252")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How is this DAO funded?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Number of Responses",
        y = "Funding Sources"
    )



# How old are you? ----
df1$age

age_v <- as.vector(df1$age)
age_tbl <- tibble(line = 1:256, text = age_v)

# need to manually change responses of year (e.g., 1994) to age (2021 - 1994 = 27)

age_tbl$text[177] <- 27
age_tbl$text[183] <- 56
age_tbl$text[185] <- 40
age_tbl$text[187] <- 29
age_tbl$text[197] <- 22
age_tbl$text[221] <- 29
age_tbl$text[226] <- 34
age_tbl$text[246] <- 25


age_tbl %>%
    count(text, sort = TRUE) %>%
    arrange(text) %>%
    drop_na() %>%
    ggplot(aes(x = text, y = n, fill = text)) +
    geom_col() +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How old are you?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Ages",
        y = "Number of Responses"
    )





# What year did you enter the space? ----
library(lubridate)

df1$year_enter

year_enter_v <- as.vector(df1$year_enter)
year_enter_tbl <- tibble(line = 1:256, text = year_enter_v)

year_enter_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    arrange(text) %>% 
    mutate(
        year = as.Date(as.character(text), format = "%Y")
    ) %>%
    ggplot(aes(x = year, y = n, fill = text)) +
    geom_col() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What year did you enter the space?",
        subtitle = "Total Survey Response (n = 256)",
        x = "",
        y = "Number of Responses"
    )







# What is your gender? ----
df1$gender

gender_v <- as.vector(df1$gender)
gender_tbl <- tibble(line = 1:256, text = gender_v)

# group, tally
# visualize basic pie, then save for further customizations
gender_pie <- 
    
library(scales)
    
gender_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    ggplot(aes(x = "", y = n, fill = text)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("#3182bd", "#9ecae1", "#bdbdbd",
                                 "#f0f0f0")) +
    theme_void() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
    labs(
        title = "What is your gender?",
        subtitle = "Total Survey Response (n = 256)",
        fill = "Gender"
    )
    





# Is your DAO built upon any of these tools? ----
df1$dao_tool

dao_tool_v <- as.vector(df1$dao_tool)
dao_tool_tbl <- tibble(line = 1:256, text = dao_tool_v)

# split words by comma, unnest, group, tally
# save as new df
dao_tool_tbl2 <- dao_tool_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE)


# manually add words
dao_tool_tbl2$n[3] <- 73

dao_tool_tbl2$n[5] <- 52

dao_tool_tbl2$phrase[6] <- "DAOHaus"
dao_tool_tbl2$n[6] <- 7

dao_tool_tbl2$n[7] <- 33

dao_tool_tbl2$n[8] <- 3

# Slice desired row, save as new df
dao_tool_tbl3 <- dao_tool_tbl2 %>%
    slice(1:8, 12, 15, 17, 22, 23, 28, 29, 33, 36, 40, 42, 45, 46, 48, 50)


dao_tool_tbl3 %>%
    filter(n > 1) %>% 
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08519c", "#2171b5",
                                 "#4292c6", "#bdbdbd", "#9ecae1",
                                 "#737373", "#deebf7", "#252525")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Is your DAO built upon any of these tools?",
        subtitle = "Total Survey Response (n = 256)",
        y = "",
        x = "Number of Responses"
    )
    



# What wallet(s) do you use? ----
df1$wallet

wallet_v <- as.vector(df1$wallet)
wallet_tbl <- tibble(line = 1:256, text = wallet_v)

wallet_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1) %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#000000", "#252525", "#525252")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What wallet(s) do you use?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Number of Responses",
        y = ""
    )




# What blockchain(s) do you use? ----
df1$blockchain

blockchain_v <- as.vector(df1$blockchain)
blockchain_tbl <- tibble(line = 1:256, text = blockchain_v)

# separate strings on comma
# unnest, group, tally, save as new df
blockchain_tbl2 <- blockchain_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1)

# manually add avax +  Avalanche
blockchain_tbl2$n[15] <- 4

blockchain_tbl2 %>%
    slice(1:15, 17:20) %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#000000", "#252525", "#525252")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What blockchain(s) do you use?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Number of Responses",
        y = ""
    )


# What Ethereum Scaling Solutions do you use? ----
df1$eth_scaling


eth_scaling_v <- as.vector(df1$eth_scaling)
eth_scaling_tbl <- tibble(line = 1:256, text = eth_scaling_v)


# separate by comma, unnest,
# get rid of white space
# group, tally
# save as new df
eth_scaling_tbl2 <- eth_scaling_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na()

# manually add categories
eth_scaling_tbl2$n[4] <- 101
eth_scaling_tbl2$n[5] <- 83

# choose desired rows with slice, then visualize
eth_scaling_tbl2 %>%
    slice(1:13, 16:18, 20:21) %>%
    ggplot(aes(x =n , y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#000000", "#252525", "#525252")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What Ethereum scaling solutions do you use?",
        subtitle = "Total Survey Response (n = 256)",
        x = "Number of Responses",
        y = ""
    )




# What are your favorite blockchain-enabled apps? ----
df1$fav_apps


fav_apps_v <- as.vector(df1$fav_apps)
fav_apps_tbl <- tibble(line = 1:256, text = fav_apps_v)

# represents the best of both worlds between comma separate, then unnest
# and unnest_token, anti_join
fav_app_one_response <- fav_apps_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    # convert to lowercase so can group, tally effectively
    mutate(across(where(is.character), tolower)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    slice(72:292)


fav_app_multi_response <- fav_apps_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    # trim white space
    mutate(across(where(is.character), str_trim)) %>%
    # convert to lowercase so can group, tally effectively
    mutate(across(where(is.character), tolower)) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    slice(1:71)


# one responses: filter for uniswap, alchemix, opensea, binance
fav_app_one_response %>%
    view()


# multi responses
# manually split and add row 53: coinbase, uniswap, gnosis
fav_app_multi_response$n[1] <- 107
fav_app_multi_response$n[8] <- 17
fav_app_multi_response$n[9] <- 14

# Visualize Top-20 for space
fav_app_multi_response %>%
    # get rid of row 53
    slice(1:20) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    scale_fill_manual(values = c("#000000", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#08306b", "#252525")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What are your favorite blockchain-enabled apps? (Top 20)",
        subtitle = "Total Survey Response (n = 256)",
        x = "Number of Responses",
        y = ""
    )



    

    
    

# try saving multiple fav_apps_tbl and strsplit with "." or ";" or "、"

# On a scale of 1 to 10, how strongly would you recommend to a friend or family member they should work for a DAO? ----
df1$family_rec

family_rec_v <- as.vector(df1$family_rec)
family_rec_tbl <- tibble(line = 1:256, text = family_rec_v)


family_rec_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    arrange(text) %>%
    mutate(text_factor = as_factor(text)) %>%
    ggplot(aes(x = text_factor, y = n, fill = text_factor)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.3) +
    scale_fill_manual(values = c("#000000", "#08306b", "#deebf7",  
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#c6dbef", "#f7fbff", "#08519c",
                                 "#2171b5")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Friend or Family Recommendation",
        subtitle = "1 = Not Recommend, 10 = Strongly Recommend",
        x = "",
        y = "Number of Responses"
    )




