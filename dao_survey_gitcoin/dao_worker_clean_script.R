# load libraries
library(tidyverse)
library(tidytext)

# load data ----
df <- read_csv("./DAO_Worker_Report_Sep1.csv")

# Rename: shorten column names to be manageable
names(df)

df1 <- df %>%
    # Rename: shorten column names to be manageable
    rename(
       timestamp = "Timestamp",
       daos_work_for = "what DAO(s) do you work for? for each DAO, how many hours/month do you work? (feel free to include multiple)",
       city = "what city are you based in?",
       twitter = "whats your twitter username?",
       eth_addr = "whats your ETH address?",
       task = "what kind of work do you do? (eg ops, coder, design, etc)",
       task_assign = "how are tasks assigned? how does your DAO decide what you’re doing and how much to pay you?",
       day_to_day = "what is your day to day like?",
       resume = "do you have a DAO version of “resume”? what does it look like?",
       improve_coord = "are there any opportunities to improve the way your work is coordinated?  if so what are they?",
       web3_tool = "what web3 tools are used to coordinate/allocate resources?",
       usd_earning = "how much do you make in USD equivilent per month?",
       income_stability = "is your income stability a priority for you?  does your DAO meet your expectations on income stability?",
       primary_income = "are DAOs your primary source of income?",
       token_volatility = "how do you handle token volatility?",
       health_insurance = "where do you get health insurance?",
       retirement = "are you saving for retirement?",
       tax = "how do you do compliance with local taxation/legal requirements for workers?",
       dispute = "what do you do if there is a dispute between yourself and the DAO? or another DAO member?",
       visa = "how do you manage visas to travel/work abroad?",
       dao_different = "what is the most different/interesting thing about working for a DAO to you?",
       favorite = "what is your favorite part of working for a DAO?",
       least_favorite = "what is your least favorite part of working for a DAO?",
       other = "Is there anything we didnt ask you want to tell us?",
       email = "whats a good email in case we have follow up questions?",
       improve_comp = "are there any opportunities to improve the way you are compensated?  if so what are they?",
       comms_tool = "what comms tools does your DAO use to coordinate?",
       comp_denom = "the majority of your compensation is",
       savings_amount = "how much savings do you have?",
       savings_denom = "most of your savings is in",
       dao_volatility = "how does your DAO handle token volatility?",
       comp_denom_breakdown = "how much of your comp is in DAO Tokens vs stablecoins/ETH?",
       num_field_filled = "Num Fields Filled out",
       include = "Include?",
       valid_res_count = "Valid Response Count"
    ) %>%
    # delete identifying information
    select(-twitter, -eth_addr, -email)

# Data Cleaning Planning
names(df1)



# Step 1: Tidy Text, summarize, then visualize
# Step 2: go through cleaning and creating document-term-matrix, then word cloud for qualitative themes

# [1] "timestamp"  
# "what DAO(s) do you work for? for each DAO, how many hours/month do you work? (feel free to include multiple)"
# "daos_work_for" gitcoin/gitcoindao (18 + 5), bankless/banklessdao (13 + 5), 
# dorg (9), metacartel (8), indexcoop (7), daohaus (6), pooltogether (5), moonshot (5), raidguild (4),
# harmony (4), dxdao (4)

# note: can also filter for below 4

daos_work_v <- as.vector(df1$daos_work_for)
daos_work_tbl <- tibble(line = 1:445, text = daos_work_v)

daos_work_tbl %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% view()
    count(word, sort = TRUE) %>%
    filter(n > 3) %>%
    drop_na() %>%
    ggplot(aes(n, reorder(word, n))) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    labs(
        title = "what DAO(s) do you work for?",
        subtitle = "count > 3",
        y = "text",
        x = "count"
    )

# DAO names ----


daos_work_long <- daos_work_tbl %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>% 
    count(word, sort = TRUE)

daos_work_long$bin <- NA

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "cre8")==TRUE), "cre8rdao", "NA")
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "mstable")==TRUE), "mstable", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "marrow")==TRUE), "marrow dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "badger")==TRUE), "badger dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "raid")==TRUE), "raid guild", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "metagame")==TRUE), "metagame", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "metaf")==TRUE), "metafactory", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "defiant")==TRUE), "defiant pixel society", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "nfd")==TRUE), "nfdao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "punks")==TRUE), "punks dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "cedge")==TRUE), "cedge dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "sushi")==TRUE), "sushi protocol", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "dal")==TRUE), "dalten collective", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "ventures")==TRUE), "metacartel ventures", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "metacartel")==TRUE), "metacartel", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "bank")==TRUE), "bankless dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "tally")==TRUE), "tally", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "friend")==TRUE), "friends with benefits", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "fwb")==TRUE), "friends with benefits", daos_work_long$bin)


daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "pleas")==TRUE), "pleasrdao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "seed")==TRUE), "seed club", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "fore")==TRUE), "forefront", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "finger")==TRUE), "fringerprints", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "gitc")==TRUE), "gitcoin dao / gitcoin", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "gtc")==TRUE), "gitcoin dao / gitcoin", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "super")==TRUE), "superrare dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "daomaker")==TRUE), "daomaker", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "maker")==TRUE), "makerdao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "mkrdao")==TRUE), "makerdao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "makedaos")==TRUE), "makerdao", daos_work_long$bin)


daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "dorg")==TRUE), "dOrg", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "rail")==TRUE), "railgun", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "mycryptoh")==TRUE), "mycryptoheroes", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "daoh")==TRUE), "daohaus", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "uber")==TRUE), "uber haus", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "hero_")==TRUE), "hero dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "index")==TRUE), "index coop", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "square")==TRUE), "dao square", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "rally")==TRUE), "rally.io", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "rarible")==TRUE), "rarible dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "genosis")==TRUE), "genesis dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "dappnode")==TRUE), "dappnode", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "radiologex")==TRUE), "radiologex", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "gridology")==TRUE), "gridology", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "yam")==TRUE), "yam", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "alchemy")==TRUE), "alchemy", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "1hive")==TRUE), "1hive", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "pharo")==TRUE), "pharo dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "poolt")==TRUE), "pooltogether", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "commitpool")==TRUE), "commitpool", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "machix")==TRUE), "machi x dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "peerion")==TRUE), "peerion dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "dxdao")==TRUE), "dxdao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "karma")==TRUE), "karma dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "doingud")==TRUE), "doingud", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "unis")==TRUE), "uniswap", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "gryffone")==TRUE), "gryffone", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "harmony")==TRUE), "harmony dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "buidl")==TRUE), "buidlguidl", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "airs")==TRUE), "airswap.io", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "aave")==TRUE), "aave grants", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "sourcecred")==TRUE), "sourcecred", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "ghosting")==TRUE), "goodghosting", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "beauty")==TRUE), "historians dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "historian")==TRUE), "historians dao", daos_work_long$bin)


daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "meebit")==TRUE), "meebits dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "alas")==TRUE), "alas dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "buzz")==TRUE), "buzzed bear hideout", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "mcn")==TRUE), "mcn ventures", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "fraktal")==TRUE), "fraktal dao", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "decrypt")==TRUE), "decrypt", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "pollen")==TRUE), "pollen dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "synthetix")==TRUE), "synthetix", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "balancer")==TRUE), "balancer", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "flow")==TRUE), "flow community", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "daosquare")==TRUE), "daosquare", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "daosuqre")==TRUE), "daosquare", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "aladdindao")==TRUE), "aladdindao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "aladdin")==TRUE), "aladdindao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "sandao")==TRUE), "sandao", daos_work_long$bin)


daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "compound")==TRUE), "compound", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "gamedao")==TRUE), "gamedao", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "halo")==TRUE), "halodao", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "qidao")==TRUE), "qidao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "soudao")==TRUE), "soudao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "curve")==TRUE), "curve dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "coordinape")==TRUE), "coordinape", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "nash")==TRUE), "nash dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "discord")==TRUE), "discord", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "ocean")==TRUE), "ocean dao", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "parity")==TRUE), "parity", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "sporkdao")==TRUE), "sporkdao", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "epns")==TRUE), "ethereum push notification service", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "scribedao")==TRUE), "scribeddao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "trips")==TRUE), "trips community", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "daostack")==TRUE), "daostack", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "opolis")==TRUE), "opolis", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "aragon")==TRUE), "aragon", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "renaissancedao")==TRUE), "renaissancedao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "rabbhithole")==TRUE), "rabbithole", daos_work_long$bin)

daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "cryptex")==TRUE), "cryptex.finance", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "olympus")==TRUE), "olympus dao", daos_work_long$bin)
daos_work_long$bin <- if_else((str_detect(daos_work_long$word, "mirror")==TRUE), "mclubdao", daos_work_long$bin)

daos_work_long %>%
    filter(bin != "NA") %>%
    select(bin, n) %>%
    arrange(bin) %>%
    view()

# DAO Names: Data cleaning ----
# Create new df
daos_work_long2 <- daos_work_long %>%
    filter(bin != "NA") %>%
    select(bin, n) %>%
    arrange(bin)



# manually add same DAOs different rows together
# airswap.io = 2
daos_work_long2$n[3] <- 2

# aladdindao = 4
daos_work_long2$n[5] <- 4

# bankless dao = 35
daos_work_long2$n[12] <- 35

# cedgedao = 2
daos_work_long2$n[18] <- 2

# cre8rdao = 5
daos_work_long2$n[23] <- 5

# daosquare = 10
daos_work_long2$n[30] <- 10

# flow community = 3
daos_work_long2$n[41] <- 3

# friends with benefits = 2
daos_work_long2$n[45] <- 2

# gitcoin = 29
daos_work_long2$n[50] <- 29

# historians dao = 3
daos_work_long2$n[60] <- 3

# index coop = 15
daos_work_long2$n[62] <- 15

# makerdao = 8
daos_work_long2$n[66] <- 8

# metafactory = 3
daos_work_long2$n[77] <- 3

# mstable = 5
daos_work_long2$n[80] <- 5

# olympus dao = 3
daos_work_long2$n[86] <- 3

# pooltogether = 7
daos_work_long2$n[94] <- 7

# raid guild = 8
daos_work_long2$n[99] <- 8

# rally.io = 2
daos_work_long2$n[102] <- 2

# manually delete duplicate rows
# safe in new df
daos_work_long3 <- daos_work_long2[-c(4, 6, 13, 14, 15, 19, 24, 28, 31, 42, 46, 51, 52, 53,
                  61, 63, 67, 68, 69, 70, 78, 81, 87, 95, 100, 103),] %>% 
    arrange(desc(n))

# visualize
daos_work_long3 %>%
    filter(n > 1) %>%
    ggplot(aes(x = n, y = reorder(bin, n), fill = bin)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    scale_fill_manual(values = c("#9ecae1", "#252525", "#525252",
                                 "#737373", "#969696", "#bdbdbd",
                                 "#d9d9d9", "#f0f0f0", "#ffffff",
                                 "#08306b", "#08519c", "#2171b5", 
                                 "#4292c6", "#6baed6", "#9ecae1", 
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#081d58", "#253494", "#225ea8",
                                 "#525252", "#000000", "#252525", 
                                 "#737373", "#969696", "#bdbdbd",
                                 "#d9d9d9", "#f0f0f0", "#ffffff",
                                 "#08306b", "#08519c", "#2171b5", 
                                 "#4292c6", "#6baed6", "#9ecae1", 
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#081d58", "#253494", "#225ea8",
                                 "#525252", "#525252", "#4292c6", 
                                 "#6baed6", "#000000")) +
    labs(
        title = "What DAO(s) do you work in?",
        subtitle = "Responses (n = 422)",
        x = "Number of Respondents",
        y = ""
    )



# "city" --- A) lower case, then split all words, group and tally            
# "task" --- A) lower case, then split all words, group and tally (Separate Text Column by Comma)              
# "task_assign" --- qualitative content theme (likely split, group, tally)     
# [6] "day_to_day"  --- qualitative content theme  (word cloud)       
# "resume" --- A) lower case, then split all words, group and tally (YES v NO)             
# "improve_coord" --- A) lower case, then split all words group and tally (YES v NO), also theme    
# "web3_tool" --- A) lower case, split, group and tally (people use multiple tools)      
# "usd_earning" --- A) regex to only locate the numbers, then group, tally        
# [11] "income_stability" A) lower case, then split all words, group and tally (YES v NO) ,also theme    
# "primary_income" A) group & tally as is   
# "token_volatility" --- qualitative content theme     
# "health_insurance" --- loop through and count the number of times a Survey option appears,    
# "retirement" --- A) lower case, split, group and tally         
# [16] "tax" --- qualitative content theme                
# "dispute" --- qualitative content theme              
# "visa" --- qualitative content theme                
# "dao_different" --- qualitative content theme       
# "favorite" --- qualitative content theme           
# [21] "least_favorite" --- qualitative content theme      
# "other" --- A) lower case, split, group and tally (NO vs YES)               
# "improve_comp" --- A) lower case, split, group and tally (NO vs YES)        
# "comms_tool" --- A) group and tally          
# "comp_denom" --- A) group and tally         
# [26] "savings_amount" --- A) group and tally       
# "savings_denom" --- A) group and tally          
# "dao_volatility" --- qualitative content theme      
# "comp_denom_breakdown" --- NA
# "num_field_filled" --- NA   
# [31] "include" --- NA            
# "valid_res_count" --- NA   


# What web tool do you use? ----

web3_tool_v <- as.vector(df1$web3_tool)
web3_tool_tbl <- tibble(line = 1:445, text = web3_tool_v)

# expand color palette


web3_tool_tbl %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>% 
    filter(n > 3) %>% 
    drop_na() %>%
    ggplot(aes(n, reorder(word, n), fill = word)) +
    geom_col() +
    scale_fill_manual(values = c("#000000", "#252525", "#525252",
                                  "#737373", "#969696", "#bdbdbd",
                                  "#d9d9d9", "#f0f0f0", "#08306b",
                                  "#08519c", "#2171b5", "#4292c6",
                                  "#6baed6", "#9ecae1", "#c6dbef",
                                  "#deebf7")) +
    geom_text(aes(label = n), hjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What web3 tools are used to coordinate/allocate resources?",
        subtitle = "Responses (n = 422)",
        y = "Tools",
        x = "Number of Responses"
    )
    
    
?scale_fill_brewer

# What communications tool? ----

comms_tool_v <- as.vector(df1$comms_tool)
comms_tool_tbl <- tibble(line = 1:445, text = comms_tool_v)

comms_tool_tbl %>%
    unnest_tokens(word, text) %>% 
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    filter(n > 3) %>% 
    drop_na() %>%
    ggplot(aes(n, reorder(word, n))) +
    geom_col(aes(fill = word)) +
    scale_fill_manual(values = c("#000000", "#252525", "#525252",
                                 "#737373", "#969696", "#bdbdbd",
                                 "#d9d9d9", "#f0f0f0", "#08306b",
                                 "#08519c", "#2171b5", "#4292c6",
                                 "#6baed6", "#9ecae1", "#c6dbef",
                                 "#deebf7")) +
    geom_text(aes(label = n), hjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What communications tools does your DAO use to coordinate?",
        subtitle = "Responses (n = 422)",
        y = "Tools",
        x = "Number of Responses"
    )

# How much savings do you have? ----

savings_amount_v <- as.vector(df1$savings_amount)
savings_amount_tbl <- tibble(line = 1:445, text = savings_amount_v)

savings_amount_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    ggplot(aes(n, reorder(text, n))) +
    geom_col(aes(fill = text)) +
    scale_fill_manual(values = c("#525252", "#969696", "#4292c6", "#6baed6")) +
    geom_text(aes(label = n), hjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How much savings do you have?",
        subtitle = "Responses (n = 422)",
        y = "Runway",
        x = "Number of Responses"
    )

# Most of your savings is in which assets? ----

savings_denom_v <- as.vector(df1$savings_denom)
savings_denom_tbl <- tibble(line = 1:445, text = savings_denom_v)

savings_denom_tbl %>%
    #unnest_tokens(word, text) %>% 
    #anti_join(stop_words) %>%
    count(text, sort = TRUE) %>% 
    filter(n > 3) %>% 
    drop_na() %>%
    ggplot(aes(n, reorder(text, n))) +
    geom_col(aes(fill = text)) +
    coord_flip() +
    scale_fill_manual(values = c("#4292c6", "#525252", "#969696")) +
    geom_text(aes(label = n), vjust = -1) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Most of your savings is in which asset?",
        subtitle = "Responses (n = 422)",
        y = "Asset",
        x = "Number of Responses"
    )


# What role do you play in your DAO? ----
# What kind of work do you do? (eg ops, coder, design, etc)

task_v <- as.vector(df1$task)
task_tbl <- tibble(line = 1:445, text = task_v)

task_tbl %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    filter(n > 3) %>%
    drop_na() %>%
    ggplot(aes(n, reorder(word, n))) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    labs(
        title = "What kind of work do you do?",
        subtitle = "count > 3",
        y = "text",
        x = "count"
    )

# create task_tabl2
task_tabl2 <- task_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 5)

# manual addition
task_tabl2$n[2] <- 180
task_tabl2$n[3] <- 127
task_tabl2$n[4] <- 118
task_tabl2$n[5] <- 90
task_tabl2$n[6] <- 83
task_tabl2$n[7] <- 86
task_tabl2$n[8] <- 80
task_tabl2$n[10] <- 105
task_tabl2$n[12] <- 55
task_tabl2$n[16] <- 26

# manual deletion (create new table)
task_tabl3 <- task_tabl2[-c(9, 13, 14, 15, 17, 18, 19, 20, 21, 22),] 

# visualize (with task_tabl3)
task_tabl3 %>%
    arrange(desc(n)) %>% 
    ggplot(aes(x = reorder(phrase, n), y = n, fill = phrase)) +
    geom_col() +
    scale_fill_manual(values = c("#000000", "#252525", "#525252",
                                 "#737373", "#969696", "#bdbdbd",
                                 "#d9d9d9", "#f0f0f0", "#ffffff",
                                 "#08306b", "#08519c", "#2171b5", 
                                 "#4292c6", "#6baed6", "#9ecae1", 
                                 "#c6dbef", "#deebf7", "#f7fbff",
                                 "#081d58", "#253494", "#225ea8",
                                 "#525252")) +
    geom_text(aes(label = n), vjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 0.8),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "What role do you play in your DAO?",
        subtitle = "Responses (n = 422)",
        x = "",
        y = "Number of Responses"
    )


# Source of Primary Income ----

primary_income_v <- as.vector(df1$primary_income)
primary_income_tbl <- tibble(line = 1:445, text = primary_income_v)

primary_income_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1) %>%
    ggplot(aes(x = reorder(text, n), y = n, fill = text)) +
    geom_col() +
    scale_fill_manual(values = c("#000000", "#2171b5", "#08519c",
                                 "#4292c6")) +
    geom_text(aes(label = n), vjust = -0.5) +
    theme_minimal() +
    theme(
        legend.position = "none",
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        axis.text.x = element_text(size = 10)
    ) +
    labs(
        title = "Are DAOs your primary source of income?",
        subtitle = "Responses (n = 422)",
        x = "",
        y = "Number of Responses"
    )

# How much do you make in USD per month ----

usd_earning_v <- as.vector(df1$usd_earning)
usd_earning_tbl <- tibble(line = 1:445, text = usd_earning_v)

# save new df to filter if row contains a numeric value
# removing responses where n = 1
usd_earning_tbl2 <- usd_earning_tbl %>%
    count(text, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1) %>%
    arrange(desc(text)) %>%
    slice(6:15)

# manually add response from 10000 to $7k - $10k
usd_earning_tbl2$n[1] <- 12

# reorder rows, save as new df
usd_earning_tbl3 <- usd_earning_tbl2 %>%
    slice(4, 6, 7, 1:3, 5, 8:9)

# need to sort by factors before visualize
usd_earning_tbl3 %>% 
    mutate(text_factor = as_factor(text)) %>% 
    ggplot(aes(x = text_factor, y = n, fill = text)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#525252", "#737373", "#969696")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How much do you make in USD equivalent per month?",
        subtitle = "Responses (n = 422)",
        x = "",
        y = "Number of Responses"
    )


# The majority of your compensation is from ----

comp_denom_v <- as.vector(df1$comp_denom)
comp_denom_tbl <- tibble(line = 1:445, text = comp_denom_v)

# split string by comma
# unnest from vector
# group_by and sort (count)
# save as new df

comp_denom_tbl2 <- comp_denom_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    drop_na()

# manually add same compensation categories
comp_denom_tbl2$n[1] <- 158
comp_denom_tbl2$n[3] <- 159
comp_denom_tbl2$n[8] <- 104
comp_denom_tbl2$n[6] <- 78
comp_denom_tbl2$n[11] <- 55

# select specific rows, then visualize
comp_denom_tbl2 %>%
    slice(1,3,4,6,8,11) %>%
    arrange(desc(n)) %>%
    ggplot(aes(x = reorder(phrase,n), y = n, fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#525252", "#737373", "#969696")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Source of Compensation",
        subtitle = "Total Survey Responses (n = 422)",
        x = "",
        y = "Number of Responses"
    )

# How much of your comp is in DAO Tokens vs stablecoins/ETH ? ----
# (Response Options Too Vague to make sense)

comp_denom_breakdown_v <- as.vector(df1$comp_denom_breakdown)
comp_denom_breakdown_tbl <- tibble(line = 1:445, text = comp_denom_breakdown_v)

comp_denom_breakdown_tbl %>%
    drop_na() %>%
    count(text, sort = TRUE) %>%
    slice(3,1,2,6,7,9,5,4) %>%
    mutate(text_factor = as_factor(text)) %>%
    ggplot(aes(x = n, y = text_factor, fill = text_factor)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#4292c6", "#6baed6", "#9ecae1",
                                 "#525252", "#737373", "#969696")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "How much of your comp is in DAO Tokens vs Stablecoins / ETH?",
        subtitle = "Total Survey Respondents (n = 445)",
        y = "Ratio: DAO Tokens-to-Stablecoins or ETH",
        x = ""
    )




comp_denom_breakdown_tbl %>%
    unnest_tokens(word, text) %>% view()
    anti_join(stop_words) %>% 
    count(word, sort = TRUE) %>%
    filter(n > 3) %>% 
    drop_na() %>%
    
    
    
    ggplot(aes(n, reorder(word, n))) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    labs(
        title = "How much of your comp is in DAO Tokens vs stablecoins/ETH?",
        subtitle = "count > 3",
        y = "text",
        x = "count"
    )

# Are you saving for retirement? ----

retirement_v <- as.vector(df1$retirement)
retirement_tbl <- tibble(line = 1:445, text = retirement_v)


# unnesting words to count with "yes" and "no"
# other words counted include: "not" (not yet) (23), "nope" (3)
# alternatives to 'yes': "yse" (1), "yup" (1), "yeah" (4), "sure" (3), "yep" (3)
# slice to only yes or no
retirement_tbl2 <- retirement_tbl %>%
    unnest_tokens(word, text) %>% 
    count(word, sort = TRUE) %>%
    drop_na() %>%
    slice(1:2)

# manually add alternatives to "yes" and "no
retirement_tbl2$n[1] <- 220
retirement_tbl2$n[2] <- 89

# compute position of label, save new df
retirement_tbl3 <- retirement_tbl2 %>%
    arrange(desc(word)) %>%
    mutate(prop = n / sum(retirement_tbl2$n) * 100) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop)


    

# visualize as bar chart
retirement_tbl3 %>%
    mutate(
        prop2 = format(round(prop, 2), nsmall = 2)
    ) %>%
    ggplot(aes(x=word, y=n, fill=word)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = c("#969696", "#08306b")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Are you saving for retirement?",
        subtitle = "Total Survey Responses (n = 422)",
        x = "",
        y = "Number of Responses"
    )

# Where do you get health insurance? ----

health_insurance_v <- as.vector(df1$health_insurance)
health_insurance_tbl <- tibble(line = 1:445, text = health_insurance_v)

# split string by comma
# unnest, then group,tally
# save as new df
health_insurance_tbl2 <- health_insurance_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    filter(n > 1)

# manually add items in same category
health_insurance_tbl2$n[2] <- 120
health_insurance_tbl2$n[3] <- 77
health_insurance_tbl2$n[6] <- 44

# select rows through slice, then visualize
health_insurance_tbl2 %>%
    slice(1:3, 6, 8) %>%
    ggplot(aes(x = n, y = reorder(phrase, n), fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.5) +
    scale_fill_manual(values = c("#08306b", "#08519c", "#2171b5",
                                 "#525252", "#969696")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(hjust = 1.1),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Where do you get health insurance?",
        subtitle = "Total Survey Responses (n = 422)",
        x = "Number of Respondents",
        y = ""
    )


# MISC: Which DAOs are providing health insurance?

df1 %>%
    select(daos_work_for, health_insurance) %>%
    view()


# Is your income stability a priority for you? ----


income_stability_v <- as.vector(df1$income_stability)
income_stability_tbl <- tibble(line = 1:445, text = income_stability_v)


income_stability_tbl %>%
    count(text, sort = TRUE) %>%
    view()


income_stability_tbl2 <- 
    
income_stability_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    mutate(
        phrase2 = str_match(phrase, "[Yy]es|[Nn]o")[,1],
        phrase2 = str_to_lower(phrase2)
    ) %>%
    view()
    
# create two separate columns to capture other "No" or "Yes" responses not available in first unnest
# then save as df, manually add up extra "no" and "yes"
income_stability_tbl2 <- income_stability_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    mutate(
        phrase_no = str_match(phrase, "[Nn]o|[Nn]ot")[,1],
        phrase_no = str_to_lower(phrase_no)
    ) %>%
    mutate(
        phrase_yes = str_match(phrase, "[Yy]es|[Yy]eah")[,1],
        phrase_yes = str_to_lower(phrase_yes)
    )

# manually check each row to make sure it does not contain *both* "no" and "yes"
# if contain both, choose one

income_stability_tbl2$phrase_no[14] <- NA
income_stability_tbl2$phrase_yes[241] <- NA
income_stability_tbl2$phrase_yes[244] <- NA
income_stability_tbl2$phrase_no[286] <- NA
income_stability_tbl2$phrase_no[288] <- NA
income_stability_tbl2$phrase_no[289] <- NA
income_stability_tbl2$phrase_no[290] <- NA
income_stability_tbl2$phrase_no[292] <- NA
income_stability_tbl2$phrase_no[297] <- NA
income_stability_tbl2$phrase_no[300] <- NA
income_stability_tbl2$phrase_no[305] <- NA
income_stability_tbl2$phrase_no[306] <- NA
income_stability_tbl2$phrase_no[311] <- NA
income_stability_tbl2$phrase_yes[317] <- NA
income_stability_tbl2$phrase_yes[318] <- NA
income_stability_tbl2$phrase_yes[319] <- NA
income_stability_tbl2$phrase_yes[320] <- NA
income_stability_tbl2$phrase_yes[321] <- NA
income_stability_tbl2$phrase_yes[324] <- NA


# group, tally, count all "phrase_no" (102 - 1) and "phrase_yes" (45 - 1)
# to manually add

income_stability_tbl2 %>%
    count(phrase_no, sort = TRUE) %>%
    view()

income_stability_tbl2 %>%
    count(phrase_yes, sort = TRUE) %>%
    view()

# go back to original phrase column to add "phrase_no" (101) and "phrase_yes" (44)

income_stability_tbl2$n[1] <- 150
income_stability_tbl2$n[3] <- 134

income_stability_tbl2 %>%
    slice(1,3) %>%
    ggplot(aes(x = phrase, y = n, fill = phrase)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.5) +
    scale_fill_manual(values = c("#525252", "#08519c")) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
    ) +
    labs(
        title = "Is your income stability a priority for you?",
        subtitle = "Total Survey Response (n = 422)",
        x = "",
        y = "Number of Responses"
    )
    

# Where are you from (city)? ----

df1$city

city_v <- as.vector(df1$city)
city_tbl <- tibble(line = 1:445, text = city_v)

city_tbl %>%
    mutate(phrase = strsplit(as.character(text), ",")) %>%
    unnest(phrase) %>%
    count(phrase, sort = TRUE) %>%
    drop_na() %>%
    view()

city_tbl %>%
    count(text, sort = TRUE) %>%
    view()



