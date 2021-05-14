library(tidyverse)

# read in dataframe
role_df <- read_csv('role_df.csv')

# change column names
role_df2 <- role_df %>%
    rename(
        index = `X1`,
        roles = `0`
    )

# aggregate data
# filter for roles with at least 10 volunteers
role_df2 %>%
    group_by(roles) %>%
    tally(sort = TRUE) %>%
    filter(n > 1) %>%
    ggplot(aes(x = reorder(roles, -n), y = n, fill = roles)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.25) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
        y = "Number of Volunteers",
        x = "Roles",
        title = "Talent at BanklessDAO",
        subtitle = "Roles with more than 1 volunteers",
        caption = "Data: BanklessDAO | Analysis: @Airbayer, @paulapivat"
    )
    

# filter for LESS than 10 volunteers
role_df2 %>%
    group_by(roles) %>%
    tally(sort = TRUE) %>%
    filter(n < 10) %>% 
    ggplot(aes(x = reorder(roles, -n), y = n, fill = roles)) +
    geom_col() +
    geom_text(aes(label = n), vjust = -0.25) +
    theme_minimal() +
    theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    coord_flip()
    labs(
        y = "Number of Volunteers",
        x = "Roles",
        title = "Talent at BanklessDAO",
        subtitle = "Roles with at least 10 volunteers",
        caption = "Authors: @Airbayer, @paulapivat"
    )

# NOTE: first draft 
