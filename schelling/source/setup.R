### PREPARE RESPONSE DATA ###

## MAKE HUMAN-FRIENDLY ##

# Sensible column names
# The resulting google sheet from a google form uses the full text of each question as the column name, which is impractical for analysis.

responses <- responses_raw

colnames(responses) <- c('timestamp', 'stranger_where', 'stranger_when', 'student_where', 'friend_where', 'coin_same', 'coin_different', 'number_0to10', 'number_selection', 'number_big', 'uc_status', 'demos', 'language', 'stranger_error','student_error','friend_error')

responses <- responses %>% 
    mutate(across(ends_with("error"), replace_na, FALSE)) #TODO: Update for new across() syntax
    
    
    
# There should not be any variables that are lists, but if NAs aren't being read as NAs, there might be
# This is a common issue with google forms data
# These lines will convert any list columns to character columns
list_cols <- which(sapply(responses,class)=="list")
responses[,list_cols] <- sapply(responses[,list_cols], as.character)


## ACCOUNT FOR DIFFERENT RESPONDENT GROUPS ##

# Assign participants to groups based on timestamps of their responses
# This survey has been used to collect data and give examples across multiple years and in multiple settings/audiences. 
# We might want to account for differences across time (e.g. mid-COVID responses could be odd; dorms & establishments open and close over the years; seasons in Chicago may impact indoor/outdoor meeting choices)
# We might want to account for differences across audiences (e.g. students in a 200 person audience might feel more anonymous and be more candid or be paying less attention; housing and on-campus time are different between undergrad and grad students)

# UPDATE REGULARLY!!
# April/May 2020: Undergraduates in a remote quarter of the large lecture class "Mind" (this was the 1st quarter of remote learning so all students had at least 2 quarters of experience being on campus)
# February 1st 2023: Attendees at the SGM lab meeting; primarily psychology graduate students and postdocs
# February 2nd 2023: Undergrads in Dowling section of Mind responding to the survey in class
# February 7th-9th 2023: Undergrads in Mind; most responded in-person during the lecture on 2/7 but some watched the lecture and responded in the following few days 
# February 2023 besides the dates above: "testers" who all had experience living and attending/working at UChicago but were otherwise diverse
# November 2023: Graduate students in MAPS 300 (MAPSS-Psychology)

# timestamp value listed in POSIX format; create date variable extracting just the date in YEAR-MONTH-DAY format
# Demo items were not included in the survey when administered for some groups, so we infer uc_status / group membership from the timestamp when necessary
# Note that UC status has been added for a few specific responses that were confirmed by the respondent 
# Remember no one is claiming this is not a rigorous study!

responses <- responses %>% 
    mutate(date = as.Date(timestamp)) %>% 
    mutate(uc_status = case_when(
        !(is.na(uc_status)) ~ uc_status,
        date < as.Date("2020-06-01") ~ "UChicago undergraduate student",
        date == as.Date("2023-02-01") ~ "UChicago PhD student",
        date == as.Date("2023-02-02") ~ "UChicago undergraduate student",
        (date >= as.Date("2023-02-07") & date <= as.Date("2023-02-10")) ~ "UChicago undergraduate student",
        TRUE ~ "Other/Unknown"
    ))


### MISC & AESTHETIC

# "Random" contrasting colors generated with http://phrogz.net/css/distinct-colors.html
# We need to communicate that there is a lot of variation in responses. We'd like to be able to clearly discern what the most common responses are (ie map color to factor level) but it's not important to be able to visually map most colors to their labels. 
# Sites like this one ^^ can generate n colors and interweave their order to meet these needs. ggplot can pick colors on its own, but chances are that with this many color values it will be pretty ugly. Feeding in a pre-set, pre-sorted list of hex colors can make the chaos a little easier to look at for everyone *and* you can strategically create palettes that are (relatively) colorblind friendly.

colors_string <- "#ff9e9c, #00c28b, #ed88db, #bd3f35, #008862, #ff00c1, #eb6758, #00eeb3, #9b3781, #ff9e8a, #004638, #ffa2dc, #be3e22, #30debd, #ff009f, #ed6745, #006c5a, #d76cae, #ff9d77, #00a58c, #b80071, #cac6bf, #00c3be, #841759, #b2aa9b, #006d6a, #6a003a, #615e58, #004647, #9a0052, #494640, #00f0f4, #ff0085, #33302b, #00a7af, #cd0067, #d4c5a3, #00d2e7, #b14276, #8a8374, #006e7a, #ff006d, #675d48, #008da2, #fa86b8, #92825a, #004756, #bf004c, #37301c, #003256, #9f0036, #c2a964, #23496f, #6e0026, #514629, #3e6088, #ff004d, #98823e, #6d85a8, #e1698c, #3f2f00, #42536c, #a83352, #6e5d25, #263140, #ffa0b8, #554500, #a9c9f6, #c30030, #007900, #bcc7db, #700018, #75dd7a, #7e848e, #ff0035, #003e07, #595f68, #a1001d, #005111, #424750, #c4001c, #00a32d, #c5c6c8, #ff8494, #006b1e, #838485, #ac313a, #00ed78, #464748, #710005, #00c166, #303032, #a20004, #006c39, #ff5bfa, #e96869, #009858, #610057, #004629, #7c1b70"

# The site didn't really give us comma-separated values, it gave us a string. We have to make it a vector
colors_100 <- str_split_1(colors_string, ", ")


# Quick labels for in-text reference to specific values

stranger_times <- responses %>% 
    filter(!is.na(stranger_when)) %>%     # filter out any NA values
    mutate(stranger_when = format(as.POSIXct(stranger_when), "%H:%M"), # extract only the time (hours and minutes) from the posix since the date is meaningless
           stranger_when = as.factor(stranger_when)) # make it a factor so we can count it to graph
           
# ?Q?: Why do some of these functions produce numbers with an L at the end? Is it a problem? What should we do about it (if anything)?

# count the number of times that the time (stranger_when) is on the hour
on_the_hour <- sum(grepl("*:00", stranger_times$stranger_when)) 

# on the half-hour
on_the_thirty <- sum(grepl("*:30", stranger_times$stranger_when))

# on anything else
on_anything_else <- sum(grepl("*:00|*:30", stranger_times$stanger_when)==FALSE)

most_pop_time <- levels(fct_infreq(stranger_times$stranger_when))[1]

# total people meeting at the most popular time
love_connections <- sum(most_pop_time == stranger_times$stranger_when)

# add a column that checks whether a respondent's meeting time is unique
unique_times <- sum(ifelse(duplicated(stranger_times$stranger_when), FALSE, TRUE))

# singular or plural text following
lonely_hearts <- ifelse(unique_times ==1, "person", "people")

# Same as above but ignoring on-the-hour and -half-hour times

unique_times_other <- sum(ifelse(duplicated(filter(stranger_times, grepl("*:00|*:30", stranger_when)==FALSE)), FALSE, TRUE))

lonely_hearts_other <- ifelse(unique_times_other ==1, "person", "people")



