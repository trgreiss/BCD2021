library(tidyverse)

# Initial data load
womens <- read_csv("https://raw.githubusercontent.com/bigdatacup/Big-Data-Cup-2021/main/hackathon_womens.csv") %>%
  filter(str_detect(`Home Team`,"Olympic")) %>%
  select(Team:`Detail 2`,`Player 2`) %>%
  rowid_to_column("rn")

rosters <- womens %>%
  count(Team, Player) %>%
  select(Team, Player) %>%
  mutate(Team = str_sub(Team,19))

# goals
goals <- womens %>%
  filter(Event == "Goal")
n.goals <- nrow(goals)


# scoring plays
scoring.index <- womens %>%
  filter(Event == "Goal")
scoring.rns.prelim <- c(scoring.index$rn,scoring.index$rn-1,scoring.index$rn-2,scoring.index$rn-3,scoring.index$rn-4)
scoring.prev <- womens %>%
  filter(rn %in% scoring.rns.prelim) %>%
  add_column(goal.group = rep(1:45,each=5)) %>%
  group_by(goal.group) %>%
  filter(Team == Team[which.max(rn)],
         Player != Player[which.max(rn)],
         Event %in% c("Play","Shot"))
scoring.rns <- scoring.prev$rn

# faceoff component
faceoff.wins <- womens %>%
  filter(Event == "Faceoff Win") %>%
  count(Player,name='wins')
faceoff.losses <- womens %>%
  filter(Event == "Faceoff Win") %>%
  count(`Player 2`,name='losses')
faceoffs <- full_join(faceoff.wins,faceoff.losses,by=c("Player" = "Player 2")) %>%
  replace_na(list(wins = 0, losses = 0))

# puck recovery
recovery <- womens %>%
  filter(Event == "Puck Recovery")

# dump in/out
dump <- womens %>%
  filter(Event == "Dump In/Out") %>%
  count(`Event`,Player,`Detail 1`)

# zone entry
zone.entry.offense <- womens %>%
  filter(Event == "Zone Entry", 
         `Detail 1` != "Dumped") %>%
  count(Event,Player,`Detail 1`)
zone.entry.defense <- womens %>%
  filter(Event == "Zone Entry",
         `Detail 1` != "Dumped") %>%
  count(Event,`Player 2`,`Detail 1`)

# play
play <- womens %>%
  filter(Event == "Play",
         ! rn %in% scoring.rns)

# incomplete play
incomplete.play <- womens %>%
  filter(Event == "Incomplete Play",
         ! rn %in% scoring.rns)

# takeaway
takeaway <- womens %>%
  filter(Event == "Takeaway") %>%
  count(Player)

# SOG
shot <- womens %>%
  filter(Event == "Shot",
         `Detail 2` == "On Net",
         ! rn %in% scoring.rns)

# shot attempt
shot.attempt <- womens %>%
  filter(Event == "Shot",
         `Detail 2` != "On Net",
         ! rn %in% scoring.rns)

# penalty taken
penT <- womens %>%
  filter(Event == "Penalty Taken")

# penalty drawn
penD <- womens %>%
  filter(Event == "Penalty Taken")

# frequencies to goals
goal.frequency <- n.goals / nrow(goals)
scoring.play.frequency <- n.goals / nrow(scoring.prev)
faceoff.win.frequency <- n.goals / sum(faceoffs$wins)
faceoff.loss.frequency <- -1 * faceoff.win.frequency
recovery.frequency <- n.goals / nrow(recovery)
dump.loss.frequency <- -1 * n.goals / sum(dump %>% filter(`Detail 1` == "Lost") %>% .$n)
dump.retain.frequency <- -1 * dump.loss.frequency
zone.entry.offense.frequency <- n.goals / sum(zone.entry.offense$n)
zone.entry.defense.frequency <- -1 * zone.entry.offense.frequency
play.frequency <- n.goals / nrow(play)
incomplete.play.frequency <- -1 * play.frequency
takeaway.frequency <- n.goals / sum(takeaway$n)
sog.frequency <- n.goals / nrow(shot)
shot.attempt.frequency <- n.goals / nrow(shot.attempt)
penT.frequency <- -29/156
penD.frequency <- -1 * penT.frequency

frequency.table <- tibble(Event = c("Goal",
                                    "Scoring Play",
                                    "Faceoff Win",
                                    "Faceoff Loss",
                                    "Puck Recovery",
                                    "Dump In/Out Lost",
                                    "Dump In/Out Retained",
                                    "Zone Entry Offense",
                                    "Zone Entry Defense",
                                    "Play",
                                    "Incomplete Play",
                                    "Takeaway",
                                    "SOG",
                                    "Shot Attempt",
                                    "Penalty Taken",
                                    "Penalty Drawn"),
                          weight = c(goal.frequency,
                                     scoring.play.frequency,
                                     faceoff.win.frequency,
                                     faceoff.loss.frequency,
                                     recovery.frequency,
                                     dump.loss.frequency,
                                     dump.retain.frequency,
                                     zone.entry.offense.frequency,
                                     zone.entry.defense.frequency,
                                     play.frequency,
                                     incomplete.play.frequency,
                                     takeaway.frequency,
                                     sog.frequency,
                                     shot.attempt.frequency,
                                     penT.frequency,
                                     penD.frequency
                                     ))

# data re-shaping

faceoff.losses.bind <- womens %>%
  filter(Event == "Faceoff Win") %>%
  mutate(Event = "Faceoff Loss",
         Player = `Player 2`,
         `X Coordinate` = 200 - `X Coordinate`,
         `Y Coordinate` = 85 - `Y Coordinate`)

zone.entry.defense.bind <- womens %>%
  filter(Event == "Zone Entry") %>%
  mutate(Event = "Zone Entry Defense",
         Player = `Player 2`,
         `X Coordinate` = 200 - `X Coordinate`,
         `Y Coordinate` = 85 - `Y Coordinate`)

penalty.drawn.bind <- womens %>%
  filter(Event == "Penalty Taken") %>%
  mutate(Event = "Penalty Drawn",
         Player = `Player 2`)


womens.final <- womens %>%
  mutate(Event = if_else(Event == "Dump In/Out",
                         if_else(`Detail 1` == "Retained",
                                 "Dump In/Out Retained",
                                 "Dump In/Out Lost"),
                         Event),
         Event = case_when(
           rn %in% scoring.rns ~ "Scoring Play",
           Event == "Zone Entry" ~ "Zone Entry Offense",
           rn %in% shot$rn ~ "SOG",
           rn %in% shot.attempt$rn ~ "Shot Attempt",
           TRUE ~ Event
           )
         ) %>%
  bind_rows(faceoff.losses.bind,
            zone.entry.defense.bind,
            penalty.drawn.bind) %>%
  left_join(frequency.table,
            by = "Event") %>%
  select(-Team) %>%
  left_join(rosters,
            by = "Player") %>%
  select(Player,Team,Event,`X Coordinate`,`Y Coordinate`,weight) %>%
  drop_na() %>%
  mutate(weight = round(weight,3))

rink.img <- png::readPNG("rink.png")