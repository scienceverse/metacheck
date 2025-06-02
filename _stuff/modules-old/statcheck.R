stat_table <- papercheck::stats(paper)

if (nrow(stat_table) == 0) {
  tl = "na"
} else if (all(stat_table$error == "FALSE") ) {
  tl = "green"
} else {
  tl = "red"
}

list(
  table = stat_table[stat_table$error, ],
  traffic_light = tl
)
