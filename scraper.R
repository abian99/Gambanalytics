library(rvest)
require(stringr)

isfl_index <- read_html("https://index.sim-football.com/ISFLS38/GameResults.html")

weeks <- isfl_index %>%
  html_nodes(".left") %>%
  html_text()

for(week in weeks){
  week <- str_trim(str_replace_all(week, fixed("(top)"), ""))
  print(week)
}

teams <- isfl_index %>%
  html_nodes(".width: 99%") %>%
  html_text()
teams