#' Gets a league's base url from the MFL API and stores it in a database.
#'
#' @param league_id An MFL league identifier
#'
#' @return A tibble with the base URL string for the league and an updated date.
#' @export
get_base_url <- function(league_id) {
  league_json <- tryCatch(jsonlite::fromJSON(glue::glue("https://www.myfantasyleague.com/{season}/export?TYPE=league&L={league_id}&JSON=1")),
                          error = function(e) {return(NA)})

  base_url <- tibble::tibble(base_url = league_json$league$baseURL) |>
    dplyr::mutate(updated_date = as.character(Sys.Date()))

  conn <- DBI::dbConnect(RSQLite::SQLite(), "data/base-url.sqlite")
  DBI::dbWriteTable(conn, "base_url", base_url, overwrite = TRUE)
  DBI::dbDisconnect(conn)

  base_url

}

#' Loads the base URL for a league from a database or requests a updated URL from MFL.
#'
#' @param league_id An MFL league identifier
#'
#' @importFrom dplyr collect mutate summarize tbl select
#' @importFrom cli cli_alert_info
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @return A tibble with the base URL string for the league and an updated date.
#' @export
load_base_url <- function(league_id) {
  if(!file.exists("data")) {
    dir.create(file.path(getwd(), "data"))
  }

  if(!file.exists("data/base-url.sqlite")) {
    cli::cli_alert_info("Creating a new base_url database.")

    get_base_url(league_id)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), "data/base-url.sqlite")
  # get the updated date in the database
  db_updated <- dplyr::tbl(conn, "base_url") |>
    dplyr::collect() |>
    dplyr::mutate(updated_date = as.Date(updated_date)) |>
    dplyr::summarize(updated_date = max(updated_date))

  # if the db was updated today, then used the cached data
  # if not, make an API call to MFL.
  if(db_updated$updated_date == Sys.Date()) {
    cli::cli_alert_info("Reading base_url from database.")

    base_url <- dplyr::tbl(conn, "base_url") |>
      dplyr::select(base_url) |>
      dplyr::collect()
  } else {
    cli::cli_alert_info("Calling the MFL API.")

    base_url <- get_base_url(league_id) |>
      dplyr::select(base_url)
  }

  DBI::dbDisconnect(conn)

  return(base_url)
}

#' Get all player names in a league
#'
#' @param season An NFL season
#' @param league_id An MFL league identifier
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#'
#' @return A tibble of player data including draft_team, name, position, team, draft_year,
#' stats_global_id, id, twitter_username, fleaflicker_id, stats_id, cbs_id, sportsdata_id,
#' rotowire_id, height, rotoworld_id, college, draft_round, weight, jersey, birthdate, draft_pick, espn_id,
#' nfl_id, and a binary flag for rookie
#'
#' @importFrom dplyr select mutate
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @export
#'
#' @examples
#' # api_key <- "your API key here"
#' # league_id <- 59406
#' # season <- 2023
#' # get_players(season, league_id, api_key)
get_players <- function(season, league_id, api_key) {
  base_url <- load_base_url(league_id) |>
    dplyr::pull(base_url)

  players_json <- tryCatch(jsonlite::fromJSON(glue::glue("{base_url}/{season}/export?TYPE=players&L={league_id}&APIKEY={api_key}&DETAILS=1&SINCE=&PLAYERS=&JSON=1")),
                           error = function(e) {return(NA)})

  players <- players_json$players$player |>
    dplyr::mutate(rookie = status,
                  id = as.numeric(id)) |>
    dplyr::select(-status) |>
    dplyr::mutate(updated_date = as.character(Sys.Date()))

  conn <- DBI::dbConnect(RSQLite::SQLite(), "data/mfl-players.sqlite")
  DBI::dbWriteTable(conn, "players", players, overwrite = TRUE)
  DBI::dbDisconnect(conn)

  return(players)
}

#' Load players data from a cached database if updated recently. We want to limit
#' the number of API calls we make to MFL.
#'
#' @param season An NFL season
#' @param league_id An MFL league identifier
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#'
#' @importFrom cli cli_alert_info
#' @importFrom dplyr select collect mutate summarize tbl
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
#'
#' @return A tibble of player data including draft_team, name, position, team, draft_year,
#' stats_global_id, id, twitter_username, fleaflicker_id, stats_id, cbs_id, sportsdata_id,
#' rotowire_id, height, rotoworld_id, college, draft_round, weight, jersey, birthdate, draft_pick, espn_id,
#' nfl_id, and a binary flag for rookie
#' @export
#'
#' @examples
#' # api_key <- "your API key here"
#' # league_id <- 59406
#' # season <- 2023
#' # load_players(season, league_id, api_key)
load_players <- function(season, league_id, api_key) {
  if(!file.exists("data")) {
    dir.create(file.path(getwd(), "data"))
  }

  if(!file.exists("data/mfl-players.sqlite")) {
    cli::cli_alert_info("Creating a new player database.")

    get_players(season, league_id, api_key)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), "data/mfl-players.sqlite")
  # get the updated date in the database
  db_updated <- dplyr::tbl(conn, "players") |>
    dplyr::select(updated_date) |>
    dplyr::collect() |>
    dplyr::mutate(updated_date = as.Date(updated_date)) |>
    dplyr::summarize(updated_date = max(updated_date))

  # if the db was updated today, then used the cached data
  # if not, make an API call to MFL. Assures we don't overuse the API
  if(db_updated$updated_date == Sys.Date()) {
    cli::cli_alert_info("Reading players data from database.")

    player_names <- dplyr::tbl(conn, "players") |>
      dplyr::select(id, name, position, team, draft_year) |>
      dplyr::collect()
  } else {
    cli::cli_alert_info("Calling the MFL API.")

    player_names <- get_players(season, league_id, api_key) |>
      dplyr::select(id, name, position, team, draft_year)
  }

  DBI::dbDisconnect(conn)

  return(player_names)
}

#' Title
#'
#' @param season An NFL season
#' @param league_id An MFL league identifier
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' @param current_week The most current week for which data exists
#'
#' @importFrom cli cli_alert
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom tibble tibble
#' @importFrom dplyr mutate bind_rows left_join select
#'
#' @return a tibble of player stats for each week
#' @export
get_player_per_game_scores <- function(season, league_id, api_key, current_week) {
  weeks <- tibble::tibble()
  base_url <- load_base_url(league_id) |>
    dplyr::pull(base_url)

  for(week in 1:current_week) {
    cli::cli_alert(glue::glue("Getting week {week} player scores"))

    player_points <- tryCatch(jsonlite::fromJSON(glue::glue("{base_url}/{season}/export?TYPE=playerScores&L={league_id}&W={week}&YEAR={season}&PLAYERS=&POSITION=&STATUS=&RULES=&COUNT=&JSON=1")),
                              error = function(e) {return(NA)})
    Sys.sleep(2) # being polite to the MFL API

    # check in case the user enters a week in the future for which there is no data
    if(player_points[["playerScores"]][["playerScore"]][["score"]][1] != "") {
      points <- player_points[["playerScores"]][["playerScore"]] |>
        dplyr::mutate(id = as.numeric(id))

      weeks <- dplyr::bind_rows(weeks, points)
    }
  }

  players <- load_players(season, league_id, api_key)

  weeks <- weeks |>
    dplyr::left_join(players, by = "id") |>
    dplyr::mutate(season = as.numeric(season)) |>
    dplyr::select(id, name, position, team, season, week, score, draft_year)
}

#' Get all teams from a given MFL league
#'
#' @param season An NFL season
#' @param league_id An MFL league identifier
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom cli cli_abort
#' @importFrom dplyr rename mutate select
#'
#' @return a tibble of team data including team id, team name, division, available cap,
#' and conference
#' @export
get_teams <- function(season, league_id, api_key) {
  base_url <- load_base_url(league_id) |>
    dplyr::pull(base_url)

  ifl_teams_json <- tryCatch(jsonlite::fromJSON(glue::glue("{base_url}/{season}/export?TYPE=league&L={league_id}&JSON=1")),
                             error = function(e) {return(NA)})

  Sys.sleep(2)

  if(!is.null(ifl_teams_json)) {
    ifl_teams <- ifl_teams_json[["league"]][["franchises"]][["franchise"]] |>
      dplyr::rename(team_id = id) |>
      dplyr::mutate(team_id = as.numeric(team_id),
                    available_cap_M = as.numeric(bbidAvailableBalance) / 1000000,
                    conference = ifelse(team_id %in% c(1,2,19:32), "AFC", "NFC")) |>
      dplyr::select(team_id, team_name = name, division, available_cap_M, conference)
  } else (
    cli::cli_abort("MFL returned an error.")
  )


}

#' Get the rosters for each team for a given week. Includes players on Taxi and
#' IR.
#'
#' @param league_id An MFL league ID
#' @param season An NFL season
#' @param week A week in the NFL season
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom glue glue
#' @importFrom cli cli_abort
#' @importFrom dplyr left_join mutate select bind_rows
#' @importFrom tibble tibble
#'
#' @return A tibble of rosters for each team in the league for a given week
#' @export
get_week_roster <- function(league_id, season, week, api_key) {
  base_url <- load_base_url(league_id) |>
    dplyr::pull(base_url)

  rosters_json <- tryCatch(jsonlite::fromJSON(glue::glue("{base_url}/{season}/export?TYPE=rosters&L={league_id}&APIKEY={api_key}&FRANCHISE=&W={week}&JSON=1")),
                           error = function(e) {return(NA)})

  if(!is.null(rosters_json[["rosters"]][["franchise"]][["id"]])){
    team_rosters <- tibble::tibble()

    for(team in as.numeric(rosters_json[["rosters"]][["franchise"]][["id"]])) {

      df <- rosters_json[["rosters"]][["franchise"]][["player"]][[team]] |>
        dplyr::mutate(team_id = team,
                      id = as.numeric(id),
                      salary = as.numeric(salary) / 1000000)

      team_rosters <- dplyr::bind_rows(team_rosters, df)
    }
  }

  teams <- get_teams(season, league_id, api_key)

  player_names <- load_players(season, league_id, api_key)

  team_rosters <- team_rosters |>
    dplyr::left_join(teams, by = "team_id") |>
    dplyr::left_join(player_names, by = "id")
}

#' Gets weekly scores for each team's players up to the current week.
#' Includes players on Taxi and IR.
#'
#' @param season An NFL season
#' @param league_id An MFL league ID
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#' @param current_week The current NFL week. If the season is over, use 18.
#'
#' @importFrom glue glue
#' @importFrom cli cli_alert
#' @importFrom dplyr mutate select left_join arrange bind_rows
#' @importFrom tibble tibble
#'
#' @return A tibble of player scores by team
#' @export
get_weekly_results <- function(season, league_id, api_key, current_week) {
  weeks <- tibble::tibble()

  for(week in 1:current_week) {
    cli::cli_alert(glue::glue("Getting week {week} rosters"))

    week_rosters <- get_week_roster(league_id, season, week, api_key) |>
      dplyr::mutate(week = week)

    Sys.sleep(2) # being polite to the MFL API

    weeks <- dplyr::bind_rows(weeks, week_rosters)
  }

  player_scores <- get_player_per_game_scores(season, league_id, api_key, current_week) |>
    dplyr::select(id, week, score) |>
    dplyr::mutate(week = as.integer(week))

  weeks <- weeks |>
    dplyr::left_join(player_scores, by = c("id", "week")) |>
    dplyr::mutate(score = as.numeric(score)) |>
    dplyr::mutate(score = ifelse(is.na(score), 0, score)) |>
    dplyr::arrange(week, conference, division, team_name) |>
    dplyr::select(conference, division, team_name, week, id, name, position, score, status)

  return(weeks)
}

#' Using linear programming, solves for the optimal 21 player lineup given
#' a series of constraints.
#'
#' @param roster A roster to optimize for.
#'
#' @importFrom lpSolveAPI make.lp set.objfn lp.control set.type add.constraint get.variables
#' @importFrom dplyr rename bind_cols filter
#' @importFrom tibble tibble
#'
#' @return a tibble of 21 players
#' @export
solve_for_pp <- function(roster) {

  num_players <- nrow(roster)
  lp_model = lpSolveAPI::make.lp(0, num_players)
  lpSolveAPI::set.objfn(lp_model, roster$score)

  lpSolveAPI::lp.control(lp_model, sense = "max")
  lpSolveAPI::set.type(lp_model, 1:num_players, "binary")
  # List position-specific constraints
  # HC
  lpSolveAPI::add.constraint(lp_model, roster$coach_check, "=", 1)
  # QB
  lpSolveAPI::add.constraint(lp_model, roster$qb_check, "=", 1)
  # PK
  lpSolveAPI::add.constraint(lp_model, roster$pk_check, "=", 1)
  # PN
  lpSolveAPI::add.constraint(lp_model, roster$pn_check, "=", 1)
  # Offense
  lpSolveAPI::add.constraint(lp_model, roster$off_check, "=", 1)
  # RBs 0-3
  lpSolveAPI::add.constraint(lp_model, roster$rb_check, "<=", 3)
  # WRs 0-5
  lpSolveAPI::add.constraint(lp_model, roster$wr_check, "<=", 5)
  # TE 0-3
  lpSolveAPI::add.constraint(lp_model, roster$te_check, "<=", 3)
  # DEs
  lpSolveAPI::add.constraint(lp_model, roster$de_check, "=", 2)
  # DTs
  lpSolveAPI::add.constraint(lp_model, roster$dt_check, ">=", 1)
  lpSolveAPI::add.constraint(lp_model, roster$dt_check, "<=", 2)
  # CBs
  lpSolveAPI::add.constraint(lp_model, roster$cb_check, ">=", 2)
  lpSolveAPI::add.constraint(lp_model, roster$cb_check, "<=", 3)
  # Ss
  lpSolveAPI::add.constraint(lp_model, roster$s_check, ">=", 2)
  lpSolveAPI::add.constraint(lp_model, roster$s_check, "<=", 3)
  # LBs
  lpSolveAPI::add.constraint(lp_model, roster$lb_check, ">=", 2)
  lpSolveAPI::add.constraint(lp_model, roster$lb_check, "<=", 4)
  # skill position check
  lpSolveAPI::add.constraint(lp_model, roster$skill_pos_check, "=", 5)
  # total number of players needed
  lpSolveAPI::add.constraint(lp_model, roster$player_count, "=", 21)

  solve(lp_model)

  optimals <- lpSolveAPI::get.variables(lp_model)

  optimals <- tibble::as_tibble(optimals) |>
    dplyr::rename(optimals = value)

  roster <- roster |>
    dplyr::bind_cols(optimals) |>
    dplyr::filter(optimals == 1)

}

#' Master function that calculates the top 21 players for each team
#' in a given week subject to positional constraints
#'
#' @param season An NFL season
#' @param league_id An MFL league ID
#' @param api_key An MFL api key. See Help > Developer API while logged into
#' any of your leagues on myfantasyleague.com and scroll to the bottom of the page.
#' @param current_week The current NFL week. If the season is over, use 18.
#' @param force_update Defaults to FALSE. Set to TRUE if you are getting errors like "Couldn't resolve host name"
#'
#' @return A filtered tibble of roster data with the top 21 players on each team
#' for each week of play, given the roster constraints defined in solve_for_pp
#'
#' @importFrom dplyr arrange mutate bind_rows select
#' @importFrom tidyr crossing
#' @importFrom purrr pmap
#'
#' @export
calculate_top_21 <- function(season, league_id, api_key, current_week, force_update = FALSE) {

  if(force_update == TRUE) {
    get_base_url(league_id)
  }

  lineups <- get_weekly_results(season, league_id, api_key, current_week)

  # hot encode postions
  add_checks <- lineups |>
    dplyr::arrange(week, conference, division, team_name, -score) |>
    dplyr::mutate(qb_check = ifelse(position == "QB", 1, 0),
                  coach_check = ifelse(position == "Coach", 1, 0),
                  pk_check = ifelse(position == "PK", 1, 0),
                  pn_check = ifelse(position == "PN", 1, 0),
                  off_check = ifelse(position == "Off", 1, 0),
                  de_check = ifelse(position == "DE", 1, 0),
                  rb_check = ifelse(position == "RB", 1, 0),
                  te_check = ifelse(position == "TE", 1, 0),
                  wr_check = ifelse(position == "WR", 1, 0),
                  dt_check = ifelse(position == "DT", 1, 0),
                  cb_check = ifelse(position == "CB", 1, 0),
                  s_check = ifelse(position == "S", 1, 0),
                  lb_check = ifelse(position == "LB", 1, 0),
                  skill_pos_check = ifelse(position %in% c("RB", "WR", "TE"), 1, 0),
                  player_count = 1)

  # sort of weird to have a nested function here, but I didn't want to pass
  # around the add_checks tibble. There are surely better ways to do this but it
  # works for my purposes.
  get_optimals <- function(teams, weeks) {

    filtered_df <- add_checks |> dplyr::filter(week == weeks,
                                               team_name == teams)

    optimum_lu <- solve_for_pp(filtered_df)

  }

  team_weeks <- tidyr::crossing(teams = unique(add_checks$team_name),
                                weeks = 1:max(add_checks$week))

  all_optimals <- purrr::pmap(list(team_weeks$teams, team_weeks$weeks), get_optimals)

  all_optimals_df <-  dplyr::bind_rows(all_optimals, .id = "column_label")

  filtered_optimals_df <- all_optimals_df |>
    dplyr::select("conference", "division", "team_name", "week",
                  "id", "name", "position", "score", "status", "optimals")

}

#' Make a table of lowest potential points scored by team, filtered to the AFC
#'
#' @param df A tibble returned from calculate_top_21
#'
#' @importFrom dplyr filter mutate group_by summarize arrange
#' @importFrom gt gt tab_header
#' @importFrom gtExtras gt_theme_538
#'
#' @return a GT object. Use View() to send it to the RStudio viewer or save, print, etc.
#' @export
make_afc_pp_table <- function(df) {

  the_week <- max(df$week)

  table <- df |>
    dplyr::filter(conference == "AFC") |>
    dplyr::mutate(pp = as.numeric(score)) |>
    dplyr::group_by(team_name) |>
    dplyr::summarise(PP = round(sum(pp), 1)) |>
    dplyr::arrange(PP) |>
    dplyr::rename(team = team_name) |>
    gt::gt() |>
    gtExtras::gt_theme_538() |>
    gt::tab_header(title = glue::glue("Who has the smallest PP in the AFC?"),
                   subtitle = glue::glue("Through Week {the_week}"))

}

#' Make a table of lowest potential points scored by team, filtered to the NFC
#'
#' @param df A tibble returned from calculate_top_21
#'
#' @importFrom dplyr filter mutate group_by summarize arrange
#' @importFrom gt gt tab_header
#' @importFrom gtExtras gt_theme_538
#'
#' @return a GT object. Use View() to send it to the RStudio viewer or save, print, etc.
#' @export
make_nfc_pp_table <- function(df) {

  the_week <- max(df$week)

  df |>
    dplyr::filter(conference == "NFC") |>
    dplyr::mutate(pp = as.numeric(score)) |>
    dplyr::group_by(team_name) |>
    dplyr::summarise(PP = round(sum(pp), 1)) |>
    dplyr::arrange(PP) |>
    dplyr::rename(team = team_name) |>
    gt::gt() |>
    gtExtras::gt_theme_538() |>
    gt::tab_header(title = glue::glue("Who has the smallest PP in the NFC?"),
                   subtitle = glue::glue("Through Week {the_week}"))

}
