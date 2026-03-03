# Menu-based Questionnaire Analyses
# Author: Alana Little, NEPHU (alana.little@austin.org.au)

################################################################################
# Create crosstabs
################################################################################
f_create_crosstab <- function(data, menu_items) {
  
  data <- map(menu_items, ~ {
    
    data %>%
      janitor::tabyl(.data[[.x]], symptomatic) %>%    
      #
      dplyr::mutate(menu_item = (.x)) %>% 
      # 
      dplyr::rename(consumed = (.x)) %>% 
      #
      dplyr::filter(!is.na(consumed)) %>%   
      #
      dplyr::mutate(consumed = factor(dplyr::case_when(
        consumed == 1 ~ "Yes",
        consumed == 0 ~ "No",
        TRUE          ~ NA_character_),
        #
        levels = c("Yes",
                   "No"))) %>%
      #
      tidyr::complete(consumed,
                      fill = list(not_unwell = 0,
                                  unwell     = 0,
                                  menu_item  = (.x))) %>% 
      #
      dplyr::arrange(consumed)
    
  })
  
  return(data)
  
}

################################################################################
# Calculate attack rates
################################################################################
f_calculate_attack <- function(data) {
  
  data <- data %>%
    dplyr::bind_rows() %>% 
    #
    dplyr::select(menu_item,
                  consumed,
                  unwell,
                  not_unwell) %>% 
    #
    dplyr::mutate(total = unwell + not_unwell,
                  #
                  attack_rate = round((unwell / total) * 100, digits = 0))
  
  return(data)
  
}

################################################################################
# Create univariate logistic regression models
################################################################################
f_create_univariate_model <- function(data, menu_items) {
  
  data <- menu_items %>% 
    stringr::str_c("unwell ~ ", .) %>% 
    #
    purrr::map(.f = ~glm(formula = as.formula(.x),
                         family  = "binomial",
                         data    = data)) %>% 
    #
    purrr::map(.f = ~broom::tidy(.x,
                                 exponentiate = TRUE,
                                 conf.int     = TRUE)) %>% 
    #
    dplyr::bind_rows() %>% 
    #
    dplyr::filter(term != "(Intercept)") %>% 
    #
    dplyr::select(menu_item  = term,
                  odds_ratio = estimate,
                  p_value    = p.value,
                  lower_ci   = conf.low,
                  upper_ci   = conf.high)
  
  return(data)
  
}

################################################################################
# Format table
################################################################################
f_format_table <- function(data) {
  
  data <- data %>% 
    dplyr::mutate(
      # Reclassify incalculable odds ratios to NA
      odds_ratio = dplyr::case_when(
        is.na(lower_ci) ~ NA_real_,
        is.na(upper_ci) ~ NA_real_,
        TRUE            ~ odds_ratio),
      # Format numeric values to 2 decimal places for pretty printing
      odds_ratio = dplyr::case_when(
        consumed == "No"        ~ " ",
        is.na(odds_ratio)       ~ NA_character_,
        is.infinite(odds_ratio) ~ NA_character_,
        TRUE                    ~ sprintf(fmt = "%#.2f", odds_ratio)),
      #
      p_value = dplyr::case_when(
        consumed == "No"     ~ " ",
        is.na(p_value)       ~ NA_character_,
        is.infinite(p_value) ~ NA_character_,
        is.na(odds_ratio)    ~ NA_character_,
        TRUE                 ~ sprintf(fmt = "%#.2f", p_value)),
      #
      lower_ci = dplyr::case_when(
        consumed == "No"      ~ " ",
        is.na(lower_ci)       ~ NA_character_,
        is.infinite(lower_ci) ~ NA_character_,
        is.na(odds_ratio)     ~ NA_character_,
        TRUE                  ~ sprintf(fmt = "%#.2f", lower_ci)),
      #
      upper_ci = dplyr::case_when(
        consumed == "No"      ~ " ",
        is.na(upper_ci)       ~ NA_character_,
        is.infinite(upper_ci) ~ NA_character_,
        is.na(odds_ratio)     ~ NA_character_,
        TRUE                  ~ sprintf(fmt = "%#.2f", upper_ci)))
  
  table <- data %>% 
    knitr::kable(table.attr = "style = \"color: black;\"",
                 align      = "lcrrrrrrrr",
                 col.names  = c("Menu item", "Ate item", "Unwell", "Not unwell", 
                                "Total", "Attack rate", "Odds ratio", 
                                "p-value", "Lower 95% CI", "Upper 95% CI")) %>%
    #
    kableExtra::kable_classic_2(full_width = TRUE,
                                html_font  = "Arial",
                                font_size  = 13) %>%
    #
    kableExtra::column_spec(column = 1,
                            width  = "2in") %>%
    #
    kableExtra::column_spec(column = 2,
                            width  = "1.25in") %>%
    #
    kableExtra::column_spec(column = 3:10,
                            width  = "1in") %>%
    #
    kableExtra::row_spec(row       = 0,
                         bold      = TRUE,
                         extra_css = "border-top: 2px solid lightgray; border-bottom: 2px solid lightgray;") %>%
    #
    kableExtra::row_spec(row       = seq(2, nrow(data) - 2, by = 2),
                         extra_css = "border-bottom: 1px solid lightgray;") %>%
    #
    kableExtra::collapse_rows(columns = 1,
                              valign  = "top")
  
  return(table)
  
}

