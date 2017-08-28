#' Construct economic data used in PNAS paper
#'
#' This function will calculate the revenue for a watershed-year combination
#' for watersheds that have a certain amount of prairie.
#'
#' @param prairie_cost_per_acre A scalar indicating how much the prairie per
#' acre in dollars.
#' @return A \code{data.frame} containing the columns PI, source, watershed,
#' year, response, and value.
#' @import STRIPSMeta
#' @import dplyr
#' @export
#' @examples
#' d <- pnas_data()
#' summary(d)
#'
pnas_data <- function(prairie_cost_per_acre = 95) {
  hectares_per_acre = 0.404686

  watersheds <- STRIPSMeta::watersheds  %>%
    mutate(prairie_prop = prairie_pct/100) %>%
    select(watershed, prairie_prop)

  # Calculate watershed total revenue which includes cost of prairie
  d <- revenue %>%
    left_join(watersheds) %>%
    mutate(value = (1-prairie_prop) * revenue - prairie_prop * prairie_cost_per_acre,
           value = value / hectares_per_acre) %>%
    filter(year>2007) %>%
    left_join(STRIPSMeta::crop_seed_info) %>%
    select_("watershed","year","value","crop_species")

  corn = d %>% filter(crop_species=="corn"   ) %>% mutate(response = "corn revenue ($/ha)")
  soy  = d %>% filter(crop_species=="soybean") %>% mutate(response = "soybean revenue ($/ha)")

  bind_rows(corn,soy) %>%
    select(-crop_species) %>%
    mutate(PI = "Tyndall",
           source = "econ") %>%
    select(PI, source, year, watershed, response, value)
}
