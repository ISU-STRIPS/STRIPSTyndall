#' Construct economic data used in PNAS paper
#'
#' This function will calculate the revenue for a watershed-year combination
#' for watersheds that have a certain amount of prairie.
#'
#' @param prairie_cost A scalar indicating how much the prairie costs.
#' @return A \code{data.frame} containing the columns PI, source, watershed,
#' year, response, and value.
#' @import STRIPSMeta
#' @import dplyr
#' @export
#' @examples
#' d <- pnas_data()
#' summary(d)
#'
pnas_data <- function(prairie_cost = 95) {
  hectares_per_acre = 0.404686

  watersheds <- STRIPSMeta::watersheds  %>%
    mutate(crop_prop = 1-prairie_pct/100,
           crop_acre = size_ha*crop_prop,
           prairie_acre = size_ha*(1-crop_prop)) %>%
    select_("watershed", "crop_acre", "prairie_acre")

  # Calculate watershed total revenue which includes cost of prairie
  d <- revenue %>%
    left_join(watersheds) %>%
    mutate(value = (crop_acre * revenue - prairie_acre * prairie_cost)/(crop_acre+prairie_acre),
           value = value * hectares_per_acre) %>%
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
