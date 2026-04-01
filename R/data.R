#' Pedigree data — Guinea pig breeding study (4 generations)
#'
#' A pedigree dataset recording parent-offspring relationships across four
#' generations of a domestic guinea pig (\emph{Cavia porcellus}) breeding
#' population.
#'
#' @format A data frame with 10,817 rows and 3 columns:
#' \describe{
#'   \item{ID}{Character. Individual identifier.}
#'   \item{dadID}{Character. Sire (father) identifier. \code{NA} for founders.}
#'   \item{momID}{Character. Dam (mother) identifier. \code{NA} for founders.}
#' }
#' @source Figshare. \doi{10.6084/m9.figshare.31513204.v2}
#' @seealso \code{\link{pheno}}, \code{\link{ped_pheno}}
"ped"


#' Phenotype and body weight data — Guinea pig breeding study
#'
#' Phenotypic measurements for domestic guinea pigs (\emph{Cavia porcellus}),
#' including body weights recorded at days 0, 15, 30, 45, 60, and 90 of life,
#' along with birth characteristics and housing information.
#'
#' @format A data frame with 7,834 rows and 20 columns:
#' \describe{
#'   \item{ID}{Character. Individual identifier.}
#'   \item{dadID}{Character. Sire (father) identifier.}
#'   \item{momID}{Character. Dam (mother) identifier.}
#'   \item{gener}{Integer. Generation number (1–4).}
#'   \item{n_parto}{Integer. Parity number (litter birth order for the dam).}
#'   \item{tc_destete}{Integer. Litter size at weaning
#'     (\emph{tamaño de camada al destete}).}
#'   \item{tc_nac}{Integer. Litter size at birth
#'     (\emph{tamaño de camada al nacimiento}).}
#'   \item{peso_al_parto}{Numeric. Dam body weight at parturition (grams).}
#'   \item{color}{Character. Coat color (standardized to lowercase):
#'     \code{"blanco"} (white), \code{"alazan"} (chestnut),
#'     \code{"bayo"} (cream/buff).}
#'   \item{dedos}{Integer. Number of toes.}
#'   \item{sexo}{Character. Sex: \code{"H"} (hembra, female),
#'     \code{"M"} (macho, male).}
#'   \item{clima}{Character. Season of birth: \code{"invierno"} (winter),
#'     \code{"verano"} (summer).}
#'   \item{poza}{Integer. Housing unit (pen) identifier.}
#'   \item{p0}{Numeric. Body weight at day 0 (birth weight, grams).
#'     \code{NA} for abortions.}
#'   \item{p15}{Numeric. Body weight at day 15 (grams).}
#'   \item{p30}{Numeric. Body weight at day 30 (grams).}
#'   \item{p45}{Numeric. Body weight at day 45 (grams).}
#'   \item{p60}{Numeric. Body weight at day 60 (grams).}
#'   \item{p90}{Numeric. Body weight at day 90 (grams).}
#'   \item{tasa_cp}{Numeric. Growth rate index.}
#' }
#' @source Figshare. \doi{10.6084/m9.figshare.31513204.v2}
#' @seealso \code{\link{ped}}, \code{\link{ped_pheno}}, \code{\link{ped_growth}}
"pheno"


#' Pedigree joined with phenotype data — Guinea pig breeding study
#'
#' A full join of \code{\link{ped}} and \code{\link{pheno}}, combining pedigree
#' structure with individual-level phenotypic measurements. Individuals present
#' in only one source dataset are retained with \code{NA} for missing columns.
#'
#' @format A data frame combining columns from \code{\link{ped}} and
#'   \code{\link{pheno}}. See those help pages for column descriptions.
#' @source Figshare. \doi{10.6084/m9.figshare.31513204.v2}
#' @seealso \code{\link{ped}}, \code{\link{pheno}}, \code{\link{ped_growth}}
"ped_pheno"


#' Growth trajectories in long format — Guinea pig breeding study
#'
#' A long-format version of \code{\link{ped_pheno}}, restricted to individuals
#' with a recorded birth weight (\code{p0}). Each row represents one body
#' weight measurement for one individual.
#'
#' @format A data frame with 6 columns:
#' \describe{
#'   \item{ID}{Character. Individual identifier.}
#'   \item{sexo}{Character. Sex: \code{"H"} (female), \code{"M"} (male).}
#'   \item{dadID}{Character. Sire (father) identifier.}
#'   \item{momID}{Character. Dam (mother) identifier.}
#'   \item{day}{Numeric. Day of measurement: 0, 15, 30, 45, 60, or 90.}
#'   \item{weight}{Numeric. Body weight (grams).}
#' }
#' @source Figshare. \doi{10.6084/m9.figshare.31513204.v2}
#' @seealso \code{\link{ped_pheno}}
"ped_growth"


# ── Wars of the Roses ────────────────────────────────────────────────────────

#' Wars of the Roses pedigree
#'
#' A pedigree of the English royal families involved in the Wars of the Roses
#' (1455–1487), rooted at Edward III. Includes names, Wikipedia URLs, and twin
#' information where applicable. Processed with
#' \code{\link[BGmisc]{ped2fam}} and \code{\link[BGmisc]{checkSex}}.
#'
#' @format A data frame with 95 rows and 9 columns:
#' \describe{
#'   \item{id}{Integer. Individual identifier.}
#'   \item{famID}{Integer. Family group identifier (from
#'     \code{\link[BGmisc]{ped2fam}}).}
#'   \item{momID}{Numeric. Mother's \code{id}. \code{NA} for founders.}
#'   \item{dadID}{Numeric. Father's \code{id}. \code{NA} for founders.}
#'   \item{name}{Character. Full name of the individual.}
#'   \item{sex}{Character. Sex: \code{"M"} (male), \code{"F"} (female),
#'     \code{"U"} (unknown/phantom).}
#'   \item{url}{Character. Wikipedia URL for the individual (where available).}
#'   \item{twinID}{Numeric. Co-twin's \code{id}, or \code{NA} if not a twin.}
#'   \item{zygosity}{Character. Twin zygosity (\code{"MZ"}, \code{"DZ"}), or
#'     \code{NA} if not a twin.}
#' }
#' @source Compiled from public genealogical records. Wikipedia URLs are
#'   provided for reference.
#' @seealso \url{https://en.wikipedia.org/wiki/Wars_of_the_Roses}
"warsofroses"


# ── Kluane Red Squirrel Project ───────────────────────────────────────────────

#' Kluane Red Squirrel Project — core pedigree and fitness data
#'
#' Pedigree and fitness data for North American red squirrels
#' (\emph{Tamiasciurus hudsonicus}) from the long-running Kluane Red Squirrel
#' Project (Yukon, Canada; 1987–present). Restricted to family units that
#' include at least one recorded father.
#'
#' Annual reproductive success (\code{ars_mean}) is summarized per individual
#' across all observed years.
#'
#' @format A data frame with 5,251 rows and 9 columns:
#' \describe{
#'   \item{personID}{Integer. Individual identifier.}
#'   \item{momID}{Integer. Mother's \code{personID}. \code{NA} for founders.}
#'   \item{dadID}{Integer. Father's \code{personID}. \code{NA} for founders.}
#'   \item{sex}{Character. Sex: \code{"F"} (female), \code{"M"} (male).}
#'   \item{famID}{Integer. Family group identifier.}
#'   \item{byear}{Integer. Birth year. \code{NA} if unknown.}
#'   \item{dyear}{Integer. Death year. \code{NA} if unknown or still alive.}
#'   \item{lrs}{Numeric. Lifetime reproductive success (total offspring
#'     recruited). \code{NA} if not available.}
#'   \item{ars_mean}{Numeric. Mean annual reproductive success across observed
#'     years. \code{NA} if no annual records available.}
#' }
#' @source Dryad. \doi{10.5061/dryad.n5q05}
#' @seealso \code{\link{redsquirrels_full}}
"redsquirrels"


#' Kluane Red Squirrel Project — full pedigree and fitness data
#'
#' Full version of \code{\link{redsquirrels}}, including all individuals
#' regardless of whether their father is recorded, and retaining additional
#' annual reproductive success summary statistics.
#'
#' @format A data frame with 7,799 rows and 16 columns. Includes all columns
#'   from \code{\link{redsquirrels}}, plus:
#' \describe{
#'   \item{ars_max}{Numeric. Maximum annual reproductive success.}
#'   \item{ars_med}{Numeric. Median annual reproductive success.}
#'   \item{ars_min}{Numeric. Minimum annual reproductive success.}
#'   \item{ars_sd}{Numeric. Standard deviation of annual reproductive success.}
#'   \item{ars_n}{Integer. Number of years with ARS records.}
#'   \item{year_first}{Integer. First year the individual was observed.}
#'   \item{year_last}{Integer. Last year the individual was observed.}
#' }
#' @source Dryad. \doi{10.5061/dryad.n5q05}
#' @seealso \code{\link{redsquirrels}}
"redsquirrels_full"
