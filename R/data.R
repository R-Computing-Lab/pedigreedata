# ── Guinea pig (Cavia porcellus) ─────────────────────────────────────────────

#' Guinea pig pedigree and growth data
#'
#' Pedigree and phenotypic measurements for domestic guinea pigs
#' (\emph{Cavia porcellus}) from a four-generation controlled breeding program.
#' Guinea pigs (known as \emph{cuy} in Andean countries) are an important
#' livestock species in South America, raised for meat production and
#' increasingly studied for quantitative genetic analyses of growth and
#' reproductive traits.
#'
#' The dataset combines the pedigree structure with repeated body weight
#' measurements at six time points across the first 90 days of life (days 0,
#' 15, 30, 45, 60, and 90), along with birth characteristics, litter
#' information, coat color, season of birth, and housing unit. Guinea pigs are
#' polytocous (litters of 1--5 offspring) and precocial (newborns are fully
#' furred and mobile), making them well-suited for studying early-life growth
#' trajectories, maternal effects, and litter-size effects within a
#' quantitative genetics framework.
#'
#' Birth weight (\code{p0}) is \code{NA} for abortions (originally recorded as
#' \code{"ABORTO"} in the source data). Column names are translated and
#' standardized from the original Spanish-language source.
#'
#' @format A data frame with columns:
#' \describe{
#'   \item{ID}{Character. Unique individual identifier.}
#'   \item{dadID}{Character. Sire (father) identifier. \code{NA} for
#'     founders with no recorded father.}
#'   \item{momID}{Character. Dam (mother) identifier. \code{NA} for
#'     founders with no recorded mother.}
#'   \item{gener}{Integer. Generation number within the breeding program
#'     (1--4).}
#'   \item{n_parto}{Integer. Parity number: the birth order of this litter
#'     for the dam.}
#'   \item{tc_destete}{Integer. Litter size at weaning
#'     (\emph{tamaño de camada al destete}): number of offspring that
#'     survived to weaning.}
#'   \item{tc_nac}{Integer. Litter size at birth
#'     (\emph{tamaño de camada al nacimiento}): total number of offspring
#'     born in the litter.}
#'   \item{peso_al_parto}{Numeric. Dam body weight at parturition (grams).}
#'   \item{color}{Character. Coat color, standardized to lowercase:
#'     \code{"blanco"} (white), \code{"alazan"} (chestnut/reddish),
#'     \code{"bayo"} (cream/buff).}
#'   \item{dedos}{Integer. Number of toes.}
#'   \item{sexo}{Character. Sex of the offspring: \code{"H"}
#'     (hembra, female) or \code{"M"} (macho, male).}
#'   \item{clima}{Character. Season of birth: \code{"invierno"} (winter)
#'     or \code{"verano"} (summer).}
#'   \item{poza}{Integer. Housing unit (pen) identifier.}
#'   \item{p0}{Numeric. Body weight at day 0 (birth weight, grams).
#'     \code{NA} for abortions.}
#'   \item{p15}{Numeric. Body weight at day 15 (grams).}
#'   \item{p30}{Numeric. Body weight at day 30 (grams).}
#'   \item{p45}{Numeric. Body weight at day 45 (grams).}
#'   \item{p60}{Numeric. Body weight at day 60 (grams).}
#'   \item{p90}{Numeric. Body weight at day 90 (grams).}
#'   \item{tasa_cp}{Numeric. Growth rate index derived from the weight
#'     measurements.}
#' }
#' @keywords datasets
#' @source Figshare. \doi{10.6084/m9.figshare.31513204.v2}
#' @examples
#' head(guinea_pigs)
#' # Count founders (individuals with no recorded parents)
#' sum(is.na(guinea_pigs$dadID) & is.na(guinea_pigs$momID))
#' # Birth weight by sex
#' tapply(guinea_pigs$p0, guinea_pigs$sexo, mean, na.rm = TRUE)
"guinea_pigs"


# ── Wars of the Roses ────────────────────────────────────────────────────────

#' War of the Roses pedigree
#'
#' A pedigree dataset representing the familial relationships among key figures
#' in the Wars of the Roses, a series of English civil wars for control of the
#' throne of England fought between the houses of Lancaster and York during the
#' 15th century (1455--1487). The pedigree is rooted at Edward III
#' (r. 1327--1377), whose numerous descendants through multiple lines of
#' succession gave rise to the rival dynastic claims at the heart of the
#' conflict.
#'
#' The dataset is useful for illustrating complex pedigree structures,
#' including multiple mating events, consanguineous marriages, half-siblings,
#' and the kind of dense, overlapping family networks that characterize
#' historical royal genealogies. It includes Wikipedia URLs for each
#' individual, making it suitable for teaching pedigree construction and
#' relationship tracing with reference to publicly available biographical
#' sources. Twin and zygosity fields are included where applicable.
#'
#' Pedigree integrity was verified and phantom individuals were added as
#' needed using \code{\link[BGmisc]{checkSex}},
#' \code{\link[BGmisc]{checkParentIDs}}, and
#' \code{\link[BGmisc]{checkPedigreeNetwork}} from the \pkg{BGmisc} package.
#' Family group assignment used \code{\link[BGmisc]{ped2fam}}.
#'
#' @format A data frame with 95 rows and 9 columns:
#' \describe{
#'   \item{id}{Integer. Unique individual identifier.}
#'   \item{famID}{Integer. Family group identifier assigned by
#'     \code{\link[BGmisc]{ped2fam}}.}
#'   \item{momID}{Numeric. Mother's \code{id}. \code{NA} for founders or
#'     individuals whose mother is not recorded.}
#'   \item{dadID}{Numeric. Father's \code{id}. \code{NA} for founders or
#'     individuals whose father is not recorded.}
#'   \item{name}{Character. Full historical name of the individual.}
#'   \item{sex}{Character. Sex: \code{"M"} (male), \code{"F"} (female),
#'     \code{"U"} (unknown or phantom individual added for pedigree
#'     integrity).}
#'   \item{url}{Character. Wikipedia URL for the individual, where
#'     available.}
#'   \item{twinID}{Numeric. The \code{id} of the co-twin, or \code{NA}
#'     if the individual is not a twin.}
#'   \item{zygosity}{Character. Twin zygosity: \code{"MZ"} (monozygotic)
#'     or \code{"DZ"} (dizygotic). \code{NA} if not a twin.}
#' }
#' @keywords datasets
#' @source Compiled from public genealogical records.
#'   \url{https://en.wikipedia.org/wiki/Wars_of_the_Roses}
#' @examples
#' head(war_of_the_roses[, c("id", "name", "sex", "momID", "dadID")])
#'
#' # Founders (individuals whose parents are not in the dataset)
#' war_of_the_roses[
#'   is.na(war_of_the_roses$momID) & is.na(war_of_the_roses$dadID),
#'   c("id", "name")
#' ]
#'
#' if (requireNamespace("ggpedigree", quietly = TRUE)) {
#'   ggpedigree::ggPedigree(war_of_the_roses,
#'     personID = "id", momID = "momID", dadID = "dadID", famID = "famID",
#'     config = list(
#'       code_male = "M",
#'       code_female = "F",
#'       code_na = "U",
#'       label_column = "name",
#'       label_method = "ggrepel",
#'       label_include = TRUE,
#'       label_text_size = 2
#'     )
#'   )
#' }
"war_of_the_roses"


# ── Kluane Red Squirrel Project ───────────────────────────────────────────────

#' Kluane Red Squirrel Project pedigree and fitness data
#'
#' Pedigree and fitness data for North American red squirrels
#' (\emph{Tamiasciurus hudsonicus}) from the Kluane Red Squirrel Project,
#' a long-term field study conducted in the boreal forests of Yukon, Canada,
#' running continuously since 1987. The project has individually marked and
#' monitored thousands of squirrels across multiple generations, producing one
#' of the most detailed wild-animal pedigrees available for quantitative
#' genetic research.
#'
#' Red squirrels in this population occupy individual year-round territories
#' centered on a food cache (midden). Key fitness traits include annual
#' reproductive success (ARS: the number of offspring surviving to
#' independence in a given year) and lifetime reproductive success (LRS: the
#' total number of such offspring over an individual's lifetime). These traits
#' have been used to study the heritability of fitness in a natural population,
#' with the original publication finding very low levels of direct additive
#' genetic variance.
#'
#' Family group IDs were assigned using \code{\link[BGmisc]{ped2fam}} from
#' the \pkg{BGmisc} package. The original data are published under a
#' CC0 1.0 Universal Public Domain Dedication.
#'
#' @format A data frame with 7,799 rows and 16 columns:
#' \describe{
#'   \item{personID}{Integer. Unique individual identifier.}
#'   \item{momID}{Integer. Mother's \code{personID}. \code{NA} for
#'     founders or individuals whose mother was not identified.}
#'   \item{dadID}{Integer. Father's \code{personID}. \code{NA} for
#'     founders or individuals whose father was not identified.}
#'   \item{sex}{Character. Sex: \code{"F"} (female), \code{"M"} (male).}
#'   \item{famID}{Integer. Family group identifier assigned by
#'     \code{\link[BGmisc]{ped2fam}}.}
#'   \item{byear}{Integer. Birth year. \code{NA} if unknown.}
#'   \item{dyear}{Integer. Death year. \code{NA} if the individual's fate
#'     was not recorded or it was still alive at the end of the study.}
#'   \item{lrs}{Numeric. Lifetime reproductive success: total number of
#'     offspring surviving to independence across the individual's entire
#'     life. \code{NA} if not available.}
#'   \item{ars_mean}{Numeric. Mean annual reproductive success across all
#'     years in which the individual was observed. \code{NA} if no annual
#'     records are available.}
#'   \item{ars_max}{Numeric. Maximum ARS value recorded across all observed
#'     years.}
#'   \item{ars_med}{Numeric. Median ARS value across all observed years.}
#'   \item{ars_min}{Numeric. Minimum ARS value across all observed years.}
#'   \item{ars_sd}{Numeric. Standard deviation of ARS values across all
#'     observed years.}
#'   \item{ars_n}{Integer. Number of years for which an ARS value was
#'     recorded. Zero if the individual has no ARS records.}
#'   \item{year_first}{Integer. First calendar year in which the individual
#'     was observed.}
#'   \item{year_last}{Integer. Last calendar year in which the individual
#'     was observed.}
#' }
#' @keywords datasets
#' @source McFarlane, S.E., Boutin, S., Humphries, M.M., et al. (2015).
#'   Very low levels of direct additive genetic variance in fitness and
#'   fitness components in a red squirrel population. Dryad.
#'   \doi{10.5061/dryad.n5q05}
#' @examples
#' head(red_squirrels)
#' str(red_squirrels)
#'
#' # LRS distribution by sex
#' tapply(red_squirrels$lrs, red_squirrels$sex, summary)
#'
#' # Single-family pedigree plot
#' if (requireNamespace("ggpedigree", quietly = TRUE)) {
#'   fam160 <- subset(red_squirrels, famID == 160)
#'   ggpedigree::ggPedigree(fam160,
#'     personID = "personID", momID = "momID", dadID = "dadID",
#'     config = list(add_phantoms = TRUE, code_male = "M")
#'   )
#' }
"red_squirrels"
