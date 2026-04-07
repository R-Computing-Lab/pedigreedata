# devtools::install_github("R-Computing-Lab/BGmisc")
library(tidyverse)
library(here)
library(readr)
library(usethis)
library(BGmisc)


## Load raw data ---------------------------------------------------------------

raw_df <- read_csv("data-raw/df_raw_wor.csv",
  col_types = cols(
    twinID   = col_double(),
    zygosity = col_character()
  )
) %>%
  select(-famID) # famID will be recalculated after cleaning


## Initial ped2fam and type coercions ------------------------------------------

df <- ped2fam(raw_df, personID = "id") %>%
  rename(personID = id) %>%
  # -- Type coercions ---------------------------------------------------------
  mutate(
    personID = as.integer(personID),
    momID = as.integer(momID),
    dadID = as.integer(dadID),

    # -- Name standardization ---------------------------------------------------
    # Fixes fall into four categories:
    #   (1) Critical: names the vignette filter()s on — must match exactly
    #   (2) Typo corrections
    #   (3) Commas before titles (e.g., "John Duke of Bedford" → "John, Duke of Bedford")
    #   (4) Parenthetical forms → cleaner equivalents
    name = case_when(
      # (1) Critical — vignette looks for these exact strings
      personID == 8 ~ "Lionel of Antwerp", # vignette york_names
      personID == 10 ~ "John of Gaunt", # vignette lancaster_names
      personID == 21 ~ "Philippa of Clarence", # vignette york_names (daughter of Lionel)
      personID == 32 ~ "Henry IV", # vignette lancaster_names
      personID == 64 ~ "Henry VII", # vignette henry-vii section
      personID == 65 ~ "Elizabeth of York", # vignette henry-vii section
      personID == 73 ~ "Roger Mortimer", # vignette york_names
      personID == 80 ~ "Margaret Beaufort", # vignette henry-vii section

      # (2) Typo corrections
      personID == 2 ~ "Philippa of Hainault", # raw: "Phillipa" (missing 'p')

      # (3) Add commas before titles
      personID == 3 ~ "Edward, the Black Prince",
      personID == 5 ~ "Isabella of England, Countess of Bedford",
      personID == 13 ~ "Edmund of Langley, Duke of York",
      personID == 15 ~ "Thomas of Woodstock, Duke of Gloucester",
      personID == 22 ~ "Edmund Mortimer, Earl of March",
      personID == 29 ~ "John Holland, 1st Duke of Exeter",
      personID == 36 ~ "Thomas, Duke of Clarence",
      personID == 37 ~ "John, Duke of Bedford",
      personID == 38 ~ "Humphrey, Duke of Gloucester",
      personID == 45 ~ "John Beaufort, 1st Earl of Somerset",
      personID == 53 ~ "Richard, Duke of York",
      personID == 61 ~ "George, Duke of Clarence",
      personID == 68 ~ "Humphrey de Bohun, 7th Earl of Hereford",
      personID == 69 ~ "Joan Fitzalan, Countess of Hereford",
      personID == 70 ~ "Thomas Holland, 2nd Earl of Kent",
      personID == 72 ~ "Edmund Mortimer, 3rd Earl of March",
      personID == 74 ~ "Eleanor Holland, Countess of March", # also corrects archaic "Alianore"
      personID == 75 ~ "Alice FitzAlan, Countess of Kent",
      personID == 77 ~ "Edmund Tudor, 1st Earl of Richmond",
      personID == 78 ~ "John Beaufort, 1st Duke of Somerset",
      personID == 81 ~ "Thomas Montagu, 4th Earl of Salisbury",
      personID == 82 ~ "Eleanor Holland, Countess of Salisbury",
      personID == 83 ~ "Alice Montacute, 5th Countess of Salisbury",
      personID == 84 ~ "Richard Neville, 5th Earl of Salisbury",
      personID == 85 ~ "Richard Neville, 16th Earl of Warwick",
      personID == 86 ~ "Richard Beauchamp, 13th Earl of Warwick",
      personID == 87 ~ "Thomas Despenser, 1st Earl of Gloucester",
      personID == 88 ~ "Isabel Despenser, Countess of Warwick",
      personID == 89 ~ "Anne Beauchamp, 16th Countess of Warwick",
      personID == 91 ~ "Richard Woodville, 1st Earl Rivers",
      personID == 94 ~ "Edmund of Woodstock, 1st Earl of Kent",
      personID == 95 ~ "Margaret Wake, 3rd Baroness Wake of Liddell",

      # cleaner names
      personID == 17 ~ "Edward of Angoulême", # raw: "Edward (son of the Black Prince)"
      personID == 24 ~ "Constance of York", # raw: "Constance (daughter of Edmund of Langley)"
      personID == 28 ~ "Philippa of Lancaster", # raw: "Philippa (of Lancaster)"
      personID == 30 ~ "Elizabeth of Lancaster", # raw: "Elizabeth Plantagenet"
      personID == 39 ~ "Blanche of England", # raw: "Blanche (daughter of Henry IV)"
      personID == 40 ~ "Philippa of England", # raw: "Philippa (daughter of Henry IV)"
      personID == 51 ~ "Isabel of Cambridge", # raw: "Isabel Plantagenet"
      personID == 54 ~ "Anne of York", # raw: "Anne (daughter of Richard Duke of York)"
      personID == 57 ~ "Edmund of Rutland", # raw: "Edmund (son of Richard Duke of York)"
      personID == 58 ~ "Elizabeth of York, Duchess of Suffolk", # raw: "Elizabeth (daughter of Richard Duke of York)"
      personID == 59 ~ "Margaret of York", # raw: "Margaret (daughter of Richard Duke of York)"
      TRUE ~ name
    ),
    dadID = case_when(
      personID == 1 ~ 115, # Edward III's father was Edward II, who is missing from the original dataset but is a critical ancestor for the pedigree structure. We add him as a new row with personID = 115 (see below).
      TRUE ~ dadID
    ),
    momID = case_when(
      personID == 1 ~ 116, # Edward III's mother was Isabella of France
      TRUE ~ momID
    )
  ) %>%
  # ── New additions: children of Edward IV + Elizabeth Woodville ───────────────
  # SVG notes "others" — these children complete the York generation
  addPersonToPed(
    personID = 96, name = "Mary of York",
    sex = "F", momID = 56, dadID = 55,
    url = "https://en.wikipedia.org/wiki/Mary_of_York"
  ) %>%
  addPersonToPed(
    personID = 97, name = "Cecily of York",
    sex = "F", momID = 56, dadID = 55,
    url = "https://en.wikipedia.org/wiki/Cecily_of_York"
  ) %>%
  addPersonToPed(
    personID = 98, name = "Anne of York, Viscountess Bourchier",
    sex = "F", momID = 56, dadID = 55,
    url = "https://en.wikipedia.org/wiki/Anne_of_York,_Viscountess_Bourchier"
  ) %>%
  addPersonToPed(
    personID = 99, name = "Catherine of York",
    sex = "F", momID = 56, dadID = 55,
    url = "https://en.wikipedia.org/wiki/Catherine_of_York"
  ) %>%
  addPersonToPed(
    personID = 100, name = "Bridget of York",
    sex = "F", momID = 56, dadID = 55,
    url = "https://en.wikipedia.org/wiki/Bridget_of_York"
  ) %>%
  # ── New additions: children of Henry VII + Elizabeth of York ─────────────────
  # The SVG ends with "Continues with House of Tudor" — these are the resolution
  addPersonToPed(
    personID = 101, name = "Arthur, Prince of Wales",
    sex = "M", momID = 65, dadID = 64,
    url = "https://en.wikipedia.org/wiki/Arthur,_Prince_of_Wales"
  ) %>%
  addPersonToPed(
    personID = 102, name = "Margaret Tudor",
    sex = "F", momID = 65, dadID = 64,
    url = "https://en.wikipedia.org/wiki/Margaret_Tudor"
  ) %>%
  addPersonToPed(
    personID = 103, name = "Henry VIII",
    sex = "M", momID = 65, dadID = 64,
    url = "https://en.wikipedia.org/wiki/Henry_VIII"
  ) %>%
  addPersonToPed(
    personID = 104, name = "Mary Tudor, Queen of France",
    sex = "F", momID = 65, dadID = 64,
    url = "https://en.wikipedia.org/wiki/Mary_Tudor,_Queen_of_France"
  ) %>%
  addPersonToPed(
    personID = 105, name = "Marie I de Coucy, Countess of Soissons",
    sex = "F", momID = 5, dadID = 6,
    url = "https://en.wikipedia.org/wiki/Marie_I_de_Coucy,_Countess_of_Soissons"
  ) %>%
  # https://en.wikipedia.org/wiki/Catherine_of_Lancaster
  addPersonToPed(
    personID = 106, name = "Catherine of Lancaster",
    sex = "F", momID = 11, dadID = 10,
    url = "https://en.wikipedia.org/wiki/Catherine_of_Lancaster"
  ) %>%
  # https://en.wikipedia.org/wiki/Henry_of_Grosmont,_Duke_of_Lancaster
  addPersonToPed(
    personID = 107, name = "Henry of Grosmont, Duke of Lancaster",
    sex = "M", momID = 109,
    dadID = 108, # Henry, 3rd Earl of Lancaster
    url = "https://en.wikipedia.org/wiki/Henry_of_Grosmont,_Duke_of_Lancaster"
  ) %>%
  # https://en.wikipedia.org/wiki/Henry,_3rd_Earl_of_Lancaster
  addPersonToPed(
    personID = 108, name = "Henry, 3rd Earl of Lancaster",
    sex = "M",
    momID = 110, # 	Blanche of Artois
    dadID = 111, # Edmund Crouchback, 1st Earl
    url = "https://en.wikipedia.org/wiki/Henry,_3rd_Earl_of_Lancaster"
  ) %>%
  # https://en.wikipedia.org/wiki/Maud_Chaworth
  addPersonToPed(
    personID = 109, name = "Maud Chaworth",
    sex = "F",
    momID = NA,
    dadID = NA,
    url = "https://en.wikipedia.org/wiki/Maud_Chaworth"
  ) %>%
  # https://en.wikipedia.org/wiki/Blanche_of_Artois
  addPersonToPed(
    personID = 110, name = "Blanche of Artois",
    sex = "F",
    momID = NA,
    dadID = NA,
    url = "https://en.wikipedia.org/wiki/Blanche_of_Artois"
  ) %>%
  # https://en.wikipedia.org/wiki/Edmund_Crouchback
  addPersonToPed(
    personID = 111, name = "Edmund Crouchback",
    sex = "M",
    momID = NA,
    dadID = 112, # Henry III
    url = "https://en.wikipedia.org/wiki/Edmund_Crouchback"
  ) %>%
  # https://en.wikipedia.org/wiki/Henry_III_of_England
  addPersonToPed(
    personID = 112, name = "Henry III",
    sex = "M",
    momID = 113, # Isabella of Angoulême
    dadID = 114, # John, King of England
    url = "https://en.wikipedia.org/wiki/Henry_III_of_England"
  ) %>%
  addPersonToPed(
    personID = 113, name = "Isabella of Angoulême",
    sex = "F",
    momID = NA,
    dadID = NA,
    url = "https://en.wikipedia.org/wiki/Isabella_of_Angoul%C3%AAme"
  ) %>%
  addPersonToPed(
    personID = 114, name = "John, King of England",
    sex = "M",
    momID = NA,
    dadID = NA,
    url = "https://en.wikipedia.org/wiki/John,_King_of_England"
  ) %>%
  # edward ii
  addPersonToPed(
    personID = 115, name = "Edward II",
    sex = "M",
    momID = NA, # 	Eleanor of Castile
    dadID = NA, # Edward I
    url = "https://en.wikipedia.org/wiki/Edward_II_of_England"
  ) %>%
  # 	Isabella of France
  addPersonToPed(
    personID = 116, name = "Isabella of France",
    sex = "F",
    momID = NA,
    dadID = NA,
    url = "https://en.wikipedia.org/wiki/Isabella_of_France"
  ) %>%
  # ── Additions from Wikipedia Wars of the Roses family tree template ───────────
  # Henry Percy "Hotspur" — founder; married Elizabeth Mortimer (118)  # Father	Henry Percy, 1st Earl of Northumberland
                                                                       # Mother	Margaret Neville
  addPersonToPed(
    personID = 117, name = "Henry Percy, Hotspur",
    sex = "M", momID = NA, dadID = NA,
    url = "https://en.wikipedia.org/wiki/Henry_Percy_(Hotspur)"
  ) %>%
  # Elizabeth Mortimer — sibling of Roger Mortimer (73); both children of Edmund Mortimer
  # 3rd Earl (72) and Philippa of Clarence (21)
  addPersonToPed(
    personID = 118, name = "Elizabeth Mortimer",
    sex = "F", momID = 21, dadID = 72,
    url = "https://en.wikipedia.org/wiki/Elizabeth_Mortimer"
  ) %>%
  # Edmund Beaufort, 2nd Duke of Somerset — son of John Beaufort 1st Earl (45) and
  # Margaret Holland (46); brother of John Beaufort 1st Duke (78)
  addPersonToPed(
    personID = 119, name = "Edmund Beaufort, 2nd Duke of Somerset",
    sex = "M", momID = 46, dadID = 45,
    url = "https://en.wikipedia.org/wiki/Edmund_Beaufort,_2nd_Duke_of_Somerset"
  ) %>%
  # Anne Neville, Duchess of Buckingham — daughter of Joan Beaufort (49) and Ralph
  # Neville (50); sister of Cecily Neville (52) and Richard Neville 5th Earl (84)
  addPersonToPed(
    personID = 120, name = "Anne Neville, Duchess of Buckingham",
    sex = "F", momID = 49, dadID = 50,
    url = "https://en.wikipedia.org/wiki/Anne_Neville,_Duchess_of_Buckingham"
  ) %>%
  # Eleanor Neville, Countess of Northumberland — daughter of Joan Beaufort (49) and
  # Ralph Neville (50)
  addPersonToPed(
    personID = 121, name = "Eleanor Neville, Countess of Northumberland",
    sex = "F", momID = 49, dadID = 50,
    url = "https://en.wikipedia.org/wiki/Eleanor_Neville,_Countess_of_Northumberland"
  ) %>%
  # William Neville, Earl of Kent — son of Joan Beaufort (49) and Ralph Neville (50)
  addPersonToPed(
    personID = 122, name = "William Neville, Earl of Kent",
    sex = "M", momID = 49, dadID = 50,
    url = "https://en.wikipedia.org/wiki/William_Neville,_1st_Earl_of_Kent"
  ) %>%
  # Henry Percy, 2nd Earl of Northumberland — son of Hotspur (117) and Elizabeth
  # Mortimer (118)
  addPersonToPed(
    personID = 123, name = "Henry Percy, 2nd Earl of Northumberland",
    sex = "M", momID = 118, dadID = 117,
    url = "https://en.wikipedia.org/wiki/Henry_Percy,_2nd_Earl_of_Northumberland"
  ) %>%
  # Eleanor Beauchamp — daughter of Richard Beauchamp 13th Earl (86) and Isabel
  # Despenser (88); sibling of Anne Beauchamp (89)
  addPersonToPed(
    personID = 124, name = "Eleanor Beauchamp",
    sex = "F", momID = 88, dadID = 86,
    url = "https://en.wikipedia.org/wiki/Eleanor_Beauchamp"
  ) %>%
  # Henry Beaufort, 3rd Duke of Somerset — son of Edmund Beaufort 2nd Duke (119) and
  # Eleanor Beauchamp (124); KIA at Battle of Hexham
  addPersonToPed(
    personID = 125, name = "Henry Beaufort, 3rd Duke of Somerset",
    sex = "M", momID = 124, dadID = 119,
    url = "https://en.wikipedia.org/wiki/Henry_Beaufort,_3rd_Duke_of_Somerset"
  ) %>%
  # Edmund Beaufort (died 1471) — son of Edmund Beaufort 2nd Duke (119) and Eleanor
  # Beauchamp (124); executed after Battle of Tewkesbury
  addPersonToPed(
    personID = 126, name = "Edmund Beaufort, Duke of Somerset",
    sex = "M", momID = 124, dadID = 119,
    url = "https://en.wikipedia.org/wiki/Edmund_Beaufort_(died_1471)"
  ) %>%
  # Margaret Beaufort, Countess of Stafford — daughter of Edmund Beaufort 2nd Duke
  # (119) and Eleanor Beauchamp (124); not to be confused with Lady Margaret Beaufort (80)
  addPersonToPed(
    personID = 127, name = "Margaret Beaufort, Countess of Stafford",
    sex = "F", momID = 124, dadID = 119,
    url = "https://en.wikipedia.org/wiki/Margaret_Beaufort,_Countess_of_Stafford"
  ) %>%
  # Humphrey Stafford, 1st Duke of Buckingham — founder; married Anne Neville (120)
  addPersonToPed(
    personID = 128, name = "Humphrey Stafford, 1st Duke of Buckingham",
    sex = "M", momID = NA, dadID = NA,
    url = "https://en.wikipedia.org/wiki/Humphrey_Stafford,_1st_Duke_of_Buckingham"
  ) %>%
  # Humphrey Stafford, Earl of Stafford — son of Humphrey 1st Duke (128) and Anne
  # Neville (120); predeceased his father
  addPersonToPed(
    personID = 129, name = "Humphrey Stafford, Earl of Stafford",
    sex = "M", momID = 120, dadID = 128,
    url = "https://en.wikipedia.org/wiki/Humphrey_Stafford,_1st_Earl_of_Stafford"
  ) %>%
  # Henry Percy, 3rd Earl of Northumberland — son of Henry Percy 2nd Earl (123) and
  # Eleanor Neville (121); KIA at Battle of Towton
  addPersonToPed(
    personID = 130, name = "Henry Percy, 3rd Earl of Northumberland",
    sex = "M", momID = 121, dadID = 123,
    url = "https://en.wikipedia.org/wiki/Henry_Percy,_3rd_Earl_of_Northumberland"
  ) %>%
  # John Neville, 1st Marquess of Montagu — son of Richard Neville 5th Earl (84) and
  # Alice Montacute (83); sibling of Richard Neville Kingmaker (85)
  addPersonToPed(
    personID = 131, name = "John Neville, 1st Marquess of Montagu",
    sex = "M", momID = 83, dadID = 84,
    url = "https://en.wikipedia.org/wiki/John_Neville,_1st_Marquess_of_Montagu"
  ) %>%
  # Jasper Tudor — son of Owen Tudor (76) and Catherine of Valois (35); brother of
  # Edmund Tudor (77); later married Catherine Woodville (133)
  addPersonToPed(
    personID = 132, name = "Jasper Tudor",
    sex = "M", momID = 35, dadID = 76,
    url = "https://en.wikipedia.org/wiki/Jasper_Tudor"
  ) %>%
  # Catherine Woodville, Duchess of Buckingham — daughter of Jacquetta of Luxembourg
  # (90) and Richard Woodville (91); sibling of Elizabeth Woodville (56)
  addPersonToPed(
    personID = 133, name = "Catherine Woodville, Duchess of Buckingham",
    sex = "F", momID = 90, dadID = 91,
    url = "https://en.wikipedia.org/wiki/Catherine_Woodville,_Duchess_of_Buckingham"
  ) %>%
  # Henry Stafford, 2nd Duke of Buckingham — son of Humphrey Stafford Earl (129) and
  # Margaret Beaufort Countess of Stafford (127); executed 1483
  addPersonToPed(
    personID = 134, name = "Henry Stafford, 2nd Duke of Buckingham",
    sex = "M", momID = 127, dadID = 129,
    url = "https://en.wikipedia.org/wiki/Henry_Stafford,_2nd_Duke_of_Buckingham"
  )

## Recalculate family groups after all modifications ---------------------------

war_of_the_roses <- df %>%
  select(-famID) %>%
  ped2fam(personID = "personID", famID = "famID") %>%
  rename(id = personID)


## BGmisc pedigree checks ------------------------------------------------------

df_repaired <- checkSex(war_of_the_roses,
  code_male   = "M",
  code_female = "F",
  verbose     = TRUE,
  repair      = TRUE
) %>%
  checkParentIDs(
    addphantoms       = TRUE,
    repair            = TRUE,
    parentswithoutrow = FALSE,
    repairsex         = FALSE
  ) %>%
  rename(personID = ID)


## (Optional) plot — wrapped in if (FALSE) so it never runs automatically ------

if (FALSE) {
  ggpedigree::ggPedigreeInteractive(df_repaired,
    personID = "personID",
    momID = "momID",
    dadID = "dadID",
    famID = "famID",
    config = list(
      code_male               = "M",
      code_female             = "F",
      code_na                 = "U",
      focal_fill_include      = TRUE,
      focal_fill_use_log = TRUE,
      focal_fill_mid_color = "orange",
      focal_fill_low_color = "#9F2A63FF",
      focal_fill_force_zero   = TRUE,
      focal_fill_personID     = 1, # Edward III
      label_column            = "name",
      label_method = "geom_text",
      label_include           = TRUE,
      label_text_angle        = -90,
      label_text_size         = 2,
      label_nudge_x           = -0.05,
      sex_legend_show         = FALSE,
      sex_color_include       = FALSE
    ),
    tooltip_columns = c("personID", "name","focal_fill")
  )
}


## ID and network checks -------------------------------------------------------

checkIDs(df_repaired)

checkis_acyclic <- checkPedigreeNetwork(df_repaired,
  personID = "personID",
  momID    = "momID",
  dadID    = "dadID",
  verbose  = TRUE
)
checkis_acyclic

if (checkis_acyclic$is_acyclic) {
  message("The pedigree is acyclic.")
  write_csv(war_of_the_roses, here("data-raw", "war_of_the_roses.csv"))
  usethis::use_data(war_of_the_roses, overwrite = TRUE, compress = "xz")
} else {
  message("The pedigree contains cyclic relationships.")
}


## Inspect founders ------------------------------------------------------------

war_of_the_roses %>%
  filter(is.na(momID) & is.na(dadID)) %>%
  select(id, name, famID, momID, dadID, sex) %>%
  mutate(
    first_name = str_extract(name, "^[^ ,]+"),
    last_name  = str_extract(name, "[^ ,]+$")
  ) %>%
  arrange(last_name, id)
