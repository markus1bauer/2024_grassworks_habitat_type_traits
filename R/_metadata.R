#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# GRASSWORKS Project
# CWMs of EUNIS habitat types ####
# Metadata
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Markus Bauer
# 2025-10-02


### Packages ###
library(here)
library(tidyverse)
library(EML)
library(emld)
# remotes::install_github("ropenscilabs/emldown", build = FALSE)
library(emldown)
# remotes::install_github("EDIorg/EMLassemblyline")
library(EMLassemblyline)

### Start ###
rm(list = ls())



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# A Collect metadata ##########################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



## 1 Methods and units #######################################################


#methods_file <- here("data", "text", "methods.odt")
#methods <- set_methods(methods_file)

#EMLassemblyline::view_unit_dictionary()
# List of standard units, which should be used in metadata file



## 2 Raw data ################################################################

## 3 Processed data ##########################################################

## 4 Put data table together #################################################



## 5 Contact #################################################################


address <- list(
  deliveryPoint = "Emil-Ramann-Strasse 6",
  city = "Freising",
  administrativeArea = "Bavaria",
  postalCode = "85354",
  country = "Germany"
  )

creator <- eml$creator(
  individualName = eml$individualName(
    givenName = "Markus",
    surName = "Bauer"
  ),
  positionName = "PhD student",
  organizationName = "Technical University of Munich",
  address = address,
  electronicMailAddress = "markusbauer@mailbox.org",
  phone = "0049-152-56391781",
  id = "https://orcid.org/0000-0001-5372-4174"
)

associatedParty <- list(
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Alina",
      surName = "Twerski"
    ),
    role = "Researcher",
    organizationName = "Leuphana University Lüneburg",
    electronicMailAddress = "alina.twerski@hs-anhalt.de",
    id = "https://orcid.org/0000-0001-7966-1335"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Christin Juno",
      surName = "Laschke"
    ),
    role = "Researcher",
    organizationName = "Leuphana University Lüneburg",
    electronicMailAddress = "christin.laschke@leuphana.de",
    id = "https://orcid.org/0009-0008-5041-4697"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Annika",
      surName = "Schmidt"
    ),
    role = "Researcher",
    organizationName = "Anhalt University of Applied Sciences",
    electronicMailAddress = "annika.schmidt@hs-anhalt.de",
    id = "https://orcid.org/0000-0002-6414-2505"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Line",
      surName = "Sturm"
    ),
    role = "Researcher",
    organizationName = "Anhalt University of Applied Sciences",
    electronicMailAddress = "line.sturm@hs-anhalt.de",
    id = "https://orcid.org/0009-0002-2735-3060"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Miriam",
      surName = "Wiesmeier"
    ),
    role = "Researcher",
    organizationName = "Technical University of Munich",
    electronicMailAddress = "jakob.huber@posteo.de"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Anita",
      surName = "Kirmer"
    ),
    role = "Professor",
    organizationName = "Anhalt University of Applied Sciences",
    electronicMailAddress = "anita.kirmer@hs-anhalt.de",
    id = "https://orcid.org/0000-0002-2396-713X"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Vicky M.",
      surName = "Temperton"
    ),
    role = "Professor",
    organizationName = "Leuphana University of Lüneburg",
    electronicMailAddress = "vicky.temperton@leuphana.de",
    id = "https://orcid.org/0000-0003-0543-4521"
  ),
  eml$associatedParty(
    individualName = eml$individualName(
      givenName = "Johannes",
      surName = "Kollmann"
    ),
    role = "Professor",
    organizationName = "Technical University of Munich",
    address = address,
    electronicMailAddress = "johannes.kollmann@.tum.de",
    phone = "0049-8161-714144",
    id = "https://orcid.org/0000-0002-4990-3636"
  )
)

contact <- list(
  individualName = creator$individualName,
  electronicMailAddress = creator$electronicMailAddress,
  address = address,
  organizationName = "Technical University of Munich",
  onlineUrl = "https://www.lss.ls.tum.de/en/roek/home/"
)



## 6 Temporal and spatial coverage ###########################################


geographic_description <- "Three regions in northern, central and southern Germany"

coverage <- set_coverage(
  begin = "2022-05-01", end = "2023-07-31",
  sci_names = list(list(
    Subdivision = "Spermatophytina"
  )),
  geographicDescription = geographic_description,
  west = 10.168, east = 13.017,
  north = 54.377, south = 48.219,
  altitudeMin = 1, altitudeMaximum = 502,
  altitudeUnits = "meter"
)



## 7 Description #############################################################


title <- "Little sign of recovery debt for functional traits when comparing restorations with positive references, but clear delineation of negative references"

pubDate <- "2025"

alternate_identifier <- ""

abstract <- "This study is based on the Grassworks data (Temperton et al. 2025, https://doi.org/10.1111/rec.70109). Location: Mesic hay meadows and semi-dry calcareous grasslands in three ecoregions in northern, central, and southern Germany. Methods: In total, 621 vegetation surveys were conducted at restored sites (401) as well as positive (102) and negative references (118). All vegetation surveys were assigned to EUNIS habitat types. Community-weighted means (CWM) were calculated for specific leaf area (SLA), canopy height, and seed mass with trait values from the GIFT database"


# LTER controlled vocabulary
# https://vocab.lternet.edu/vocab/vocab/index.php?_search_expresion=vegetation
keyword_set <- list(
  list(
    keywordThesaurus = "LTER controlled vocabulary",
    keyword = list(
      "grasslands",
      "meadows",
      "monitoring",
      "plant communities",
      "plant height",
      "plant species composition",
      "restoration",
      "seeds",
      "specific leaf area",
      "vegetation"
      )
  ),
  list(
    keywordThesaurus = "own vocabulary",
    keyword = list(
      "community-weighted mean",
      "community assembly",
      "community ecology",
      "plant functional traits",
      "temperate grassland"
      )
  )
)

license <- list(
  licenseName = "CC-BY-4.0",
  url = "https://creativecommons.org/licenses/by/4.0/deed.en"
)

short_name <- "Grassworks: habitat type traits"

language <- "English"



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# B finalize EML ##############################################################
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



dataset <- list(
  title = title,
  shortName = short_name,
  pubDate = pubDate,
  creator = creator,
  associatedParty = associatedParty,
  licensed = license,
  alternateIdentifier = alternate_identifier,
  abstract = abstract,
  keywordSet = keyword_set,
  coverage = coverage,
  language = language,
  contact = contact#,
  #methods = methods,
  #dataTable = dataTable,
  #additonalMetadata = list(metadata = list(unitList = unitList))
  )

eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid", # type of identifier
  dataset = dataset
  )

write_eml(eml, here("METADATA.xml"))
eml_validate(here("METADATA.xml"))

render_eml(
 file = here("METADATA.xml"), outfile = "METADATA.html",
 open = TRUE, publish_mode = TRUE
 )
