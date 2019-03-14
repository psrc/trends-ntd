# abbr <- c("Central Puget Sound Regional Transit Authority"="ST",
#           "King County Department of Transportation - Metro Transit Division"="KCM",
#           "Everett Transit"="ET",
#           "Kitsap Transit"="KT",
#           "Pierce County Transportation Benefit Area Authority"="PT",
#           "Snohomish County Public Transportation Benefit Area Corporation"="CT",
#           "Pierce County Ferry Operations"="PCF",
#           "City of Seattle - Seattle Center Monorail Transit"="SDOT",
#           "Washington State Ferries"="WSF",
#           "King County Ferry District"="KCF",
#           "King County Department of Transportation"="KCF"
# )

agency.list <- list(list(fullname = "Central Puget Sound Regional Transit Authority", abbr = "ST", alias = "Sound Transit"),
                    list(fullname = "King County Department of Transportation - Metro Transit Division", abbr = "KCM", alias = "King County Metro"),
                    list(fullname = "Everett Transit", abbr = "ET", alias = "Everett Transit"),
                    list(fullname = "Kitsap Transit", abbr = "KT", alias = "Kitsap Transit"),
                    list(fullname = "Pierce County Transportation Benefit Area Authority", abbr = "PT", alias = "Pierce Transit"),
                    list(fullname = "Snohomish County Public Transportation Benefit Area Corporation", abbr = "CT", alias = "Community Transit"),
                    list(fullname = "Pierce County Ferry Operations", abbr = "PCF", alias = "Pierce County Ferries"),
                    list(fullname = "City of Seattle - Seattle Center Monorail Transit", abbr = "SDOT", alias = "City of Seattle"),
                    list(fullname = "Washington State Ferries", abbr = "WSF", alias = "Washington State Ferries"),
                    list(fullname = "King County Ferry District", abbr = "KCF", alias = "King County Marine Division"),
                    list(fullname = "King County Department of Transportation", abbr = "KCF", alias = "King County Marine Division")
                    )

modes.lookup <- data.frame(Modes=c("CB","TB","MB", "LR","SR", "CR", 
                                   "FB", "VP", "MO","MG", "DR","DT"),
                           AModes=c("bus", "bus", "bus", "light_rail", "light_rail", 
                                    "commuter_rail", "ferry", "vanpool", "monorail", "monorail", 
                                    "demand_reponse", "demand_reponse"))

valuetype.lookup <- c("UPT" = "Boardings",
                      "VRM" = "Revenue Miles",
                      "VRH" = "Revenue Hours")