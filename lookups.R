abbr <- c("Central Puget Sound Regional Transit Authority"="ST",
          "King County Department of Transportation - Metro Transit Division"="KCM",
          "Everett Transit"="ET",
          "Kitsap Transit"="KT",
          "Pierce County Transportation Benefit Area Authority"="PT",
          "Snohomish County Public Transportation Benefit Area Corporation"="CT",
          "Pierce County Ferry Operations"="PCF",
          "City of Seattle - Seattle Center Monorail Transit"="SDOT",
          "Washington State Ferries"="WSF",
          "King County Ferry District"="KCF",
          "King County Department of Transportation"="KCF"
)

modes.lookup <- data.frame(Modes=c("CB","TB","MB", "LR","SR", "CR", 
                                   "FB", "VP", "MO","MG", "DR","DT"),
                           AModes=c("bus", "bus", "bus", "light_rail", "light_rail", 
                                    "commuter_rail", "ferry", "vanpool", "monorail", "monorail", 
                                    "demand_reponse", "demand_reponse"))