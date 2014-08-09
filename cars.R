library(XML)
library(httr)
library(stringr)

urls <- c("http://www.gumtree.com/p/cars-vans-motorbikes/toyota-prius-t-spirit/1075927622",
          "http://www.gumtree.com/p/cars-vans-motorbikes/toyota-prius-07-15-vvti-t-spirit-low-mileage-40300/1076091221#photo-content",
          #"http://www.autotrader.co.uk/classified/advert/201407185922369/sort/locasc/make/toyota/quicksearch/true/radius/30/page/2/usedcars/postcode/ha27lb/advert-type/Classified/onesearchad/used/model/prius/price-to/8500?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201407306216474/sort/locasc/quicksearch/true/advert-type/Classified/price-to/8500/usedcars/page/1/onesearchad/used/radius/30/model/prius/postcode/ha27lb/make/toyota?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201406255291862/sort/locasc/quicksearch/true/advert-type/Classified/price-to/8500/usedcars/page/1/onesearchad/used/radius/30/model/prius/postcode/ha27lb/make/toyota?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201408056374096/sort/locasc/postcode/ha27lb/make/toyota/radius/30/advert-type/Classified/page/1/quicksearch/true/price-to/8500/model/prius/onesearchad/used/usedcars?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201408066403894/sort/default/usedcars/postcode/ha27lb/model/prius/onesearchad/used/price-to/8500/radius/30/quicksearch/true/make/toyota/page/3/advert-type/Classified?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201407246050898/sort/default/make/toyota/postcode/ha27lb/page/1/price-to/10000/advert-type/Classified/onesearchad/used%2Cnearlynew/maximum-mileage/up_to_50000_miles/radius/20/model/prius/usedcars?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201408056351520/sort/default/advert-type/Classified/usedcars/model/prius/radius/20/page/1/maximum-mileage/up_to_50000_miles/postcode/ha27lb/onesearchad/used%2Cnearlynew/price-to/10000/make/toyota?logcode=p",
          "http://www.autotrader.co.uk/classified/advert/201407075620591/sort/default/advert-type/Classified/radius/20/onesearchad/used%2Cnearlynew/postcode/ha27lb/page/1/price-to/10000/usedcars/maximum-mileage/up_to_50000_miles/model/prius/make/toyota?logcode=p",
          "http://www.ebay.co.uk/itm/2008-TOYOTA-PRIUS-1-5-VVTi-T4-Hybrid-5dr-CVT-Auto-/311035245664?pt=Automobiles_UK&hash=item486b253460",
          "http://www.ebay.co.uk/itm/2008-Toyota-Prius-1-5-T4-Hybrid-5-Dr-Silver-/400752991720?pt=Automobiles_UK&hash=item5d4ebd5de8",
          "http://www.ebay.co.uk/itm/2007-57-Toyota-Prius-1-5-CVT-T3-Hybrid-1-Owner-From-Brand-New-Leather-Seats-/311035245928?pt=Automobiles_UK&hash=item486b253568",
          #"http://www.ebay.co.uk/itm/Toyota-Prius-1-5-CVT-T-Spirit-/251555123470?pt=Automobiles_UK&hash=item3a91da910e",
          "http://www.ebay.co.uk/itm/2009-TOYOTA-PRIUS-1-5-VVTi-T4-Hybrid-5dr-CVT-Auto-/121396646382?pt=Automobiles_UK&hash=item1c43cdd1ee",
          "http://www.ebay.co.uk/itm/2008-Toyota-Prius-1-5-CVT-T4-Hybrid-5DR-08-REG-Petrol-Black-/131210274172?pt=Automobiles_UK&hash=item1e8cbde57c",
          "http://www.ebay.co.uk/itm/Toyota-Prius-1-5-CVT-T-Spirit-31K-MILES-FULL-BLACK-LEATHER-/360999964771?pt=Automobiles_UK&hash=item540d465063",
          #"http://www.gumtree.com/p/cars-vans-motorbikes/2007-57-toyota-prius-15-cvt-t3-hybrid-1-owner-from-brand-new-leather-seats/1075368078#photo-content",
          #"http://www.gumtree.com/p/cars-vans-motorbikes/2008-toyota-prius-15-vvti-t4-hybrid-5dr-cvt-auto/1075361740",
          "http://www.gumtree.com/p/cars-vans-motorbikes/toyota-prius-t-spirit-for-sale/1074803486",
          "http://www.gumtree.com/p/cars-vans-motorbikes/2007-toyota-prius-t3-34-000-miles-black-full-dealership-service-history-12-months-motimmaculate/1073999298",
          "http://www.drive24.co.uk/hertfordshire/vehicledetailsview.aspx?carid=35851602&searchguid=29516857-0e63-4b7f-bc20-9a22e2443458",
          "http://www.drive24.co.uk/hertfordshire/vehicledetailsview.aspx?carid=35335474&searchguid=29516857-0e63-4b7f-bc20-9a22e2443458",
          "http://www.drive24.co.uk/hertfordshire/vehicledetailsview.aspx?carid=35820518&searchguid=29516857-0e63-4b7f-bc20-9a22e2443458",
          "http://www.drive24.co.uk/hertfordshire/vehicledetailsview.aspx?carid=35843990&searchguid=29516857-0e63-4b7f-bc20-9a22e2443458"
          
)

fsh <- function(specs) {
  fsh = "Y"
  hasfsh = regexpr("(full.*history|fsh\\b)", specs, ignore.case=TRUE)[[1]]
  if (hasfsh == -1) fsh = "N"
  fsh
}

autotraderExtractor <- function(url) {
  html <- htmlTreeParse(url, useInternalNodes=T)
  price <- xpathSApply(html, "//span[@id='price']", xmlValue)
  car <- xpathSApply(html, "//span[@id='fullPageMainTitle']", xmlValue)
  sellerspecs <- xpathSApply(html, "//p[@class='sellerspecs-para']", xmlValue)
  listFacts <- xpathSApply(html, "//*[@id='fpa']/div[3]/div[4]/ul", xmlValue)
  distance <- xpathSApply(html, "//div[@class='fpa-printcontactname']/p", xmlValue)
  if  (length(distance) ==0) {
    distance <- xpathSApply(html, "//div[@class='sellerDistance']/p", xmlValue)
  }
  phone <- xpathSApply(html, "//span[@data-test='dealerContacts-phone1']", xmlValue)
  if (length(phone) == 0) {
    phone <- xpathSApply(html, "//span[@data-test='sellerContacts-phone1']", xmlValue)
  }
  phone = phone[[1]]
  facts <- strsplit(listFacts,"\n")[[1]]
  year = facts[1]
  mileage = facts[5]
  mileage = str_trim(sub("Miles", "", mileage, ignore.case = TRUE))
  mileage = sub(",", "", mileage)
  row <- cbind(year, mileage, price, fsh(sellerspecs), car, distance,phone,  sellerspecs,url)
  row
}

ebayExtractor <- function(url) {
  html <- htmlTreeParse(url, useInternalNodes=T)
  price <- xpathSApply(html, "//span[@id='prcIsum']", xmlValue)
  car <- xpathSApply(html, "//*[@id='itemTitle']/text()", xmlValue)
  distance <- xpathSApply(html, "//*[@id='LeftSummaryPanel']/div/form/div[15]", xmlValue)
  sellerspecs <- str_trim(xpathSApply(html, "//*[@id='emp_description']", xmlValue))
  phone <- str_trim(xpathSApply(html, "//*[@id='emp_contact_box_overview_phone']/text()[3]", xmlValue))
  year <- xpathSApply(html, "//*[@id='emp_search_attributes']/div[2]/div[2]/b", xmlValue)
  mileage = xpathSApply(html, "//*[@id='emp_search_attributes']/div[2]/div[3]/b", xmlValue)
  mileage = str_trim(sub("Miles", "", mileage, ignore.case = TRUE))
  mileage = sub(",", "", mileage)
  row <- cbind(year, mileage, price, fsh(sellerspecs), car, distance,phone,  sellerspecs,url)
  row
}

gumtreeExtractor <- function(url) {
  html <- htmlTreeParse(url, useInternalNodes=T)
  price <- xpathSApply(html, "//*[@id='primary-h1']/span[2]/span", xmlValue)
  car <- xpathSApply(html, "//*[@id='primary-h1']/span[1]", xmlValue)
  distance <- xpathSApply(html, "//*[@id='breadcrumbs']/li[4]/a", xmlValue)
  sellerspecs <- str_trim(xpathSApply(html, "//*[@id='vip-description-text']", xmlValue))
  sellerspecs <-  str_replace_all(sellerspecs, "\r", " ")
  phone <- xpathSApply(html, "//*[@id='contact']/span[2]/span[1]", xmlValue)
  if (is.null(phone)) phone <- "see desc"
  year <- xpathSApply(html, "//*[@id='vip-attributes']/li[3]/p", xmlValue)
  mileage = xpathSApply(html, "//*[@id='vip-attributes']/li[4]/p", xmlValue)
  mileage = str_trim(sub("Miles", "", mileage, ignore.case = TRUE))
  mileage = sub(",", "", mileage)
  row <- cbind(year, mileage, price,fsh(sellerspecs), car, distance,phone,  sellerspecs,url)
  row
}

drive24Extractor <- function(url) {
  html <- htmlTreeParse(url, useInternalNodes=T)
  price <- xpathSApply(html, "//*[@id='carHeader']/div[2]/h2", xmlValue)
  car <- xpathSApply(html, "//*[@id='carHeader']/div[1]/h2", xmlValue)
  distance <- xpathSApply(html, "//*[@id='contact']/li[4]/h4[1]", xmlValue)
  if (is.null(distance)) distance <- "on request"
  sellerspecs <- xpathSApply(html, "//*[@id='mainDetails']/div[2]/div/p/text()", xmlValue)
  sellerspecs <-  str_replace_all(sellerspecs, "\n", " ")
  sellerspecs <-  str_replace_all(sellerspecs, "\r", " ")
  phone <- xpathSApply(html, "//*[@id='contact']/li[4]/h4[2]", xmlValue)
  if (is.null(phone)) phone <- "see desc"
  year <- xpathSApply(html, "//*[@id='mainDetails']/div[2]/ul[2]/li[1]/text()", xmlValue)
  year <- grep("\\d\\d\\d\\d", year, value = TRUE)
  mileage = xpathSApply(html, "//*[@id='mainDetails']/div[2]/ul[2]/li[2]/text()", xmlValue)
  mileage = str_trim(sub("Miles", "", mileage, ignore.case = TRUE))
  mileage = sub(",", "", mileage)
  row <- cbind(year, mileage,price, fsh(sellerspecs),car, distance,phone, sellerspecs,url)
  row
}

df <- data.frame()

for (url in urls) {
  print(url)
  u = parse_url(url)
  if (u["hostname"] == "www.autotrader.co.uk") {
    r = autotraderExtractor(url)
    df <- rbind(df,r)
  }
  if (u["hostname"] == "www.ebay.co.uk") {
    r = ebayExtractor(url)
    df <- rbind(df,r)
  }
  if (u["hostname"] == "www.gumtree.com") {
    r = gumtreeExtractor(url)
    df <- rbind(df,r)
  }
  if (u["hostname"] == "www.drive24.co.uk") {
    r = drive24Extractor(url)
    df <- rbind(df,r)
  }
  write.csv(df, "c:/data/prii.xls")
}


