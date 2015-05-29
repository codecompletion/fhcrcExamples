library(RODBC); library(Rcpp);

## Define dir for BLOB xor SQL export
BinaryPath <- FALSE
SqlTable <- FALSE

## Input for reading in data treatment objects
InputPath <- "//tax/"

## UDFs ##

# Remove leading spaces
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Given `input' list of space split strings, 
# remove `vector' from all, paste together.
vectorErase <- function(input, vector) {
	subString <- paste0("\\s+",vector,"\\s+", collapse="|")  
	return( 
		paste0(gsub(subString, " ", input, perl=T, ignore.case = T)) 
	)
}

# Retrieve all obs of `vars' from SQL table
GetAllTax <- function(MatchSetChannel,vars) {
	return(
		sqlQuery(MatchSetChannel, 
						 paste0(
						 	"select ",vars," from TaxData "
						 )              
						 ,stringsAsFactors=F)
	)
}

## Compiles, loads appendRcpp()
# Use to preallocate matrix
# Speeds up assign for ragged arrays
# sourceCpp(paste0(InputPath, "cppHelpers.cpp"))
# Deprecated, use if string splitting

## OPS ##


## Define columns
ColumnNamesMatchandAppend <- read.csv("/ColumnNamesMatchandAppend.csv")
ColumnNamesMatchandAppend$X <- NULL

## Data treatment objects
# For Street Name Suffixes
StreetNamesandAbbreviationsFinal <- read.csv(paste0(InputPath,"/StreetNamesandAbbreviationsFinal.csv"))
StreetNamesandAbbreviationsFinal <- as.character(as.array(StreetNamesandAbbreviationsFinal[,1]))
StreetNamesandAbbreviationsFinal <- toupper(gsub("\\.","\\\\\\.",StreetNamesandAbbreviationsFinal))

# Added to prevent grep compiler crash
StreetNamesandAbbreviationsFinal <- unique(gsub("\\\\\\.","",StreetNamesandAbbreviationsFinal))

# For Street Name Directionals
CompassRoseFinal <- read.csv(paste0(InputPath,"/CompassRoseFinal.csv")) 
CompassRoseFinal <- as.character(as.array(CompassRoseFinal[,1]))

# For Street Number
StreetNumberPossibilities <- read.csv(paste0(InputPath,"/StreetNumberPossibilities.csv"))
StreetNumberPossibilities <- as.character(as.array(StreetNumberPossibilities[,1]))

## For City
TaxDataInformation <- read.csv(paste0(InputPath,"/TaxDataInformation.csv"))
CityDirections <- c("NORTH","SOUTH","EAST","WEST", "NORTHEAST","NORTHWEST","SOUTHEAST","SOUTHWEST")


## Variables for query
vars <- "[FIPS CODE], [ETC] "

## SQL Connection string to data provider
MatchSetChannel <- odbcDriverConnect("driver={SQL Server}; etc.")

## Retrieve data
# Benchmark as needed
# ptm <- proc.time()
MatchSet <- GetAllTax(MatchSetChannel,vars)
# proc.time() - ptm

## Prepare Matchset
# Use only first 5 of zip code
# NOTE: Assume each new vector to consume at least 2 GB of RAM to hold in env at peak
MatchSet$ZipCodeCorrected <- substr(MatchSet$"PROPERTY ZIPCODE",1,5)      

# Use leading 0 for FIPS code
MatchSet$FIPSCorrected <- formatC(MatchSet$"FIPS CODE",width=5,flag = "0")

# Treatment for Street Num
# Benchmark: 5-10 minutes per op, 147 million obs
MatchSet$PropertyHouseNumber1 <- ifelse(is.na(MatchSet$"PROPERTY HOUSE NUMBER PREFIX") | 
																					MatchSet$"PROPERTY HOUSE NUMBER PREFIX"==0 |
																					MatchSet$"PROPERTY HOUSE NUMBER PREFIX" == "","",MatchSet$"PROPERTY HOUSE NUMBER PREFIX")
MatchSet$PropertyHouseNumber2 <- ifelse(is.na(MatchSet$"PROPERTY HOUSE NUMBER") | 
																					MatchSet$"PROPERTY HOUSE NUMBER"==0 |
																					MatchSet$"PROPERTY HOUSE NUMBER" == "","",as.integer(MatchSet$"PROPERTY HOUSE NUMBER"))
MatchSet$PropertyHouseNumber3 <- ifelse(is.na(MatchSet$"PROPERTY HOUSE NUMBER SUFFIX") | 
																					MatchSet$"PROPERTY HOUSE NUMBER SUFFIX"==0 |
																					MatchSet$"PROPERTY HOUSE NUMBER SUFFIX"== "","",MatchSet$"PROPERTY HOUSE NUMBER SUFFIX")
MatchSet$PropertyHouseNumber4 <- ifelse(is.na(MatchSet$"PROPERTY APARTMENT UNIT NUMBER") | 
																					MatchSet$"PROPERTY APARTMENT UNIT NUMBER"==0 |
																					MatchSet$"PROPERTY APARTMENT UNIT NUMBER" == "","",MatchSet$"PROPERTY APARTMENT UNIT NUMBER")
MatchSet$HouseNumberCorrected <- paste0(MatchSet$PropertyHouseNumber1,MatchSet$PropertyHouseNumber2,MatchSet$PropertyHouseNumber3,MatchSet$PropertyHouseNumber4)
MatchSet$HouseNumberCorrected <- gsub("[[:punct:]]| ", "", toupper(as.character(MatchSet$HouseNumberCorrected)))

# Initial treatment for Street Name Directional
MatchSet$DirectionCorrected <- ifelse(is.na(MatchSet$"PROPERTY DIRECTION") | MatchSet$"PROPERTY DIRECTION"==0,"",MatchSet$"PROPERTY DIRECTION")

# Initial treatment for City
MatchSet$CityUpper <- toupper(as.character(MatchSet$"PROPERTY CITY"))
MatchSet$CityCorrected <- ""


# Prepare City using grep function
MatchSet$CityCorrected <- sapply( MatchSet$"PROPERTY CITY", vectorErase, CityDirections )
MatchSet$CityCorrected <- trim(gsub("[[:punct:]]| ", "", toupper(as.character(MatchSet$CityCorrected))))

# Prepare Street Name using grep function.
MatchSet$StreetNameCorrected <- sapply( MatchSet$"PROPERTY STREET NAME", vectorErase, StreetNamesandAbbreviationsFinal )
MatchSet$StreetNameCorrected <- trim(gsub("[[:punct:]]| ", "", toupper(as.character(MatchSet$StreetNameCorrected))))


## Code when we used string split:
# temp <- appendRcpp( str_split(z) )
# output$z <- MatchSet$StreetNameCorrected <- sapply( 1:length(temp), 
#                                         function(x) vectorErase(temp[[x]], StreetNamesandAbbreviationsFinal) )


# Return for binary
if( BinaryPath & !SqlTable ) save(MatchSet, BinaryPath)

# Return for SQL write
if( !BinaryPath & SqlTable ) sqlSave(MatchSetChannel, MatchSet, SqlTable)

# Write variable error handling
if( (BinaryPath & SqlTable) | (!BinaryPath & !SqlTable) cat("Write failed!\n Select either binary or SQL write.")