assign_simpage <- function (x) {
	x$simpage[x$age_group==1] <- 1
	x$simpage[x$age_group==2] <- 2
	x$simpage[x$age_group==3] <- 3
	x$simpage[x$age_group>3] <- 4
	return(x$simpage)
}

assign_simprace <- function (x){
	x$simprace[x$race==1] <-1
	x$simprace[x$race==2] <-2
	x$simprace[x$race==3] <-9
	x$simprace[x$race==4] <-9
	x$simprace[x$race==5] <-9
	x$simprace[x$race==6] <-9
	x$simprace[x$race==8] <-8
	x$simprace[x$race==9] <-9
	return(x$simprace)
}

assign_simppay <- function (x){
	x$simppay[x$payor==1] <-1
	x$simppay[x$payor==2] <-2
	x$simppay[x$payor==3] <-3
	x$simppay[x$payor==4] <-9
	x$simppay[x$payor==5] <-9
	x$simppay[x$payor==6] <-9
	x$simppay[x$payor==7] <-9
	x$simppay[x$payor==8] <-8
	x$simppay[x$payor==9] <-9
	x$simppay[x$payor==10] <-9
	x$simppay[x$payor==11] <-9
	return(x$simppay)
}

assign_simpsource <- function (x){
	x$simpsource[x$source==1] <-1
	x$simpsource[x$source==2] <-1
	x$simpsource[x$source==3] <-1
	x$simpsource[x$source==4] <-2
	x$simpsource[x$source==5] <-1
	x$simpsource[x$source==6] <-1
	x$simpsource[x$source==7] <-7
	x$simpsource[x$source==8] <-1
	x$simpsource[x$source==9] <-1
	x$simpsource[x$source==10] <-1
	x$simpsource[x$source==11] <-1
	return(x$simpsource)
}

assign_simpage2 <- function (x) {
	x$simpage[x$age_group2==1] <- 1
	x$simpage[x$age_group2==2] <- 2
	x$simpage[x$age_group2==3] <- 3
	x$simpage[x$age_group2>3] <- 4
	return(x$simpage)
}


assign_simprace2 <- function (x){
	x$simprace[x$race2==1] <- "White"
	x$simprace[x$race2==2] <- "Black"
	x$simprace[x$race2==3] <- "Other"
	x$simprace[x$race2==4] <- "Other"
	x$simprace[x$race2==5] <- "Other"
	x$simprace[x$race2==6] <- "Other"
	x$simprace[x$race2==8] <- "Hispanic"
	x$simprace[x$race2==9] <- "Other"
	return(x$simprace)
}


assign_simppay2 <- function (x){
	x$simppay[x$payor2==1] <-1
	x$simppay[x$payor2==2] <-2
	x$simppay[x$payor2==3] <-3
	x$simppay[x$payor2==4] <-9
	x$simppay[x$payor2==5] <-9
	x$simppay[x$payor2==6] <-9
	x$simppay[x$payor2==7] <-9
	x$simppay[x$payor2==8] <-8
	x$simppay[x$payor2==9] <-9
	x$simppay[x$payor2==10] <-9
	x$simppay[x$payor2==11] <-9
	return(x$simppay)
}

assign_decade_index <- function (x) {
	x$simpage[x$decade_index==1] <- 1
	x$simpage[x$decade_index==2] <- 2
	x$simpage[x$decade_index==3] <- 3
	x$simpage[x$decade_index>3] <- 4
	return(x$simpage)
}

assign_simprace_index <- function (x){
	x$simprace[x$race_index==1] <-1
	x$simprace[x$race_index==2] <-2
	x$simprace[x$race_index==3] <-9
	x$simprace[x$race_index==4] <-9
	x$simprace[x$race_index==5] <-9
	x$simprace[x$race_index==6] <-9
	x$simprace[x$race_index==8] <-8
	x$simprace[x$race_index==9] <-9
	return(x$simprace)
}

assign_simppay_index <- function (x){
	x$simppay[x$payor_index==1] <-1
	x$simppay[x$payor_index==2] <-2
	x$simppay[x$payor_index==3] <-3
	x$simppay[x$payor_index==4] <-9
	x$simppay[x$payor_index==5] <-9
	x$simppay[x$payor_index==6] <-9
	x$simppay[x$payor_index==7] <-9
	x$simppay[x$payor_index==8] <-8
	x$simppay[x$payor_index==9] <-9
	x$simppay[x$payor_index==10] <-9
	x$simppay[x$payor_index==11] <-9
	return(x$simppay)
}

assign_simpsource_index <- function (x){
	x$simpsource[x$source_index==1] <-1
	x$simpsource[x$source_index==2] <-1
	x$simpsource[x$source_index==3] <-1
	x$simpsource[x$source_index==4] <-2
	x$simpsource[x$source_index==5] <-1
	x$simpsource[x$source_index==6] <-1
	x$simpsource[x$source_index==7] <-7
	x$simpsource[x$source_index==8] <-1
	x$simpsource[x$source_index==9] <-1
	x$simpsource[x$source_index==10] <-1
	x$simpsource[x$source_index==11] <-1
	return(x$simpsource)
}
assign_simpsource_index <- function (x){
	x$simpsource[x$source_index==1] <-1
	x$simpsource[x$source_index==2] <-1
	x$simpsource[x$source_index==3] <-1
	x$simpsource[x$source_index==4] <-2
	x$simpsource[x$source_index==5] <-1
	x$simpsource[x$source_index==6] <-1
	x$simpsource[x$source_index==7] <-7
	x$simpsource[x$source_index==8] <-1
	x$simpsource[x$source_index==9] <-1
	x$simpsource[x$source_index==10] <-1
	x$simpsource[x$source_index==11] <-1
	return(x$simpsource)
}



assign_decade_readmit <- function (x) {
	x$simpage[x$decade_readmit==1] <- 1
	x$simpage[x$decade_readmit==2] <- 2
	x$simpage[x$decade_readmit==3] <- 3
	x$simpage[x$decade_readmit>3] <- 4
	return(x$simpage)
}

assign_simprace_readmit <- function (x){
	x$simprace[x$race_readmit==1] <-1
	x$simprace[x$race_readmit==2] <-2
	x$simprace[x$race_readmit==3] <-9
	x$simprace[x$race_readmit==4] <-9
	x$simprace[x$race_readmit==5] <-9
	x$simprace[x$race_readmit==6] <-9
	x$simprace[x$race_readmit==8] <-8
	x$simprace[x$race_readmit==9] <-9
	return(x$simprace)
}

assign_simppay_readmit <- function (x){
	x$simppay[x$payor_readmit==1] <-1
	x$simppay[x$payor_readmit==2] <-2
	x$simppay[x$payor_readmit==3] <-3
	x$simppay[x$payor_readmit==4] <-9
	x$simppay[x$payor_readmit==5] <-9
	x$simppay[x$payor_readmit==6] <-9
	x$simppay[x$payor_readmit==7] <-9
	x$simppay[x$payor_readmit==8] <-8
	x$simppay[x$payor_readmit==9] <-9
	x$simppay[x$payor_readmit==10] <-9
	x$simppay[x$payor_readmit==11] <-9
	return(x$simppay)
}

assign_simpsource_readmit <- function (x){
	x$simpsource[x$source_readmit==1] <-1
	x$simpsource[x$source_readmit==2] <-1
	x$simpsource[x$source_readmit==3] <-1
	x$simpsource[x$source_readmit==4] <-2
	x$simpsource[x$source_readmit==5] <-1
	x$simpsource[x$source_readmit==6] <-1
	x$simpsource[x$source_readmit==7] <-7
	x$simpsource[x$source_readmit==8] <-1
	x$simpsource[x$source_readmit==9] <-1
	x$simpsource[x$source_readmit==10] <-1
	x$simpsource[x$source_readmit==11] <-1
	return(x$simpsource)
}
assign_simpsource_readmit <- function (x){
	x$simpsource[x$source_readmit==1] <-1
	x$simpsource[x$source_readmit==2] <-1
	x$simpsource[x$source_readmit==3] <-1
	x$simpsource[x$source_readmit==4] <-2
	x$simpsource[x$source_readmit==5] <-1
	x$simpsource[x$source_readmit==6] <-1
	x$simpsource[x$source_readmit==7] <-7
	x$simpsource[x$source_readmit==8] <-1
	x$simpsource[x$source_readmit==9] <-1
	x$simpsource[x$source_readmit==10] <-1
	x$simpsource[x$source_readmit==11] <-1
	return(x$simpsource)
}

