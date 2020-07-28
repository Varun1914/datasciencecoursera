## Best Hospital Name display
best <- function(state, outcome) {
    ## Read outcome data and dividing into required parameters
    ## state, hear failure, heart attack and pneumonia
    data_outcome <- read.csv("outcome-of-care-measures.csv")
    d0 <- data.frame(data_outcome$State,
                    data_outcome$Hospital.Name,
                    data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                    data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                    data_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    d0[,3] <- as.numeric(d0[,3])
    d0[,4] <- as.numeric(d0[,4])
    d0[,5] <- as.numeric(d0[,5])
    # sapply(2:4, function(i) do[,i] <- as.numeric(d0[,i]))
    names(d0) <- c("state","hospital_name", "heart attack", "heart failure", "pneumonia" )
    state_name <- unique(d0$state)
    ## Check that statehy and outcome are valid
    for(i in 1:length(state_name))
    {
        if(state_name[i] == state)
            break
    }
    if(i == length(state_name))

                return()

    state_seg <- d0[(d0$state == state),]
    if(outcome == "heart attack" )
        state_seg <- state_seg[order(state_seg$`heart attack`),]
    else if (outcome == "heart failure")
        state_seg <- state_seg[order(state_seg$`heart failure`),]
    else if (outcome == "pneumonia")
        state_seg <- state_seg[order(state_seg$`pneumonia`),]
    else
    {
        message("invalid disease provided !!!!")
        return()
    }
    ## Return hospital name in that state with lowest 30-day death
    print(state_seg[1,2])
    return(state_seg)
    ## rate
}


rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    x <- best(state,outcome)
    x <- na.exclude(x)

    # assigning index value based on num
    index <- if(num =="best")
                1
            else if(num == "worst")
                nrow(x)
            else
                num

    # extracting the information
    s1 <- if(outcome == "heart attack" )
        {
           x[(x$`heart attack` == x[index,3]),]

        }
        else if (outcome == "heart failure")
        {
           x[(x$`heart failure` == x[index,4]),]

        }
        else if (outcome == "pneumonia")
        {
            x[(x$`pneumonia` == x[index,5]),]

        }

    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    i <- 1
    while(s1[i,2] != x[index,2])
    {
        i <- i + 1
    }

    s1 <- s1[order(s1$`hospital_name`),]
    return(s1[i, 2])
}
