#! /usr/bin/awk -f

BEGIN { profit = -1 }
/^[0-9]+ 0000000 0000000 0000000$/ { if (profit != -1) {
                        print profit
                        delete plan
                    }
                    profit = 0
                }
/^[0-9]+ [0-9]+ [0-9]+ [0-9]+$/ { rent($2, $3, $4) }
END { print profit }

function cash(time) {
    t = key(time)
    profit = max(plan[t], profit)
}

function rent(time,duration,price) {
    if(duration) {
        t = key(time + duration)
        plan[t] = max(profit + price, plan[t]) 
    }
    else { 
        cash(time) 
    }
}

function key(t) {
    return(sprintf("%07d",t))
}

function max(a, b) {
    return a > b ? a : b
}

