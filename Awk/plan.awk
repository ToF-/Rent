#! /usr/bin/awk -f

BEGIN { case = -2 }
/^[0-9]+$/ { case = case + 1
             if(case >= 0) {
                printf "%02d %07d %07d %07d\n", case, 0, 0, 0
            }
        }
/[0-9]+ [0-9]+ [0-9]+/ { printf "%02d %07d %07d %07d\n%02d %07d %07d %07d\n", 
                                 case, $1+$2, 0, 0, 
                                 case, $1,   $2, $3 }
