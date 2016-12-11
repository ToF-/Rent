Solving our problem in C will involve very few tools and libraries. We need a way to read the input and print results of course, and computing profit will be done using solely sorting and binary search. 

The algorithm to follow for each case in the input is described below:

initialization:

- read all orders in an array *Rent*  ( *start* = *order.start_time*, *end* = *start+order.duration*, *price* )
- initialize an array of all time points given by each *Rent* start and end times 
- sort the array of time points
- initialize an array *Plan* of cells given by each unique time point ( *time*, *value* )
- sort the array *Rent* in ascending order of start time

computation:

- *Profit* = 0
- current rent index *r* = 0
- current plan index *p* = 0
- while (*p* < size of *Plan*)
    - *Profit* = max(*Profit*, *Plan[p].value*)
    - if (*r* < size of *Rent* and *Rent[r].start* < *Plan[p].time*)
       - find in *Plan* from (*p*+1) to (size of *Plan* - 1), the position *e* such that *Plan[e].time* = *Rent[r].end*
       - *Plan[e].value* = max(*Plan[e].value*, *Profit* + *Rent[r].price*)
       - *r* = *r* + 1
    - else
       -   *p* = *p* + 1
