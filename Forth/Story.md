<link rel="stylesheet" href="./story.css">
#RENT in FORTH!

Let's recap the algorithm at the center of the program that we want to implement:

> given:<br>
>    O, an array of length N containing orders, each order being defined by its start time, duration, and price;<br>
>    B, an array of length N+1 containing the best profit at the start time of each order, except for B[N] = 0;<br>
> 
> sort O by order start time;<br>
> for i starting at N-1 until 0:<br>
>   compute B[i] : max(price(O[i]) + B[k], B[i+1])<br>
>   where k = mimimum[ j | start_time(O[j]) ≥ start_time(O[i]) + duration(O[i])
>
> B[0] = best profit for the array of orders.
