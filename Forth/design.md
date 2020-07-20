
adapt this
````forth
: mid ( l r -- mid ) over - 2/ -cell and + ;
 
: exch ( addr1 addr2 -- ) dup @ >r over @ swap ! r> swap ! ;
 
: partition ( l r -- l r r2 l2 )
  2dup mid @ >r ( r: pivot )
  2dup begin
    swap begin dup @  r@ < while cell+ repeat
    swap begin r@ over @ < while cell- repeat
    2dup <= if 2dup exch >r cell+ r> cell- then
  2dup > until  r> drop ;
 
: qsort ( l r -- )
  partition  swap rot
  \ 2over 2over - + < if 2swap then
  2dup < if recurse else 2drop then
  2dup < if recurse else 2drop then ;
 
: sort ( array len -- )
  dup 2 < if 2drop exit then
  1- cells over + qsort ;
```

to an array of pointer to orders

in `partition` 
each `<` between cell values becomes `order@<` 
```Forth
: order@< ( a,a' -- f )
    @ order@
    rot @ order@
    2swap
    d< ;
```

the `exch` is left unchanged, since we echange pointers instead of values

to create an array of order pointers
```Forth
: orders->pointers ( a,l,ap -- )
    swap
    0 do      \ a,ap
        2dup !  \ a,ap -- ap <- a
        cell+   \ a,ap+1 --
        swap order% + swap \ a+o%,ap+1
    loop 2drop ;
```

once we have a sorted array of order pointers, creating a new sorted array of orders is done by storing the orders in `ap @` into `a`
and then storing the order in `ap cell+ @` into `a order% +` and so on

or we can search the array of pointers instead of searching the array, using indirection before comparisons
    

