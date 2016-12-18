\ bsearch.fs source: http://rosettacode.org/wiki/Binary_search#Forth

defer (compare)
' - is (compare) \ default to numbers
 
: cstr-compare ( cstr1 cstr2 -- <=> ) \ counted strings
  swap count rot count compare ;
 
: mid ( u l -- mid ) tuck - 2/ -cell and + ;
 
: bsearch ( item upper lower -- where found? )
  rot >r
  begin  2dup >
  while  2dup mid
         dup @ r@ (compare)
         dup
  while  0<
         if   nip cell+   ( upper mid+1 )
         else rot drop swap ( mid lower )
         then
  repeat drop nip nip             true
  else   max ( insertion-point ) false
  then
  r> drop ;
