REQUIRE Rent.fs

CREATE ORDERS

633801 787631 4831 MAKE-ORDER 2,
169364 205610 5826 MAKE-ORDER 2,
624081 670441 74451 MAKE-ORDER 2,
782246 591410 61449 MAKE-ORDER 2,
338420 204078 27498 MAKE-ORDER 2,
438532 581929 80400 MAKE-ORDER 2,
805570 592179 92921 MAKE-ORDER 2,
998383 572991 33659 MAKE-ORDER 2,
788405 872314 7928 MAKE-ORDER 2,
631091 392005 71511 MAKE-ORDER 2,
483083 578703 14913 MAKE-ORDER 2,
306902 61760 44575 MAKE-ORDER 2,
592894 871395 13977 MAKE-ORDER 2,
943993 718129 67065 MAKE-ORDER 2,
699471 340925 96866 MAKE-ORDER 2,
670740 314748 92633 MAKE-ORDER 2,
192342 983705 98627 MAKE-ORDER 2,
498027 880322 21696 MAKE-ORDER 2,
453461 30096 8761 MAKE-ORDER 2,
723390 86522 93161 MAKE-ORDER 2,
182788 798932 56820 MAKE-ORDER 2,
177008 646408 33432 MAKE-ORDER 2,
133488 975969 10094 MAKE-ORDER 2,
724961 276799 28756 MAKE-ORDER 2,
870423 823939 99624 MAKE-ORDER 2,
304081 333867 87834 MAKE-ORDER 2,
301009 318991 548 MAKE-ORDER 2,
789539 905257 4965 MAKE-ORDER 2,
643384 81943 73579 MAKE-ORDER 2,
22694 31083 44250 MAKE-ORDER 2,
385154 343730 15563 MAKE-ORDER 2,
562221 463722 4886 MAKE-ORDER 2,
858898 755281 57872 MAKE-ORDER 2,
579177 967317 8299 MAKE-ORDER 2,
950514 37008 96464 MAKE-ORDER 2,
670087 877534 19420 MAKE-ORDER 2,
327261 816007 86928 MAKE-ORDER 2,
327261 239099 20028 MAKE-ORDER 2,
732 238014 31845 MAKE-ORDER 2,
905113 408578 54898 MAKE-ORDER 2,
0 0 0 MAKE-ORDER 2,

ORDERS 40 ADD-SENTINEL
CREATE ORDER-POINTERS 41 CELLS ALLOT
ORDERS ORDER-POINTERS 41 ORDER@-ARRAY
ORDER-POINTERS 41 PRINT-ORDER-ARRAY CR
ORDER-POINTERS 41 SORT
ORDER-POINTERS 41 PRINT-ORDER-ARRAY 
ORDER-POINTERS 41 ORDERS-VALUE CR . CR
BYE
