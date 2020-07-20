
: READLN \ adr,max -- length flag
    STDIN READ-LINE THROW ;

: WRITELN \ adr,l -- 
    STDOUT WRITE-LINE THROW ;

: S>INT \ adr,l -- i
    S>NUMBER? IF
        DROP
    ELSE
        S" not an integer" EXCEPTION THROW
    THEN ;

: INT>S \ i -- adr,l
    0 <# #S #> ;

: READ-INT \ adr,max -- i,f
    DUP 40 READLN
    IF S>INT -1 ELSE 0 THEN ;
    
: PROCESS
    BEGIN
        PAD DUP 40 READLN
    WHILE
        S>INT
        DUP 42 = IF EXIT ELSE INT>S WRITELN THEN
    REPEAT 
    STDOUT CLOSE-FILE THROW ;

PROCESS
BYE
