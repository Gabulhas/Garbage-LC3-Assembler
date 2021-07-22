.ORIG x3000 
LEA R0 HELLO
HELLO
    .STRINGZ "Hello World!\n" ; \n = new line
HEYO
    .STRINGZ "Hello World!\n" ; \n = new line
.BLKW #4
LEA R0 HELLO
BRnzp HELLO 
.END
