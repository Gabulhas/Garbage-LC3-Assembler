.ORIG x3000
LD R4 GLOBAL_DATA_POINTER
LD R6 STACK_BOTTOM
LD R5 STACK_BOTTOM
BRnzp MAIN
GLOBAL_DATA_POINTER .FILL GLOBAL_DATA_START
STACK_BOTTOM .FILL #65023

;--FUNCTIONS--

MAIN
AND R0 R0 #0
ADD R0 R0 #4
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #5
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #5
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #6
JSR STACK_PUSH
JSR MUL_FUNC
AND R0 R0 #0
ADD R0 R0 #5
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #6
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #4
JSR STACK_PUSH
JSR SUBTR_FUNC
JSR SUBTR_FUNC
JSR ADD_FUNC
JSR ADD_FUNC
JSR ADD_FUNC
TRAP x25
MUL_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL
ADD R2 R0 #0
AND R0 R0 #0
MUL_FUNC_LOOP
ADD R0 R0 R2
ADD R1 R1 #-1
BRP MUL_FUNC_LOOP
JSR STACK_PUSH
ADD R7 R5 #0
RET
ADD_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL
ADD R0 R1 R0
JSR STACK_PUSH
ADD R7 R5 #0
RET
SUBTR_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL


NOT R0 R0
ADD R0 R0 #1
ADD R0 R1 R0
JSR STACK_PUSH
ADD R7 R5 #0
RET
DIV_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL
ADD R2 R0 #0
AND R0 R0 #0


NOT R2 R2
ADD R2 R2 #1
DIV_FUNC_LOOP
ADD R1 R1 R2
BRN DIV_FUNC_LOOP_END
ADD R0 R0 #1
BRP DIV_FUNC_LOOP
DIV_FUNC_LOOP_END
JSR STACK_PUSH
ADD R7 R5 #0
RET
MODULO_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL


NOT R0 R0
ADD R0 R0 #1
MODULO_FUNC_LOOP
ADD R1 R1 R0
BRP MODULO_FUNC_LOOP


NOT R0 R0
ADD R0 R0 #1
ADD R1 R1 R0
ADD R0 R1 #0
JSR STACK_PUSH
ADD R7 R5 #0
RET
STACK_PULL
LDR R0 R6 #0
ADD R6 R6 #1
RET
STACK_PUSH
ADD R6 R6 #-1
STR R0 R6 #0
RET
GLOBAL_DATA_START


.END
