.ORIG #12288
;--Register Initialization--
ORIGEM
LD R6 STACK_BOTTOM_POINTER
LD R5 STACK_BOTTOM_POINTER
LD R4 GLOBAL_DATA_POINTER
BRNZP MAIN
;--Pointers--
GLOBAL_DATA_POINTER .FILL GLOBAL_DATA
STACK_BOTTOM_POINTER .FILL #65023
MAIN
AND R0 R0 #0
ADD R0 R0 #5
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #4
JSR STACK_PUSH
JSR SUBTR_FUNC
AND R0 R0 #0
ADD R0 R0 #4
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #6
JSR STACK_PUSH
JSR MUL_FUNC
AND R0 R0 #0
ADD R0 R0 #2
JSR STACK_PUSH
AND R0 R0 #0
ADD R0 R0 #2
JSR STACK_PUSH
JSR ADD_FUNC
AND R0 R0 #0
ADD R0 R0 #4
JSR STACK_PUSH
JSR DIV_FUNC
JSR ADD_FUNC
JSR ADD_FUNC
TRAP x25
;----MUL_FUNC_START----
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
;----MUL_FUNC_END----
;----ADD_FUNC_START----
ADD_FUNC
ADD R5 R7 #0
JSR STACK_PULL
ADD R1 R0 #0
JSR STACK_PULL
ADD R0 R1 R0
JSR STACK_PUSH
ADD R7 R5 #0
RET
;----ADD_FUNC_END----
;----SUBTR_FUNC_START----
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
;----SUBTR_FUNC_END----
;----DIV_FUNC_START----
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
;----DIV_FUNC_END----
;----MODULO_FUNC_START----
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
;----MODULO_FUNC_END----
;--Stack Manipulation--
STACK_PULL
LDR R0 R6 #0
ADD R6 R6 #1
RET
STACK_PUSH
ADD R6 R6 #-1
STR R0 R6 #0
RET
;--Global Data Start--
GLOBAL_DATA .FILL #0
.END
