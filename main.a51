;Definition
	N1		EQU 	30H								;#!Ram byte location holds first operand 
	N2		EQU		31H								;#!Ram byte locations holds second operand
	OP		EQU		32H								;#!Ram byte location holds operation ascii code
	R		EQU		33H								;#!Ram byte location holds result
	SIGN	EQU		34H								;#!Ram byte location holds result's sign
	TEMP	EQU		35H								;#!Ram byte location holds temporary data
	DIF		BIT		0AH								;#!Digit input flag bit(0,1,2,3,4,5,6,7,8,9)
	OIF		BIT		0BH								;#!Operator input flag bit(+,-,*,/)
	AIF		BIT		0CH								;#!Assignment operator flag bit(=)
	Space	DATA	32d
	  ; Reset Vector
      org   0000h
      jmp   Start

;====================================================================
; CODE SEGMENT
;====================================================================

      org   0100h
Start:	
	;Code start
		MOV 	TMOD, #021h
		LCALL 	LCDInit
		MOV		R5,Space
Loop:	
		LCALL 	memoryInit
		LCALL 	readInput
		LCALL 	CalculateResult
		LCALL 	PrintOutput
		JMP 	Loop
LCDInit:
		MOV 	TH1, #0FDh
		MOV 	SCON, #50h
		SETB 	TR1
		RET
;====================================================
memoryInit:										
		CLR		A								;#!Clear ACC						
		MOV		N1,A							;#!Clear first operand location in ram 
		MOV		N2,A							;#!Clear second operand location in ram 
		MOV		OP,A							;#!Clear operation location in ram 
		MOV		R,A								;#!Clear result location in ram
		MOV		SIGN,A							;#!Clear sign location in ram
		MOV		TEMP,A							;#!Clear result location in ram
		CLR		C								;#!Clear carry bit
		MOV		DIF,C							;#!Clear digit flag bit		
		MOV		OIF,C							;#!Clear operator flag bit
		MOV		AIF,C							;#!Clear assignment operator flag bit
;		MOV		R5,Space						;LCD's position
		RET										;#!return to MAIN routine
;====================================================
readInput:
		ACALL 	readKey							;Input 1st operand
		ACALL 	validateInput
		JB		OIF,ERROR						
		JB		AIF,ERROR
		ACALL	LCDWrite
		DEC		R5
		ANL		A,#0FH							;Convert to number
		MOV 	N1,A							;Save 1st operand
		
		ACALL 	readKey							;Input operation
		ACALL 	validateInput
		JB		DIF,ERROR						
		JB		AIF,ERROR
		ACALL 	LCDWrite
		DEC		R5
		MOV		OP,A							;Sav operation
		
		
		ACALL	readKey							;Input 2nd operand
		ACALL	validateInput
		JB		OIF,ERROR
		JB		AIF,ERROR
		ACALL	LCDWrite
		DEC		R5
		ANL		A,#0FH							;Convert to number
		MOV		N2,A							;Save 2nd operand
		
		ACALL 	readKey							;Input =
		ACALL 	validateInput
		JB		DIF,ERROR						
		ACALL 	LCDWrite
		DEC		R5
		JMP 	NOERROR
		RET
ERROR:											;Write "ERROR!"
		CALL	ClrScreen						
		MOV 	A,#"E"
		CALL 	LCDWrite
		DEC		R5
		MOV 	A,#"R"
		CALL 	LCDWrite
		DEC		R5
		MOV 	A,#"R"
		CALL 	LCDWrite
		DEC		R5
		MOV 	A,#"O"
		CALL 	LCDWrite
		DEC		R5
		MOV 	A,#"R"
		CALL 	LCDWrite
		DEC		R5
		MOV 	A,#"!"
		CALL 	LCDWrite
		DEC		R5
;		ACALL	LDELAY
;		ACALL	LDELAY
;		ACALL 	ClrScreen
		JMP 	Loop
NOERROR:
		RET
;====================================================
readKey:
		MOV 	P1,#0FH							;#!Make P1 an input port
K1: 	
		MOV 	P2,#0                           ;#!Ground all rows at once
		MOV 	A,P1							;#!Read all colums 
		ANL 	A,#00001111B					;#!Masked unused bits
		CJNE 	A,#00001111B,K1                 ;#!Till all keys release
K2: 	
		LCALL 	DELAY                          	
		MOV 	A,P1                            ;#!See if any key is pressed
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,OVER               ;#!Key pressed, find row
		SJMP 	K2                              ;#!Check till key pressed
OVER: 	
		LCALL 	DELAY                          	
		MOV 	A,P1                            ;#!Check key closure
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,OVER1              ;#!Key pressed, find row
		SJMP 	K2                            	;#!If none, keep polling
OVER1: 	
		MOV 	P2,#11111110B                 	;#!Ground row 0
		MOV 	A,P1                            ;#!Read all columns
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,ROW_0              ;#!Key row 0, find col.
		MOV 	P2,#11111101B                   ;#!Ground row 1
		MOV 	A,P1                            ;#!Read all columns
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,ROW_1              ;#!Key row 1, find col.
		MOV 	P2,#11111011B                   ;#!Ground row 2
		MOV 	A,P1                            ;#!Read all columns
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,ROW_2              ;#!Key row 2, find col.
		MOV 	P2,#11110111B                   ;#!Ground row 3
		MOV 	A,P1                            ;#!Read all columns
		ANL 	A,#00001111B                    ;#!Mask unused bits
		CJNE 	A,#00001111B,ROW_3              ;#!Key row 3, find col.
		LJMP 	K2                              ;#!If none, false input, repeat
ROW_0: 	
		MOV 	DPTR,#KCODE0                    ;#!Set DPTR=start of row 0
		SJMP 	FIND                            ;#!Find col. Key belongs to
ROW_1: 	
		MOV 	DPTR,#KCODE1                    ;#!Set DPTR=start of row
		SJMP 	FIND                            ;#!Find col. Key belongs to
ROW_2: 	
		MOV 	DPTR,#KCODE2                    ;#!Set DPTR=start of row 2
		SJMP 	FIND                            ;#!Find col. Key belongs to
ROW_3: 	
		MOV 	DPTR,#KCODE3                    ;#!Set DPTR=start of row 3
FIND: 	
		RRC 	A                               ;#!See if any CY bit low
		JNC 	MATCH                           ;#!If zero, get ASCII code
		INC 	DPTR                            ;#!Point to next col. addr
		SJMP 	FIND                            ;#!Keep searching		
MATCH: 	
		CLR 	A                               ;#!Set A=0 (match is found)
		MOVC 	A,@A+DPTR                       ;#!Get ASCII from table
		JZ		ON								;If AC is Press jump to AC func
		RET										;#!return
ON:												;AC button
		ACALL 	ClrScreen						
		JMP		Start
;====================================================
validateInput:									;#!Detemine and set flags for each kind of input
		CJNE	A,#"+", next1					;#!Check if input is add operator,
		AJMP	found							;#!Jump to found if input match with (+,-,*,/)
next1:	
		CJNE	A,#"-", next2					;#!Check if input is sub operator,
		AJMP	found							;#!Jump to found if input match with (+,-,*,/)
next2:	
		CJNE	A,#"*", next3					;#!Check if input is multiply operator 
		AJMP	found							;#!Jump to found if input match with (+,-,*,/)
next3:	
		CJNE 	A,#"/", next4					;#!Check if input is add operator,
		AJMP	found							;#!Jump to found if input match with (+,-,*,/)
next4:	
		CJNE	A,#"=", next5					;#!Check if input is assignment operator
		CLR		DIF								;#!CLR digit input flag for non numbers
		CLR		OIF								;#!CLR operation input flag for non operation 
		SETB	AIF								;#!SET assignment input flag for (=)
		RET										;#!return to MAIN
next5:	
		SETB	DIF								;#!SET digit input flag for numbers
		CLR		OIF								;#!CLR operation input flag for non operatio
		CLR		AIF								;#!CLR assignment input flag if not (=)
		RET										;#!return to MAIN
found:	
		CLR		DIF								;#!CLR digit input flag for non numbers
		SETB	OIF								;#!SET operation input flag for any of operations
		CLR		AIF								;#!CLR assignment input flag if not (=)
		RET										;#!return
;====================================================
CalculateResult:
		MOV		A,N1							;#!Copy first number to ACC
		MOV		B,N2							;#!Copy second number to B
		MOV		R7,OP							;#!Copy operation to R7
		CJNE 	R7,#"+",NEXT11					;#!Check firstly if the operation was "+", if not jump
		ADD		A,B								;#!If so, Add the two operands
		MOV		R,A								;#!Save result at RAM 
		MOV		SIGN,#"+"						;#!Set result sign to positive
		RET										;#!return to MAIN 
NEXT11:	
		CJNE	R7,#"-",NEXT22					;#!Check secondly if the operation was "-", if not jump
		SUBB	A,B								;#!If so, Sub the two operands 
		JC		NIGATIV							;#!Check on carry for nigative result
		MOV		R,A								;#!Save result at RAM 
		MOV		SIGN,#"+"						;#!Set result sign to positive
		RET										;#!return to MAIN
NIGATIV:
		CPL		A								;#!Convert to 1's complement if negativ
		INC 	A								;#!Incremet to get 2's complement 
		MOV		R,A								;#!Save result at RAM
		MOV		SIGN,#"-"						;#!Set result sign to nigative
		RET										;#!return to MAIN
NEXT22:	
		CJNE	R7,#"*",NEXT33					;#!Check thirdly if the operation was "*", if not jump
		MUL		AB								;#!If so, Multibly the oeprands
		MOV		R,A								;#!Save result in RAM 
		MOV		SIGN,#"+"						;#!Set result sign to positive
		RET										;#!return to MAIN
NEXT33:	
		CJNE	R7,#"/",NEXT44					;#!Check finally if the operation was "/", if not jump
		MOV		TEMP,B							;#!Copy to TEMP location 
		DIV		AB								;#!Carry out division to the two operands 
		MOV		R,A								;#!Save the quotient in RAM 
		MOV		A,#0AH							;#!Fill ACC with 10
		MUL		AB								;#!Multibly reminder by 10
		MOV		B,TEMP							;#!Get the second number again
		DIV		AB								;#!Divide again 
		MOV		TEMP,A							;#!Save the result into RAM
		MOV		SIGN,#"+"						;#!Set result sign to positive
NEXT44:	RET										;#!return to MAIN 
;====================================================
PrintOutput:
		MOV		R7,TEMP
		CJNE	R7,#0,POINTED					;Check result point number
		MOV		R6,SIGN
		CJNE	R6,#"+",SINGED					;Check sign positve or negative
RETURN:
		MOV		A,R
		MOV		B,#0AH
		DIV		AB
		JZ		LESSTEN							;Check if result < 10
		ADD		A,#30h
		ACALL	LCDWrite
		DEC		R5
		MOV		A,B
		ADD		A,#30h
		ACALL	LCDWrite
		DEC		R5
		JMP 	DONE
LESSTEN:
		MOV		A,B
		ADD		A,#30h
		ACALL	LCDWrite
		DEC		R5
		JMP		DONE
SINGED:
		MOV 	A,#"-"							;Print "-" sign
		ACALL	LCDWrite
		DEC		R5
		JMP		RETURN							;Return for number part
POINTED:
		MOV		A,R								;For decimal number
		ADD		A,#30h							
		ACALL	LCDWrite
		DEC		R5
		MOV		A,#"."							;Write dot point to LCD
		ACALL	LCDWrite
		DEC		R5
		MOV		A,TEMP
		ADD		A,#30h
		ACALL	LCDWrite
		DEC		R5
		JMP		DONE
DONE:	
;		ACALL 	LDELAY
;		ACALL	LDELAY
;		ACALL	ClrScreen
		RET
;====================================================
ClrScreen:
		MOV 	A,#" "
ResetPosition:		
		ACALL 	LCDWrite
		DJNZ 	R5,ResetPosition
		MOV 	R5,Space
Clear:
		ACALL	LCDWrite
		DJNZ	R5,Clear
		MOV 	R5,Space
		RET
;====================================================
LCDWrite:
		MOV 	SBUF,A
Wait:	
		JNB 	TI, Wait
		CLR 	TI
		RET
;====================================================
DELAY:
LOO:
		MOV 	TL0, #LOW(-20000)
		MOV 	TH0, #HIGH(-20000)
		SETB 	TR0 
LOOP2:
		JNB 	TF0, LOOP2
		CLR 	TR0
		CLR 	TF0
		RET
;=====================================================
LDELAY:
LLOO:
		MOV 	TL0, #LOW(-100000)
		MOV 	TH0, #HIGH(-100000)
		SETB 	TR0 
LLOOP2:
		JNB 	TF0, LLOOP2
		CLR 	TR0
		CLR 	TF0
		RET 
;=====================================================
		ORG		300H

;-------ASCII LOOK-UP TABLE FOR EACH ROW			
KCODE0: DB 		"7","8","9","/" 				;#!ROW 0
KCODE1: DB 		"4","5","6","*" 				;#!ROW 1
KCODE2: DB 		"1","2","3","-" 				;#!ROW 2
KCODE3: DB 		0,"0","=","+" 					;#!ROW 3


;====================================================================
;Code End
End