#include <p16F887.inc>
	__CONFIG    _CONFIG1, _LVP_OFF & _FCMEN_OFF & _IESO_OFF & _BOR_OFF & _CPD_OFF & _CP_OFF & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF & _INTRC_OSC_NOCLKOUT
	__CONFIG    _CONFIG2, _WRT_OFF & _BOR21V
	
	errorlevel -302 ;turn off "operand not in bank 0" warning
	
	cblock 0x20
	   Enemy 		;LED0 doesn't vary
	   FireBallEnemy ;every 0.2 sec shifts
	   FireBallPlayer;created when switch is pressed and every 0.1 sec
	   currentFrame	;hold the value of the current frame
	   nextFrame	;hold the value for the next frame (when player fires ball)
	   temp
	   PlayerPos	;hold player position
	   winVar		;called when players wins
	   loseVar		;called when players loses
	   d1			;variables for delays
	   d2
	endc
	
	; Flag Definitions
     cblock 0x70     ; put these up in unbanked RAM
		W_Save
		STATUS_Save
     endc
	
	org 0X000
	Reset_V:
		 goto	Start
	
	;******************* INTERRUPT HERE**********************
	org 0x004
	PER_INT_V:	
	
		movwf   W_Save              ; Save context
		movf    STATUS,w
		movwf   STATUS_Save
	
		BANKSEL PIR1
		btfsc   PIR1,TMR1IF	;if timer finished counting(=1)
		goto 	ServiceTimer1

		
		goto 	ExitISR		;return to the point where the interrupt occured in the main
	

		
	ServiceTimer1:
		
		;clear timer flag
		BANKSEL TMR1H
		clrf    TMR1H
		BANKSEL TMR1L
		clrf    TMR1L
		BANKSEL PIR1
		bcf	PIR1, TMR1IF
		
		;check if player's fireball is hit by the enemy's ball in the current frame
		movf	FireBallPlayer,w
		subwf	FireBallEnemy,w
		btfsc	STATUS,Z
		CALL	AreEqual
		
		;check if player's fireball is hit by the enemy's ball in the next frame
		movf	nextFrame,w
		subwf	FireBallEnemy,w
		btfsc	STATUS,Z
		CALL	AreEqual
		
		;shift to right
		bcf	STATUS,C
		clrw
		rrf	FireBallPlayer,w
		movwf	FireBallPlayer
		
		;shift to right
		bcf     STATUS,C
		clrw
		rrf	nextFrame,w
		movwf   nextFrame
		
		;shift to left
		bcf	STATUS,C
		btfsc	FireBallEnemy,7 ;if = 1
		bsf	STATUS,C
		clrw
		rlf	FireBallEnemy,w
		movwf	FireBallEnemy

		
	ExitISR:
		movf    STATUS_Save,w       ; Restore context
		movwf   STATUS
		swapf   W_Save,f            ; swapf doesn't affect Status bits, but MOVF would
		swapf   W_Save,w
		retfie
		
		;if both fire balls hit they cancel each other
	AreEqual:
		clrf	FireBallEnemy
		bsf	FireBallEnemy,0
		clrf	FireBallPlayer
		
		return
	;********************************************************
	;INIT
	Start:
		;init registers
		BANKSEL TRISA
		movlw   0xFF
		movwf   TRISA               ; Make PortA all input
		BANKSEL PORTD
		clrf    PORTD               ; init LEDs to all off
		BANKSEL TRISD
		clrf    TRISD               ; Make PortD all output
		BANKSEL PORTB
		clrf    PORTB
		BANKSEL TRISB
		movlw   0x01     
		movwf   TRISB               ; Make RBO pin input (switch)
		BANKSEL ANSEL
		movlw   0x1F                
		movwf   ANSEL               ; PortA pins are all analog, PortE pins are digital
		BANKSEL ANSELH
		movlw   0x00
		movwf   ANSELH              ; PortB pins are digitial (important as RB0 is switch)
		BANKSEL	ADCON1
		movlw   0x00           ; Left Justified, Vdd-Vss referenced
		movwf   ADCON1
		
		BANKSEL	ADCON0
		movlw   0x41
		movwf   ADCON0         ; configure A2D for Fosc/8, Channel 0 (RA0), and turn on the A2D module
		
		;Timer1 init
		BANKSEL T1CON
		bcf	T1CON, TMR1ON
		BANKSEL TMR1H
		clrf    TMR1H
		BANKSEL TMR1L
		clrf    TMR1L
		BANKSEL	INTCON
		clrf    INTCON
		BANKSEL PIR1
		clrf    PIR1
		BANKSEL PIE1
		clrf    PIE1
		bsf	PIE1, TMR1IE

		BANKSEL PIR1
		clrf    PIR1
		BANKSEL INTCON

		bsf	INTCON, PEIE
		bsf	INTCON, GIE	;enable global interrupts

		;init win/lose variables
		clrf	winVar
		clrf	loseVar
		bsf	winVar,0
		bsf	winVar,2
		bsf	winVar,4
		bsf	winVar,6
		
		bsf	loseVar,0
		bsf	loseVar,1
		bsf	loseVar,2
		bsf	loseVar,3
		bsf	loseVar,4
		bsf	loseVar,5
		bsf	loseVar,6
		bsf	loseVar,7
		
	;game resets here
	StartGame:	
		;initializing variables
		clrf	Enemy
		clrf	Player
		clrf	FireBallEnemy
		clrf	FireBallPlayer
		clrf	currentFrame
		clrf	PlayerPos
		
		bsf	Enemy,0
		bsf	FireBallEnemy,1

		
		BANKSEL T1CON
		movlw   b'10110001'
		movwf   T1CON
		goto	Main

	;called each frame
	Main:
		;for player movement
		BANKSEL	ADCON0
		bsf	ADCON0, GO_DONE	;start A2D conversion

		;check status of PORTD each frame
		movf	FireBallEnemy,w
		subwf	PlayerPos,w
		btfsc	STATUS,Z
		CALL	LOSE
		
		;if player reaches any of these LEDs they win
		movf	Enemy,w
		subwf	PlayerPos,w
		btfsc	STATUS,Z
		CALL	WIN
		
		btfsc	PlayerPos,2
		CALL	WIN
		
		btfsc	PlayerPos,1
		CALL	WIN
		
		;reads player position using lookup table
		swapf   ADRESH,w            ; read the A2D, move the high nybble to the low part
		CALL    PlayerMovement    
		movwf	PlayerPos
		
		;if a switch is pressed in this frame
		BANKSEL	PORTB
		btfss	PORTB,0
		CALL	FIREBALL
		
		;update PORTD status
		clrw
		movf	PlayerPos,w
		xorwf	Enemy,w
		xorwf	FireBallEnemy,w
		xorwf	FireBallPlayer,w
		movwf	currentFrame
				
		
		;display on LEDs
		BANKSEL	PORTD
		movf	currentFrame,w
		movwf	PORTD
		
		;keeps looping
		goto	Main

; called when player wins
WIN:
    BANKSEL TMR1H
    clrf    TMR1H
    BANKSEL TMR1L
    clrf    TMR1L
    BANKSEL PIR1
    bcf	    PIR1, TMR1IF
    BANKSEL T1CON
    bcf	    T1CON,0
    BANKSEL PORTD
    clrf    PORTD
    movf    winVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    winVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    winVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    winVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    
    goto StartGame ;resets game

; called when player loses
LOSE:
    
    BANKSEL TMR1H
    clrf    TMR1H
    BANKSEL TMR1L
    clrf    TMR1L
    BANKSEL PIR1
    bcf	    PIR1, TMR1IF
    BANKSEL T1CON
    bcf	    T1CON,0
    BANKSEL PORTD
    clrf    PORTD
    movf    loseVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    loseVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    loseVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    movf    loseVar,w
    movwf   PORTD
    CALL    Delay_0.2
    clrf    PORTD
    CALL    Delay_0.2
    
    goto StartGame ; resets game

;creates a fireball depending on the players position
FIREBALL:
    movwf   FireBallPlayer
    bcf	    STATUS,C
    clrw
    rrf	    FireBallPlayer,w
    movwf   FireBallPlayer
    movfw   FireBallPlayer
    movwf   nextFrame
    bcf	    STATUS,C
    clrw
    rrf	    nextFrame,w
    movwf   nextFrame
    return
	
Delay_0.2:
    movlw	0x3F
    movwf	d1
    movlw	0x9D
    movwf	d2
Delay_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	Delay_0

		    ;2 cycles
    goto	$+1
    return

;called each frame to get player position depending on the potentiometer
PlayerMovement:
     andlw     0x0F                ; mask off invalid entries
     movwf     temp
     movlw     high TableStart     ; get high order part of the beginning of the table
     movwf     PCLATH
     movlw     low TableStart      ; load starting address of table
     addwf     temp,w              ; add offset
     btfsc     STATUS,C            ; did it overflow?
     incf      PCLATH,f            ; yes: increment PCLATH
     movwf     PCL                 ; modify PCL
;Lookup-table
TableStart:
     retlw     b'10000000'             ; 0
     retlw     b'10000000'             ; 1
     retlw     b'01000000'             ; 2
     retlw     b'01000000'             ; 3
     retlw     b'00100000'             ; 4
     retlw     b'00100000'             ; 5
     retlw     b'00010000'             ; 6
     retlw     b'00010000'             ; 7
     retlw     b'00001000'             ; 8
     retlw     b'00001000'             ; 9
     retlw     b'00000100'             ; 10
     retlw     b'00000100'             ; 11
     retlw     b'00000010'             ; 12
     retlw     b'00000010'             ; 13
     retlw     b'00000001'             ; 14
     retlw     b'00000001'             ; 15

     end
	
	end