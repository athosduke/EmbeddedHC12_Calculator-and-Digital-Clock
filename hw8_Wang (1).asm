                                     ;*******************************************************
;* CMPEN 472, HW8 Real Time Interrupt, MC9S12C128 Program
;* Nov 6 2019
;* Songmeng Wang
;* 
;* 10 second timer using Real Time Interrupt.
;* This program is a 10 second count down timer using 
;* a Real Time Interrupt service subroutine (RTIISR).  This program
;* displays the time remaining on the Hyper Terminal screen every 1 second.  
;* That is, this program displays '987654321098765432109876543210 . . . ' on the 
;* Hyper Terminal connected to MC9S12C128 chip on CSM-12C128 board.  
;* User may enter 'stop' command followed by an enter key to stop the timer 
;* and re-start the timer with 'run' command followed by an enter key.
;*
;* Please note the new feature of this program:
;* RTI vector, initialization of CRGFLG, CRGINT, RTICTL, registers for the
;* Real Time Interrupt.
;* We assumed 24MHz bus clock and 4MHz external resonator clock frequency.  
;* This program evaluates user input (command) after the enter key hit and allow 
;* maximum five characters for user input.  This program ignores the wrong 
;* user inputs, and continue count down.
;* 
;*******************************************************
; export symbols
            XDEF        Entry        ; export 'Entry' symbol
            ABSENTRY    Entry        ; for assembly entry point

; include derivative specific macros
SCISR1      EQU         $00cc        ; Serial port (SCI) Status Register 1
SCIDRL      EQU         $00cf        ; Serial port (SCI) Data Register

CRGFLG      EQU         $0037        ; Clock and Reset Generator Flags
CRGINT      EQU         $0038        ; Clock and Reset Generator Interrupts
RTICTL      EQU         $003B        ; Real Time Interrupt Control

CR          equ         $0d          ; carriage return, ASCII 'Return' key
LF          equ         $0a          ; line feed, ASCII 'next line' character

;*******************************************************
; variable/data section
            ORG     $3000            ; RAMStart defined as $3000
                                     ; in MC9S12C128 chip

ctr2p5m     DS.W    1                ; 16bit interrupt counter for 2.5 mSec. of time
count       DS.B    1                ; user input type buffer fill count
buff        DS.B    10                ; user input type buffer, maximum 1 char
buffhh      DS.B    1
buffmm      DS.B    1
buffss      DS.B    1
hh          DS.B    1
mm          DS.B    1
ss          DS.B    1
firsttime   DS.B    1
cquit       DS.B    1
cerror      DS.B    1
storex      DS.B    2
storeb      DS.B    1
checknew    DS.B    1



; following escape sequence works on Hyper Terminal (but NOT the TestTerminal)
ClearScreen     DC.B    $1B, '[2J', $00               ; clear the Hyper Terminal screen
ClearLine       DC.B    $1B, '[2K', $00               ; clear the line
Scroll4Enable   DC.B    $1B, '[1', $3B, '6r',  $00    ; enable scrolling to top 4 lines
UnSavePosition  DC.B    $1B, '8', $00                 ; restore the saved cursor position
SavePosition    DC.B    $1B, '7', $00                 ; save the current cursor position
CursorToTop     DC.B    $1B, '[2',  $3B, '1f',  $00   ; move cursor to 2,1 position
CursorToPP      DC.B    $1B, '[6',  $3B, '1f',  $00   ; move cursor to 5,1 position, prompt
CursorToCenter  DC.B    $1B, '[14', $3B, '36H', $00   ; move cursor to 8,25 position


;*******************************************************
; interrupt vector section

            ORG     $3FF0            ; Real Time Interrupt (RTI) interrupt vector setup
            DC.W    rtiisr

;*******************************************************
; code section
            ORG     $3100
Entry
            LDS     #Entry           ; initialize the stack pointer

            ldx     #ClearScreen     ; clear the Hyper Terminal Screen first
            jsr     printmsg
            ldx     #CursorToTop     ; and move the cursor to top left corner (2,1) to start
            jsr     printmsg
            ldx     #msg1            ; print the first message, 'Hello'
            jsr     printmsg
            jsr     nextline
            ldx     #msg2            ; print the second message, user instruction
            jsr     printmsg
            jsr     nextline
            ldx     #msg3            ; print the 3rd message
            jsr     printmsg
            jsr     nextline
            ldx     #msg4            ; print the 4th message
            jsr     printmsg
            
            ldx     #Scroll4Enable   ; enable top 5 lines to scroll if necessary
            jsr     printmsg
            ldx     #CursorToPP      ; move cursor at (4,1) upper left corner for the prompt
            jsr     printmsg
            
            bset  RTICTL,%00011001   ; set RTI: dev=10*(2**10)=2.555msec for C128 board
                                     ;      4MHz quartz oscillator clock
            bset  CRGINT,%10000000   ; enable RTI interrupt
            bset  CRGFLG,%10000000   ; clear RTI IF (Interrupt Flag)

            ldx   #msg5
            jsr   printmsg
            ldx   #buff
            
loop        jsr   upClock            ; update display, each 1 second 

            jsr   getchar            ; user may enter command
            cmpa  #$00                     ;  save characters if typed
            beq   loop

            cmpa  #CR                ; ENTER??
            beq   enter
            
            jsr   putchar            ; display what you type
            inc   count              ; count ++
            ldab  count
            cmpb  #$0A               ; count > 10?
            bgt   loop
            
            staa  1,x+
            bra   loop

enter       
            jsr   nextline
            jsr   NewCommand         ; check command buffer for a new command entered.
            clr   count
            ldaa  cerror
            cmpa  #$00
            bne   initial
            ldaa  cquit
            cmpa  #$00
            bne   quitclear
            inc   checknew
            jsr   upClock
 
initial 
            clr  cerror
            ldx  #buff
            bra  loop
            
quitclear   ldx   #SavePosition      ; save the current cursor posion (user input in
            jsr   printmsg           ;   progress at the prompt).
            ldx   #CursorToCenter    ; and move the cursor to center(relative) to print time
            jsr   printmsg
            ldaa  #$20
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            jsr   putchar 
            ldx   #UnSavePosition    ; back to the prompt area for continued user input
            jsr   printmsg                  

typewrite   jsr   getchar            ; user may enter command
            tsta                     ;  save characters if typed
            beq  typewrite
            
            jsr   putchar
            bra   typewrite  
;subroutine section below

;***********RTI interrupt service routine***************
rtiisr      bset  CRGFLG,%10000000   ; clear RTI Interrupt Flag
            ldx   ctr2p5m
            inx
            stx   ctr2p5m            ; every time the RTI occur, increase interrupt count
rtidone     RTI
;***********end of RTI interrupt service routine********

;***************Update Display**********************
;* Program: Update count down timer display if 1 second is up
;* Input:   ctr2p5m variable
;* Output:  timer display on the Hyper Terminal
;* Registers modified: CCR
;* Algorithm:
;    Check for 1 second passed
;      if not 1 second yet, just pass
;      if 1 second has reached, then update display, toggle LED, and reset ctr2p5m
;**********************************************
upClock
            psha
            pshx
            pshb
            
            ldaa  checknew
            cmpa  #$00
            bne   newClock
            ldx   ctr2p5m
            cpx   #399             ; 2.5msec * 400 = 1 sec        0 to 399 count is 400
            lblo   UpDone           ; if interrupt count less than 400, then not 1 sec yet.
                                               ;    no need to update display.
            ldx   #0               ; interrupt counter reached 400 count, 1 sec up now
            stx   ctr2p5m          ; clear the interrupt count to 0, for the next 1 sec.


newClock
            ldx   #SavePosition      ; save the current cursor posion (user input in
            jsr   printmsg           ;   progress at the prompt).
            ldx   #CursorToCenter    ; and move the cursor to center(relative) to print time
            jsr   printmsg


            ldab  hh
            ldaa  #$00
            ldx   #$000A
            idiv
            stab  storeb
            stx   storex
            ldd   storex
            tba
            adda  #$30
            jsr   putchar
            ldaa  storeb   
            adda  #$30
            jsr   putchar
            
            ldaa  #$3A
            jsr   putchar
            
            ldab  mm
            ldaa  #$00
            ldx   #$000A
            idiv
            stab  storeb
            stx   storex
            ldd   storex
            tba
            adda  #$30
            jsr   putchar
            ldaa  storeb   
            adda  #$30
            jsr   putchar
            
            ldaa  #$3A
            jsr   putchar
            
            ldab  ss
            ldaa  #$00
            ldx   #$000A
            idiv
            stab  storeb
            stx   storex
            ldd   storex
            tba
            adda  #$30
            jsr   putchar
            ldaa  storeb   
            adda  #$30
            jsr   putchar

            ldx   #UnSavePosition    ; back to the prompt area for continued user input
            jsr   printmsg

            ldaa  checknew
            cmpa  #$00
            bne   UpDone
            
            inc   ss
            ldaa  ss
            cmpa  #59
            bls   UpDone
            ldaa  #$00
            staa  ss
            inc   mm
            ldaa  mm
            cmpa  #59
            bls   UpDone
            ldaa  #$00
            staa  mm
            inc   hh
            ldaa  hh
            cmpa  #11
            bls   UpDone
            ldaa  #$00
            staa  hh
            bra   UpDone
            
UpDone      clr   checknew
            pulb
            pulx
            pula
            rts
;***************end of Update Display***************

;***************New Command Process*******************************
;* Program: Check for 'run' command or 'stop' command.
;* Input:   Command buffer filled with characters, and the command buffer character count
;*             cbuf, cbufct
;* Output:  Display on Hyper Terminal, count down characters 9876543210 displayed each 1 second
;*             continue repeat unless 'stop' command.
;*          When a command is issued, the count display reset and always starts with 9.
;*          Interrupt start with CLI for 'run' command, interrupt stops with SEI for 'stop' command.
;*          When a new command is entered, cound time always reset to 9, command buffer cleared, 
;*             print error message if error.  And X register pointing at the begining of 
;*             the command buffer.
;* Registers modified: X, CCR
;* Algorithm:
;*     check 'run' or 'stop' command, and start or stop the interrupt
;*     print error message if error
;*     clear command buffer
;*     Please see the flow chart.
;* 
;**********************************************
NewCommand
            psha
            pshb
            pshx

            ldaa  count
            cmpa  #$01            ; count = 1? -> checkquit
            lbeq   checkquit
            
            cmpa  #$0A
            beq   checkset        ; count = 10? -> checkset
            
            lbra   error  
            
checkset    ldx   #buff           ; x->buff[0]
            ldaa  1,x+            ; a = buff[0]
            cmpa  #'s'            ; = s ?
            lbne  error
            
            ldaa  1,x+            ; a = buff[1]
            cmpa  #$20            ; = space?
            lbne   error

            ldaa  1,x+            ; a = buff[2]
            cmpa  #$30            ; < 0?
            lblt   error
            cmpa  #$39            ; >9 ?
            lbgt   error
           
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffhh          ; mul -> buffhh
            
            ldaa  1,x+            ; a = buff[3]
            cmpa  #$30            ; < 0?
            lblt  error
            cmpa  #$39            ; >9 ?
            lbgt   error
            
            suba  #$30
            adda  buffhh
            cmpa  #11
            lbgt   error
            staa  buffhh

            ldaa  1,x+            ; a = buff[4]
            cmpa  #$3A            ; = :?
            lbne   error

            ldaa  1,x+            ; a = buff[5]
            cmpa  #$30            ; < 0?
            blt   error
            cmpa  #$39            ; >9 ?
            bgt   error
            
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffmm          ; mul -> buffmm
            
            ldaa  1,x+            ; a = buff[6]
            cmpa  #$30            ; < 0?
            blt   error
            cmpa  #$39            ; >9 ?
            bgt   error
            
            suba  #$30
            adda  buffmm
            cmpa  #59
            bgt   error
            staa  buffmm   
            
            ldaa  1,x+            ; a = buff[7]
            cmpa  #$3A            ; = :?
            bne   error
            
            ldaa  1,x+            ; a = buff[8]
            cmpa  #$30            ; < 0?
            blt   error
            cmpa  #$39            ; >9 ?
            bgt   error
            
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffss          ; mul -> buffss
            
            ldaa  1,x+            ; a = buff[9]
            cmpa  #$30            ; < 0?
            blt   error
            cmpa  #$39            ; >9 ?
            bgt   error
            
            suba  #$30
            adda  buffss
            cmpa  #59
            bgt   error
            staa  buffss
            
            ldaa  buffhh
            staa  hh
            ldaa  buffmm
            staa  mm
            ldaa  buffss
            staa  ss
            
            ldaa  firsttime
            cmpa  #$00             ; firsttime = 0?
            bne   NCdone
            cli                    ; turn on interrupt
            inc   firsttime
            bra   NCdone
                        
error       
            ldx   #msgerror
            jsr   printmsg
            jsr   nextline
            inc   cerror
            bra   NCdone           
                        
checkquit   ldx   #buff
            ldaa  1,x+
            cmpa  #$71
            bne   error
            sei                    ; turn off interrupt
            
            ldx   #msgquit
            jsr   printmsg
            jsr   nextline
            inc   cquit
            
            pulx
            pulb
            pula
            rts         

NCdone      ldx   #msg5
            jsr   printmsg
                        
            pulx
            pulb
            pula
            rts
;***************end of New Command Process***************


;***********printmsg***************************
;* Program: Output character string to SCI port, print message
;* Input:   Register X points to ASCII characters in memory
;* Output:  message printed on the terminal connected to SCI port
;* 
;* Registers modified: CCR
;* Algorithm:
;     Pick up 1 byte from memory where X register is pointing
;     Send it out to SCI port
;     Update X register to point to the next byte
;     Repeat until the byte data $00 is encountered
;       (String is terminated with NULL=$00)
;**********************************************
NULL            equ     $00
printmsg        psha                   ;Save registers
                pshx
printmsgloop    ldaa    1,X+           ;pick up an ASCII character from string
                                       ;   pointed by X register
                                       ;then update the X register to point to
                                       ;   the next byte
                cmpa    #NULL
                beq     printmsgdone   ;end of strint yet?
                bsr     putchar        ;if not, print character and do next
                bra     printmsgloop
printmsgdone    pulx 
                pula
                rts
;***********end of printmsg********************

;***************putchar************************
;* Program: Send one character to SCI port, terminal
;* Input:   Accumulator A contains an ASCII character, 8bit
;* Output:  Send one character to SCI port, terminal
;* Registers modified: CCR
;* Algorithm:
;    Wait for transmit buffer become empty
;      Transmit buffer empty is indicated by TDRE bit
;      TDRE = 1 : empty - Transmit Data Register Empty, ready to transmit
;      TDRE = 0 : not empty, transmission in progress
;**********************************************
putchar     brclr SCISR1,#%10000000,putchar   ; wait for transmit buffer empty
            staa  SCIDRL                      ; send a character
            rts
;***************end of putchar*****************

;****************getchar***********************
;* Program: Input one character from SCI port (terminal/keyboard)
;*             if a character is received, other wise return NULL
;* Input:   none    
;* Output:  Accumulator A containing the received ASCII character
;*          if a character is received.
;*          Otherwise Accumulator A will contain a NULL character, $00.
;* Registers modified: CCR
;* Algorithm:
;    Check for receive buffer become full
;      Receive buffer full is indicated by RDRF bit
;      RDRF = 1 : full - Receive Data Register Full, 1 byte received
;      RDRF = 0 : not full, 0 byte received
;**********************************************

getchar     brclr SCISR1,#%00100000,getchar7
            ldaa  SCIDRL
            rts
getchar7    clra
            rts
;****************end of getchar**************** 

;****************nextline**********************
nextline    ldaa  #CR              ; move the cursor to beginning of the line
            jsr   putchar          ;   Cariage Return/Enter key
            ldaa  #LF              ; move the cursor to next line, Line Feed
            jsr   putchar
            rts
;****************end of nextline***************
msg2        DC.B    'You may type the following two types of command with enter key:', $00
msg1        DC.B    'Hello, this is a Digital Clock program.', $00
msg3        DC.B    '"s hh:mm:ss" to set the time of the clock.', $00
msg4        DC.B    '"q" to quit the program. ', $00
msgerror    DC.B    'Invalid time format. Correct example => s hh:mm:ss', $00
msg5        DC.B    'clock> ', $00
msgquit     DC.B    'Typewrite program started', $00


            END                    ; this is end of assembly source file
                                   ; lines below are ignored - not assembled