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
SCIBDH      EQU         $00c8        ; Serial port (SCI) Baud Rate Register H

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
buffclk     DS.B    10                ; user input type buffer, maximum 1 char
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
countcalc   DS.B    1
Count1      DS.B    1
Count2      DS.B    1
Countsig    DS.B    1
Buff        DS.B    8
Buff1       DS.B    3
Buffsig     DS.B    1
Buff2       DS.B    3
num1        DS.B    2
num2        DS.B    2
checkWrong  DS.B    1
result      DS.B    2
checkY      DS.B    2
checkoverflow DS.B    1
leadzero    DS.B    1
xstore      DS.B    2




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
            
            ldx     #msg6
            jsr     printmsg
            jsr     nextline
            ldx     #msg7
            jsr     printmsg
            BCLR    SCIBDH,%11111111
            LDX     #$000D
            stx     SCIBDH     
           
startloop   jsr     getchar
            cmpa    #0
            beq     startloop
            JSR     putchar
            cmpa    #CR
            bne     startloop
            
                    
            ldx     #ClearScreen     ; clear the Hyper Terminal Screen first
            jsr     printmsg
            ldx     #CursorToTop     ; and move the cursor to top left corner (2,1) to start
            jsr     printmsg
           
            ldx     #msg2            ; print the first message, 'Hello'
            jsr     printmsg
            jsr     nextline
            ldx     #msg3            ; print the second message, user instruction
            jsr     printmsg
            jsr     nextline
            ldx     #msg4            ; print the 3rd message
            jsr     printmsg
            jsr     nextline
            ldx     #msg1            ; print the 4th message
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
            
modechoose  jsr   upClock
            jsr   getchar
            cmpa  #$00
            beq   modechoose
            
            cmpa  #$30
            blt   clockmode
            cmpa  #$39
            bgt   clockmode
            
            lbra   calcmode

                        
clockmode   ldx   #buffclk
            lbra   firstclock
            
loop        jsr   upClock            ; update display, each 1 second 

            jsr   getchar            ; user may enter command
            cmpa  #$00                     ;  save characters if typed
            beq   loop

firstclock  cmpa  #CR                ; ENTER??
            beq   enter
            
            jsr   putchar            ; display what you type
            
            ldab  count
            cmpb  #$0A
            bgt   loop
            
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
            ldx  #buffclk
            bra  modechoose
            
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
            
calcmode      ldy   #Buff
              ldx   #Buff1
              bra   firstcalc
looop         jsr   upClock
              jsr   getchar            ; type writer - check the key board
              cmpa  #$00               ;  if nothing typed, keep checking
              beq   looop
                                       ;  otherwise - what is typed on key board
firstcalc     jsr   putchar            ; is displayed on the terminal window
             
              staa  1,Y+               ; save char in buff, increment X
              inc   countcalc              ; increment Count
                            
              cmpa  #$2A
              beq   checksig           ; *
                                          
              cmpa  #$2B
              beq   checksig           ; +

              
              cmpa  #$2D
              beq   checksig           ; -

              
              cmpa  #$2F
              beq   checksig           ; /

              cmpa  #CR
              beq   entercalc              ; check enter
              
              cmpa  #$30
              blt   wrong              ; 0~9
              cmpa  #$39
              bgt   wrong
              
              ldab  Countsig
              cmpb  #$01               ;Countsig = 1?
              beq   secondnum
              
                          
              ldab  Count1
              cmpb  #$03               ;Count1 >= 3?
              bge   wrong
              bra   storeinBuff1              
                              
storeinBuff1  
              staa  1,X+                ; store num1 in Buff1
              inc   Count1              ; increment Count1
              bra   looop
              

secondnum     ldab  Count2
              cmpb  #$03                ;Count2 >=3?
              bge   wrong
              bra   storeinBuff2
                            

storeinBuff2  
              staa  1,X+                ; store num2 in Buff2
              inc   Count2              ; increment Count2
              bra   looop
                                          
checksig      ldab  Countsig
              cmpb  #$01                ; Countsig = 1?
              beq   wrong               

              ldab  Count1
              beq   wrong               ;Count1 = 0?
              ldx   #Buffsig
              staa  X                   ; store sig in BuffSig
              ldx   #Buff2              ; set x point to Buff2
              inc   Countsig
              bra   looop
              
wrong         jsr   upClock
              jsr   getchar            ; type writer - check the key board
              cmpa  #$00               ;  if nothing typed, keep checking
              beq   wrong
                                       ;  otherwise - what is typed on key board
              jsr   putchar            ; is displayed on the terminal window
              
              cmpa  #CR
              bne   wrong              ; if Enter/Return key is pressed, move the             
              inc   checkWrong
              iny
              inc   countcalc
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF                ; cursor to next line
              jsr   putchar
              bra   printsaved

entercalc         
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              ldaa  Count2
              cmpa  #$00
              bne   printsaved
              ldaa  Countsig
              cmpa  #$01
              bne   printsaved
              inc   checkWrong
              bra   printsaved
              
printsaved    dey                       
              dec   countcalc
              ldab  countcalc
deyloop       dey
              decb
              bne   deyloop
printloop     ldaa  1,Y+
              jsr   putchar             ;print on screen
              dec   countcalc
              bne   printloop
              ldaa  checkWrong
              cmpa  #$01
              bne   validinput
overflow     
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              ldaa  checkoverflow
              cmpa  #$01
              beq   printoverflow
              ldx   #invalidmsg
              jsr   printmsg
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              lbra   initialize

printoverflow ldx   #overflowmsg
              jsr   printmsg
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              lbra  initialize              
              
validinput    jsr   getnum1
num1saved     jsr   getnum2
num2saved
              ldd   num2
              cmpa  #$00
              bne   continue
              cmpb  #$00
              bne   continue
              ldaa  Buffsig
              cmpa  #$00
              beq   onenum
              bra   overflow
              
         
continue      ldaa  Buffsig             
              
              cmpa  #$2A
              lbeq   multiply            ; *
                                        
              cmpa  #$2B
              lbeq   add                 ; +
              
              cmpa  #$2D
              lbeq   minus               ; -
              
              cmpa  #$2F
              lbeq   divide              ; /
              
onenum        ldd   num1
              std   result
                                                       
computedone   
              jsr   printfinal
              ldaa  #CR                ; move the cursor to beginning of the line
              jsr   putchar
              ldaa  #LF
              jsr   putchar
              
              lbra   initialize
initialize                  
              ldaa  #$20
              ldx   #countcalc
initialloop   clr   1,X+
              deca
              bne   initialloop
              ldx   #msg5
              jsr   printmsg
              
              lbra   modechoose
  
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
            
            lbra   errorclc  
            
checkset    ldx   #buffclk           ; x->buff[0]
            ldaa  1,x+            ; a = buff[0]
            cmpa  #'s'            ; = s ?
            lbne  errorclc
            
            ldaa  1,x+            ; a = buff[1]
            cmpa  #$20            ; = space?
            lbne   errorclc

            ldaa  1,x+            ; a = buff[2]
            cmpa  #$30            ; < 0?
            lblt   errorclc
            cmpa  #$39            ; >9 ?
            lbgt   errorclc
           
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffhh          ; mul -> buffhh
            
            ldaa  1,x+            ; a = buff[3]
            cmpa  #$30            ; < 0?
            lblt  errorclc
            cmpa  #$39            ; >9 ?
            lbgt   errorclc
            
            suba  #$30
            adda  buffhh
            cmpa  #11
            lbgt   errorclc
            staa  buffhh

            ldaa  1,x+            ; a = buff[4]
            cmpa  #$3A            ; = :?
            lbne   errorclc

            ldaa  1,x+            ; a = buff[5]
            cmpa  #$30            ; < 0?
            blt   errorclc
            cmpa  #$39            ; >9 ?
            bgt   errorclc
            
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffmm          ; mul -> buffmm
            
            ldaa  1,x+            ; a = buff[6]
            cmpa  #$30            ; < 0?
            blt   errorclc
            cmpa  #$39            ; >9 ?
            bgt   errorclc
            
            suba  #$30
            adda  buffmm
            cmpa  #59
            bgt   errorclc
            staa  buffmm   
            
            ldaa  1,x+            ; a = buff[7]
            cmpa  #$3A            ; = :?
            bne   errorclc
            
            ldaa  1,x+            ; a = buff[8]
            cmpa  #$30            ; < 0?
            blt   errorclc
            cmpa  #$39            ; >9 ?
            bgt   errorclc
            
            suba  #$30
            ldab  #$0A            ; b = 10
            mul
            stab  buffss          ; mul -> buffss
            
            ldaa  1,x+            ; a = buff[9]
            cmpa  #$30            ; < 0?
            blt   errorclc
            cmpa  #$39            ; >9 ?
            bgt   errorclc
            
            suba  #$30
            adda  buffss
            cmpa  #59
            bgt   errorclc
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
                        
errorclc       
            ldx   #msgerror
            jsr   printmsg
            jsr   nextline
            inc   cerror
            bra   NCdone           
                        
checkquit   ldx   #buffclk
            ldaa  1,x+
            cmpa  #$71
            bne   errorclc
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
;*****************getnum***********************
;*input: Count, Buff, X
;*output: number in num
;**********************************************
getnum1         
               ldaa   Count1
               ldx    #Buff1
num1loop       deca
               beq    num1lastdig     ;X now point to the last digit of num1
               inx
               bra    num1loop

num1lastdig    ldab   1,X-    ; load last char in B, decrement Y

               subb   #$30    ; subtract $30
               clra
               std    num1
               dec    Count1
               lbeq   num1saved
               
               ldab   1,X-
               
               subb   #$30
               ldaa   #$0A
               mul            ; multiply A and B, stored in A:B
               addd   num1    ; add last num
               std    num1    ; store in num1
               dec    Count1
               lbeq   num1saved
               
               ldab   1,X-               
               clra               
               subb   #$30
               ldy    #$0064    ; Y = 100
               emul           ; multiply D and Y, Y:D
               addd   num1
               std    num1    ; D to num1
               rts
               
;**********************************************   
               
 ;*****************getnum***********************
;*input: Count, Buff, X
;*output: number in num
;**********************************************
getnum2         
               ldaa   Count2
               ldx    #Buff2
num2loop       deca
               beq    num2lastdig     ;X now point to the last digit of num1
               inx
               bra    num2loop

num2lastdig    ldab   1,X-    ; load last char in B, decrement Y

               subb   #$30    ; subtract $30
               clra
               std    num2
               dec    Count2
               lbeq   num2saved
               
               ldab   1,X-
               
               subb   #$30
               ldaa   #$0A
               mul            ; multiply A and B, stored in A:B
               addd   num2    ; add last num
               std    num2    ; store in num1
               dec    Count2
               lbeq   num2saved
               
               ldab   1,X-
                              
               subb   #$30
               clra
               ldy    #$0064    ; Y = 100
               emul           ; multiply D and Y, Y:D
               addd   num2
               std    num2    ; D to num1
               rts
;**********************************************                 
 ;*****************add***********************
;*input: 
;*output: 
;**********************************************
add
               ldd    num1     ;num1 to A, num1+1 to B
               addd   num2     ;A:B + num2:num2+1, stored in A:B
               std    result
                                             
               lbra   computedone
;********************************************** 

 ;*****************minus***********************
;*input: 
;*output: 
;**********************************************
minus
               ldd    num1     ;num1 to A, num1+1 to B
               subd   num2     ;A:B + num2:num2+1, stored in A:B
               std    result
               lbra   computedone
;**********************************************

 ;*****************multiply***********************
;*input: 
;*output: 
;**********************************************
multiply
               ldy    num2     ;num2 to Y
               ldd    num1     ; num1 to D
               emul            ; D * Y, Y:D
               std    result   ; D to result
               sty    checkY   ; Y to checkY
               ldd    checkY   ; Y to A:B
               inc    checkoverflow
               cmpa   #$00
               lbne    overflow
               cmpb   #$00
               lbne    overflow ; check if Y = 0?
               lbra    computedone 
               
               
;**********************************************                 

 ;*****************divide***********************
;*input: 
;*output: 
;**********************************************
divide
               ldx    num2     ;num2 to X
               ldd    num1     ; num1 to D
               idiv            ; D / X => X, remainder => D
               stx    result   ; X to result
               lbra    computedone 
               
               
;**********************************************                 
                 
;*****************printfinal***********************
;*input: 
;*output: 
;**********************************************                
printfinal     
               ldaa   #$3D
               jsr    putchar                 ; print ="="
               ldaa   Buffsig
               cmpa   #$2D                    ; -
               lbeq   couldneg                ;jump for -
               
tenthousand    ldd    result                  ;result to D
               ldx    #$2710                   ; X = 10000
               idiv                           ; D / X => X, remainder => D
               std    result                  ; remainder => result
               stx    xstore                  ; X to D
               ldd    xstore
               cmpb   #$00                    ; if = 0, don't print zero
               beq    thousand
               tba                            ;b to a
               adda   #$30
               jsr    putchar                 ;printchar                           
               inc    leadzero
               
thousand       ldd    result                  ; result to D
               ldx    #$03E8                  ; X = 1000
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzerothousand
               tba
               adda   #$30                     ;print char
               jsr    putchar
               inc    leadzero
               
hundred        ldd    result                  ; result to D
               ldx    #$0064                  ; X = 100
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzerohundred
               tba
               adda   #$30                    ;print char
               jsr    putchar
               inc    leadzero
               
ten            ldd    result                  ; result to D
               ldx    #$000A                  ; X = 10
               idiv                           ; D / X => X, remainder => D        
               std    result                  ; remainder to result
               stx    xstore
               ldd    xstore                  ; X to D
               cmpb   #$00
               beq    ifzeroten
               tba
               adda   #$30                    ;print char
               jsr    putchar
               inc    leadzero               
               
one            ldd    result
               cmpb   #$00
               beq    ifonezero
               tba
               adda   #$30
               jsr    putchar
               rts
               
                              
ifzerothousand ldaa   leadzero
               cmpa   #$00
               ble    hundred                   ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    hundred
ifzerohundred  ldaa   leadzero
               cmpa   #$00
               ble    ten                       ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    ten
ifzeroten      ldaa   leadzero
               cmpa   #$00
               ble    one                   ; leadzero > 0 ?
               ldaa   #$30                      ; print 0
               jsr    putchar
               bra    one
ifonezero      ldaa   #$30
               jsr    putchar
               rts
      
couldneg       ldaa   result
               anda   #%10000000
               cmpa   #%10000000                 ; check first digit 1?
               lbne    tenthousand
               ldd    result
               cmpb   #$00
               beq    negboth
               negb                              ;two's complement of B
               coma                              ;one's complement of A
negatedone     std    result
               ldaa   #$2D
               jsr    putchar                     ;print -
               lbra   hundred
               
negboth        negb
               nega                              
               bra    negatedone
                       
               
               
;OPTIONAL
;more variable/data section below
; this is after the program code section
; of the RAM.  RAM ends at $3FFF
; in MC9S12C128 chip
;*************************************************
msg2        DC.B    'Hello, you may type the following three types of command with enter key:', $00
msg3        DC.B    '"s hh:mm:ss" to set the time of the clock.', $00
msg4        DC.B    '"q" to quit the program. ', $00
msgerror    DC.B    'Invalid time format. Correct example => s hh:mm:ss', $00
msg5        DC.B    'Tcalc> ', $00
msg1        DC.B    'or enter the number you want to calculate', $00
msgquit     DC.B    'Typewrite program started', $00
overflowmsg DC.B    'overflow error ', $00
invalidmsg  DC.B    'invalid input, error ', $00           
msg6        DC.B    'Please change Hyper Terminal to 115.2K baud  ', $00
msg7        DC.B    'Then press enter to continue  ', $00



            END                    ; this is end of assembly source file
                                   ; lines below are ignored - not assembled