;ShutDown v1.2 written by BlackLight 2001
;Download my other software from my website at www.blacklight.wxs.org
;
;assemble this prog with the MAGIC ASSEMBLER by Magic Software Rotterdam
;Download it here (freeware):
;http://www.math.leidenuniv.nl/~bgreeven
;http://www.caiw.nl/~bert
;
;What's new?
;v1.2 - added smartdrive flush
;v1.1 - added command-line parameter support
;v1.0 - first version

            mov  ax,160a                  ;check for Windows
            int  2f
            cmp  ax,0000
            jne  @NoWin
            mov  dx,offset(WinErr)        ;print error message
            mov  ah,9
            int  21
            jmp  @Exit
@NoWin      jmp  @Start

WinErr      db   'This program cannot be run under Windows.' 0a 0d '$'
Txt         db   'ShutDown v1.2 þ www.blacklight.wxs.org' 0a 0d '$'
SyntaxTxt   db   'Syntax: SHUTDOWN [S(hutdown)|R(estart)]' 0a 0d '$'
Question    db   'S(hutdown), R(estart), or C(ancel)? $'
NoATX       db   'Could not shutdown! No ATX maybe?$' 0a 0d
KeyOff      db   'S'
KeyRes      db   'R'
KeyCan      db   'C'
KeyEsc      db   %d27
                                          ;-------------------------------
@Syntax     mov  ah,9
            mov  dx,offset(SyntaxTxt)
            int  21
            jmp  @Exit
                                          ;-------------------------------
@Start      mov  ah,9                     ;Show program name
            mov  dx,offset(Txt)
            int  21
                                          ;-------------------------------
            cmp  byte [0081],0d           ;Check if any parameters given
            je   @NoPars

            mov  si,81                    ;get parameters
@ParLoop    lodsb
            cmp  al,0d                    ;if end reached with no result
            je   @Syntax                  ;   show syntax
            and  al,DF                    ;convert AL to uppercase
            cmp  al,KeyOff                ;check for S parameter
            je   @DoOffW
            cmp  al,KeyRes                ;check for R parameter
            je   @DoResW
            jmps @ParLoop                 ;not recognized,goto next char.

                                          ;-------------------------------

@NoPars     mov  ah,9                     ;Show question
            mov  dx,offset(Question)
            int  21

@DoAsk      xor  ah,ah                    ;Ask for key
            int  16

            cmp  al,KeyEsc                ;Check if 'Esc'-key pressed
            je   @DoCan
            and  al,DF                    ;convert AL to uppercase

            cmp  al,KeyOff                ;Check if 'S'-key pressed
            je   @DoOff

            cmp  al,KeyRes                ;Check if 'R'-key pressed
            je   @DoRes

            cmp  al,KeyCan                ;Check if 'C'-key pressed
            je   @DoCan

            jmps @DoAsk                   ;Invalid key pressed, ask again...

@ShowKey    mov  ah,2                     ;Show the pressed key
            mov  dl,al
            int  21
            mov  ah,9                     ;Show CrLf
            mov  dx,offset(CrLf)
            int  21
            ret                           ;return

CrLf        db   0a 0d '$'

@DoRes      call @ShowKey
@DoResW     call @FlushSD
            jmpf ffff:0000                ;this instruction will reboot the
                                          ;computer
@DoOff      call @ShowKey
@DoOffW     jmps @ATXOff

@DoCan      call @ShowKey
@Exit       mov  ax,4c00                  ;exit to DOS
            int  21

@ATXOff     call @FlushSD                 ;flush smartdrive cache
            mov  ax,5301           ;Function 5301h: APM þ Connect real-mode interface
            xor  bx,bx             ;Device ID:      0000h (=system BIOS)
            int  15                ;Call interrupt: 15h

            mov  ax,530e           ;Function 530Eh: APM þ Driver version
            mov  cx,0102           ;Driver version: APM v1.2
            int  15                ;Call interrupt: 15h

            mov  ax,5307           ;Function 5307h: APM þ Set system power state
            mov  bl,01             ;Device ID:      0001h (=All devices)
            mov  cx,0003           ;Power State:    0003h (=Off)
            int  15                ;Call interrupt: 15h

            ;if the program is still running here, there was an error...
            mov  ah,9
            mov  dx,offset(NoATX)
            int  21

            jmps @Exit

FlushMsg1   db   'Flushing SMARTDRV buffers...$'
FlushMsg2   db   'done' 0a 0d '$'

@FlushSD    mov  ah,9
            mov  dx,offset(FlushMsg1)
            int  21
            mov  ax,4A10       ;flush smartdrv/pccache buffers
            mov  bx,1
            int  1A
            mov  ah,9
            mov  dx,offset(FlushMsg2)
            int  21
            ret
