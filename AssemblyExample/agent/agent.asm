      		.386
      		.model flat, stdcall
      		option casemap :none
include 	windows.inc
include	Objects.inc 
include 	user32.inc
includelib 	user32.lib
include 	kernel32.inc
includelib 	kernel32.lib
include	ole32.inc             ; Interfacing COM
includelib 	ole32.lib             ; Interfacing COM 
include	oleaut32.inc          ; Used for SysAllocString and Varient
includelib 	oleaut32.lib          ; COM tools lib   

include	oaidl.inc        	  ; Used for Coinvoke and others
include	bstrlib.inc           ; BSTR functions
include	L.inc                 ; BSTR functions
includelib 	bstrlib.lib           ; The BSTR lib

include	MSAgent.asm           ; Will include COM itself.

		.data
DLG_MAIN	equ	1000
hAgent 	dd 	0
szWelcome	db     "Hello Everybody, I am Merlin!",0dh,0ah,
			"Welcome to the Aogo Assembly site!",0
szExitApp	db	"Application Exiting",0

		.code
_ProcDlgMain 	proc hWin,uMsg,wParam,lParam
	mov	eax,uMsg
    	.if 	eax == WM_INITDIALOG
        	mov hAgent, $NEW( MSAgent )
        	.if	hAgent            
               	METHOD 	hAgent, MSAgent, Show
			METHOD 	hAgent, MSAgent, Gesture, POS_PROCESS
			METHOD 	hAgent, MSAgent, Gesture, POS_SUGGEST
           		METHOD 	hAgent, MSAgent, Speak, addr szWelcome
			METHOD 	hAgent, MSAgent, ReadClipboard, hWin
			METHOD 	hAgent, MSAgent, Hide
        	.endif

	.elseif eax == WM_CLOSE
		.if	hAgent
          		METHOD 	hAgent, MSAgent, Show
          		METHOD 	hAgent, MSAgent, Speak, addr szExitApp          		
        	.endif
		invoke		PostQuitMessage,NULL 

      .elseif eax == WM_DESTROY
      		.if	hAgent
			invoke		Sleep,2000
      			DESTROY 	hAgent
			invoke		Sleep,2000
      		.endif
		invoke 	ExitProcess,NULL

      .else
       	mov	eax,FALSE
       	ret    	
    	.endif    	
    	mov	eax,TRUE
    	ret
_ProcDlgMain endp

start:
      	invoke 	GetModuleHandle,NULL
      	invoke 	DialogBoxParam,eax,DLG_MAIN,NULL,offset _ProcDlgMain,NULL
      	invoke 	ExitProcess,NULL
end start