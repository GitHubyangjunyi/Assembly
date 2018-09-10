         ;代码清单15-2
         ;文件名：c15.asm
         ;文件说明：用户程序 
         ;创建日期：2011-11-15 19:11   

;===============================================================================
SECTION header vstart=0

         program_length   dd program_end          ;程序总长度#0x00
         
         head_len         dd header_end           ;程序头部的长度#0x04

         stack_seg        dd 0                    ;用于接收堆栈段选择子#0x08
         stack_len        dd 1                    ;程序建议的堆栈大小#0x0c
                                                  ;以4KB为单位
                                                  
         prgentry         dd start                ;程序入口#0x10 
         code_seg         dd section.code.start   ;代码段位置#0x14
         code_len         dd code_end             ;代码段长度#0x18

         data_seg         dd section.data.start   ;数据段位置#0x1c
         data_len         dd data_end             ;数据段长度#0x20
;-------------------------------------------------------------------------------
         ;符号地址检索表
         salt_items       dd (header_end-salt)/256 ;#0x24
         
         salt:                                     ;#0x28
         PrintString      db  '@PrintString'
                     times 256-($-PrintString) db 0
                     
         TerminateProgram db  '@TerminateProgram'
                     times 256-($-TerminateProgram) db 0
                     
         ReadDiskData     db  '@ReadDiskData'
                     times 256-($-ReadDiskData) db 0
                 
header_end:
  
;===============================================================================
SECTION data vstart=0                

         message_1        db  0x0d,0x0a
                          db  '[USER TASK]: Hi! nice to meet you,'
                          db  'I am run at CPL=',0
                          
         message_2        db  0
                          db  '.Now,I must exit...',0x0d,0x0a,0

data_end:

;===============================================================================
      [bits 32]
;===============================================================================
SECTION code vstart=0
start:
         ;任务启动时，DS指向头部段，也不需要设置堆栈 
         mov eax,ds
         mov fs,eax
     
         mov eax,[data_seg]
         mov ds,eax
     
         mov ebx,message_1
         call far [fs:PrintString]
;第66~69行,计算当前特权级,转换成ASCII码后填写到数据段,作为第二个字符串的第一个字符,当前特权级别是由CS当前内容的低2位指示的,因此先将CS中的内容传送到AX
         mov ax,cs                           ;接着清除AX的高6位,保留低2位,最后将这个数值加上0x30转换成可显示和打印的ASCII码,并填写到数据段中由标号message_2所指示的字节单元中
         and al,0000_0011B
         or al,0x0030
         mov [message_2],al
;第71~72行,显示包括特权级数值在内的第二个字符串,据我们所知,当前任务的特权级是3,因此显示的完整内容是:
         mov ebx,message_2
         call far [fs:PrintString]
     
         call far [fs:TerminateProgram]      ;退出，并将控制权返回到核心 
    
code_end:

;-------------------------------------------------------------------------------
SECTION trail
;-------------------------------------------------------------------------------
program_end: