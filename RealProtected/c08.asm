         ;代码清单8-2
         ;文件名：c08.asm
         ;文件说明：用户程序 
         ;创建日期：2011-5-5 18:17
         
;===============================================================================
SECTION header vstart=0                     ;定义用户程序头部段 
    program_length  dd program_end          ;标号program_end所代表的汇编地址在数值上等于程序总长度[0x00],此处及以下代码注释中由中括号括起来的十六进制数是当前代码的汇编地址,这条指令的汇编地址是0x00
    ;该段地址仅仅是编译阶段确定的段地址,当用户程序加载到内存中时,需根据加载的位置重新计算,尽管在16位环境中,一个段最长位64KB,但是却可以起始于任何20位的物理地址处,不能用16位保存20位的地址,所以用32位保存
    ;声明并初始化用户程序入口点
    code_entry      dw start                ;标号start=偏移地址,vstart=0子句,所以标号start所代表的汇编地址是相对于当前代码段code_1的起始位置,从0开始[0x04]
                    dd section.code_1.start ;表达式section.code_1.start=段地址[0x06] 
    
    realloc_tbl_len dw (header_end-code_1_segment)/4
                                            ;段重定位表项个数[0x0a]
    ;段重定位表项个数是在编译阶段确定的
    ;实际的段重定位表,每个表项用一个dd声明一个双字,5个表项依次计算段开始汇编地址的表达式并进行初始化
    code_1_segment  dd section.code_1.start ;[0x0c]
    code_2_segment  dd section.code_2.start ;[0x10]
    data_1_segment  dd section.data_1.start ;[0x14]
    data_2_segment  dd section.data_2.start ;[0x18]
    stack_segment   dd section.stack.start  ;[0x1c]
    
    header_end:                
    
;===============================================================================
SECTION code_1 align=16 vstart=0         ;定义代码段1（16字节对齐） 
put_string:                              ;显示串(0结尾)。
                                         ;输入：DS:BX=串地址
         mov cl,[bx]
         or cl,cl                        ;cl=0 ?
         jz .exit                        ;是的，返回主程序 
         call put_char
         inc bx                          ;下一个字符 
         jmp put_string

   .exit:
         ret

;-------------------------------------------------------------------------------
put_char:                                ;显示一个字符
                                         ;输入：cl=字符ascii
         push ax
         push bx
         push cx
         push dx
         push ds
         push es

         ;以下取当前光标位置
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;高8位 
         mov ah,al

         mov dx,0x3d4
         mov al,0x0f
         out dx,al
         mov dx,0x3d5
         in al,dx                        ;低8位 
         mov bx,ax                       ;BX=代表光标位置的16位数

         cmp cl,0x0d                     ;回车符？
         jnz .put_0a                     ;不是。看看是不是换行等字符 
         mov ax,bx                       ;此句略显多余，但去掉后还得改书，麻烦 
         mov bl,80                       
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

 .put_0a:
         cmp cl,0x0a                     ;换行符？
         jnz .put_other                  ;不是，那就正常显示字符 
         add bx,80
         jmp .roll_screen

 .put_other:                             ;正常显示字符
         mov ax,0xb800
         mov es,ax
         shl bx,1
         mov [es:bx],cl

         ;以下将光标位置推进一个字符
         shr bx,1
         add bx,1

 .roll_screen:
         cmp bx,2000                     ;光标超出屏幕？滚屏
         jl .set_cursor

         mov ax,0xb800
         mov ds,ax
         mov es,ax
         cld
         mov si,0xa0
         mov di,0x00
         mov cx,1920
         rep movsw
         mov bx,3840                     ;清除屏幕最底一行
         mov cx,80
 .cls:
         mov word[es:bx],0x0720
         add bx,2
         loop .cls

         mov bx,1920

 .set_cursor:
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         mov dx,0x3d5
         mov al,bh
         out dx,al
         mov dx,0x3d4
         mov al,0x0f
         out dx,al
         mov dx,0x3d5
         mov al,bl
         out dx,al

         pop es
         pop ds
         pop dx
         pop cx
         pop bx
         pop ax

         ret

;-------------------------------------------------------------------------------
  start:                                  ;应用程序入口点
         ;初始执行时，DS和ES指向用户程序头部段
         mov ax,[stack_segment]           ;设置到用户程序自己的堆栈 
         mov ss,ax
         mov sp,stack_end
         
         mov ax,[data_1_segment]          ;设置到用户程序自己的数据段
         mov ds,ax

         mov bx,msg0
         call put_string                  ;显示第一段信息 

         push word [es:code_2_segment]
         mov ax,begin
         push ax                          ;可以直接push begin,80386+
         
         retf                             ;转移到代码段2执行 
         
  continue:
         mov ax,[es:data_2_segment]       ;段寄存器DS切换到数据段2 
         mov ds,ax
         
         mov bx,msg1                      ;将刚才那个字符串的起始偏移地址传送到bx
         call put_string                  ;从屏幕光标处显示第二段信息 

         jmp $                            ;无限循环

;===============================================================================
SECTION code_2 align=16 vstart=0          ;定义代码段2（16字节对齐）

  begin:
         push word [es:code_1_segment]
         mov ax,continue
         push ax                          ;可以直接push continue,80386+
         
         retf                             ;转移到代码段1接着执行 
         
;===============================================================================
SECTION data_1 align=16 vstart=0

    msg0 db '  This is NASM - the famous Netwide Assembler. '
         db 'Back at SourceForge and in intensive development! '
         db 'Get the current versions from http://www.nasm.us/.'
         db 0x0d,0x0a,0x0d,0x0a
         db '  Example code for calculate 1+2+...+1000:',0x0d,0x0a,0x0d,0x0a
         db '     xor dx,dx',0x0d,0x0a
         db '     xor ax,ax',0x0d,0x0a
         db '     xor cx,cx',0x0d,0x0a
         db '  @@:',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     add ax,cx',0x0d,0x0a
         db '     adc dx,0',0x0d,0x0a
         db '     inc cx',0x0d,0x0a
         db '     cmp cx,1000',0x0d,0x0a
         db '     jle @@',0x0d,0x0a
         db '     ... ...(Some other codes)',0x0d,0x0a,0x0d,0x0a
         db 0

;===============================================================================
SECTION data_2 align=16 vstart=0

    msg1 db '  The above contents is written by LeeChung. '
         db '2011-05-06'
         db 0

;===============================================================================
SECTION stack align=16 vstart=0
           
         resb 256                         ;保留256字节的栈空间,此处声明了未初始化的空间,编译时会产生警告信息

stack_end:  

;===============================================================================
SECTION trail align=16
program_end:
;===============================================================================
;处理器的工作模式是将内存分成逻辑上的段,指令和数据的获取一律按段地址:偏移地址进行,一个规范的程序应当包括代码段,数据段,附加段和栈段
;这样段的划分在程序加载到内存之前就已经准备好,NASM使用汇编指令SECTION或者SEGMENT定义段,一旦定义段,后面的内容就都属于该段,除非又出现另一个段
;有时候程序并不以一个段定义语句开始,这些内容默认成为一个段
;Intel处理器要求段在内存中的起始物理地址起码是16字节对齐的,即物理地址必须是16的倍数
;相应的在汇编源程序中定义的段,也有对齐的要求,具体做法是使用align=子句,指定某个SECTION的汇编地址对齐方式,align=16/32
;编译器在程序编译阶段将根据align子句确定段的起始汇编地址,比如这里分别定义了三个段,每个段里只有一个字节数据
;section data1 align=16
;       db 0x55
;section data2 align=16
;       db 0xaa
;section data3 align=16
;       db 0x99
;如果不考虑段的对齐方式,段data1的汇编地址是0,段data2的汇编地址是1,段data3的汇编地址是2
;但是这里每个段的定义中都包含了align=16子句,编译后的内容如下
;55 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
;aa 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
;99
;段的汇编地址实际上是段内第一个元素的汇编地址,段data1的声明的0x55位于汇编地址0x00000000处
;段data2也要求16字节对齐,只有0x00000010才能被16整除,于是编译器将0x00000010作为段data2的汇编地址,并在两个段之间填充15字节的0x00(段data1只有1字节长度)
;每个段都有汇编地址,它是相对于整个程序的开头0的,为了方便取得该段的汇编地址,NASM编译器提供了以下表达式
;       section.段名称.start
;下面是一个汇编语言源程序
;                       ________________________________________________________________________
;包含一些注释                    /|\                             /|\                            /|\
;                               |                               |                              |
;                               |                               |                              |
;                     section.header.start=0                    |                              |
;                               |                               |                              |
;                               |                               |                              |
;SECTION header vsart=0 _______\|/_______________               |                              |
;       .                                                       |                              |
;       .                                               section.code.start                     |
;       .                                                       |                              |
;SECTION code align=16 vstart=0________________________________\|/______________               |
;       .                                   /|\                                                |
;       .                                    |                                                 |
;       .                                  putch                                               |
;       push ax                              |                                                 |
;putch:mov ax,string    ____________________\|/_________________________________               |
;       mov dh,0x07                                                                            |
;       .                                                                                      |
;       .                                                                                  program_end
;       .                                                                                      |
;SECTION data align=16 vstart=0                                                                |
;       .                                                                                      |
;       .                                                                                      |
;       .                                                                                      |
; string db 'How are you ?'                                                                    |
;       .                                                                                      |
;       .                                                                                      |
;       .                                                                                      |
;SECTION extra align=16 vstart=0                                                               |
;       .                                                                                      |
;       .                                                                                      |
;       .                                                                                      |
;SECTION stack align=16 vstart=0                                                               |
;       .                                                                                      |
;       .                                                                                      |
;       .                                                                                      |
;SECTION tail align=16                                                                         |
;program_end:_________________________________________________________________________________\|/_
;段header相对于整个程序开头的汇编地址是section.header.start=0
;段code相对于整个程序开头的汇编地址是section.code.start
;段定义语句还可以包含vstart=子句,这是因为尽管定义了段,但是引用某个标号时,该标号处的汇编地址依然是从整个程序的开头计算的,而不是从段的开头计算的
;使用vstart可以解决这个问题,putch是段code中的一个标号,原则上,该标号代表的汇编地址应该从程序头开始算,但是因为段code的定义中有vstart=0子句,所以标号putch的汇编地址要从它所在的段的开头从0开始算
;同样,引用标号string时,标号string所代表的汇编地址是相对于其所在段data的,也就是传送到ax中的数值是标号string相对于段data起始处的长度
;program_end所代表的汇编就是整个程序的长度(以字节计),因为SECTION tail没有包含vstart=0子句
;
;一般来说加载器和用户程序是在不同的公司不同的人开发的,两者之间不了解对方的结构和功能,双方黑盒,但也不能完全黑盒,加载器必须了解一些必要的信息以加载用户程序
;比如在用户程序内部某个固定位置包含基本的结构信息,加载器也固定在这个位置读取,一般放在程序头部
;                 ________________
;用户程序头部----->|    程序总长度    |<-----也就是双方都知道的协议部分
;                |      入口点     |
;                |  段重定位表项数   |
;                |    段重定位表    |
;                 --------------- |
;                |     用户程序    |
;                |   包括程序头部   |
;                |                |
;                |                |
;                |________________|
;
;                 ________________
;                |                |
;                |                |
;                |     加载器      |
;                |                |
;                |                |
;                 ----------------
;程序头部需要以一个段开始,也就是第7行的
;SECTION header vstart=0                     ;定义用户程序头部段
;并包含以下信息
;用户程序的尺寸(以字节计)
;       加载器根据这一信息决定读取多少个逻辑扇区
;       第8行伪指令dd定义一个双字(32位),因为程序可能很大,程序的长度取自程序中的一个标号,这是允许的,编译阶段,编译器将将标号所代表的汇编地址写在这里
;应用程序入口点
;       包括段地址和偏移地址,详细介绍在程序指令注释处,因为加载器并不清楚用户程序的分段情况,也不知道要执行的第一条指令在什么位置,并且第一条指令不一定位于代码段段内偏移地址为0的地方
;       且用户程序并非只有一个代码段,所以要给出第一条指令的段地址和偏移地址
;段重定位表
;       用户程序可能由多个段,段的用途不关加载器的事,但前提是程序加载到内存后,每个段地址必须重新确定,段的重定位是加载器的工作,加载器需要知道每个段在用户程序中的位置,即分别位于用户程序内的多少字节处
;       代码注释中由中括号括起来的十六进制数是当前代码的汇编地址,program_length  dd program_end          ;程序总长度[0x00],这条指令的汇编地址是0x00
;       用户程序的段数量是不确定的,所以程序第14行声明并初始化了段重定位表的项目数