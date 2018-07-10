         ;代码清单6-1
         ;文件名：c06_mbr.asm
         ;文件说明：硬盘主引导扇区代码
         ;创建日期：2011-4-12 22:12 
      
         jmp near start
         
  mytext db 'L',0x07,'a',0x07,'b',0x07,'e',0x07,'l',0x07,' ',0x07,'o',0x07,\
            'f',0x07,'f',0x07,'s',0x07,'e',0x07,'t',0x07,':',0x07
  number db 0,0,0,0,0
  
  start:
         mov ax,0x7c0                  ;设置数据段基地址 
         mov ds,ax                     ;将数据段基地址传入ds数据段寄存器
         
         mov ax,0xb800                 ;设置附加段基地址
         mov es,ax                     ;将附加段基地址传入es附加段寄存器
         
         cld                           ;方向标志清零指令cld,无操作数指令,将df清零以表示传送方向为正,与其相反的是std置方向标志指令
         mov si,mytext                 ;设置si寄存器的内容到源串的首地址,也就是mytext处的汇编地址
         mov di,0                      ;设置目的地的首地址到di寄存器,屏幕上第一个字符的位置对应着0xB800段的开始处,所以设置di为0
         mov cx,(number-mytext)/2      ;设置要批量传送的字节数到cx寄存器,因为数据串是在两个标号number和mytext之间声明的,而标号代表的是汇编地址,所以允许将他们相减并除以2(每个要显示的字符占2字节)来得到,这个阶段是在编译时进行的,而不是在指令执行时,实际上等于 13
         rep movsw                     ;movsw一次传送一个字,rep表示不为零就重复执行
     
         ;得到标号所代表的偏移地址并传到寄存器ax
         mov ax,number
         
         ;计算各个数位
         mov bx,ax                     ;使bx指向该处的偏移地址,等效于mov bx,number用寄存器传递更快更方便
         mov cx,5                      ;将循环次数5传到cx 
         mov si,10                     ;将除数10传到si 
  digit: 
         xor dx,dx                     ;异或dx与dx,作用是将dx清零
         div si
         mov [bx],dl                   ;将dl中得到的余数传到由bx所指示的内存单元中(在8086处理器上如果要用寄存器来提供偏移地址,只能使用bx/si/di/bp这四个寄存器),保存数位
         inc bx                        ;将bx内容加一指向下一个内存单元
         loop digit                    ;loop指令的功能是重复执行一段代码,在这里将cx减一并判断是否为零,如果不为零则跳转到标号digit所在的位置处执行
         
         ;显示各个数位
         mov bx,number 
         mov si,4                      
   show:
         mov al,[bx+si]
         add al,0x30
         mov ah,0x04
         mov [es:di],ax
         add di,2
         dec si                        ;将si内容减一
         jns show
         
         mov word [es:di],0x0744

         jmp near $

  times 510-($-$$) db 0
                   db 0x55,0xaa