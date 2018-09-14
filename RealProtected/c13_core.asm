         ;代码清单13-2
         ;文件名：c13_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2011-10-26 12:11

         ;以下常量定义部分。内核的大部分内容都应当固定,这些内存段的选择子,它们对应的描述符会在内核初始化的时候创建,这些段是内核的段,供内核代码使用,对内核代码是透明的(重要的是RPL=0),并且伪指令equ仅仅是允许用符号代替数值,声明的数值不占用空间
         core_code_seg_sel     equ  0x38    ;内核代码段选择子,equ伪指令声明的数值不占用空间
         core_data_seg_sel     equ  0x30    ;内核数据段选择子 
         sys_routine_seg_sel   equ  0x28    ;系统公共例程代码段的选择子 
         video_ram_seg_sel     equ  0x20    ;视频显示缓冲区的段选择子
         core_stack_seg_sel    equ  0x18    ;内核堆栈段选择子
         mem_0_4_gb_seg_sel    equ  0x08    ;整个0-4GB内存的段的选择子
        ;内核代码"知道"每个段选择子的具体值,但是段选择子的具体数值是和它们在GDT中的位置相关的,为了不至于在往后因为调整段的位置而修改程序代码,将它们声明成常数是最好的
;-------------------------------------------------------------------------------
         ;以下是系统核心的头部，用于加载核心程序,记录了各个段的汇编位置,供加载时定位内核的各个部分,也就是告诉初始化代码如何加载内核
         core_length      dd core_end       ;核心程序总长度#00,记录了整个内核文件的大小,以字节为单位

         sys_routine_seg  dd section.sys_routine.start
                                            ;系统公用例程段位置#04,系统公用例程段的起始汇编地址

         core_data_seg    dd section.core_data.start
                                            ;核心数据段位置#08,核心数据段的起始汇编地址

         core_code_seg    dd section.core_code.start
                                            ;核心代码段位置#0c,核心代码段的起始汇编地址


         core_entry       dd start          ;核心代码段入口点#10,用于指示内核入口点,在主引导程序加载了内核之后,从这里把控制权交给内核代码
                          dw core_code_seg_sel;入口点共有6个字节,低地址部分是一个双字,指示段内偏移,将来会传送到指令指针寄存器EIP,它来自一个标号start,位于第531行
;高地址部分是一个字,指定一个内存代码段的选择子,在这里填充的是刚刚在内核代码第7行声明过的常数core_code_seg_sel=0x38,在主引导程序加载了内核之后,从这里把处理器的控制权交给内核代码
;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0                ;系统公共例程代码段,这些例程既可用于内核,也可供用户程序调用,提供各种用途和功能的子过程以简化代码的编写
;-------------------------------------------------------------------------------
         ;字符串显示例程
put_string:                                 ;显示0终止的字符串并移动光标 
                                            ;输入：DS:EBX=串地址
         push ecx
  .getc:
         mov cl,[ebx]
         or cl,cl
         jz .exit
         call put_char
         inc ebx
         jmp .getc

  .exit:
         pop ecx
         retf                               ;段间返回

;-------------------------------------------------------------------------------
put_char:                                   ;在当前光标处显示一个字符,并推进
                                            ;光标。仅用于段内调用 
                                            ;输入：CL=字符ASCII码 
         pushad

         ;以下取当前光标位置
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;高字
         mov ah,al

         dec dx                             ;0x3d4
         mov al,0x0f
         out dx,al
         inc dx                             ;0x3d5
         in al,dx                           ;低字
         mov bx,ax                          ;BX=代表光标位置的16位数

         cmp cl,0x0d                        ;回车符？
         jnz .put_0a
         mov ax,bx
         mov bl,80
         div bl
         mul bl
         mov bx,ax
         jmp .set_cursor

  .put_0a:
         cmp cl,0x0a                        ;换行符？
         jnz .put_other
         add bx,80
         jmp .roll_screen

  .put_other:                               ;正常显示字符
         push es
         mov eax,video_ram_seg_sel          ;0xb8000段的选择子
         mov es,eax
         shl bx,1
         mov [es:bx],cl
         pop es

         ;以下将光标位置推进一个字符
         shr bx,1
         inc bx

  .roll_screen:
         cmp bx,2000                        ;光标超出屏幕？滚屏
         jl .set_cursor

         push ds
         push es
         mov eax,video_ram_seg_sel
         mov ds,eax
         mov es,eax
         cld
         mov esi,0xa0                       ;小心！32位模式下movsb/w/d 
         mov edi,0x00                       ;使用的是esi/edi/ecx 
         mov ecx,1920
         rep movsd
         mov bx,3840                        ;清除屏幕最底一行
         mov ecx,80                         ;32位程序应该使用ECX
  .cls:
         mov word[es:bx],0x0720
         add bx,2
         loop .cls

         pop es
         pop ds

         mov bx,1920

  .set_cursor:
         mov dx,0x3d4
         mov al,0x0e
         out dx,al
         inc dx                             ;0x3d5
         mov al,bh
         out dx,al
         dec dx                             ;0x3d4
         mov al,0x0f
         out dx,al
         inc dx                             ;0x3d5
         mov al,bl
         out dx,al

         popad
         ret                                

;-------------------------------------------------------------------------------
read_hard_disk_0:                           ;从硬盘读取一个逻辑扇区
                                            ;EAX=逻辑扇区号
                                            ;DS:EBX=目标缓冲区地址
                                            ;返回：EBX=EBX+512
         push eax 
         push ecx
         push edx
      
         push eax
         
         mov dx,0x1f2
         mov al,1
         out dx,al                          ;读取的扇区数

         inc dx                             ;0x1f3
         pop eax
         out dx,al                          ;LBA地址7~0

         inc dx                             ;0x1f4
         mov cl,8
         shr eax,cl
         out dx,al                          ;LBA地址15~8

         inc dx                             ;0x1f5
         shr eax,cl
         out dx,al                          ;LBA地址23~16

         inc dx                             ;0x1f6
         shr eax,cl
         or al,0xe0                         ;第一硬盘  LBA地址27~24
         out dx,al

         inc dx                             ;0x1f7
         mov al,0x20                        ;读命令
         out dx,al

  .waits:
         in al,dx
         and al,0x88
         cmp al,0x08
         jnz .waits                         ;不忙，且硬盘已准备好数据传输 

         mov ecx,256                        ;总共要读取的字数
         mov dx,0x1f0
  .readw:
         in ax,dx
         mov [ebx],ax
         add ebx,2
         loop .readw

         pop edx
         pop ecx
         pop eax
      
         retf                               ;段间返回 

;-------------------------------------------------------------------------------
;汇编语言程序是极难一次成功，而且调试非常困难。这个例程可以提供帮助,它的工作原理很简单,EDX是32位的,从右至左,将它以4位为一组,分成8组,每一组的值都在0~15之间,将其转换成相应的字符即可
put_hex_dword:                              ;在当前光标处以十六进制形式显示
                                            ;一个双字并推进光标 
                                            ;输入：EDX=要转换并显示的数字
                                            ;输出：无
         pushad
         push ds
;为了将数值转换成可显示的ASCII码,可以使用处理器的查表指令   xlat(Table Look-up Translation),该指令不影响任何标志位
         mov ax,core_data_seg_sel           ;切换到核心数据段 
         mov ds,ax
;该指令要求事先在DS:(E)BX处定义一个用于转换编码的表格,指令执行时,处理器访问该表格,用AL寄存器的内容作为偏移量,从表格中取出一字节,传回AL寄存器,定义的表格在第374行,声明了标号bin_hex,并初始化了16个字符,这是一个二进制到十六进制的对照检索表,偏移(索引)为0的位置是字符0,0x0f的位置是字符F
         mov ebx,bin_hex                    ;指向核心数据段内的转换表,使EBX指向对照检索表的起始处
         mov ecx,8
  .xlt:;转换过程使用了循环,每次将EDX的内容循环左移4位,共需循环8次,每次移位后的内容被传送到EAX中,并用and指令保留低4位,高位清零
         rol edx,4
         mov eax,edx
         and eax,0x0000000f
         xlat                               ;xlat指令用AL中的值作为索引访问对照表,取出相应的字符并回传到AL
      
         push ecx                           ;每次从对照表中得到一个字符,就要调用put_char过程显示它,但put_char过程需要使用CL作为参数
         mov cl,al                          ;因此第220行,在显示之前先要将ECX压栈保护
         call put_char
         pop ecx
       
         loop .xlt
      
         pop ds
         popad
         retf
      
;-------------------------------------------------------------------------------
allocate_memory:                            ;分配内存
                                            ;输入：ECX=希望分配的字节数
                                            ;输出：ECX=起始线性地址 
         push ds
         push eax
         push ebx
      
         mov eax,core_data_seg_sel          ;常数core_data_seg_sel=0x30
         mov ds,eax                         ;使ds指向内核数据段
      
         mov eax,[ram_alloc]                ;ram_alloc        dd  0x00100000    ;下次分配内存时的起始地址
         add eax,ecx                        ;下一次分配时的起始地址在eax中
      
         ;这里应当有检测可用内存数量的指令
        
         mov ecx,[ram_alloc]                ;返回分配的起始地址

         mov ebx,eax                        ;下一次分配时的起始地址在eax中,将其传送到ebx中
         and ebx,0xfffffffc                 ;0xfffffffc=1111 1111 1111 1111 1111 1111 1111 1100,能被4整除的数的最低两位比特是0
         add ebx,4                          ;强制对齐,ebx中是没有判断是否已经对齐,先强制对齐再说
         test eax,0x00000003                ;下次分配的起始地址最好是4字节对齐,这一句测试eax是否已经对齐了,如果不为0,则表示eax没有对齐
         cmovnz eax,ebx                     ;如果没有对齐，则强制对齐,把已经强制对齐的ebx覆盖eax,cmovnz表示不为零则传送
         mov [ram_alloc],eax                ;下次从该地址分配内存,写回内核数据段中ram_alloc标号处,用于下次分配内存使用
                                            ;cmovcc指令可以避免控制转移 
         pop ebx
         pop eax
         pop ds

         retf

;-------------------------------------------------------------------------------
set_up_gdt_descriptor:                      ;在GDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符 
                                            ;输出：CX=描述符的选择子
         push eax
         push ebx
         push edx
      
         push ds                            ;压栈保存DS和ES
         push es
      
         mov ebx,core_data_seg_sel          ;切换到核心数据段,常数core_data_seg_sel=0x30
         mov ds,ebx                         ;使DS指向内核数据段
;sgdt指令将GDTR中的基地址和边界信息保存到指定的内存位置,前面在内核数据段的头部声明了标号pgdt,并初始化了6个字节,供sgdt指令使用
         sgdt [pgdt]                        ;使用sgdt指令取得GDT的基地址和大小,将其保存在内核数据段中标号pgdt处以便开始处理GDT

         mov ebx,mem_0_4_gb_seg_sel         ;常数mem_0_4_gb_seg_sel=0x08,整个0-4GB内存的段的选择子
         mov es,ebx                         ;令附加段指向4GB内存段以操作GDT
;使用这个movzx带零扩展的传送指令原因:GDT界限是16位的,允许64KB大小,8192个描述符,似乎不需要32位的EBX寄存器,事实上还是需要的,因为后面要用来计算新描述符的32位线性地址,加法指令add要求的是两个32位操作数
         movzx ebx,word [pgdt]              ;内核数据段中的标号pgdt处的前16字节保存的是GDT的界限值,后32字节保存的是GDT的线性基地址
         inc bx                             ;将bx中的界限值加一得到GDT总字节数，实际上也是新描述符在GDT内的偏移量
         add ebx,[pgdt+2]                   ;这个偏移量加上GDT的线性地址就是用于安装新描述符的线性地址,将该新描述符的线性地址保存到ebx,下一步使用,下面两句将EDX:EAX=描述符,安装到GDT的最后一项
;第282行,将GDT的界限值加1,就是GDT的总字节数,也是新描述符在GDT内的偏移量,不过我们用的是    inc bx      而不是      inc ebx,这是有道理的,就一般情况来说,在这里用两条指令中的哪一条都没有问题
         mov [es:ebx],eax                   ;此时EBX存的是指向GDT最后一个项的起始线性地址,这一句安装低32位,下一句安装高32位       ;(接上)但是如果这是计算机启动以来,第一次在GDT中安装描述符可能就会有问题,在初始状态下,也就是计算机启动之后,这时还没有使用GDT
         mov [es:ebx+4],edx                 ;(接上)GDTR寄存器中的基地址为0x0000 0000,界限是0xFFFF,当GDTR寄存器的界限部分是0xFFFF时,表明GDT中还没有描述符,因此将此值加一,结果是0x1 0000,截断成0x0000
;(接上)同样的道理,因为EBX中的内容是GDT的界限值0x0000 FFFF,如果执行的是指令    inc ebx     那么ebx中的内容将会是0x0001 0000,以它作为第一个描述符的偏移量显然是不对的,相反,如果执行的指令是  inc bx      那么因为bx寄存器只有16位,结果为0x0000,进位被丢弃,此指令执行后,EBX=0x0000 0000
         add word [pgdt],8                  ;获取原来的界限值并加上8生成新的界限值,具体操作是增加一个描述符的大小(8)   
      
         lgdt [pgdt]                        ;重新加载GDTR使对GDT的更改生效
       
         mov ax,[pgdt]                      ;得到GDT界限值
         xor dx,dx
         mov bx,8
         div bx                             ;除以8，去掉余数,余数在dx中,商也就是我们所要的描述符索引号
         mov cx,ax                          ;将ax复制到cx
         shl cx,3                           ;左移cx的内容,将索引号移到正确位置,留出选择子的TI位和RPL位,然后就是我们所要生成的选择子 

         pop es                             ;下面恢复调用前现场
         pop ds

         pop edx
         pop ebx
         pop eax
      
         retf
;-------------------------------------------------------------------------------
make_seg_descriptor:                        ;构造存储器和系统的段描述符
                                            ;输入：EAX=线性基地址
                                            ;      EBX=段界限
                                            ;      ECX=属性。各属性位都在原始
                                            ;          位置，无关的位清零 
                                            ;返回：EDX:EAX=描述符
         mov edx,eax                        ;首先将32位段基地址从EAX复制一份给EDX,过一会儿构造描述符还要用到基地址
         shl eax,16                         ;将EAX中的32位基地址左移16次,使基地址低16位部分就位
         or ax,bx                           ;描述符前32位(EAX)构造完毕,这句指令将段界限的低16位传送到EAX的低16位,段界限在EBX中

         and edx,0xffff0000                 ;清除基地址中无关的位,低16位
         rol edx,8                          ;令段基地址在寄存器两头就位,下一步互换
         bswap edx                          ;装配基址的31~24和23~16  (80486+)

         xor bx,bx                          ;先清除EBX的低16位,然后同EDX合并,这里假设EBX寄存器的高12位全是0,所以用了xor bx,bx指令
         or edx,ebx                         ;装配段界限的高4位,同EDX合并

         or edx,ecx                         ;装配属性,就是将ECX合并到EDX 

         retf

;===============================================================================
SECTION core_data vstart=0                  ;系统核心的数据段,提供了一段可读写的内存空间,供内核自己使用
;-------------------------------------------------------------------------------
         pgdt             dw  0             ;用于设置和修改GDT 
                          dd  0

         ram_alloc        dd  0x00100000    ;下次分配内存时的起始地址

         ;符号地址检索表,该表可按需扩展,每个条目包括两部分,第一部分是256字节的符号名,第二部分是例程的入口地址,包括4字节的偏移地址,2字节的段选择子
         salt:
         salt_1           db  '@PrintString'            ;256字节的符号名
                     times 256-($-salt_1) db 0
                          dd  put_string                ;4字节的偏移地址
                          dw  sys_routine_seg_sel       ;2字节的段选择子

         salt_2           db  '@ReadDiskData'           ;256字节的符号名
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0          ;4字节的偏移地址
                          dw  sys_routine_seg_sel       ;2字节的段选择子

         salt_3           db  '@PrintDwordAsHexString'  ;256字节的符号名
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword             ;4字节的偏移地址
                          dw  sys_routine_seg_sel       ;2字节的段选择子

         salt_4           db  '@TerminateProgram'       ;256字节的符号名
                     times 256-($-salt_4) db 0
                          dd  return_point              ;4字节的偏移地址
                          dw  core_code_seg_sel         ;2字节的段选择子

         salt_item_len   equ $-salt_4
         salt_items      equ ($-salt)/salt_item_len

         message_1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message_5        db  '  Loading user program...',0     ;程序加载之前需要先显示一段字符,意思是要开始加载用户程序了
         
         do_status        db  'Done.',0x0d,0x0a,0
         
         message_6        db  0x0d,0x0a,0x0d,0x0a,0x0d,0x0a
                          db  '  User program terminated,control returned.',0

         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 
         core_buf   times 2048 db 0         ;内核用的缓冲区

         esp_pointer      dd 0              ;内核用来临时保存自己的栈指针     

         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 52 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0

;===============================================================================
SECTION core_code vstart=0                  ;内核代码段开始,用于分配内存,读取和加载用户程序,控制用户程序的执行
;-------------------------------------------------------------------------------
load_relocate_program:                      ;加载并重定位用户程序
                                            ;输入：ESI=起始逻辑扇区号
                                            ;返回：AX=指向用户程序头部的选择子 
         push ebx
         push ecx
         push edx
         push esi
         push edi
      
         push ds                            ;过程中要用到DS,ES,先压栈保存
         push es
;399~404行预先读取用户程序第一个扇区以得到用户程序的大小
         mov eax,core_data_seg_sel          ;内核数据段选择子,core_data_seg_sel=0x30
         mov ds,eax                         ;切换DS到内核数据段
       
         mov eax,esi                        ;读取程序头部数据,esi之前已经传入用户程序的起始逻辑扇区号,esi=50
         mov ebx,core_buf                   ;数据的存放地点core_buf   times 2048 db 0         ;内核用的缓冲区,定义在内核数据段内
         call sys_routine_seg_sel:read_hard_disk_0;读取第一个扇区

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ;程序尺寸,用户程序的大小就在用户程序头部内偏移量为0x00的地方,直接访问内核缓冲区取得这个字
         mov ebx,eax                        ;eax和ebx都有用户程序的大小
         and ebx,0xfffffe00                 ;使之512字节对齐（能被512整除的数，低9位都为0),这条指令等于是去掉那些不足512字节的零头
         add ebx,512                        ;ebx加上512,将零头凑整
         test eax,0x000001ff                ;程序的大小正好是512的倍数吗?(01ff=511)test指令的功能与and指令是一样的,根据结果设置相应的标志位,但是test指令执行完丢弃运算结果
         cmovnz eax,ebx                     ;不是。使用凑整的结果,全为0,则cmovnz什么也不做,依然采用用户程序原本的长度值,cmovnz表示不为零则传送
      
         mov ecx,eax                        ;实际需要申请的内存数量传入ecx作为内存分配过程的参数
         call sys_routine_seg_sel:allocate_memory
         mov ebx,ecx                        ;ebx -> 申请到的内存首地址,传送到ebx作为起始地址从硬盘上加载整个用户程序
         push ebx                           ;保存该首地址,以便以后访问用户程序头部
         xor edx,edx                        ;清零edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;将总扇区数传送到计数寄存器ecx准备循环读取用户程序 
      
         mov eax,mem_0_4_gb_seg_sel         ;切换DS到0-4GB的段,常数mem_0_4_gb_seg_sel=0x08,整个0-4GB内存的段的选择子
         mov ds,eax                         ;这样就可以加载用户程序了

         mov eax,esi                        ;起始逻辑扇区号 
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                           ;循环读，直到读完整个用户程序

         ;建立程序头部段描述符
         pop edi                            ;恢复程序装载的首地址到edi,该地址也是用户程序头部的起始地址
         mov eax,edi                        ;程序头部起始线性地址传送到eax,参数一:EAX中的线性基地址
         mov ebx,[edi+0x04]                 ;访问4GB内存段,从用户程序头部偏移0x04处取出用户程序的头部段的长度到EBX
         dec ebx                            ;参数二:EBX段界限=此时的EBX用户程序的头部段的长度-1
         mov ecx,0x00409200                 ;参数三:ECX属性,字节粒度的数据段描述符,0000 0000 0100 0000 1001 0010 0000 0000,G=0字节粒度,D=32位操作数,P=1段存在,DPL=00特权级,S=1代码段或数据段,TYPE=0010读写
         call sys_routine_seg_sel:make_seg_descriptor;构造存储器和系统的段描述符,返回EDX:EAX=描述符,使用上面的三个参数
         call sys_routine_seg_sel:set_up_gdt_descriptor;在GDT内安装一个新的描述符,输出CX=描述符的选择子,使用上面的返回EDX:EAX=描述符
         mov [edi+0x04],cx                  ;安装完用户头部段的描述符后,将该段的选择子写回到用户程序头部

         ;建立程序代码段描述符
         mov eax,edi
         add eax,[edi+0x14]                 ;代码起始线性地址
         mov ebx,[edi+0x18]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x00409800                 ;字节粒度的代码段描述符
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+0x14],cx

         ;建立程序数据段描述符
         mov eax,edi
         add eax,[edi+0x1c]                 ;数据段起始线性地址
         mov ebx,[edi+0x20]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x00409200                 ;字节粒度的数据段描述符
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+0x1c],cx

         ;建立程序堆栈段描述符
         mov ecx,[edi+0x0c]                 ;4KB的倍率,从用户程序头部偏移为0x0c的地方取得一个建议的栈大小,这是一个倍率,至少应当为1,说明用户希望分配4KB的栈,如果为2,则说明用户希望分配8KB的栈,以此类推
         mov ebx,0x000fffff                 ;如果栈段的粒度是4KB,那么用0xF FFFF减去倍率,就是用来创建描述符的段界限,如果用户程序建议的倍率是2,那么意味着他想创建的栈空间为8KB,
         sub ebx,ecx                        ;得到段界限,因此段的界限值为       0xF FFFF-2=0xF FFFD,结合第12章的关于栈段的界限值计算,因为实际使用的段界限是用描述符中的段界限乘以0x1000(4KB) + 0xFFF 得到的
         mov eax,4096                       ;0xF FFFD X 0x1000 + 0xFFF =0xFFFF DFFF,EIP的变化范围是0xFFFF DFFF~0xFFFF FFFF,刚好8KB
         mul dword [edi+0x0c]               ;mul指令将eax=4096乘以用户程序所需的栈大小(倍率),EAX中得到以字节为单位的栈大小
         mov ecx,eax                        ;准备为堆栈分配内存,将EAX已经乘以4096的栈大小传送到ECX
         call sys_routine_seg_sel:allocate_memory;分配内存,输入：ECX=希望分配的字节数,输出：ECX=起始线性地址
         add eax,ecx                        ;因为allocate_memory返回所分配内存的低端地址,和一般数据段不同,栈段描述符中的基地址是栈空间的高端地址,所以用allocate_memory返回的地段地址加上栈的大小,得到堆栈的高端物理地址
         mov ecx,0x00c09600                 ;4KB粒度的堆栈段描述符G=1,B=1使用ESP,P=1描述符存在GDT,特权级DPL=00,S=1代码段数据段堆栈段,TYPE=0110读写向下扩展
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+0x08],cx                  ;将栈段选择子写回到用户程序头部,供用户程序在接管处理器控制权之后使用

         ;重定位SALT
         mov eax,[edi+0x04]                 ;访问4GB段,从用户程序头部偏移0x04处取出刚刚安装好的头部段选择子
         mov es,eax                         ;es -> 用户程序头部,es指向用户程序头部,因为U-SALT在用户程序头部段
         mov eax,core_data_seg_sel
         mov ds,eax                         ;令ds指向内核数据段,因为C-SALT在内核数据段
      
         cld                                ;清标志寄存器的方向标志,使cmps按正向比较

         mov ecx,[es:0x24]                  ;将取的次数(用户程序的SALT条目数,在用户程序头部salt_items标号处存放着)从用户程序头部段内取出,并放到ecx
         mov edi,0x28                       ;用户程序内的SALT位于头部内0x2c处,第485行用于将U-SALT在头部段内的偏移量传送到EDI,前面已经使用ES指向了头部段,U-SALT第一个表项位于头部段偏移0x28处
  .b2: 
         push ecx                           ;这是外循环部分,由于内循环也要使用ECX和EDI寄存器,并有可能破坏它们,进入循环之前先压栈,以便退出内循环继续使用
         push edi                           ;ECX=用户程序的SALT项目数,EDI=用户程序的SALT第一个表项在头部段的偏移量
;压栈保护后进入内循环,每次从外循环进入内循环时,都要重新设置比对次数,并重新使ESI指向C-SALT的开始处,从内核的SALT的第一项开始比较,这是第490~491所做的工作,
         mov ecx,salt_items                 ;获得C-SALT的条目数到ECX,因为要从内核SALT的第一个开始比较起
         mov esi,salt                       ;另ESI=内核SALT表头地址
  .b3:
         push edi
         push esi
         push ecx
;第497~503行,是整个比对过程的核心部分,每当处理器执行到这里时,DS:ESI和ES:EDI都各自指向C-SALT和U-SALT中的某个条目
         mov ecx,64                         ;检索表中，每条目的比较次数,因为每个条目的符号名部分是256字节,每次用cmpsd指令比较4个字节,故每个条目至多需要比对64次,第497行把立即数64传入ECX以控制整个比较过程
         repe cmpsd                         ;开始比对直到发现一个不相符的地方,每次比较4字节,如果两个字符串相同,则需要连续比对64次,而且在比对结束时,ZF=1,表示最后4个字节也相同,如果两个字符串不相同,对比过程会提前结束,且ZF=0,在最坏情况下,这两个字符串可能只有最后4字节是不同的,在这种情况下也需要比对64次,但ZF=0
         jnz .b4                            ;无论哪种情况,如果在退出      repe cmpsd 指令时ZF=0,即表明两个字符串是不同的,所以第499行,如果ZF=0,则表明两个字符串不同,直接转移到内循环的末尾,开始下一次内循环
         mov eax,[esi]                      ;若匹配，esi恰好指向其后的地址数据(如果两个字符串是相同的,那么比较执行后,ESI正好指向C-SALT每个条目后的入口数据,要知道,C-SALT中的每个条目是262字节,最后6字节分别是偏移地址和段选择子)
         mov [es:edi-256],eax               ;将字符串改写成偏移地址,此时已经比较完了,并且匹配,此时es:edi指向用户SALT表的当前比较条目的末尾,用末尾减去256回到比较的条目头,然后将上面的esi指向的内核例程入口地址替换用户符号名
         mov ax,[esi+4]                     ;因此现在的任务是将这结尾的6字节传送到U-SALT当前条目的开始部分,这是第500~503行做的
         mov [es:edi-252],ax                ;以及段选择子
  .b4:;最后结果是U-SALT中的当前条目,其开始的6字节被改写成为一个入口地址
;对于内循环的每一次执行,都要把ESI,EDI和ECX压栈保护,每次对比结束后,第506~509行依次弹出这些寄存器的值,并把ESI的内容加上C-SALT每个条目的长度262字节,以指向下一个C-SALT条目
         pop ecx
         pop esi
         add esi,salt_item_len
         pop edi                            ;从头比较 
         loop .b3                           ;loop指令执行时,将ECX的内容减1并判断是否继续循环
      
         pop edi                            ;外循环的任务是从U-SALT中依次取出表项,因此当内循环完成比对之后,第512~513行从栈中弹出EDI的原始内容,并加上256以指向U-SALT下一个条目
         add edi,256
         pop ecx                            ;第514~515行,从栈中弹出ECX的原值,loop指令将ECX的内容减去1,根据结果判断是否继续循环
         loop .b2

         mov ax,[es:0x04]                   ;把用户程序头部段的选择子传送到AX寄存器,AX寄存器中的选择子是作为参数返回到主程序的,主程序将用它来找到用户程序的入口,并从那里进入
;第519~528行,从栈中弹出并恢复各个寄存器的原始内容,并返回调用者,AX寄存器中的选择子是作为参数返回到主程序的,主程序将用它来找到用户程序的入口,并从那里进入
         pop es                             ;恢复到调用此过程load_relocate_program前的es段 
         pop ds                             ;恢复到调用此过程load_relocate_program前的ds段
      
         pop edi
         pop esi
         pop edx
         pop ecx
         pop ebx
      
         ret
;从load_relocate_program过程返回后,第572~573行用于在屏幕上显示信息,表示加载和重定位已经完成
;-------------------------------------------------------------------------------
start:                                       ;c13_mbr.asm主引导程序执行完跳转到此处,这里是内核入口
         mov ecx,core_data_seg_sel           ;初始化ds,使ds指向核心数据段,core_data_seg_sel等于前面定义的常数0x30
         mov ds,ecx                          ;加载数据段选择子

         mov ebx,message_1                   ;调用公共例程段内的一个过程来显示字符串,该call指令属于直接远转移,指令中给出了公共例程段的选择子和段内偏移量
         call sys_routine_seg_sel:put_string ;这段文字意思是"如果你看到这段消息,那么这意味着我们正在保护模式下运行,内核已经加载,而且显示例程工作正常"
                                         
         ;显示处理器品牌信息,需要使用0x80000002~0x80000004号功能,分三次进行
         mov eax,0x80000002
         cpuid
         mov [cpu_brand + 0x00],eax          ;cpu_brand是在系统核心数据段中声明的标号,并初始化了52字节
         mov [cpu_brand + 0x04],ebx          ;将得到的信息保存在系统核心数据段中,起始位置由cpu_brand标号指定
         mov [cpu_brand + 0x08],ecx
         mov [cpu_brand + 0x0c],edx
      
         mov eax,0x80000003
         cpuid
         mov [cpu_brand + 0x10],eax
         mov [cpu_brand + 0x14],ebx
         mov [cpu_brand + 0x18],ecx
         mov [cpu_brand + 0x1c],edx

         mov eax,0x80000004
         cpuid
         mov [cpu_brand + 0x20],eax
         mov [cpu_brand + 0x24],ebx
         mov [cpu_brand + 0x28],ecx
         mov [cpu_brand + 0x2c],edx
;第560~565行,现在屏幕上留出空行,在显示处理器品牌信息,然后再留空,以突出要显示的内容
         mov ebx,cpu_brnd0                   ;cpu_brnd0        db 0x0d,0x0a,'  ',0
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1                   ;cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0
         call sys_routine_seg_sel:put_string
;第567~568行,程序加载之前需要先显示一段字符,意思是要开始加载用户程序了
         mov ebx,message_5                   ;标号message_5声明在内核数据段中,输入：DS:EBX=串地址
         call sys_routine_seg_sel:put_string ;调用公共例程段中的显示例程
         mov esi,50                          ;用户程序位于逻辑50扇区,用于指定用户程序的起始逻辑扇区号,在指令中直接指定数值不是一个好习惯,正确的做法是用伪指令equ声明成常数以便修改
         call load_relocate_program          ;该过程位于第387行,作用是加载和重定位用户程序,它是内核代码段的一个过程,esi指定起始逻辑扇区号
      
         mov ebx,do_status                   ;do_status        db  'Done.',0x0d,0x0a,0
         call sys_routine_seg_sel:put_string ;显示消息表示加载和重定位完成
;保存内核的栈指针,这是通过将ESP寄存器的当前值写入内核数据段中来完成的,写入位置由标号esp_pointer指示的,位于第378行,初始化为一个双字
         mov [esp_pointer],esp               ;临时保存内核的堆栈指针,在进入用户程序后,用户程序应当切换到自己的栈,从用户程序返回时,还要从这个内存位置还原内核栈指针
;esp_pointer      dd 0                       ;内核用来临时保存自己的栈指针,定义在内核数据段内的双字,32位
         mov ds,ax                           ;将用户程序头部段选择子传送到DS,使DS指向用户程序头部,在用户程序头部段内偏移0x10处,是用户程序的入口点,分别是32位的偏移量和16位的代码段选择子
      
         jmp far [0x10]                      ;执行一个间接远转移,控制权交给用户程序（入口点）进入用户程序内接着执行
                                             ;堆栈可能切换 
;在重新接管了处理器的控制权后,第583~584行使DS重新指向内核数据段
return_point:                                ;用户程序返回点
         mov eax,core_data_seg_sel           ;使ds指向核心数据段
         mov ds,eax
;第586~588行,切换栈,使栈段寄存器SS重新指向内核栈段,并从内核数据段中取得和恢复原先的栈指针位置
         mov eax,core_stack_seg_sel          ;切换回内核自己的堆栈
         mov ss,eax 
         mov esp,[esp_pointer]
;第590~591行,显示一条消息,表示现在已经回到内核
         mov ebx,message_6
         call sys_routine_seg_sel:put_string
;对于一个操作系统来说,下面的任务是回收前一个用户程序所占用的内存,并启动下一个用户程序,但是现在我们无事可做
         ;这里可以放置清除用户程序各种描述符的指令
         ;也可以加载并启动其它程序
;所以第596行,使处理器进入停机状态,别忘了在进入保护模式之前,我们用 cli指令关闭了中断,所以除非由NMI产生,处理器将一直处于停机状态
         hlt
            
;===============================================================================
SECTION core_trail                           ;尾部,用于计算内核长度
;-------------------------------------------------------------------------------
core_end:
;以下是注释说明:
;关于重定位SALT那段代码的解释以及那两层循环
;
;比较前准备工作:
;         ;重定位SALT
;         mov eax,[edi+0x04]                 ;访问4GB段,从用户程序头部偏移0x04处取出刚刚安装好的头部段选择子
;         mov es,eax                         ;es -> 用户程序头部,es指向用户程序头部,因为U-SALT在用户程序头部段
;         mov eax,core_data_seg_sel
;         mov ds,eax                         ;令ds指向内核数据段,因为C-SALT在内核数据段
;      
;         cld                                ;清标志寄存器的方向标志,使cmps按正向比较
;
;         mov ecx,[es:0x24]                  ;将取的次数(用户程序的SALT条目数,在用户程序头部salt_items标号处存放着)从用户程序头部段内取出,并放到ecx
;         mov edi,0x28                       ;用户程序内的SALT位于头部内0x2c处,第485行用于将U-SALT在头部段内的偏移量传送到EDI,前面已经使用ES指向了头部段,U-SALT第一个表项位于头部段偏移0x28处
;
;
;
;外循环结构:
;.b2: 
;         push ecx                           ;这是外循环部分,由于内循环也要使用ECX和EDI寄存器,并有可能破坏它们,进入循环之前先压栈,以便退出内循环继续使用
;         push edi                           ;ECX=用户程序的SALT项目数,EDI=用户程序的SALT第一个表项在头部段的偏移量
;压栈保护后进入内循环,每次从外循环进入内循环时,都要重新设置比对次数,并重新使ESI指向C-SALT的开始处,的是第490~491所做的工作
;
;         ;此处放置内循环代码,用于实际进行比较
;
;         pop edi                            ;外循环的任务是从U-SALT中依次取出表项,因此当内循环完成比对之后,第512~513行从栈中弹出EDI的原始内容,并加上256以指向下一个条目
;         add edi,256
;         pop ecx
;         loop .b2                           ;第514~515行,从栈中弹出ECX的原值,loop指令将ECX的内容减去1,根据结果判断是否继续循环
;
;
;
;内循环结构:
;         mov ecx,salt_items
;         mov esi,salt
;  .b3:
;         push edi
;         push esi
;         push ecx
;
;                                     下面是实际进行比对的代码:
;
;第497~503行,是整个比对过程的核心部分,每当处理器执行到这里时,DS:ESI和ES:EDI都各自指向C-SALT和U-SALT中的某个条目
;         mov ecx,64                         ;检索表中，每条目的比较次数,因为每个条目的符号名部分是256字节,每次用cmpsd指令比较4个字节,故每个条目至多需要比对64次,第497行把立即数64传入ECX以控制整个比较过程
;         repe cmpsd                         ;开始比对直到发现一个不相符的地方,每次比较4字节,如果两个字符串相同,则需要连续比对64次,而且在比对结束时,ZF=1,表示最后4个字节也相同,如果两个字符串不相同,对比过程会提前结束,且ZF=0,在最坏情况下,这两个字符串可能只有最后4字节是不同的,在这种情况下也需要比对64次,但ZF=0
;         jnz .b4                            ;无论哪种情况,如果在退出      repe cmpsd 指令时ZF=0,即表明两个字符串是不同的,所以第499行,如果ZF=0,则表明两个字符串不同,直接转移到内循环的末尾,开始下一次内循环
;         mov eax,[esi]                      ;若匹配，esi恰好指向其后的地址数据,如果两个字符串是相同的,那么比较执行后,ESI正好指向C-SALT每个条目后的入口数据,要知道,C-SALT中的每个条目是262字节,最后6字节分别是偏移地址和段选择子
;         mov [es:edi-256],eax               ;将字符串改写成偏移地址 
;         mov ax,[esi+4]                     ;因此现在的任务是将这结尾的6字节传送到U-SALT当前条目的开始部分,这是第500~503行做的
;         mov [es:edi-252],ax                ;以及段选择子 
;  .b4:;最后结果是U-SALT中的当前条目,其开始的6字节被改写成为一个入口地址
;对于内循环的每一次执行,都要把ESI,EDI和ECX压栈保护,每次对比结束后,第506~509行依次弹出这些寄存器的值,并把ESI的内容加上C-SALT每个条目的长度262字节,以指向下一个C-SALT条目
;
;                                     上面是实际进行比对的代码
;
;         pop ecx
;         pop esi
;         add esi,salt_item_len
;         pop edi                            ;从头比较
;         loop .b3                           ;loop指令执行时,将ECX的内容减1并判断是否继续循环