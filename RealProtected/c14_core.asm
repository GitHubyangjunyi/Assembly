         ;代码清单14-1
         ;文件名：c14_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2011-11-6 18:37

         ;以下常量定义部分。内核的大部分内容都应当固定,文件起始部分的常数定义了内核所有段的选择子,显然这些选择子的RPL都是0,内核请求访问自己的段,请求特权级应当为0
         core_code_seg_sel     equ  0x38    ;内核代码段选择子
         core_data_seg_sel     equ  0x30    ;内核数据段选择子 
         sys_routine_seg_sel   equ  0x28    ;系统公共例程代码段的选择子 
         video_ram_seg_sel     equ  0x20    ;视频显示缓冲区的段选择子
         core_stack_seg_sel    equ  0x18    ;内核堆栈段选择子
         mem_0_4_gb_seg_sel    equ  0x08    ;整个0-4GB内存的段的选择子

;-------------------------------------------------------------------------------
         ;以下是系统核心的头部，用于加载核心程序
         core_length      dd core_end       ;核心程序总长度#00

         sys_routine_seg  dd section.sys_routine.start
                                            ;系统公用例程段位置#04

         core_data_seg    dd section.core_data.start
                                            ;核心数据段位置#08

         core_code_seg    dd section.core_code.start
                                            ;核心代码段位置#0c


         core_entry       dd start          ;核心代码段入口点#10
                          dw core_code_seg_sel

;===============================================================================
         [bits 32]
;===============================================================================
SECTION sys_routine vstart=0                ;系统公共例程代码段 
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
;汇编语言程序是极难一次成功，而且调试非常困难。这个例程可以提供帮助 
put_hex_dword:                              ;在当前光标处以十六进制形式显示
                                            ;一个双字并推进光标 
                                            ;输入：EDX=要转换并显示的数字
                                            ;输出：无
         pushad
         push ds
      
         mov ax,core_data_seg_sel           ;切换到核心数据段 
         mov ds,ax
      
         mov ebx,bin_hex                    ;指向核心数据段内的转换表
         mov ecx,8
  .xlt:    
         rol edx,4
         mov eax,edx
         and eax,0x0000000f
         xlat
      
         push ecx
         mov cl,al                           
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
      
         mov eax,core_data_seg_sel
         mov ds,eax
      
         mov eax,[ram_alloc]
         add eax,ecx                        ;下一次分配时的起始地址
      
         ;这里应当有检测可用内存数量的指令
          
         mov ecx,[ram_alloc]                ;返回分配的起始地址

         mov ebx,eax
         and ebx,0xfffffffc
         add ebx,4                          ;强制对齐 
         test eax,0x00000003                ;下次分配的起始地址最好是4字节对齐
         cmovnz eax,ebx                     ;如果没有对齐，则强制对齐 
         mov [ram_alloc],eax                ;下次从该地址分配内存
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

         push ds
         push es

         mov ebx,core_data_seg_sel          ;切换到核心数据段
         mov ds,ebx

         sgdt [pgdt]                        ;以便开始处理GDT

         mov ebx,mem_0_4_gb_seg_sel
         mov es,ebx

         movzx ebx,word [pgdt]              ;GDT界限
         inc bx                             ;GDT总字节数，也是下一个描述符偏移
         add ebx,[pgdt+2]                   ;下一个描述符的线性地址

         mov [es:ebx],eax
         mov [es:ebx+4],edx

         add word [pgdt],8                  ;增加一个描述符的大小

         lgdt [pgdt]                        ;对GDT的更改生效

         mov ax,[pgdt]                      ;得到GDT界限值
         xor dx,dx
         mov bx,8
         div bx                             ;除以8，去掉余数
         mov cx,ax
         shl cx,3                           ;将索引号移到正确位置

         pop es
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
         mov edx,eax
         shl eax,16
         or ax,bx                           ;描述符前32位(EAX)构造完毕

         and edx,0xffff0000                 ;清除基地址中无关的位
         rol edx,8
         bswap edx                          ;装配基址的31~24和23~16  (80486+)

         xor bx,bx
         or edx,ebx                         ;装配段界限的高4位

         or edx,ecx                         ;装配属性

         retf

;-------------------------------------------------------------------------------
make_gate_descriptor:                       ;构造门的描述符（调用门等）
                                            ;输入：EAX=门代码在段内偏移地址
                                            ;       BX=门代码所在段的选择子 
                                            ;       CX=段类型及属性等（各属
                                            ;          性位都在原始位置）
                                            ;返回：EDX:EAX=完整的描述符
         push ebx
         push ecx
      
         mov edx,eax                        ;先在EDX中得到32位偏移地址的复制品,然后将低16位清除,只留下32位偏移地址的高16位部分,并同CX中的属性值一起,形成调用门描述符的高32位
         and edx,0xffff0000                 ;得到偏移地址高16位 
         or dx,cx                           ;组装属性部分到EDX
;第344~346行,将EAX的高16位清除,只留下32位偏移地址的低16位,接着将EBX逻辑左移16次,使得段选择子位于它的高16位,最后用or指令将这两个寄存器合并,就得到了调用门描述符的低32位
         and eax,0x0000ffff                 ;得到偏移地址低16位 
         shl ebx,16                          
         or eax,ebx                         ;组装段选择子部分
      
         pop ecx
         pop ebx
      
         retf                                   
                             
sys_routine_end:

;===============================================================================
SECTION core_data vstart=0                  ;系统核心的数据段 
;------------------------------------------------------------------------------- 
         pgdt             dw  0             ;用于设置和修改GDT 
                          dd  0

         ram_alloc        dd  0x00100000    ;下次分配内存时的起始地址

         ;符号地址检索表
         salt:
         salt_1           db  '@PrintString'
                     times 256-($-salt_1) db 0
                          dd  put_string                ;前4字节是例程在目标代码段内的偏移量
                          dw  sys_routine_seg_sel       ;后2字节是例程所在代码段的选择子,创建完调用门描述符后,内核会将返回的门描述符选择子回填到这里

         salt_2           db  '@ReadDiskData'
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0          ;前4字节是例程在目标代码段内的偏移量
                          dw  sys_routine_seg_sel       ;后2字节是例程所在代码段的选择子,创建完调用门描述符后,内核会将返回的门描述符选择子回填到这里

         salt_3           db  '@PrintDwordAsHexString'
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword             ;前4字节是例程在目标代码段内的偏移量
                          dw  sys_routine_seg_sel       ;后2字节是例程所在代码段的选择子,创建完调用门描述符后,内核会将返回的门描述符选择子回填到这里

         salt_4           db  '@TerminateProgram'
                     times 256-($-salt_4) db 0
                          dd  return_point              ;前4字节是例程在目标代码段内的偏移量
                          dw  core_code_seg_sel         ;后2字节是例程所在代码段的选择子,创建完调用门描述符后,内核会将返回的门描述符选择子回填到这里

         salt_item_len   equ $-salt_4                   ;当前行汇编地址减去标号salt_4处的汇编地址得到每个SALT条目的长度
         salt_items      equ ($-salt)/salt_item_len     ;条目数

         message_1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message_2        db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0
         
         message_3        db  0x0d,0x0a,'  Loading user program...',0
         
         do_status        db  'Done.',0x0d,0x0a,0
         
         message_6        db  0x0d,0x0a,0x0d,0x0a,0x0d,0x0a
                          db  '  User program terminated,control returned.',0

         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 2048 db 0         ;内核用的缓冲区

         esp_pointer      dd 0              ;内核用来临时保存自己的栈指针     
;用来保存处理器信息的区域
         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 52 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0
;用来保存任务控制块链区域
         ;任务控制块链
         tcb_chain        dd  0;标号tcb_chain初始化为一个双字,初始的数值为0,实际上它是一个指针,用来指向第一个任务的TCB线性基地址,当它为零时,表示任务的数量为0,也就是没有任务
;在创建了第一个任务后,应当把该任务的TCB线性基地址填写到这里,每个TCB的第一个双字,也是一个双字长度的指针,用于指向下一个任务的TCB,如果该位置是0,表示后面没有任务,这是链表上的最后一个任务
core_data_end:;否则它的数值就是下一个任务的TCB线性基地址,如下图所示,所有任务都按照被创建的先后顺序链接在一起,从tcb_chain开始,可以依次找到每一个任务
               
;===============================================================================
SECTION core_code vstart=0
;-------------------------------------------------------------------------------
fill_descriptor_in_ldt:                     ;在LDT内安装一个新的描述符
                                            ;输入：EDX:EAX=描述符
                                            ;          EBX=TCB基地址
                                            ;输出：CX=描述符的选择子
         push eax
         push edx
         push edi
         push ds
;第430~433行,先使段寄存器DS指向4GB内存段,然后访问TCB,从中取得LDT的基地址传送到EDI
         mov ecx,mem_0_4_gb_seg_sel
         mov ds,ecx

         mov edi,[ebx+0x0c]                 ;获得LDT基地址
;第435~440行计算用于安装新描述符的线性地址,并把它安装到那里
         xor ecx,ecx
         mov cx,[ebx+0x0a]                  ;获得LDT界限
         inc cx                             ;LDT的总字节数，即新描述符偏移地址
         
         mov [edi+ecx+0x00],eax
         mov [edi+ecx+0x04],edx             ;安装描述符
;第442~443行,将LDT的总字节数在原来的基础上加8字节,再减去1,这就是新界限值
         add cx,8                           
         dec cx                             ;得到新的LDT界限值 

         mov [ebx+0x0a],cx                  ;更新LDT界限值到TCB
;第447~450行,将描述符的界限值除以8,余数丢弃,所得的商就是当前新描述符的索引号
         mov ax,cx
         xor dx,dx
         mov cx,8
         div cx
;第452~454行,将CX中的索引号左移3次,并将TI位置1,表示指向LDT,这就得到当前描述符的选择子
         mov cx,ax
         shl cx,3                           ;左移3位，并且
         or cx,0000_0000_0000_0100B         ;使TI位=1，指向LDT，最后使RPL=00 

         pop ds
         pop edi
         pop edx
         pop eax
     
         ret
      
;------------------------------------------------------------------------------- 
load_relocate_program:                      ;加载并重定位用户程序
                                            ;输入: PUSH 逻辑扇区号
                                            ;      PUSH 任务控制块基地址
                                            ;输出：无 
         pushad                             ;PUSHAD指令压入32位寄存器，其入栈顺序是:EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI
      
         push ds                            ;在32位模式下,访问栈段用的是ESP,而且每次栈操作的默认操作数是32位,处理器在执行压栈指令时,如果发现指令的操作数是段寄存器(CS,SS,DS,ES,FS,GS),那么先执行一个内部的零扩展操作
         push es                            ;将段寄存器中的16位值扩展成32位,高16位是全0,然后再执行压栈操作,当然,出栈pop会执行相反的操作,将32位的值截断成16位,并传送到相应的段寄存器
      
         mov ebp,esp                        ;为访问通过堆栈传递的参数做准备
;第475~476行,令ES指向4GB内存段
         mov ecx,mem_0_4_gb_seg_sel
         mov es,ecx
;第478行,先从栈中取得TCB的线性首地址,注意,因为源操作数部分使用的是基址寄存器EBP,故该指令默认使用SS来访问内存(栈)
         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址,用EBP来寻址时,不需要使用段超越前缀SS:,因为EBP出现在指令中的地址部分时,默认使用SS段寄存器,至于为什么使用[ebp+11*4]看第1525行和第1721行
;接着第481~484行申请分配160字节的内存空间用于创建LDT,并登记LDT的初始界限和起始地址到TCB中,LDT的段界限也是16位的,只允许8192个描述符,和GDT一样,界限值是表的总字节数减一,因为我们刚创建LDT,所以总字节数为0,所以当前段界限值应当是0xFFFF(0减去1)
         ;以下申请创建LDT所需要的内存
         mov ecx,160                        ;允许安装20个LDT描述符
         call sys_routine_seg_sel:allocate_memory;我们的用户程序很简单,不会划分为太多的段,160字节的空间可以安装20个描述符,应当足够了,LDT的线性起始地址是登记在TCB内偏移0x0C处的,LDT的界限是登记在TCB内偏移0x0A处的
         mov [es:esi+0x0c],ecx              ;登记LDT基地址到TCB中,TCB当初也是动态分配的,需要通过段寄存器ES指向的4GB段来访问
         mov word [es:esi+0x0a],0xffff      ;登记LDT初始的界限到TCB中 
;第487~500行,先将用户程序头部读入内核缓冲区中,根据它的大小决定分配多少内存,具体的方法和策略已经在上一章讲过了,唯一需要说明的是在调用过程sys_routine_seg_sel:read_hard_disk_0之前,用户程序的起始逻辑扇区是从栈中获得的
         ;以下开始加载用户程序 
         mov eax,core_data_seg_sel
         mov ds,eax                         ;切换DS到内核数据段
       
         mov eax,[ebp+12*4]                 ;从堆栈中取出用户程序起始扇区号 
         mov ebx,core_buf                   ;读取程序头部数据     
         call sys_routine_seg_sel:read_hard_disk_0

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ;程序尺寸
         mov ebx,eax
         and ebx,0xfffffe00                 ;使之512字节对齐（能被512整除的数低 
         add ebx,512                        ;9位都为0 
         test eax,0x000001ff                ;程序的大小正好是512的倍数吗? 
         cmovnz eax,ebx                     ;不是。使用凑整的结果
;第502~504行,根据用户程序的实际大小申请分配内存空间,并将线性基地址和用户程序的大小登记到TCB中
         mov ecx,eax                        ;实际需要申请的内存数量
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x06],ecx              ;登记程序加载基地址到TCB中
;第506~519行的工作就是加载整个用户程序,这和上一章也是相同的,唯一不同的是,第515行,从栈中重新取得用户程序的起始逻辑扇区号
         mov ebx,ecx                        ;ebx -> 申请到的内存首地址
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;总扇区数 
;第512行,从TCB中取得用户程序在内存中的基地址,早在第478行,我们就已经让ESI指向了TCB的基地址,当然TCB的基地址位于栈中,也可以从栈中取得
         mov eax,mem_0_4_gb_seg_sel         ;切换DS到0-4GB的段
         mov ds,eax

         mov eax,[ebp+12*4]                 ;起始扇区号 
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                           ;循环读，直到读完整个用户程序

         mov edi,[es:esi+0x06]              ;获得程序加载基地址
;第524~528行,因为用户程序头部的起始地址就是整个用户程序的起始地址,故将EDI的内容传送到EAX,作为过程sys_routine_seg_sel:make_seg_descriptor的第一个参数,即,段的起始地址
         ;建立程序头部段描述符;接着从头部中取得用户程序头部段的长度,作为第二个参数传送到EBX寄存器,因为段界限是段的长度减1,故还要将EBX的内容减1,最后作为第三个参数,在ECX中置入段的属性
         mov eax,edi                        ;程序头部起始线性地址
         mov ebx,[edi+0x04]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f200                 ;字节粒度的数据段描述符，特权级3 
         call sys_routine_seg_sel:make_seg_descriptor
;第531~532行用于用于调用另一个过程fill_descriptor_in_ldt把刚才创建的描述符安装到LDT中
         ;安装头部段描述符到LDT中 
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
;第534~536行,用于将选择子的请求特权级RPL设置为3,登记到TCB中,并回填用户程序头部
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [es:esi+0x44],cx               ;登记程序头部段选择子到TCB
         mov [edi+0x04],cx                  ;和头部内 
;从第539行开始,一直到576行结束,分别是创建用户程序代码段,数据段和栈段描述符,并将它们安装到LDT中,除了往LDT中安装描述符,以及其他一些细节上的差别外,这部分代码和上一章相比,大体上是一致的
         ;建立程序代码段描述符
         mov eax,edi
         add eax,[edi+0x14]                 ;代码起始线性地址
         mov ebx,[edi+0x18]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f800                 ;字节粒度的代码段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x14],cx                  ;登记代码段选择子到头部

         ;建立程序数据段描述符
         mov eax,edi
         add eax,[edi+0x1c]                 ;数据段起始线性地址
         mov ebx,[edi+0x20]                 ;段长度
         dec ebx                            ;段界限 
         mov ecx,0x0040f200                 ;字节粒度的数据段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x1c],cx                  ;登记数据段选择子到头部

         ;建立程序堆栈段描述符
         mov ecx,[edi+0x0c]                 ;4KB的倍率 
         mov ebx,0x000fffff
         sub ebx,ecx                        ;得到段界限
         mov eax,4096                        
         mul ecx                         
         mov ecx,eax                        ;准备为堆栈分配内存 
         call sys_routine_seg_sel:allocate_memory
         add eax,ecx                        ;得到堆栈的高端物理地址 
         mov ecx,0x00c0f600                 ;字节粒度的堆栈段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [edi+0x08],cx                  ;登记堆栈段选择子到头部
;从第579行开始,到第620行结束,用于重定位用户程序的U-SALT表,和第13章相比,绝大多数代码都是相同的,具体的工作流程也几乎没有变化,当然,因为涉及特权级,个别的差异还是有的
         ;重定位SALT,在本章中,用户程序各个段的描述符位于LDT中,尽管已经安装,但还没有生效(还没有加载局部描述符表寄存器LDTR),在这种情况下,只能通过4GB的段来访问U-SALT表
         mov eax,mem_0_4_gb_seg_sel         ;这里和前一章不同，头部段描述符
         mov es,eax                         ;已安装，但还没有生效，故只能通
                                            ;过4GB段访问用户程序头部          
         mov eax,core_data_seg_sel
         mov ds,eax
      
         cld

         mov ecx,[es:edi+0x24]              ;U-SALT条目数(通过访问4GB段取得) 
         add edi,0x28                       ;U-SALT在4GB段内的偏移 
  .b2: 
         push ecx
         push edi
      
         mov ecx,salt_items
         mov esi,salt
  .b3:
         push edi
         push esi
         push ecx
;第605~606行,因为ESI指向当前条目的地址部分,所以4字节之后的地方该地址选择子部分,需要首先传送到AX,紧接着,修改它的RPL字段,使该选择子的请求特权级RPL=3
         mov ecx,64                         ;检索表中，每条目的比较次数 
         repe cmpsd                         ;每次比较4字节 
         jnz .b4
         mov eax,[esi]                      ;若匹配，则esi恰好指向其后的地址
         mov [es:edi-256],eax               ;将字符串改写成偏移地址 
         mov ax,[esi+4]
         or ax,0000000000000011B            ;以用户程序自己的特权级使用调用门
                                            ;故RPL=3 
         mov [es:edi-252],ax                ;回填调用门选择子 
  .b4:
      
         pop ecx
         pop esi
         add esi,salt_item_len
         pop edi                            ;从头比较 
         loop .b3
      
         pop edi
         add edi,256
         pop ecx
         loop .b2

         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址,它是作为过程参数压入当前栈中的
;第625~628行,申请创建0特权级栈所需的4KB内存,并在TCB中登记该栈的尺寸,登记到TCB中的尺寸值要求是以4KB为单位的,所以还要逻辑右移12次,相当于除以4096,得到一个4KB的倍数
         ;创建0特权级堆栈,第629~630行,先申请内存,然后用申请到的内存基地址加上栈的尺寸,得到栈的高端地址,并将此地址登记到TCB中,一般来说,栈应当使用高端地址作为其线性基地址
         mov ecx,4096
         mov eax,ecx                        ;为生成堆栈高端地址做准备 
         mov [es:esi+0x1a],ecx
         shr dword [es:esi+0x1a],12         ;登记0特权级堆栈尺寸到TCB 
         call sys_routine_seg_sel:allocate_memory;第631~634行,用给定的段界限和段属性调用公共例程段内的过程make_seg_descriptor创建描述符,段属性表明这是一个栈段,4KB粒度,我们创建的是0特权级栈,要求描述符DPL=0
         add eax,ecx                        ;堆栈必须使用高端地址为基地址
         mov [es:esi+0x1e],eax              ;登记0特权级堆栈基地址到TCB 
         mov ebx,0xffffe                    ;段长度（界限）
         mov ecx,0x00c09600                 ;4KB粒度，读写，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt;第635~636行,调用内核代码段内的近过程fill_descriptor_in_ldt将刚创建的描述符安装到LDT中,该过程要求使用EBX作为参数提供TCB的线性基地址,所以在调用该过程前先将该地址传送到EBX
         ;or cx,0000_0000_0000_0000          ;设置选择子的特权级为0
         mov [es:esi+0x22],cx               ;登记0特权级堆栈选择子到TCB
         mov dword [es:esi+0x24],0          ;登记0特权级堆栈初始ESP到TCB
      
         ;创建1特权级堆栈
         mov ecx,4096
         mov eax,ecx                        ;为生成堆栈高端地址做准备
         mov [es:esi+0x28],ecx
         shr dword [es:esi+0x28],12         ;登记1特权级堆栈尺寸到TCB;不加dword编译出错?
         call sys_routine_seg_sel:allocate_memory
         add eax,ecx                        ;堆栈必须使用高端地址为基地址
         mov [es:esi+0x2c],eax              ;登记1特权级堆栈基地址到TCB
         mov ebx,0xffffe                    ;段长度（界限）
         mov ecx,0x00c0b600                 ;4KB粒度，读写，特权级1
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0001          ;设置选择子的特权级为1
         mov [es:esi+0x30],cx               ;登记1特权级堆栈选择子到TCB
         mov dword [es:esi+0x32],0          ;登记1特权级堆栈初始ESP到TCB

         ;创建2特权级堆栈
         mov ecx,4096
         mov eax,ecx                        ;为生成堆栈高端地址做准备
         mov [es:esi+0x36],ecx
         shr dword [es:esi+0x36],12         ;登记2特权级堆栈尺寸到TCB;不加dword编译出错?
         call sys_routine_seg_sel:allocate_memory
         add eax,ecx                        ;堆栈必须使用高端地址为基地址
         mov [es:esi+0x3a],ecx              ;登记2特权级堆栈基地址到TCB
         mov ebx,0xffffe                    ;段长度（界限）
         mov ecx,0x00c0d600                 ;4KB粒度，读写，特权级2
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0010          ;设置选择子的特权级为2
         mov [es:esi+0x3e],cx               ;登记2特权级堆栈选择子到TCB
         mov dword [es:esi+0x40],0          ;登记2特权级堆栈初始ESP到TCB
;第676~679行调用公共例程段的过程make_seg_descriptor创建LDT描述符,作为传入的参数,EAX寄存器的内容是从TCB中取出的LDT基地址,EBX的内容是从TCB中取出的LDT长度,ECX的内容是描述符的属性
         ;在GDT中登记LDT描述符
         mov eax,[es:esi+0x0c]              ;LDT的起始线性地址
         movzx ebx,word [es:esi+0x0a]       ;LDT段界限
         mov ecx,0x00408200                 ;LDT描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x10],cx               ;登记LDT选择子到TCB中
;第684~688行,申请104字节的内存用于创建TSS,很显然我们是要创建一个标准大小的TSS,照例要把TSS的基地址和界限登记到任务控制块TCB中,将来创建TSS描述符时用得着
         ;创建用户程序的TSS
         mov ecx,104                        ;tss的基本尺寸
         mov [es:esi+0x12],cx              
         dec word [es:esi+0x12]             ;登记TSS界限值到TCB 
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;登记TSS基地址到TCB
;第691行,将指向前一个任务的指针(任务链接域)填写为0,表明这是唯一任务
         ;登记基本的TSS表格内容
         mov word [es:ecx+0],0              ;反向链=0
;第693~709行,等级0,1,2特权级栈的段选择子,以及它们的初始栈指针,所有的栈信息都在TCB中,先从TCB中取出,然后填写到TSS中的相应位置
         mov edx,[es:esi+0x24]              ;登记0特权级堆栈初始ESP
         mov [es:ecx+4],edx                 ;到TSS中
      
         mov dx,[es:esi+0x22]               ;登记0特权级堆栈段选择子
         mov [es:ecx+8],dx                  ;到TSS中
      
         mov edx,[es:esi+0x32]              ;登记1特权级堆栈初始ESP
         mov [es:ecx+12],edx                ;到TSS中

         mov dx,[es:esi+0x30]               ;登记1特权级堆栈段选择子
         mov [es:ecx+16],dx                 ;到TSS中

         mov edx,[es:esi+0x40]              ;登记2特权级堆栈初始ESP
         mov [es:ecx+20],edx                ;到TSS中

         mov dx,[es:esi+0x3e]               ;登记2特权级堆栈段选择子
         mov [es:ecx+24],dx                 ;到TSS中
;第711~712行,登记当前任务的LDT描述符选择子,在任务切换时,处理器需要用这里的信息找到当前任务的LDT,LDT对任务来说并不是必需的,如果高兴也可以把属于某个任务的段定义在GDT中,如果没有LDT,这里填写0
         mov dx,[es:esi+0x10]               ;登记任务的LDT选择子
         mov [es:ecx+96],dx                 ;到TSS中
;第714~715行,填写I/O许可位映射区的地址,在这里,填写的是TSS段界限103,这意味着不存在该区域
         mov dx,[es:esi+0x12]               ;登记任务的I/O位图偏移
         mov [es:ecx+102],dx                ;到TSS中 
      
         mov word [es:ecx+100],0            ;T=0
;第720~725行,先调用公共例程段内的过程make_seg_descriptor创建TSS描述符,需要传入三个参数,先从TCB中取出TSS的基地址,传送到EAX,然后EBX中的内容是TSS的界限,ECX的内容是描述符属性值
         ;在GDT中登记TSS描述符
         mov eax,[es:esi+0x14]              ;TSS的起始线性地址
         movzx ebx,word [es:esi+0x12]       ;段长度（界限）
         mov ecx,0x00408900                 ;TSS描述符，特权级0,接着调用公共例程段内的另一过程set_up_gdt_descriptor安装此描述符到GDT中,并将返回的描述符选择子登记在TCB中,TSS描述符选择子的RPL=0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x18],cx               ;登记TSS选择子到TCB

         pop es                             ;恢复到调用此过程前的es段 
         pop ds                             ;恢复到调用此过程前的ds段
      
         popad
      
         ret 8                              ;丢弃调用本过程前压入的参数 
      
;-------------------------------------------------------------------------------
append_to_tcb_link:                         ;在TCB链上追加任务控制块
                                            ;输入：ECX=TCB线性基地址
         push eax
         push edx
         push ds
         push es
;第742~745行,令段寄存器DS指向内核数据段以读写链首指针tcb_chain,而ES指向整个4GB内存空间,用于遍历和访问每一个TCB
         mov eax,core_data_seg_sel          ;令DS指向内核数据段,因为链首指针tcb_chain是在内核数据段声明并初始化的,所以令DS指向内核数据段以访问tcb_chain
         mov ds,eax
         mov eax,mem_0_4_gb_seg_sel         ;令ES指向0..4GB段,链上的每个TCB都是动态分配的,使用线性地址来访问
         mov es,eax
;第747行,要追加的TCB一定是链表上的最后一个TCB,故其用于指向下一个TCB的指针域必须清零,以表明自己是链表上最后一个TCB
         mov dword [es: ecx+0x00],0         ;当前TCB指针域清零，以指示这是最后一个TCB,任务控制块TCB第一个信息就是下一个TCB基地址
                                            ;每个TCB的空间都是动态分配的,其首地址都是线性地址,只能用由段寄存器ES所指向的4GB段来访问
;第750~752行,观察链首指针tcb_chain是否为零,若为零,则表明整个链表为空,直接转移到第763行的标号.notcb处,在那里直接将链首指针指向新的TCB,恢复现场后直接返回调用者                                       
         mov eax,[tcb_chain]                ;TCB表头指针
         or eax,eax                         ;链表为空？
         jz .notcb                          ;如果不为零,表明它不是链中最后一个TCB,于是将控制转移到.searc,令EDX指向下一个TCB,继续搜寻
;第754~758行,若链首指针不为零,表明链表非空,需要顺着整个链找到最后一个TCB,和链首指针tcb_chain不同,每个TCB需要用4GB的段来访问,即,使用段寄存器ES  
  .searc:                                   ;首先将链表中要访问的那个TCB的线性地址传送到EDX,然后访问它的TCB指针域,看是否为零,如果不为零,表明它不是链中最后一个TCB,于是将控制转移到.searc,令EDX指向下一个TCB,继续搜寻
         mov edx,eax                        ;若为零,表明它就是最后一个TCB,第760行,用ECX的内容填写其TCB指针域,让它指向新的TCB,完成后,第761行,直接转移到标号.retpc处,恢复现场并返回调用者
         mov eax,[es: edx+0x00]
         or eax,eax               
         jnz .searc
         
         mov [es: edx+0x00],ecx
         jmp .retpc
         
  .notcb:       
         mov [tcb_chain],ecx                ;若为空表，直接令表头指针指向TCB
         
  .retpc:
         pop es
         pop ds
         pop edx
         pop eax
         
         ret
         
;-------------------------------------------------------------------------------
start:                                      ;内核入口点,在执行到这里时,主引导程序已经加载了内核,并对它进行了前期的初始化工作
         mov ecx,core_data_seg_sel          ;使ds指向核心数据段 
         mov ds,ecx
;显示已经进入保护模式并且内核已经加载完成
         mov ebx,message_1                    
         call sys_routine_seg_sel:put_string
                                         
;将处理器品牌信息保存到内核数据段中 
         mov eax,0x80000002
         cpuid
         mov [cpu_brand + 0x00],eax
         mov [cpu_brand + 0x04],ebx
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
;显示处理器品牌信息
         mov ebx,cpu_brnd0                  ;显示处理器品牌信息 
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1
         call sys_routine_seg_sel:put_string
;第812~826行用于安装调用门的描述符,起始也就是等于在安装调用门
         ;以下开始安装为整个系统服务的调用门。特权级之间的控制转移必须使用门
         mov edi,salt                       ;C-SALT表的起始位置,这是第一个条目的位置,将其传送到EDI,以后每次加上262,就能对准下一个条目
         mov ecx,salt_items                 ;C-SALT表的条目数量,将其作为立即数传送到ECX
  .b3:
         push ecx                           ;转换过程中要用到ECX,所以在每次循环开始前先压栈保存,然后在loop指令执行前恢复
         mov eax,[edi+256]                  ;该条目入口点的32位偏移地址,将每个条目(例程)的32位段内偏移地址传送到EAX,每个条目长度是262字节,而它的偏移地址则位于256字节处
         mov bx,[edi+260]                   ;该条目入口点的段选择子,获取条目(例程)所在代码段的选择子,它位于条目内第260字节处
         mov cx,1_11_0_1100_000_00000B      ;特权级3的调用门(3以上的特权级才
                                            ;允许访问)，0个参数(因为用寄存器
                                            ;传递参数，而没有用栈),P=1(有效),DPL=11B(十进制3),TYPE=1100(类型为调用门),这里只需传递调用门描述符中高32位的低16位部分
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+260],cx                   ;将返回的门描述符选择子回填
         add edi,salt_item_len              ;指向下一个C-SALT条目 
         pop ecx                            ;出栈循环次数供loop指令使用
         loop .b3

         ;对门进行测试,标号salt_1指向C-SALT表中的第一个条目的起始处,再此基础上增加256,就是它的地址部分,现在我们已经知道该条目对应着公共例程段中的put_string过程,用于显示零终止的字符串
         mov ebx,message_2;表面上这是一条普通的间接绝对远调用指令call far,通过指令中给出的地址操作数,可以间接取得32位的偏移地址和16位的代码段选择子
         call far [salt_1+256]              ;通过门显示信息(偏移量将被忽略) 
;但是处理器在执行这条指令时,会用该选择子访问GDT/LDT,检查那个选择子,看它指向的是调用门描述符,还是普通的代码段描述符,如果是前者,就按调用门来处理,如果是后者,还按一般的段间控制转移处理
         mov ebx,message_3
         call sys_routine_seg_sel:put_string ;在内核中调用例程不需要通过门
      
         ;创建任务控制块。这不是处理器的要求，而是我们自己为了方便而设立的
         mov ecx,0x46                       ;用于分配创建TCB所需要的内存空间,并将其挂在TCB链上,当前版本的TCB结构需要0x46字节的内存空间
         call sys_routine_seg_sel:allocate_memory
         call append_to_tcb_link            ;将任务控制块追加到TCB链表 
;第840~843行,先以双字的长度将立即数50压入当前栈,这是用户程序的起始逻辑扇区号
         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址 
       
         call load_relocate_program
;第845~846行,在调用过程load_relocate_program创建任务之后,显示一条成功的消息
         mov ebx,do_status
         call sys_routine_seg_sel:put_string
      
         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax
;第851~852行,加载任务寄存器TR和局部描述符寄存器LDTR,为什么加载的是选择子,而不是像lgdt那样直接加载的是基地址和界限,因为刚刚初始化GDT之前,一个描述符都没有,你怎么用选择子加载
         ltr [ecx+0x18]                     ;加载任务状态段TSS选择子(存放在TCB偏移0x18处)
         lldt [ecx+0x10]                    ;加载LDT选择子(存放在TCB偏移0x10处)
;第854~855行,访问任务的TCB,从用户程序头部内取出栈段选择子和栈指针,以及代码段选择子和入口点,并将它们顺序压入当前的0特权级栈中,这部分内容要结合第13章的用户程序头部来分析
         mov eax,[ecx+0x44]
         mov ds,eax                         ;切换到用户程序头部段 

         ;以下假装是从调用门返回。摹仿处理器压入返回参数,当执行retf时,处理器从栈中恢复CS和EIP的原始内容,这里假装用户程序调用了系统代码段并从调用门返回,这样就"回到了"用户程序
         push dword [0x08]                  ;调用前的堆栈段选择子
         push dword 0                       ;调用前的esp

         push dword [0x14]                  ;调用前的代码段选择子 
         push dword [0x10]                  ;调用前的eip
      
         retf                               ;第864行,执行一个远返回指令retf,假装从调用门返回,于是控制转移到用户程序的3特权级代码开始执行,注意这里用的0特权级栈并非是来自于TSS,不过处理器不会在意这个,下次,从3特权级的段再来到0特权级执行时,就会用到TSS中的0特权级栈了

return_point:                               ;用户程序返回点
         mov eax,core_data_seg_sel          ;因为c14.asm是以JMP的方式使用调 
         mov ds,eax                         ;用门@TerminateProgram，回到这 
                                            ;里时，特权级为3，会导致异常。 
         mov ebx,message_6
         call sys_routine_seg_sel:put_string

         hlt
;事实上,能够通过调用门发起控制转移的指令还有jmp,但只用在不需要从调用门返回的场合下,而且不改变当前特权级,也就是说目标代码是在当前特权级上执行
core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
;以下是注释说明:
;在保护模式下,通过将内存分成大小不等的段,并用描述符对每个段的用途,类型和长度进行指定,就可以在程序运行时由处理器硬件施加访问保护
;比如当程序试图去写一个可执行代码段时,处理器就会阻止这种意图,再比如当程序试图让处理器访问超过段界限的内存区域时,处理器也会引发异常中断
;段保护是处理器提供的基本保护功能,但对于现实的需求来说,还是不够的
;首先,当一个程序老老实实地访问只属于它自己的段时,基本的段保护机制是很有效的,但是一个失控的程序,或者一个恶意的程序,依然可以通过追踪和修改描述符表来达到它们访问任何内存位置的目的
;比如说,用户程序知道GDT的位置,它可以通过向段寄存器加载操作系统的数据段描述符,或者在GDT中增加一个指向操作系统数据区的描述符,来修改只属于操作系统的私有数据,而所有这一切都是合法的
;其次,32位处理器是为多任务系统而设计的,所谓多任务系统,是指能够同时执行两个以上程序的系统,即使前一个程序没有执行完,其他程序也可以开始执行
;在单处理器(核)的系统中,多个程序并不可能真的同时执行,但是处理器可以在多个任务之间周期性地切换和轮转,这样它们都处于走走停停的状态
;快速的处理器加上高效的任务切换,在外界看来就是多个任务在同时运行中
;多任务系统,对任务之间的隔离和保护都提出了要求,这可以看做对段保护机制的进一步强化,同时,在多任务系统中,操作系统居于核心软件的位置,为各个任务服务,负责任务的加载,创建和执行环境的管理
;并执行任务之间的调度,对操作系统的保护显得尤为重要,事实上对于这种要求,基本的段保护机制已经无能为力了
;本章目标:
;   1.通过演示如何创建一个任务,并使之投入运行来学习任务的概念及其组成要素,包括任务的全局空间和局部空间,TSS,LDT,特权级等
;   2.必须了解特权级不是指任务的特权级,而是指组成任务的各个部分的特权级,比如任务的全局部分一般是0,1和2特权级,任务的私有部分一般是3特权级别
;   3.必须清楚CPL,DPL和RPL的含义,以及不同特权级别之间的控制转移规则
;   4.熟悉调用门的用法
;   5.掌握在Bochs下调试程序的新手段
;   6.新的X86处理器指令,lldt
;                     ltr
;                     pushf/pushfd
;                     popf/popfd
;                     ret n/retf n
;                     arpl
;                     call/jmp的新功能
;
;                               任务的隔离和特权级保护
;                       任务,任务的LDT和任务的TSS
;程序是记录在载体上的指令和数据,总是为了完成某个特定的工作,其正在执行中的一个副本,叫做任务
;如果一个程序有多个副本正在内存中运行,那么它对应着多个任务,每一个副本都是一个任务,上一章,用户程序就是任务,而内核程序就是操作系统的缩影
;一直以来我们把所有的段描述符都放在GDT中,而不管它属于内核还是用户程序
;为了有效地在任务之间实施隔离,处理器建议每个任务都应当具有自己的描述符表,称为局部描述符表LDT(Local Descriptor Table),并且把专属于自己的那些段放到LDT中
;如下图所示
;                                                                                   ----------
;                                                                                  |   TSSn   |
;                                                                                   ----------
;                                                               ----------                      任务n
;                                                              |          |         ----------
;              处理器                                           |          |        |   LDTn   |
;       -------------------                                    |   GDT    |         ----------
;      |                   |                                   |          |
;      |                   |                                   |          |
;      |      GDTR-----------------GDT的基地址和界限------------>>----------              ....
;      |                   |                                                            ....
;      |                   |
;      |                   |
;      |                   |                                                        ----------
;      |                   |                                                       |   TSS2   |
;      |       TR------------------当前任务的TSS(基地址和段界限)---------------------->>----------
;      |                   |                                                                    任务2(当前任务)
;      |                   |                                                        ----------
;      |                   |                                                       |   LDT2   |
;      |      LDTR-----------------当前任务的LDT(基地址和段界限)---------------------->>----------
;      |                   |
;       -------------------
;                                                                                   ----------
;                                                                                  |   TSS1   |
;                                                                                   ----------
;                                                                                               任务1
;                                                                                   ----------
;                                                                                  |   LDT1   |
;                                                                                   ----------
;和GDT一样,LDT也是用来存放描述符的,不同之处在于LDT只属于某个任务,或者说每个任务都有自己的LDT,每个任务私有的段,都应当在LDT中进行描述
;另外LDT的第一个描述符,也就是0号槽位,也是有效的可以使用的
;为了追踪全局描述符表,访问它内部的描述符,处理器使用了GDTR寄存器,全局描述符表是全局性的,为所有任务服务,是他们所共有的,只需要一个全局描述符表就够了
;和GDT不同,局部描述符LDT的数量不止一个,数量视任务的多少而定,为了追踪和访问这些LDT,处理器使用局部描述符表寄存器LDTR
;在一个多任务的系统中,会有很多任务轮流执行,正在执行中的那个任务称为当前任务,因为LDTR寄存器只有一个,所以只用于指向当前任务的LDT
;每当发生任务切换时,LDTR的内容被更新,以指向新任务的LDT
;和GDTR一样,LDTR包含了32位的线性基地址字段和16位段界限字段,以指示当前LDT的位置和大小
;我们知道访问内存之前需要先指定一个段,方法是向段寄存器的选择器传送一个段选择子,称为引用一个段,如下
;   mov cx,0x0008   ;0000 0000 0000 1 0 00
;   mov ds,cx
;回到第十一章在保护模式下访问一个段时,传送到段选择器的是段选择子,由三部分组成:
;第一部分是描述符的索引号,用来在描述符表中选择一个段描述符
;TI是描述符表指示器(Table Indicator),TI=0时,表示描述符在GDT中,TI=1时,描述符在LDT中(也是一个描述符表,跟GDT类似),表示从当前任务的LDT中加载描述符
;RPL是请求特权级,表示给出当前选择子的那个程序的特权级别,正是该程序要访问这个段
;段选择子的组成:
;15                          3   2  1    0
;|          描述符索引          | TI | RPL |
;
;很显然0x0008的TI位等于0,所以处理器访问GDT,从1号槽位取得描述符,并传送到DS的描述符高速缓存器中
;另一个例子
;   mov cx,0x005c   ;0000 0000 0101 1 1 00
;   mov ds,cx
;TI=1,索引号=11,所以处理器访问当前任务的LDT(该LDT在内存中的位置由LDTR指定),从它的11号槽位取出描述符,并传送到DS的描述符高速缓存器中
;很显然,因为段选择子是16位的,而且只有高13位被用作索引号来访问GDT或LDT,所以每个LDT能够容纳2的13次方,8192个描述符,每个描述符长度8字节,LDT长度最大为64KB
;
;在一个多任务的环境中,当任务切换发生时,必须保护旧任务的运行状态,或者说保护现场,保护的内容包括通用寄存器,段寄存器,栈指针寄存器ESP,指令指针寄存器EIP,状态寄存器EFLAGS等等
;否则等下次该任务又恢复执行时,一切都会变得茫然而毫无意义
;为了保存任务的状态,并在下次重新执行时恢复它们,每个任务都应当用一个额外的内存区域保存相关信息,这叫做任务状态段TSS(Task State Segment)
;如下图所示,任务状态段TSS具有固定的格式,最小尺寸是104字节,下图所标注的偏移量是十进制的,处理器固件能够识别TSS中的每个元素,并在任务切换的时候读取其中的信息,具体细节将在后面讲述
;
;   31                      15                      0
;    ------------------------------------------------
;   |     I/O映射基地址      |           保留     |  T  |100
;    ------------------------------------------------
;   |        保留           |       LDT段选择子        |96
;    ------------------------------------------------
;   |        保留           |           GS            |92
;    ------------------------------------------------
;   |        保留           |           FS            |88
;    ------------------------------------------------
;   |        保留           |           DS            |84
;    ------------------------------------------------
;   |        保留           |           SS            |80
;    ------------------------------------------------
;   |        保留           |           CS            |76
;    ------------------------------------------------
;   |        保留           |           ES            |72
;    ------------------------------------------------
;   |                     EDI                        |68
;    ------------------------------------------------
;   |                     ESI                        |64
;    ------------------------------------------------
;   |                     EBP                        |60
;    ------------------------------------------------
;   |                     ESP                        |56
;    ------------------------------------------------
;   |                     EBX                        |52
;    ------------------------------------------------
;   |                     EDX                        |48
;    ------------------------------------------------
;   |                     ECX                        |44
;    ------------------------------------------------
;   |                     EAX                        |40
;    ------------------------------------------------
;   |                    EFLAGS                      |36
;    ------------------------------------------------
;   |                     EIP                        |32
;    ------------------------------------------------
;   |                   CR3(PDBR)                    |28
;    ------------------------------------------------
;   |        保留          |           SS2            |24
;    ------------------------------------------------
;   |                    ESP2                        |20
;    ------------------------------------------------
;   |        保留          |           SS1            |16
;    ------------------------------------------------
;   |                    ESP1                        |12
;    ------------------------------------------------
;   |        保留          |           SS0            |8
;    ------------------------------------------------
;   |                    ESP0                        |4
;    ------------------------------------------------
;   |        保留            |   前一个任务的指针TSS    |0
;    ------------------------------------------------
;
;和LDT一样,处理器用TR寄存器来指向当前任务的TSS,和GDTR,LDTR一样,TR寄存器在处理器中也只有一个,当任务切换发生的时候,TR寄存器的内容也会跟着指向新任务的TSS
;这个处理过程是这样的:1.首先处理器将当前任务的现场信息保存到由TR寄存器指向的TSS
;                  2.然后再使TR寄存器指向新任务的TSS,并从新任务的TSS中恢复现场
;
;至于为什么这个寄存器叫TR,而不是TSSR,原因是TSS是一个任务存在的标志,用于区别一个任务和其他任务,所以这个寄存器叫做任务寄存器(Task Register)
;
;                       全局空间和局部空间
;现代的计算机,如果没有操作系统的支持,也可以在编程爱好者的操作下运行得很好,但没办法太通用
;再多任务系统中,操作系统肩负着任务的创建,以及在任务之间调度和切换的工作,不过更为繁重和基础的工作是对处理器,设备及存储器的管理
;从程序的编写者角度看,操作系统是他们可以信赖的朋友,首先他们不必关心自己的程序是如何加载到内存并开始运行的,操作系统自然会处理好这些事情
;其次对设备的访问涉及大量的硬件细节,而且极为繁琐,操作系统能够肩负起设备管理的职责,并提供大量的例程和数据供应用程序调用,使用操作系统提供的这些服务,大大简化程序的编写,并在访问设备时消除潜在的竞争和冲突
;比如说当中断发生时,不可能由某个任务来处理,而只能由操作系统来提供中断处理过程,并采取适当的操作,以进行一些和所有任务都有关系的全局性管理工作
;如空闲内存的查找和分配,回收已终止任务的内存空间,设备访问的排队和调度,等等
;这就是说,每个任务实际上包含两部分,全局部分和私有部分
;全局部分是所有任务共有的,含有操作系统的软件和库程序,以及可以调用的系统服务和数据
;私有部分是每个任务各自的数据和代码,与任务所要解决的具体问题有关
;
;         每个任务的全局空间和局部空间
;   ------------------------------------
;  |   任务的局部空间   |   任务的全局空间   |
;   ------------------------------------
;
;                                   多任务系统的全局空间和局部空间
;
;                                       ---------------
;                                      |      任务4     |
;                                      |     局部空间    |
;                                      |               |
;                       ---------------|------------------------------
;                      |               |               |              |
;                      |     任务1      |    全局空间    |     任务3     |
;                      |    局部空间    |               |     局部空间   |
;                      |               |               |              |
;                       ----------------------------------------------
;                                      |     任务2      |
;                                      |    局部空间     |
;                                      |               |
;                                       ---------------
;
;任务实际上是在内存中运行的,所以所谓的全局部分和私有部分,其实是地址空间的划分,即全局地址空间和局部地址空间,简称全局空间和局部空间
;地址空间的访问是依靠分段机制来进行的,具体地说需要先在描述符表中定义各个段的描述符,然后再通过描述符来访问它们
;因此全局地址空间是用全局描述符表GDT来指定,而局部地址空间是由每个任务私有的局部描述符表LDT来定义
;从程序员的角度看,任务的全局空间包含了操作系统的段,是由别人编写的,但是它可以调用这些段的代码,或者获取这些段中的数据
;任务局部空间的内容是由程序员自己创建的,通常任务会在自己的局部空间运行,当它需要操作系统提供的服务时,转入全局空间执行
;我们知道段寄存器CS,SS,DS,ES,FS,GS由16位的选择器和不可见的描述符高速缓存器组成,选择器的位2是表指示器TI,若TI=0,指向GDT,表示当前正在访问的段描述符位于GDT中,TI=1,指向LDT,表示当前正在访问的段描述符位于LDT中
;选择器的高13位指定描述符的索引号,从0开始
;每个段描述符都对应着一个内存段,很显然在一个任务的全局地址空间上,可以划分处2的13次方个段,也就是8192个,因为GDT的0号描述符不能使用,实际上是8191个段
;又因为段内偏移是32位的,段的长度最大的4GB,因此一个任务的全局地址空间,其总大小为2的13次方乘以2的32次方=32TB
;同样的道理,局部描述符表LDT可以定义2的13次方个,8192个描述符又因为段内偏移是32位的,段的长度最大4GB,因此一个任务的局部地址空间,其总大小为2的13次方乘以2的32次方=32TB
;这样每个任务的总地址空间为64TB,在一个只有32根地址线的处理器上,无论如何也不可能提供这么大的存储空间,但是这只是虚假的,或者说虚拟的地址空间
;操作系统允许程序的编写者使用该地址空间来写程序,即,使用虚拟地址或逻辑地址来访问内存,就像它真的拥有这么巨大的地址空间一样
;上面的话可以这样理解:
;   编译器不考虑处理器可寻址空间的大小,也不考虑物理内存的大小,它只是负责编译程序,当程序编译时,编译器允许生成非常巨大的程序
;但是当程序超出了物理内存的大小时,或者操作系统无法分配这么大的物理内存空间时,怎么办?
;同一块物理内存,可以让多个任务,或者每个任务的不同段来使用,当执行或访问一个新的段时,如果它不在物理内存中,而且也没有空闲的物理内存空间来加载它,那么操作系统将会挑出一个暂时用不到的段
;把它换出到磁盘中,并把那个腾出来的空间分配给马上要访问的段,并修改段的描述符,使之指向这段内存空间
;下一次当被换出的那个段马上又要用到时,再用相同的办法换回到物理内存,所有这一切,任务和程序的编写者是不用关心的,这就是虚拟内存管理的一般方法
;
;                       特权级保护概述
;引入LDT和TSS,只是从任务层面上进一步强化了分段机制,从安全保障的角度来看,只相当于构建了可靠的硬件设施,仅有设施是不够的,还需要规章制度,还要有人来执行,处理器也一样
;为此在分段机制的基础上,引入了特权级,并由固件负责实施特权级保护
;特权级也叫特权级被,是存在于描述符及其选择子中的一个数值,当这些描述符或者选择子所指向的对象要进行某种操作,或者被别的对象访问时,该数值用于控制它们所能进行的操作,或者限制它们的可访问性
;Intel处理器可以识别4个特权级别,从0到3,共4级环状保护结构,较大的数值意味着较低的特权级
;操作系统核心----特权级0
;系统服务程序----特权级1,2
;普通应用程序----特权级3
;因为操作系统是为所有的程序服务的,可靠性最高,而且必须对软硬件有完全的控制权,所以它的主体部分必须拥有特权级0,并处于整个环形结构的中心,正是因为这样,操作系统的主体部分通常称作内核
;特权级1和特权级2通常赋予那些可靠性不如内核的系统服务程序,比较典型的就是设备驱动程序,但在很多流行的操作系统中,驱动程序与内核特权级相同,都是0
;应用程序的可靠性被视为是最低的,而且通常不需要直接访问硬件和一些敏感的系统资源,调用设备驱动程序或者操作系统例程就能很好完成大多数工作,所以赋予最低特权级3
;实施特权级保护的第一步,是为所有可管理的对象赋予一个特权级,以决定谁能访问它们,回到第11章,每个描述符都有两比特的DPL字段,可以取值为00,01,10,11,分别对应0,1,2,3这4个特权级
;DPL是每个描述符都有的字段,故又称为描述符特权级(Descriptor Privilege Level),描述符总是指向它所描述的目标对象,代表着该对象,因此该字段实际上是目标对象的特权级
;比如对于数据段来说,DPL决定了访问它们所应当具备的最低特权级,如果有一个数据段,其描述符的DPL=2,那么只有特权级为0,1,2的程序才能访问它
;当特权级为3的程序也试图去读写该段时,将会被处理器阻止,并引发异常中断,对任何段的访问都要先把它的描述符加载到段寄存器,所以这种保护手段很容易实现
;我们知道,32位处理器的段寄存器,实际上由16位的段选择器和描述符高速缓存器组成,后者不能直接访问,正因为我们接触不到描述符高速缓存器,所以为了方便,以后我们提到段寄存器指的就是段选择器
;在实模式下,段寄存器存放的是段地址,在保护模式下,段寄存器存放的是段选择子,段地址则位于描述符高速缓存器中
;当处理器正在一个代码段中取指令和执行指令时,那个代码段的特权级叫做当前特权级CPL(Current Privilege Level),正在执行的这个代码段,其选择子位于段寄存器CS中,其最低两位就是当前特权级的数值
;一般来说,操作系统是最先从BIOS那里接收处理器控制权的,进入保护模式的工作也是由它做的,而且最重要的是操作系统还肩负着整个计算机系统的管理工作
;所以操作系统必须工作在0特权级上,当操作系统的代码正在执行时,当前特权级CPL就是0
;相反,普通的应用程序则工作在特权级别3上,没有人愿意将自己的程序放在特权级别3上,但是只要你在某个操作系统上写程序,就由不得你,应用程序编写时,不需要考虑GDT,LDT,分段,描述符这些东西
;它们是在程序加载时,由操作系统负责创建的,应用程序的编写者只负责具体功能就可以了,应用程序的加载和开始执行,也是由操作系统所主导的,而操作系统一定会将应用程序放在特权级3上
;当应用程序执行时,当前特权级自然就是3
;这实际上就是把一个任务分成特权级截然不同的两个部分,全局部分是特权级0,而局部空间则是特权级3的,这种划分是有好处的,全局空间是为所有任务服务的,其重要性不言而喻
;为了保证它的安全性,并能够访问所有软硬件资源,应该使它拥有最高的特权级别,当任务在字节的局部空间内执行时,当前特权级CPL是3
;当他通过调用系统服务,进入操作系统内核,在全局空间执行时,CPL就变成0,总之不能僵化地看待任务和任务的特权级
;不同特权级别的程序,所担负的职责以及在系统中扮演的角色是不同的,计算机系统的脆弱性在于一条指令就能改变它的整体运行状态,比如停机指令和对控制寄存器CR0的写操作
;像这样的指令只能由最高特权级的程序来做,因此那些只有在CPL=0时才能执行的指令称为特权指令,如lgdt(实模式下可执行),lldt,ltr,读写控制寄存器的mov,hlt等
;除了那些特权级敏感的指令外,处理器还允许对各个特权级别所能执行的I/O操作进行控制,通常这指的是端口访问的许可权,因为对设备的访问都是通过端口进行的,
;在处理器的标志寄存器EFLAGS中,位13,位12是IOPL位,也就是输入/输出特权级(I/O Privilege Level),它代表当前任务的输入/输出特权级,下图是EFLAGS的IOPL位说明
;
;   31                    21                            15      13  12  11   10   09   08   07   06   05  04   03  02   01   00
;   |     保留,设置为0    | ID |    |    |    |    |    | 0 |    | IOPL | OF | DF | IF | TF | SF | ZF | 0 | AF | 0 | PF | 1 | CF |
;
;任务是由操作系统加载和创建的,与任务相关的信息都在它字节的任务状态段TSS中,其中就包括一个EFLAGS寄存器的副本,用于指示与当前任务相关的机器状态,比如它自己的I/O特权级IOPL
;在多任务系统中,随着任务的切换,前一个任务的所有状态被保存到它自己的TSS中,新任务的各种状态从其TSS中恢复,包括EFLAGS寄存器的值
;处理器不限制特权级0的程序的I/O访问,总是允许,但是可以限制低特权级程序的I/O访问权限,这是很重要的,操作系统的功能之一是设备管理,它可能不希望应用程序拥有私自访问外设的能力
;代码段的特权级检查是很严格的,一般来说,控制转移只允许发生在两个特权级相同的代码段之间
;如果当前特权级=2,那么它可以转移到另一个DPL=2的代码段接着执行,但不允许转移到DPL为0,1,3的代码段执行,不过为了让特权级低的应用程序可以调用特权级高的操作系统例程,处理器也提供了相应的解决方法
;第一种方法是将高特权级的代码段定义为依从,回到第11章,段描述符如下
;
; 31            24 23  22  21   20  19         16 15 14 13 12 11     8 7            0 高32位
;|  段基地址31~24  | G |D/B| L | AVL | 段界限19~16 | P | DPL| S |  TYPE | 段基地址23~16 |
;
; 31                                           1615                                 0 低32位
;|                 段基地址15~0                   |             段界限15~0             |
;
;
;TYPE字段总共4位,用于指示描述符的子类型,或者说类别,对于数据段来说,这4位分别是X   E   W   A
;
;   X   E   W   A       描述符类别  表示的含义
;   0   0   0   x(叉)               只读,向上扩展
;   0   0   1   x         数据      读写,向上扩展
;   0   1   0   x                  只读,向下扩展
;   0   1   1   x                  读写,向下扩展
;
;对于代码段来说,这4位分别是X   C   R   A
;
;   X   C   R   A       描述符类别  表示的含义
;   1   0   0   x(叉)               只执行
;   1   0   1   x         代码      执行,读
;   1   1   0   x                   只执行,依从的代码段
;   1   1   1   x                   执行,读,依从的代码段
;
;代码段描述符的TYPE字段有C位,如果C=0,这样的代码段称为非依从的代码段,只能供同特权级的程序使用,如果C=1,这样的代码段称为依从的代码段,可以从特权级比它低的程序调用并进入
;但是即使是将控制转移到依从的代码段,也是有条件的,要求当前特权级CPL必须低于或者和目标代码段描述符的DPL相同,即,在数值上   CPL >= 目标代码段的描述符DPL,因为数值大的特权级低
;举例来说,如果一个依从的代码段,其描述符的DPL=1,那么只有特权级为1,2,3的程序可以调用,而特权级0的程序不能调用,在任何时候,都不允许将控制从较高的特权级转移到较低的特权级
;依从的代码段不是在它的DPL上执行,而是在调用程序的特权级上运行,这就是说当控制转移到依从的代码段上执行时,不改变当前特权级CPL,段寄存器CS的CPL字段不发生改变,被调用过程的特权级依从于调用者的特权级,这就是为什么它被称为"依从的"代码段
;
;除了依从的代码段,另一种在特权级之间转移控制的方法是使用门,门是另一种形式的描述符,称为门描述符,简称门,和段描述符不同,段描述符用于描述内存段
;门描述符用于描述可执行代码,比如一段程序,一个程序(例程)或者一个任务
;实际上根据不同的用途,门的类型有好几种,不同特权级之间的过程调用可以使用调用门,这里重点介绍调用门
;中断门/陷阱门是作为中断处理过程使用的
;任务门对应着单个的任务,用来执行任务切换
;
;所有描述符都是64位的,调用门描述符也不例外,在调用门描述符中,定义了目标过程(例程)所在代码段的选择子,以及段内偏移
;要想通过调用门进行转移控制,可以使用 jmp far 或  call far,并把调用门描述符的选择子作为操作数
;使用jmp far指令,可以将控制通过门转移到比当前特权级高的代码段,但不改变当前特权级别
;但是如果使用call far指令,则当前特权级会提升到目标代码段的特权级别
;也就是说处理器是在目标代码段的特权级上执行的,但是除了从高特权级别的例程(通常是操作系统例程)返回外,不允许从特权级高的代码段将控制转移到特权级低的代码段,因为操作系统不会引用可靠性比自己低的代码
;说了这么多好像是第一次接触特权级,事实上从第11章写第一个保护模式程序开始,就在创建DPL=0的描述符,就说上一章第13章,比较典型,既有内核程序,也有用户程序
;代码清单c13_mbr.asm第24~37行,创建了初始的几个段描述符,特权级都是0,最高级别
;
;以下建立保护模式下的描述符
;         ;跳过0#号描述符的槽位
;         ;创建1#描述符，这是一个数据段，对应0~4GB的线性地址空间
;         mov dword [ebx+0x08],0x0000ffff    ;基地址为0，段界限为0xF FFFF,特权级00
;         mov dword [ebx+0x0c],0x00cf9200    ;粒度为4KB，操作数32位,类型为数据段读写,存储器段描述符,段存在于内存中
;
;         ;创建保护模式下初始代码段描述符
;         mov dword [ebx+0x10],0x7c0001ff    ;基地址为0x0000 7c00，界限0x1FF,特权级00
;         mov dword [ebx+0x14],0x00409800    ;粒度为1个字节，操作数32位,类型为代码段只执行,代码段描述符,段存在于内存中
;
;         ;建立保护模式下的堆栈段描述符
;         mov dword [ebx+0x18],0x7c00fffe    ;基地址为0x0000 7C00，界限0xF FFFE,特权级00
;         mov dword [ebx+0x1c],0x00cf9600    ;粒度为4KB,操作数32位,类型为特殊数据段栈段向下扩展,段存在于内存中
;         
;         ;建立保护模式下的显示缓冲区描述符
;         mov dword [ebx+0x20],0x80007fff    ;基地址为0x000 B8000，界限0x0 7FFF,特权级00
;         mov dword [ebx+0x24],0x0040920b    ;粒度为字节,操作数32位,类型为数据段读写,段存在于内存中
;
;特权级保护机制只在保护模式下才能启用,而进入保护模式的方法是设置CR0寄存器的PE位,而且处理器建议在进入保护模式后,执行的第一条指令应当是跳转或过程调用指令,以清空流水线和乱序执行的结果,并串行化处理器,就像
;   jmp dword 0x0010:flush
;转移到的目标代码段是刚刚定义过的,描述符特权级等于0,要将控制转移到这样的代码段,当前特权级CPL必须为0,不过这并不是问题,在进入保护模式后,处理器自动将当前特权级CPL设定为0,以0特权级的身份开始执行保护模式的初始指令
;段选择子实际上由三部分组成:
;15                          3   2  1    0
;|          描述符索引          | TI | RPL |
;在以上指令中,段选择子0x0010的TI位是0,意味着目标代码段的描述符在GDT中,该选择子索引字段的值是2,指向GDT的2号描述符
;GDT中的1号描述符是保护模式下的初始代码段描述符,特权级DPL=0,而当前特权级CPL也是0,从初始的0特权级转移到另一个0特权级的代码段,这是允许的
;转移之后,jmp指令中的选择子0x0010被加载到CS,其低两位采用目标代码段描述符的DPL的值,也就是说控制转移之后,当前特权级CPL还是0
;
;这里遗漏了一样东西,尽管它对于处理器的特权级检查来说很重要,但更多时候是个累赘,那就是选择子中的RPL字段
;RPL的意思是请求特权级,我们知道要将控制从一个代码段转移到另一个代码段,通常是使用jmp和call指令,并在指令中提供目标代码段的选择子,以及段内偏移量
;而为了访问内存中的数据,也必须先将段选择子加载到段寄存器DS,ES,FS,GS中,不管是实施控制转移,还是访问数据段,这都可以看成是一个请求,请求者提供一个段选择子,请求访问指定的段
;从这个意义上来说,RPL也就是指请求者的特权级别(Requestor's Privilege Level)
;
;在绝大多数情况下,请求者都是当前程序自己,因此CPL=RPL,要判断请求者是谁,最简单的方法就是看谁提供了选择子,以下是两个典型的例子:
;代码清单c13_mbr.asm第55行
;   jmp dword 0x0010:flush  ;这里提供选择子的是当前程序自己
;
;再比如第59~60行
;         mov eax,0x0008                     ;加载数据段(0..4GB)选择子,0x08=0000 1 000,选择1号描述符
;         mov ds,eax
;同样是当前程序自己拿着段选择子0x0008来"请求"代入DS,以便在随后的指令中访问该段中的数据
;但是在一些并不多见的情况下,RPL和CPL并不相同,如下图所示,特权级3的应用程序希望从硬盘读一个扇区,并传送到自己的数据段,因此数据段描述符的DPL同样会是3
;
;
;           ---------------------------------------------------------------------------------------------------
;
;             ---------------                                                           ----------------
;特权级3      |应用程序,特权级=3|-----+                                              +---->|  数据段,DPL=3  |
;             ---------------      |                                              |     ----------------   
;                                  |                                              |
;           -----------------------|----------------------------------------------|----------------------------
;                                  |                                              |
;特权级2                            |                                              |
;                                  |                                              |
;           -----------------------|----------------------------------------------|----------------------------
;                            (数据段|选择子,RPL=3)                                  |
;特权级1                            |                                              |
;                                  |                                              |
;           -----------------------|----------------------------------------------|----------------------------
;                                  |                                              |
;                                  |     ------------        --------------       |
;特权级0                            +---> |   调用门   |------>|   内核例程   |------+
;                                        ------------        --------------
;           ----------------------------------------------------------------------------------------------------
;
;由于I/O特权级的限制,应用程序无法自己访问硬盘,好在位于0特权级的操作系统提供了相应的例程,但必须通过调用门才能使用,因为特权级间的控制转移必须要通过门
;假设通过调用门使用操作系统例程时,必须传入3个参数,分别是CX中的数据段选择子,EBX寄存器中的段内偏移,以及EAX中的逻辑扇区号
;高特权级别的程序可以访问低特权级别的数据段,这是没问题的,因此操作系统例程会用传入的数据段选择子代入段寄存器,以便代替应用程序访问那个段: mov ds,cx
;在执行这条指令时,CX寄存器中的段选择子,其RPL=3,当前特权级CPL已经变成0,因为通过调用门实施转移控制可以改变当前特权级,显然请求者并非当前程序,而是特权级为3的应用程序,RPL和CPL并不相等
;不过上面的例子只是表明RPL有可能和CPL不同,但并没有说明引入RPL有什么必要性,似乎是多余的,没有它程序也能正常工作不是吗?如果是这样想的,再看下面的例子
;
;想象以下,应用程序的编写者通过钻研,知道了操作系统数据段的选择子,而且希望用这个选择子访问操作系统的数据段,当然,他不可能在应用程序里访问操作系统数据段(意思是直接在应用程序里将操作系统数据段的选择子加载到ds)
;因为那个数据段的DPL=0,而应用程序工作时特权级为3,处理器会阻止
;但是他可以借助调用门,调用门工作在目标代码段的特权级上,一旦处理器的执行流离开应用程序,通过调用门进入操作系统例程时,当前特权级CPL从3变为0
;当那个不怀好意的程序将一个指向操作系统数据段的选择子通过CX作为参数传入调用门时,因为当前特权级CPL已经从3变为0,可以从硬盘读出数据,就得逞了
;
;                                       在特权级检查中引入RPL的必要性
;           ---------------------------------------------------------------------------------------------------
;
;             ---------------                                                           
;特权级3      |应用程序,特权级=3|-----+______________________________
;             ---------------      |                              \                      
;                                  |                               \这条线的意思是直接在应用程序里加载操作系统数据段选择子,进而访问操作系统数据段
;           -----------------------|--------------------------------\------------------------------------------
;                                  |                                 \                   
;特权级2                            |                                  \                   
;                                  |                                   \                
;           -----------------------|------------------------------------\--------------------------------------
;                            (数据段|选择子,RPL=0)                         \                 
;特权级1                            |                                      \             
;                                  |                                       \(X号,会被处理器阻止)      
;           -----------------------|----------------------------------------\----------------------------------
;                                  |                                         \___+     
;                                  |     -----------         --------------     \|/ ----------------------
;特权级0                            +---> |   调用门   |------>|   内核例程   |------>|     数据段,DPL=0      |
;                                        -----------         --------------        -----------------------
;           ---------------------------------------------------------------------------------------------------
;           通过调用门请求内核例程读写操作系统的数据段,这样就得逞了,所以要引入RPL,而RPL由操作系统发放,操作系统只给应用程序RPL=3的选择子
;           按照Intel公司的说法,引入RPL的意图是"确保特权代码不会代替应用程序访问一个段,除非应用程序自己拥有访问那个段的权限"
;
;处理器的智商很低,它不可能知道谁是真正的请求者,因此当前指令:    mov ds,ax   或  mov ds,cx   执行时,AX或CX中的选择子可能是操作系统自己提供的,也可能来自恶意的应用程序
;这两种情况要区别对待,但是已经超出处理器的能力和职权范围了,怎么办?
;看的出来,单纯依靠处理器硬件无法解决这个难题,但它可以再原来基础上多增加一种检查机制,并把如何能够通过这种检查的自由裁量权交给软件(编写者)
;引入请求特权级RPL的原因是处理器在遇到一条将选择子传送到段寄存器的指令时,无法区分真正的请求者是谁,但是引入RPL本身并不能完全解决这个问题,这只是处理器和操作系统之间的一种协议
;处理器负责检查RPL,判断它是否有权访问,但前提是提供了正确的RPL
;内核或操作系统负责鉴别请求者的身份,并有义务保证RPL的值和它的请求者身份相符,因为这是处理器无能为力的
;因此在引入RPL这件事上,处理器的潜台词是,仅依靠现有的CPL和DPL无法解决由请求者不同而带来的安全隐患,好吧那么再增加一道门卫,但前提是操作系统只将通行证发放给正确的人
;操作系统的编写者很清楚段选择子的来源,即,真正的请求者是谁,当他自己读写一个段时这没什么好说的
;当它提供一个服务例程时,3特权级的应用程序给出的选择子在哪里,也是由它定的,它也知道,在这种情况下,它所要做的,就是将该选择子的RPL字段设置为请求者的特权级(可以使用arpl指令)
;剩下的工作就看处理器了,每当处理器执行一个将段选择子传送到段寄存器的指令,比如   mov ds,cx   时,会检查以下两个条件是否都满足
;   1.当前特权级CPL高于或者或者和数据段描述符的DPL相同,即,在数值上  CPL <= 数据段描述符的DPL
;   2.请求特权级RPL高于或者或者和数据段描述符的DPL相同,即,在数值上  RPL <= 数据段描述符的DPL
;如果以上两个条件不能同时成立,处理器就会阻止这种操作引发异常中断
;按照Intel公司的说法,引入RPL的意图是"确保特权代码不会代替应用程序访问一个段,除非应用程序自己拥有访问那个段的权限"
;多数读者都只在字面上理解这句话的意思,而没有意识到,这句话只是如实地描述了处理器自己的工作,并没有保证它可以鉴别RPL的有效性
;最后总结一下特权级检查规则:
;首先,将控制直接转移到非依从的代码段,要求当前特权级CPL和请求特权级RPL都等于目标代码段描述符的DPL,即,在数值上
;       CPL = 目标代码段描述符的DPL
;       RPL = 目标代码段描述符的DPL
;一个典型例子就是使用jmp指令进行控制转移:   jmp 0x0012:0x0000 2000
;因为两个代码段的特权级相同,故转移后当前特权级不变
;其次,要将控制直接转移到依从的代码段,要求当前特权级CPL和请求特权级RPL都低于,或者和目标代码段描述符的DPL相同,即,在数值上
;       CPL >= 目标代码段描述符的DPL
;       RPL >= 目标代码段描述符的DPL
;控制转以后,当前特权级不变
;通过门实施的转移控制,其特权级检查规则将在后面的章节解释
;第三,高特权级别的程序可以访问低特权级别的数据,但低特权级别的程序不讷讷个访问高特权级别的数据段,访问数据之前,肯定要对DS,ES,FS,GS进行修改,比如:  mov fs,ax
;在这个时候,要求当前特权级CPL和请求特权级RPL都必须高于,或者和目标数据段描述符的DPL相同,即,在数值上
;       CPL <= 目标数据段描述符的DPL
;       RPL <= 目标数据段描述符的DPL
;最后处理器要求在任何时候,栈段的特权级别必须和当前特权级CPL相同,因此随着程序的执行,要对段寄存器SS的内容进行修改时,必须进行特权级检查,以下就是一个例子:  mov ss,ax
;在对段寄存器SS进行修改时,要求当前特权级CPL和请求特权级RPL必须等于目标栈段描述符的DPL,即,在数值上
;       CPL = 目标栈段描述符的DPL
;       RPL = 目标栈段描述符的DPL
;
;0特权级是最高特权级,当一个系统的各个部分都位于0特权级时,各种特权级检查总能够获得通过,就像这种检查并不存在一样,所以处理器的设计者建议,如果不需要使用特权机制,可以将所有程序的特权级别都设置为0
;小结:
;   1.程序员写程序时不需要指定特权级,当程序运行时,操作系统将程序创建为任务局部空间的内容,并赋予较低特权级别,比如3,操作系统对应着任务全局空间的内容,如果有多个任务,则操作系统属于所有任务的公共部分
;   2.当程序运行在局部空间时,可以在各个段之间转移控制,并访问私有数据,因为它们具由相同的特权级别,但不允许直接将控制转移到高特权级别的全局空间的段,除非通过调用门,或者目标段是依从的代码段
;   3.当通过调用门进入全局空间中执行时,操作系统可以在全局空间内的各个段之间转移控制并访问数据,因为它们具有相同的特权级,同时操作系统还可以访问任务局部空间的数据
;     即,低特权级别的数据段,但除了调用门返回外,不允许将控制权转移到低特权级别的局部空间内的代码段
;   4.任何时候,当前栈的特权级必须和CPL一样,进入不同特权级的段执行时,要切换栈,这是以后要将的内容
;
;                               内核程序的初始化
;本章没有提供主引导程序,继续使用上一章的主引导程序,毕竟主引导程序只是用来加载内核程序,并执行前期的内核初始化工作,主引导程序工作在0特权级
;现在分析这一章的内核,该内核是上一章内核程序的修改版本,使用了任务,LDT,TSS和特权级等最新的处理器特性和工作机制,代码清单中,一开始的常数定义以及程序头部的格式和前一章完全相同
;作为主引导程序和内核程序的协议部分,它们总应该是稳定不变的,文件起始部分的常数定义了内核所有段的选择子,显然这些选择子的RPL都是0,内核请求访问自己的段,请求特权级应当为0
;内核的入口点在第775行,在执行到这里的时候,主引导程序已经加载了内核,并对它进行了前期的初始化工作
;因为加载的是内核程序,而内核应当工作在0特权级,所以主引导程序在初始化内核时所创建的描述符,其目标特权级DPL都为0,如下图所示,这些描述符都是在GDT中创建的,内核加载完成之后的GDT布局
;
;             表内偏移                                           描述符索引
;                     -----------------------------------------
;                    |       核心代码段(位置和长度不定,DPL=0)      |   0x38
;                +38  -----------------------------------------
;                    |       核心数据段(位置和长度不定,DPL=0)      |   0x30
;                +30  -----------------------------------------
;                    |   公用例程段(0004 0000~长度不定,DPL=0)     |   0x28
;                +28  -----------------------------------------
;                    |  文本模式显存(000B 8000~000B FFFF,DPL=0)  |   0x20
;                +20  -----------------------------------------
;                    |   初始栈段(0000 6C00~0000 7C00,DPL=0)    |   0x18
;                +18  -----------------------------------------
;                    |   初始代码段(0000 7C00~0000 7DFF,DPL=0)  |    0x10
;                +10 ------------------------------------------
;                    | 0~4GB数据段(0000 0000~FFFF FFFF,DPL=0)   |   0x08
;                +08  -----------------------------------------
;                    |                空描述符                  |   0x00
;                +00  -----------------------------------------
;
;这些描述符所指向的段,有的是代码段,有的是数据段,如果是数据段,则只有内核自己才能访问,因为其描述符的DPL=0,低特权级别的程序访问这些段时会被阻止
;如果是代码段,则通常只有0特权级的程序才能将控制转移到该段,也就是说,只能从内核其他正在执行的部分转移到该段执行,因为它们的特权级相同
;第779~809行,用于显示初始化信息,包括一个欢迎信息和一个处理器品牌信息
;
;                       调用门
;在上一章里,内核的主要功能是加载和重定位用户程序,并将处理器控制权移交过去,用户程序执行完毕后,还要重新回收控制,现在我们已经知道,上一章里,内核赋予了用户程序特权级0,所以用户程序是在特权级0上运行的
;也正是因为如此,用户程序通过U-SALT表中的符号地址直接调用内核例程时,才会通过特权级检查,在本章里,内核也做同样的工作
;不同之处在于它将用户程序的特权级定为3,最低特权级,没有人愿意将自己的程序放在特权级3上,但系统核心一定会将它放在特权级3上
;尽管保护模式非常复杂,但这并没有加重用户程序编写者的负担,因为不必考虑很多底层的东西,这也是为什么本章没有提供用户程序的原因,将会继续沿用上一章的用户程序,只不过要作为一个任务进行加载
;加载的方法和上一章是不一样的,而且运行时的特权级是3,不再是上一章的0
;为了方便用户程序的编写,内核通常要提供大量的例程供它们调用,例如在上一章,用户程序可以调用内核例程@PrintString和@ReadDiskData,为此用户程序需要定义SALT表,并在表中填写例程的符号名
;之后再由内核将符号名转换成地址入口,也就是该例程所对应的段选择子和段内偏移量
;例程是由内核提供的,它们的特权级通常就是内核的特权级,在上一章里,内核程序和用户程序都运行在0特权级,而且都是普通的段间控制转移,所以在用户程序内字节调用内核例程不会有问题
;但是考虑以下,在本章中,用户程序运行时特权级将会是3,由于处理器禁止将控制从低特权级别的程序转移到高特权级别的程序,因此如果还像以前那样直接调用内核例程,就不会成功,会引发异常中断
;但是现实的需求也不能不考虑,任何操作系统都应当提供大量的功能调用服务,为此需要安装调用门
;
;调用门用于在不同特权级的程序之间进行控制转移,调用门本质上只是一个描述符,一个不同于代码段和数据段的描述符,可以安装在GDT或LDT中,调用门描述符格式如下
;
;
; 31                             16 15 14 13 12 11      8 7      5         0 高32位
;|          段内偏移31~16           | P | DPL| 0 |   TYPE  | 0 0 0 | 参数个数 |
;|                                              | 1 1 0 0 |                |
;
; 31                             1615                                      0 低32位
;|         例程所在代码段的选择子     |              段内偏移量15~0             |
;
;调用门描述符给出了例程所在代码段的选择子,而不是32位线性地址,有了段选择子,就能访问描述符表得到代码段的基地址,无非是间接了一点,但却可以在通过调用门进行控制转移时,实施代码段描述符有效性,段界限和特权级检查
;例程在代码段中的偏移量也是在描述符中直接指定的,只是被分成了两个16位部分,很显然,在通过调用门调用例程时,不使用指令中给出的偏移量
;描述符中的TYPE用来标识门的类型,共4比特,值1100表示调用门
;(关于处理器是如何分辨是普通的代码段描述符还是门描述符,S位=1表明是普通的描述符,S=0表明是系统段,系统段包含了各种门,详情看赵炯的<<linux内核源码完全剖析>>关于保护那一章节)
;描述符的P位是有效位,通常应该是1,当它为0时,调用这样的门会引发处理器异常中断,对于操作系统来说,这个机关可能会很有用,比如为了统计调用门的使用频率,可以将它置0,然后每当因调用而发生异常中断时
;在中断处理程序中将该门的调用次数加一,同时把P位置1,对于因P位为0而引发的中断来说,它们属于故障中断,从中断过程返回时,处理器还会重新执行引起故障的指令,此时因为P位已经置1,所以可以执行
;就当前例子而言,因为在提供调用门服务的同时,还要统计门的调用次数,故,可以在该调用门所对应的例程中将P位清零,这样下次该门被调用时,又会重复上述过程
;
;通过调用门实施特权级之间的控制转移时,可以使用jmp far指令,也可以使用call far指令,如果是后者会改变当前特权级CPL
;因为栈段的特权级必须同当前特权级保持一致,因此还要切换栈,即,从低特权级的栈切换到高特权级的栈
;比如一个特权级3的应用程序必须使用自己的3特权级栈工作,当它通过调用门进入0特权级的代码段执行时,当前特权级由3变为0,此时栈也要跟着切换,从3特权级的栈切换到0特权级的栈
;这主要是为了防止因栈空间不足产生不可预料的问题,同时也是为了防止栈数据的交叉引用
;为了切换栈,每个人任务除了自己固有的栈之外,还必须额外定义几套栈,具体数量取决于任务的特权级别
;0特权级任务不需要额外的栈,它自己固有的栈就足够使用,因为除了调用返回外,不可能将控制转移到低特权级的段
;1特权级的任务需要额外定义一个描述符特权级DPL=0的栈,以便将控制转移到0特权级时使用
;2特权级的任务则需要额外定义两个栈,描述符的DPL分别是0和1,在控制转移到0特权级和1特权级时使用
;3特权级的任务最多额外定义三个栈,描述符的DPL分别是0,1,2,在控制转移到0,1,2特权级时使用
;
;这些额外的栈也会由操作系统加载程序时自动创建,本章的源代码就演示了这一过程,这些额外创建的栈,其描述符位于任务自己的LDT中,同时还要在任务的TSS中登记,原因是栈切换是由处理器固件自动完成的,处理器需要根据TSS中的信息来完成这一过程
;在TSS内,从偏移4~24处登记有特权级0到2的栈段选择子,以及相应的ESP初始值,任务自己固有的栈信息则位于偏移量为56(ESP)和80(SS)的地方
;
;任务寄存器TR总是指向当前任务的任务状态段TSS,其内容为该TSS的基地址和界限,在切换栈时,处理器可以使用TR找到当前任务的TSS,并从TSS获得新栈的信息
;通过调用门使用高特权级的例程服务时,调用者会传递一些参数给例程,如果是通用寄存器传送,这没什么好说的,不过,要传递的参数很多时,更经常的做法是使用栈,调用者把参数压入栈,例程从栈中取出参数,这是高级语言的一贯做法
;例程需要什么参数,先压入哪个参数,后压入哪个参数,这是调用者和例程之间的约定,调用者是清楚的,否则它不会调用例程
;但是这一切对于处理器来说是懵懂的,特别是当栈切换时,参数还在旧栈中,为了使例程能够获得参数,必须将参数从旧栈复制到新栈中
;参数的复制工作是由处理器固件完成的,但它必须事先知道参数的个数,并根据该数量决定复制多少内容,所以调用门描述符中还有一个参数个数字段,共5比特,也就是说,至多允许传送31个参数
;栈切换前,SS指向的是旧栈,ESP指向旧栈的栈顶,即最后一个被压入的过程参数,栈切换后,处理器自动替换SS和ESP的内容,使他们分别为新栈的选择子和新栈的栈顶(最后一个被复制的参数)
;这一切对程序的编写者来说是透明的,所谓透明是说程序员不必关心栈的切换和参数的复制,他即使不知道还有栈切换这回事,也不影响程序编写工作,因为在栈切换前   pop edx可以得到最后一个被压入的参数
;在栈切换后,这条指令同样可以得到那个参数,尽管栈段和栈顶指针已经改变
;
;调用门描述符中的DPL和目标代码段描述符的DPL用于决定哪些特权级的程序可以访问此门,具体的规则是必须同时符合以下两个条件:
;   1.当前特权级CPL和请求特权级RPL高于或者和调用门描述符特权级DPL相同,即,在数值上
;       CPL <= 调用门描述符的DPL
;       RPL <= 调用门描述符的DPL
;2.当前特权级CPL低于或者和目标代码段描述符特权级DPL相同,即,在数值上
;       CPL >=目标代码段描述符的DPL
;
;举个例子,如果调用门描述符的DPL为2,那么只有特权级为0,1,2的程序才允许使用该调用门,特权级3的程序调用该门将引发异常中断
;如下图所示,调用门的DPL是特权级检查的下限,除此之外,目标代码段的特权级也是需要考虑的因素,调用门描述符中有目标代码段的选择子,它指向目标代码段的描述符
;当一个程序通过调用门转移控制时,处理器还要检查目标代码段描述符的DPL,该DPL决定了调用门特权级检查的上限,也就是说只有那些特权级低于或者等于目标代码段DPL的程序才允许使用此门
;调用门描述符中的一些字段没有使用,固定为0
;                                                       调用门的基本特权级检查规则
;                               __________________________________________________________________________
;                               |                                                                        |\
;                               |                                                                        |\\
;                               |    ________________________________________________________________    |\\\
;                               |   |\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|   |\\\|
;                               |   |\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\|   |\\\|
;                               |   |//|                                                             |   |\\\|
;                               |   |//|                目标代码段____描述符的DPL                       |   |\\\|
;                               |   |//|                         /|\                                 |   |\\\|
;                               |   |//|                          |                                  |   |\\\|
;                               |   |//|                  CPL     |                                  |   |\\\|
;                               |   |//|                          |                                  |   |\\\|              ----->
;                               |   |//|                          |                __________________|   |\\\|_____________/
;                               |   |//|                          |              _/                  |   |\\\|
;                               |   |//|                          |             /                    |   |\\\|
;                               |   |//|                          |            /                     |   |\\\|
;                               |   |//|                          |           /                      |   |\\\|
;                               |   |//|                          |          /                       |   |\\\|
;                               |   |//|                          |         /                        |   |\\\|
;                               |   |//|                          |        /                         |   |\\\|
;                               |   |//|                          |       /                          |   |\\\|
;                               |   |//|                          |      /                           |   |\\\|
;                               |   |//|                          |     /                            |   |\\\|
;                               |   |//|                          |    /                             |   |\\\|
;                               |   |//|                          |___/                              |   |\\\|
;                               |   |//|                         /|                                  |   |\\\|
;                               |   |//|                        / |                                  |   |\\\|
;           调用者的CPL和RPL_____________________________________/  |                                  |   |\\\|
;                               |   |//|                          |                                  |   |\\\|
;                               |   |//|               CPL,RPL    |                                  |   |\\\|
;                               |   |//|                          |                                  |   |\\\|
;                               |   |//|                          |                                  |   |\\\|
;                               |   |//|                调用门____\|/___描述符的DPL                    |   |\\\|
;                               |   |//|_____________________________________________________________|   |\\\|
;                               |____________________________________________________________________________|
;
;                       调用门的安装与测试
;第812~826行用于安装调用门的描述符,起始也就是等于在安装调用门
;安装的调用门供其他特权级的程序使用,它们在本质上是一些例程,这些例程在上一章里使用过,在上一章里,所有对外公开的例程都是以字符串的形式定义在SALT表中,该表位于内核数据段
;内核数据段中的SALT表简称C-SALT,位与c14_core.asm的第364~383行,属于内核数据段,该表由多个条目组成,每个条目262字节,前256字节是例程的名字
;后6字节是例程的地址,其中前4字节是例程在目标代码段内的偏移量,后2字节是例程所在代码段的选择子
;所有例程都位于公共例程段中,而公共例程段的DPL=0,为了使其他特权级的程序程序也能使用这些例程,必须将C-SALT表中的例程地址转换成调用门
;转换过程使用了循环,转换时需要定为每一个条目,故,第812行用于将C-SALT表的起始偏移地址传送到EDI,这是第一个条目的位置,以后每次加上262,就能对准下一个条目
;循环次数是由条目数量控制的,条目数是常数salt_items,位于第386行,第813行的指令用与将它作为立即数传送到ECX
;循环的结构是这样的:
;.b3:
;         push ecx
;                           实线围起来的是具体执行转换的指令
;___________________________________________________________________________________
;         mov eax,[edi+256]                  ;该条目入口点的32位偏移地址 
;         mov bx,[edi+260]                   ;该条目入口点的段选择子 
;         mov cx,1_11_0_1100_000_00000B      ;特权级3的调用门(3以上的特权级才
;                                            ;允许访问)，0个参数(因为用寄存器
;                                            ;传递参数，而没有用栈) 
;         call sys_routine_seg_sel:make_gate_descriptor
;         call sys_routine_seg_sel:set_up_gdt_descriptor
;         mov [edi+260],cx                   ;将返回的门描述符选择子回填
;____________________________________________________________________________________
;
;         add edi,salt_item_len              ;指向下一个C-SALT条目 
;         pop ecx
;         loop .b3
;
;因为在转换过程中要用到ECX,所以在每次循环开始前先压栈保存,然后在loop指令执行前恢复
;在循环体内,第816行,用于将每个条目(例程)的32位段内偏移地址传送到EAX,每个条目长度是262字节,而它的偏移地址则位于256字节处
;第817行用于获取条目(例程)所在代码段的选择子,它位于条目内第260字节处
;创建调用门描述符的工作实际上是调用过程make_gate_descriptor来完成,该过程位于第331行,属于公共例程段,调用该过程时
;需要传入3个参数,分别是EAX中的32位偏移地址,BX中的代码段选择子以及CX中的门属性
;调用门的属性字段是2字节的长度,通过CX寄存器传入门属性时,必须保证各属性位都在原始位置,在我们的代码中,每次传入CX的值是
;   818     mov cx,1_11_0_1100_000_00000B   ;特权级3的调用门(3以上的特权级才允许访问)，0个参数(因为用寄存器传递参数，而没有用栈)
;
;很显然,P=1,DPL=3,即,只有特权级高于等于3的代码段才能调用此门,参数的数量为0,也就是不需要通过栈传递参数
;
;例程make_gate_descriptor都做了些什么?
;第340~342行,先在EDX中得到32位偏移地址的复制品,然后将低16位清除,只留下32位偏移地址的高16位部分,并同CX中的属性值一起,形成调用门描述符的高32位
;第344~346行,将EAX的高16位清除,只留下32位偏移地址的低16位,接着将EBX逻辑左移16次,使得段选择子位于它的高16位,最后用or指令将这两个寄存器合并,就得到了调用门描述符的低32位
;第351行,retf指令使得控制返回调用者,注意,从这条指令可以看出,该过程必须以远调用的方式使用
;
;回到内核代码段
;第821~822行,在调用了例程make_gate_descriptor后,立即调用了另一个例程set_up_gdt_descriptor来安装刚才创建的调用门描述符
;在GDT中安装描述符的过程和前一章相同,显然,调用门描述符是在GDT中创建的,并用CX返回该描述符的选择子,即调用门选择子
;第823行,将返回的调用门选择子回填到条目内,用以覆盖原先的代码段选择子
;取决于C-SALT表的大小,循环过程会进行多次,在本章中,C-SALT表中共有4个条目,这4个调用门安装之后,GDT的布局如下图所示
;
;             表内偏移                                           描述符索引
;                     -----------------------------------------
;                    |     调用门(@TerminateProgram,DPL=3)      |   0x58
;                +58  -----------------------------------------
;                    |  调用门(@PrintDwordAsHexString,DPL=3)    |   0x50
;                +50  -----------------------------------------
;                    |       调用门(@ReadDiskData,DPL=3)        |   0x48
;                +48  -----------------------------------------
;                    |        调用门(@PrintString,DPL=3)        |   0x40
;                +40  -----------------------------------------
;                    |       核心代码段(位置和长度不定,DPL=0)      |   0x38
;                +38  -----------------------------------------
;                    |       核心数据段(位置和长度不定,DPL=0)      |   0x30
;                +30  -----------------------------------------
;                    |   公用例程段(0004 0000~长度不定,DPL=0)     |   0x28
;                +28  -----------------------------------------
;                    |  文本模式显存(000B 8000~000B FFFF,DPL=0)  |   0x20
;                +20  -----------------------------------------
;                    |   初始栈段(0000 6C00~0000 7C00,DPL=0)    |   0x18
;                +18  -----------------------------------------
;                    |   初始代码段(0000 7C00~0000 7DFF,DPL=0)  |    0x10
;                +10 ------------------------------------------
;                    | 0~4GB数据段(0000 0000~FFFF FFFF,DPL=0)   |   0x08
;                +08  -----------------------------------------
;                    |                空描述符                  |   0x00
;                +00  -----------------------------------------
;
;第829~830行对刚安装好的调用门进行测试,看看好不好用,测试的结果是在屏幕上显示一行文字,意思为"系统范围内的调用门已经安装"
;标号salt_1指向C-SALT表中的第一个条目的起始处,再此基础上增加256,就是它的地址部分,现在我们已经知道该条目对应着公共例程段中的put_string过程,用于显示零终止的字符串
;表面上这是一条普通的间接绝对远调用指令call far,通过指令中给出的地址操作数,可以间接取得32位的偏移地址和16位的代码段选择子
;但是处理器在执行这条指令时,会用该选择子访问GDT/LDT,检查那个选择子,看它指向的是调用门描述符,还是普通的代码段描述符,如果是前者,就按调用门来处理,如果是后者,还按一般的段间控制转移处理
;(杨俊艺:关于处理器是如何检查选择子,区分是调用门还是普通的代码段描述符,原因是这两种描述符都有S位,S=1表明是普通的代码段描述符,S=0表明是系统段,在系统段这个分类下包含各种门,然后处理器再根据TYPE字段判断是哪一种门,详情在赵炯那本<<linux内核完全注释>>关于保护那一章节)
;在这里因为salt_1条目的选择子已经被替换成调用门选择子,所以处理器按调用门的方式来执行控制转移,通过调用门实施控制转移时,处理器只使用选择子部分,salt_1条目中给出的32位偏移量部分被丢弃
;原因很简单,通过调用门进行控制转移不需要偏移量,偏移量已经在调用门描述符中给出了
;不单单是间接绝对远调用,直接绝对远调用也是这样,如果选择子指向的是调用门,偏移量也会被忽略,例如:  call 0x0040:0x0000 c000
;在这个例子中,因为是通过调用门实施控制转移,处理器将忽略偏移量0x0000 c000
;借助调用门,当程序的执行流从低特权级的代码段转入高特权级的代码段时,如果那个是非依从的代码段,当前特权级CPL也随之变为目标代码段的特权级
;不过,如果调用者和被调用者的特权级相同,则特权级不会变化,在当前例子中,是从内核代码段调用公共例程段的例程,尽管也是通过调用门,但它们的特权级都是0
;所以,在控制转移的过程中不会发生栈切换,仅仅是把返回地址CS和EIP压入当前栈,当执行retf指令后,处理器从栈中恢复CS和EIP的原始内容,于是又返回到原先的代码段接着执行
;事实上,能够通过调用门发起控制转移的指令还包括jmp,但只用在不需要从调用门返回的场合下,而且不改变当前特权级,也就是说目标代码是在当前特权级上执行
;通过调用门进行控制转移的特权级检查,既要在转移前进行,而且还要在控制返回时进行,完整的特权级检查将会在后面进行说明
;
;                               加载用户程序并创建任务
;                       任务控制块和TCB链
;继续讲解代码清单c14_core.asm
;第832~833行是以传统的方式调用内核例程显示字符串,即使不通过调用门,特权检查也是照常进行,而且更为严格,把控制从较低的特权级转移到较高的特权级,通过调用么尚有可能
;但直接控制转移则在任何时候都是不允许的,当然,这里是从0特权级的内核代码进入同样是0特权级的公共例程段,能够通过特权级检查
;在内核初始化完成后,和第13章不一样,接下来的工作就是加载和重定位用户程序,并移交控制权
;按处理器的要求标准,要使一个程序成为"任务",并且能够参与任务切换和调度,那不是简简单单就能行的,必须要有LDT和TSS
;而为了创建这两样东西,又需要更多的东西,所以加载和执行用户程序的活,比起从前是麻烦了不少
;
;加载程序并创建一个任务,需要用到很多数据,比如程序的大小,加载的位置等等,当任务执行结束后,还要依据这些信息来回收它所占用的内存空间(在本书没有实现,但一个合格的操作系统必须实现该功能)
;还有,多任务系统是多个任务同时运行的,特别是在一个单处理器(核)的系统中,为了在任务之间切换和轮转,必须能追踪到所有正在运行的任务,记录它们的状态,或者根据它们的当前状态来采取适当的操作(在第16章将学习任务的切换和轮转技术)
;为了满足上述需求,内核应当为每一个任务创建一个内存区域,来记录任务的信息和状态,称为任务控制块TCB(Task Control Block),任务控制块不是处理器的要求,而是我们为了方便而发明的
;下图是任务控制块TCB的结构,不用纠结于表中的内容和细节,有个大概印象即可
;
;
;                 15                                            0
;        +0x46 --> ---------------------------------------------
;                 |                 头部选择子                   |
;        +0x44 --> ---------------------------------------------
;                 |                 2特权级栈的                  |
;                 |                  初始ESP                    |
;        +0x40 --> ---------------------------------------------
;                 |                2特权级栈选择子                |
;        +0x3E --> ---------------------------------------------
;                 |                 2特权级栈的                  |
;                 |                   基地址                    |
;        +0x3A --> ---------------------------------------------
;                 |                  2特权级栈                   |
;                 |              以4KB为单位的长度                |
;        +0x36 --> ---------------------------------------------
;                 |                 1特权级栈的                  |
;                 |                  初始ESP                    |
;        +0x32 --> ---------------------------------------------
;                 |                1特权级栈选择子                |
;        +0x30 --> ---------------------------------------------
;                 |                 1特权级栈的                  |
;                 |                   基地址                    |
;        +0x2C --> ---------------------------------------------
;                 |                  1特权级栈                   |
;                 |              以4KB为单位的长度                |
;        +0x28 --> ---------------------------------------------
;                 |                 0特权级栈的                  |
;                 |                  初始ESP                    |
;        +0x24 --> ---------------------------------------------
;                 |                0特权级栈选择子                |
;        +0x22 --> ---------------------------------------------
;                 |                 0特权级栈的                  |
;                 |                   基地址                    |
;        +0x1E --> ---------------------------------------------
;                 |                  0特权级栈                   |
;                 |              以4KB为单位的长度                |
;        +0x1A --> ---------------------------------------------
;                 |                  TSS选择子                   |
;        +0x18 --> ---------------------------------------------
;                 |                    TSS的                    |
;                 |                    基地址                    |
;        +0x14 --> ---------------------------------------------
;                 |                   TSS界限值                  |
;        +0x12 --> ---------------------------------------------
;                 |                   LDT选择子                  |
;        +0x10 --> ---------------------------------------------
;                 |                    LDT的                    |
;                 |                    基地址                    |
;        +0x0C --> ---------------------------------------------
;                 |                 LDT当前界限值                 |
;        +0x0A --> ---------------------------------------------
;                 |                   程序加载的                  |
;                 |                    基地址                    |
;        +0x06 --> ---------------------------------------------
;                 |                   任务状态                   |
;        +0x04 --> ---------------------------------------------
;                 |                  下一个TCB的                 |
;                 |                      基地                   |
;        +0x00 --> ---------------------------------------------
;
;为了能够追踪到所有任务,应当把每个任务控制块TCB串起来,形成一个链表
;第414行,声明了标号tcb_chain并初始化为一个双字,初始的数值为0,实际上它是一个指针,用来指向第一个任务的TCB线性基地址,当它为零时,表示任务的数量为0,也就是没有任务
;在创建了第一个任务后,应当把该任务的TCB线性基地址填写到这里
;每个TCB的第一个双字,也是一个双字长度的指针,用于指向下一个任务的TCB,如果该位置是0,表示后面没有任务,这是链表上的最后一个任务
;否则它的数值就是下一个任务的TCB线性基地址,如下图所示,所有任务都按照被创建的先后顺序链接在一起,从tcb_chain开始,可以依次找到每一个任务
;下图是任务控制块链:
;
;           tcb_chain
;       -----------------
;      |           ._____|________
;       -----------------         \
;                                  \
;                             下一个TCB基地址
;                                   \                 tcb
;                                    \_______> -----------------
;                                             |           ._____|________
;                                              -----------------         \
;                                             |                 |         \
;                                             |    任务控制信息   |    下一个TCB基地址
;                                             |                 |           \                 tcb
;                                             |  _______________|            \_______> -----------------
;                                             | /                                     |         0       |
;                                             |/                                       -----------------
;                                                                                     |                 |
;                                                                                     |    任务控制信息   |
;                                                                                     |                 |
;                                                                                     |  _______________| 
;                                                                                     | / 
;                                                                                     |/
;
;第836~838行,用于分配创建TCB所需要的内存空间,并将其挂在TCB链上,当前版本的TCB结构需要0x46字节的内存空间
;将新TCB追加到链表上的工作是由过程append_to_tcb_link来做的,位于第735~772行,属于内核代码段的内部(近)过程,下图是它的整个流程图
;
;                                                                _________________
;                                                               /                 \
;                                                               \     保护现场     /
;                                                                \_______________/
;                                                                        |
;                                                                       \|/
;                                                               ---------------------
;                                                              |  令当前TCB的指针域为0  |
;                                                               ---------------------
;                                                                        |
;                                                                       \|/
;                                                                       /\
;                                                                      /  \
;                                                                     /    \
;                                                                    /      \
;                            是  .notcb                             /  链表   \
;                 <----------------------------------------------- /   为空?   \
;                 |                                                \ 头指针为0? /
;                 |                                                 \        /
;                 |                                                  \      /
;                 |                                                   \    /
;                 |                                                    \  /
;                 |                                                     \/
;                 |                                                     |       否  .searc
;                 |                                                     |
;                 |                                                     |
;                \|/                                                   \|/
;         --------------------                               -------------------------
;        |  令头指针指向当前TCB  |                             |  顺着TCB链找到最后一个TCB  |
;         --------------------                               -------------------------
;                                                                       |
;                 |                                                     |
;                 |                                                    \|/
;                 |                                           -------------------------
;                 |                                          |   令最后一个TCB的指针域    |
;                 |                                          |       指向当前TCB        |
;                 |                                           -------------------------
;                 |                                                     |
;                 >---------------------------------------------------->|
;                                                                       |
;                                                                       |   .retpc
;                                                                       |
;                                                                      \|/
;                                                             -------------------------
;                                                            |     恢复现场并返回        |
;                                                             -------------------------
;
;过程append_to_tcb_link的工作思路是遍历整个链表,找到最后一个TCB,在它的TCB指针域里填写新TCB的首地址,它需要用ECX作为传入的参数,ECX的内容应当为新TCB的线性地址
;这里有一个小小的麻烦,链首指针tcb_chain是在内核数据段声明并初始化的,只能知道它在段内的偏移,而不知道它的线性地址,因此只能通过内核数据段访问,而无法通过线性地址来访问
;相反地,链上的每一个TCB,其空间都是动态分配的只能通过线性地址来访问,因此在将两个段寄存器和两个通用寄存器压栈保护之后
;第742~745行,令段寄存器DS指向内核数据段以读写链首指针tcb_chain,而ES指向整个4GB内存空间,用于遍历和访问每一个TCB
;第747行,要追加的TCB一定是链表上的最后一个TCB,故其用于指向下一个TCB的指针域必须清零,以表明自己是链表上最后一个TCB
;每个TCB的空间都是动态分配的,其首地址都是线性地址,只能用由段寄存器ES所指向的4GB段来访问
;第750~752行,观察链首指针tcb_chain是否为零,若为零,则表明整个链表为空,直接转移到第763行的标号.notcb处,在那里直接将链首指针指向新的TCB,恢复现场后直接返回调用者
;第754~758行,若链首指针不为零,表明链表非空,需要顺着整个链找到最后一个TCB,和链首指针tcb_chain不同,每个TCB需要用4GB的段来访问,即,使用段寄存器ES
;首先将链表中要访问的那个TCB的线性地址传送到EDX,然后访问它的TCB指针域,看是否为零,如果不为零,表明它不是链中最后一个TCB,于是将控制转移到.searc,令EDX指向下一个TCB,继续搜寻
;若为零,表明它就是最后一个TCB,第760行,用ECX的内容填写其TCB指针域,让它指向新的TCB,完成后,第761行,直接转移到标号.retpc处,恢复现场并返回调用者
;
;
;                       使用栈传递过程参数
;下面的工作是加载和重定位用户程序,依然是在过程load_relocate_program中进行,该过程需要传入两个参数,分别是用户程序的起始逻辑扇区号,以及它的任务控制块TCB线性地址
;和上一章不同的是,参数不是用寄存器传入的,而是采用栈,事实上,这是更为流行和标准的做法,原因很简单,寄存器数量有限,况且还要在过程内部使用,当传入的参数很多时,栈是最好的选择
;第840~843行,先以双字的长度将立即数50压入当前栈,这是用户程序的起始逻辑扇区号
;在第10章里,我们已经知道push指令可以压入立即数,因此在这里,压入到栈中的内容将是双字0x0000 0032(十进制50),接着再压入当前任务控制块TCB的32位线性地址
;最后进入过程load_relocate_program内部执行,该过程位于第464行,是(当前)内核代码段的内部过程
;第468~473行,先做一些保护现场的工作,然后将栈指针ESP复制到EBP,以访问栈中的参数
;栈的访问有两种,一种是隐式的,由处理器执行如push,pop,call,ret等指令时自动进行,隐式地访问栈需要使用指令指针寄存器ESP
;另一种访问栈的方式不依赖于后进先出的机制,而是把栈看成是一般的数据段,直接访问其中的任何内容,在这种方式下,需要使用栈基址寄存器EBP,比如从栈中读取一个双字,该数据在栈中偏移量是由EBP所指向的
;   mov edx,[ebp]
;在32位模式下,处理器执行这条指令时,用段寄存器SS描述符高速缓存器中的32位基地址,加上EBP中的32位偏移量,形成32位线性地址,访问内存取得一个双字,传送到EDX
;很显然,用EBP来寻址时,不需要使用段超越前缀SS:,因为EBP出现在指令中的地址部分时,默认使用SS段寄存器
;下图是用ESP的内容初始化EBP后,栈的状态
;
;                    |   高字               低字   |
;                    ~                           ~
;                    ~                           ~
;     堆栈推进方向     |                           |
;          |         |                           |
;          |          ---------------------------
;          |         |   50(用户程序起始逻辑扇区号)  |<---- SS:EBP+12*4
;          |          ---------------------------
;          |         |        TCB线性地址         |<---- SS:EBP+11*4
;          |          ---------------------------
;         \|/        |           EIP             |在当前例子中,是从内核代码段调用公共例程段的例程,尽管也是通过调用门,但是它们的特权级都是0
;                     --------------------------- 所以,在控制转移的过程中不会发生栈切换,仅仅是把返回地址CS和EIP压入当前栈,当执行retf指令后
;                    |                           |处理器从栈中恢复CS和EIP的原始内容,于是又返回到原先的代码段接着执行,所以栈中才多了一个EIP,所以才有了第478行的: mov esi,[ebp+11*4](CS好像没压入)
;                    |                           |是用32位相对近调用(843行)进入的,故只压入了EIP的内容,没有压入CS的内容
;                    |                           |
;                    |      8个双字(通用寄存器)     |
;                    |                           |
;                    |                           |
;                    |                           |
;                    |                           |<---- SS:EBP+8    从SS:EBP+8开始,是pushad指令压入的8个双字,其中就包括EBP在压栈时的原始内容,在往上,是调用者的返回地址,因为load_relocate_program是一个内部过程
;                     ---------------------------
;                    |      0      |      DS     |<---- SS:EBP+4
;                     ---------------------------
;                    |      0      |      ES     |<---- SS:EBP
;                     ---------------------------
;                    |                           |
;                    |                           |
;                    ~                           ~
;                    ~                           ~
;                    | 执行mov ebp,esp指令后的栈状态|
;
;当前的栈顶位置是SS:EBP,指向一个双字,是段寄存器ES的内容,因为最近一次的压栈操作是:   push es
;在32位模式下,访问栈段用的是ESP,而且每次栈操作的默认操作数是32位,处理器在执行压栈指令时,如果发现指令的操作数是段寄存器(CS,SS,DS,ES,FS,GS),那么先执行一个内部的零扩展操作
;将段寄存器中的16位值扩展成32位,高16位是全0,然后再执行压栈操作,当然,出栈pop会执行相反的操作,将32位的值截断成16位,并传送到相应的段寄存器
;相应地,SS:EBP+4的位置是段寄存器DS的压栈值,因为栈是向下推进的,故较早压入的内容反而位于高地址方向,回溯它们需要增加EBP的值
;从SS:EBP+8开始,是pushad指令压入的8个双字,其中就包括EBP在压栈时的原始内容,在往上,是调用者的返回地址,因为load_relocate_program是一个内部过程
;是用32位相对近调用(843行)进入的,故只压入了EIP的内容,没有压入CS的内容
;好了,终于回到我们感兴趣的地方了,当初调用load_relocate_program过程的时候,压入了两个参数,分别是任务控制块TCB的线性地址,以及用户程序的起始扇区号,从上图可以看出
;TCB的线性地址是栈中的第11个双字,从0开始算起,正因为如此,TCB的线性地址在栈中的位置是SS:EBP+11*4
;同样的道理,用户程序起始逻辑扇区号在栈中的位置是SS:EBP+12*4,一会将要访问
;
;                       加载用户程序
;当用户程序被读入内存,并处于运行或者等待运行的状态时,就视为一个任务,任务有自己的代码段和数据段(包括栈),这些段必须通过描述符来引用,而这些描述符可以放在GDT中,也可以放在任务自己私有的LDT中
;GDT用于存放各个任务公有的描述符,比如公共的数据段和公共例程
;每个任务都允许有自己的LDT,而且可以定义在任何位置,所以我们现在要做3件事
;   1.分配一块内存,作为LDT来用,为创建用户程序各个段的描述符做准备
;   2.将LDT的大小和起始线性地址登记在TCB中
;   3.分配内存并加载用户程序,并将它的大小和起始线性地址登记到TCB中
;第475~476行,令ES指向4GB内存段
;第478行,先从栈中取得TCB的线性首地址,注意,因为源操作数部分使用的是基址寄存器EBP,故该指令默认使用SS来访问内存(栈)
;接着第481~484行申请分配160字节的内存空间用于创建LDT,并登记LDT的初始界限和起始地址到TCB中,LDT的段界限也是16位的,只允许8192个描述符,和GDT一样,界限值是表的总字节数减一
;因为我们刚创建LDT,所以总字节数为0,所以当前段界限值应当是0xFFFF(0减去1)
;我们的用户程序很简单,不会划分为太多的段,160字节的空间可以安装20个描述符,应当足够了,LDT的线性起始地址是登记在TCB内偏移0x0C处的,LDT的界限是登记在TCB内偏移0x0A处的
;TCB当初也是动态分配的,需要通过段寄存器ES指向的4GB段来访问
;第487~500行,先将用户程序头部读入内核缓冲区中,根据它的大小决定分配多少内存,具体的方法和策略已经在上一章讲过了,唯一需要说明的是
;在调用过程sys_routine_seg_sel:read_hard_disk_0之前,用户程序的起始逻辑扇区是从栈中获得的
;第502~504行,根据用户程序的实际大小申请分配内存空间,并将线性基地址和用户程序的大小登记到TCB中
;一旦知道了用户程序的总大小,接下来,第506~519行的工作就是加载整个用户程序,这和上一章也是相同的,唯一不同的是,第515行,从栈中重新取得用户程序的起始逻辑扇区号
;
;                       创建局部描述符表
;用户程序已经被加载到内存,现在该是在LDT中创建描述符的时候了
;第512行,从TCB中取得用户程序在内存中的基地址,早在第478行,我们就已经让ESI指向了TCB的基地址,当然TCB的基地址位于栈中,也可以从栈中取得
;第524~528行,因为用户程序头部的起始地址就是整个用户程序的起始地址,故将EDI的内容传送到EAX,作为过程sys_routine_seg_sel:make_seg_descriptor的第一个参数,即,段的起始地址
;接着从头部中取得用户程序头部段的长度,作为第二个参数传送到EBX寄存器,因为段界限是段的长度减1,故还要将EBX的内容减1,最后作为第三个参数,在ECX中置入段的属性
;可以知道这是一个32位的可读写数据段,字节粒度,尤为重要的是,其描述符的特权级DPL=3,即最低特权级,这是可以理解的,谁也不愿意自己的程序特权级最低,但这由不得你,这是操作系统决定的
;调用过程sys_routine_seg_sel:make_seg_descriptor后会在EDX:EAX中返回64位段描述符
;第531~532行用于用于调用另一个过程fill_descriptor_in_ldt把刚才创建的描述符安装到LDT中
;fill_descriptor_in_ldt是当前内核代码段的内部(近)过程,位于第421行,用于在当前任务的LDT中安装描述符,需要传入两个参数,一个是要安装的描述符,由EDX:EAX提供,另一个是当前任务控制块TCB的基地址,由EBX提供
;它用这个地址来访问TCB以获得LDT的基地址和当前大小(界限值),并在安装描述符后更新LDT的界限值
;第425~428行,执行例行的现场保护工作,将过程中用到的各个寄存器压栈保护
;第430~433行,先使段寄存器DS指向4GB内存段,然后访问TCB,从中取得LDT的基地址传送到EDI
;新描述符的线性地址可以用LDT的基地址加上LDT的总字节数得到,第435~440行计算用于安装新描述符的线性地址,并把它安装到那里
;在这里,ECX寄存器有两个关联的用途,一个是在第439和第440行寻址内存,并安装描述符
;另一个是在第436,437行用于计算LDT的大小,但只能使用其低16位的CX部分
;想想看,当第一次在LDT中安装描述符时,LDT的界限值是0xFFFF,加1之后,总大小是0x0000,进位部分要丢弃,对CX寄存器的操作不会影响到ECX的高16位
;
;和GDT不同,LDT的0号槽位也是可以用的,原因在于,其选择子的TI=1,所以不可能会有一个全0的选择子指向LDT,这就是说,一个指向LDT的选择子代入段寄存器时,它不可能是因为程序员的大意而未初始化的
;第442~443行,将LDT的总字节数在原来的基础上加8字节,再减去1,这就是新界限值
;第445行,将这个新界限值更新到TCB中
;第447~450行,将描述符的界限值除以8,余数丢弃,所得的商就是当前新描述符的索引号
;第452~454行,将CX中的索引号左移3次,并将TI位置1,表示指向LDT,这就得到当前描述符的选择子
;接着回到过程load_relocate_program中
;过程fill_descriptor_in_ldt在LDT中安装描述符后,用CX返回一个选择子
;第534~536行,用于将选择子的请求特权级RPL设置为3,登记到TCB中,并回填用户程序头部
;在LDT中安装的描述符,通常只由用户程序自己使用,即,在请求访问这些段时,请求者是用户程序自己,因此其选择子的RPL始终和用户程序的特权级一致
;
;
;                       重定位U-SALT表
;回到代码清单c14_core.asm
;从第539行开始,一直到576行结束,分别是创建用户程序代码段,数据段和栈段描述符,并将它们安装到LDT中,除了往LDT中安装描述符,以及其他一些细节上的差别外,这部分代码和上一章相比,大体上是一致的
;但是必须要说明的是,在这个过程中所创建的段描述符,其特权级DPL都是3,而且这些段描述符的选择子,其请求特权级RPL也是3
;从第579行开始,到第620行结束,用于重定位用户程序的U-SALT表,和第13章相比,绝大多数代码都是相同的,具体的工作流程也几乎没有变化,当然,因为涉及特权级,个别的差异还是有的
;U-SALT表位于用户程序头部段,为了访问它,这一章的做法是先用段寄存器ES指向用户程序的头部段,再访问该段内的U-SALT表,当然,前提是用户程序头部段的描述符已经安装并开始生效
;在本章中,用户程序各个段的描述符位于LDT中,尽管已经安装,但还没有生效(还没有加载局部描述符表寄存器LDTR),在这种情况下,只能通过4GB的段来访问U-SALT表
;所以第579~580行用于令ES指向4GB内存段,在前面的代码中,是令EDI指向用户程序起始加载地址的,这也就是用户程序头部段的起始线性地址,因为U-SALT表的条目数位于头部段内偏移0x24处
;故程序中可以用以下指令来取得该条目数(第587行): mov ecx,[es:edi+0x24]
;同样的道理,因为U-SALT表位于头部段内偏移0x28处,要想得到U-SALT表的线性基地址,使EDI指向它,程序中使用以下指令(第588行):    add edi,0x28
;具体的重定位过程在第13章已经讲的很清楚了,无非就是找到名字相同的C-SALT表,把它的地址部分复制到U-SALT的对应条目中,第13章里,复制的是16位的代码段选择子和32位段内偏移
;在本章中,这些地址不再是普通的段选择子和段内偏移,而是调用门选择子和段内偏移
;当初在创建这些调用门的时候,选择子的RPL字段是0,也就是说,这些调用门选择子的请求特权级是0,当他们被复制到U-SALT中时,应当改为用户程序的特权级3
;为此第605~606行,因为ESI指向当前条目的地址部分,所以4字节之后的地方该地址选择子部分,需要首先传送到AX,紧接着,修改它的RPL字段,使该选择子的请求特权级RPL=3
;
;
;                       创建0,1,2特权级的栈
;任务在运行时,需要调用内核或操作系统的例程,这可以认为是从同一个任务的局部地址空间转移到全局地址空间工作,而且在这个过程中涉及到特权级的变化,需要通过调用门
;通过调用门的控制转移通常会改变当前特权级CPL,同时还要切换到与目标代码段特权级相同的栈,为此必须为每个任务定义额外的栈
;对于当前的3特权级任务来说,应当创建特权级0,1,2的栈,而且应当将它们定义在每个任务自己的LDT中
;这些额外的栈是动态创建的,而且需要登记在任务段TSS中,以便处理器固件能够自动访问到它们,但是现在的问题是还没有创建TSS,有必要先将这些栈信息登记在任务控制块TCB中暂时保存
;第622行,从栈中取得当前任务的TCB基地址,它是作为过程参数压入当前栈中的
;第625~628行,申请创建0特权级栈所需的4KB内存,并在TCB中登记该栈的尺寸,登记到TCB中的尺寸值要求是以4KB为单位的,所以还要逻辑右移12次,相当于除以4096,得到一个4KB的倍数
;第629~630行,先申请内存,然后用申请到的内存基地址加上栈的尺寸,得到栈的高端地址,并将此地址登记到TCB中,一般来说,栈应当使用高端地址作为其线性基地址
;第631~634行,用给定的段界限和段属性调用公共例程段内的过程make_seg_descriptor创建描述符,段属性表明这是一个栈段,4KB粒度,我们创建的是0特权级栈,要求描述符DPL=0
;第635~636行,调用内核代码段内的近过程fill_descriptor_in_ldt将刚创建的描述符安装到LDT中,该过程要求使用EBX作为参数提供TCB的线性基地址,所以在调用该过程前先将该地址传送到EBX
;第637~639行,将安装描述符后返回的段选择子登记在TCB中,相应地应当将该选择子的请求特权级RPL设置为0,注意,过程返回的选择子本来就是RPL=0的,所以那条指令是作为注释存在的
;同时登记的还有0特权级栈指针的初始值,按老规矩,应当为0
;第642~673行是创建1,2特权级的栈,并将它们的信息登记在TCB中,并使用了和上面相同的方法,要注意,为他们分配的额特权级是各不相同的
;
;
;                       安装LDT描述符到GDT中
;尽管局部描述符表LDT和全局描述符表GDT都用来存放各种描述符,比如段描述符,但这掩盖不了它们也是内存段的事实,简单地说,它们也是段,但是因为它们是用于系统管理,所以称为系统的段或系统段
;全局描述符表GDT是唯一的,整个系统中只有一个,所以只需要用GDTR存放其线性基地址和段界限即可,但LDT不同,每个任务一个,所以为了追踪它们,处理器要求在GDT中安装每一个LDT的描述符
;当要使用这些LDT时,可以用它们的选择子来访问GDT,将LDT描述符加载到LDTR寄存器,在一些人看来,这个理由很牵强,这么做也很别扭,但是如果不这样,处理器将没有机会来做存储器和特权级的保护工作
;第676~679行调用公共例程段的过程make_seg_descriptor创建LDT描述符,作为传入的参数,EAX寄存器的内容是从TCB中取出的LDT基地址,EBX的内容是从TCB中取出的LDT长度,ECX的内容是描述符的属性
;各属性位与它们在描述符高32位中相同,无关的位要清零,下图是LDT描述符的格式
;
;
; 31            24 23  22  21   20  19         16 15 14 13 12 11     8 7            0 高32位
;|                |   |   |   |     |            |   |    |   |       |             |
;|  段基地址31~24  | G | D | L | AVL | 段界限19~16 | P | DPL| S |  TYPE | 段基地址23~16 |
;|                |   | 0 | 0 |     |            |   |    | 0 |  0010 |             |
;
; 31                                           1615                                 0 低32位
;|                 段基地址15~0                   |             段界限15~0             |
;
;
;LDT本身也是一种特殊的段,最大尺寸是64KB,段基地址指示LDT在内存中的起始地址,段界限指示LDT的范围,描述的G位是粒度位,适用于LDT描述符,以表示LDT的界限值是以字节为单位还是以4KB为单位
;即使是以4KB为单位,它也不能超过64KB的大小
;D位(或者叫B位)和L位对LDT来说没有意义,固定为0
;AVL和P位的含义与存储器的段描述符相同
;LDT描述符中的S位固定为0,表示系统的段描述符或者门描述符,以相对于存储器的段描述符(S=1),因为LDT描述符属于系统的段描述符
;在描述符为系统的段描述符时,即,S=0的前提下,TYPE字段为0010表明这是一个LDT描述符,因此传送到ECX的属性值0x00408200表示这是一个LDT描述符,描述符的特权级DPL=0,其他无关的位都已清零
;过程返回后,创建的描述符在EDX:EAX中,第680~681行,立即调用过程set_up_gdt_descriptor安装此描述符到全局描述符表GDT中,然后将返回的描述符选择子写入任务控制块TCB中的相应位置
;
;
;                       任务状态段TSS的格式
;到目前为止,任务的所有内存段都已创建完毕,除了任务状态段TSS,现在就来创建TSS,在此之前,先来全面了解下TSS的各个组成部分
;
;   31                      15                      0
;    ------------------------------------------------
;   |     I/O映射基地址      |           保留     |  T  |100
;    ------------------------------------------------
;   |        保留           |       LDT段选择子        |96
;    ------------------------------------------------
;   |        保留           |           GS            |92
;    ------------------------------------------------
;   |        保留           |           FS            |88
;    ------------------------------------------------
;   |        保留           |           DS            |84
;    ------------------------------------------------
;   |        保留           |           SS            |80
;    ------------------------------------------------
;   |        保留           |           CS            |76
;    ------------------------------------------------
;   |        保留           |           ES            |72
;    ------------------------------------------------
;   |                     EDI                        |68
;    ------------------------------------------------
;   |                     ESI                        |64
;    ------------------------------------------------
;   |                     EBP                        |60
;    ------------------------------------------------
;   |                     ESP                        |56
;    ------------------------------------------------
;   |                     EBX                        |52
;    ------------------------------------------------
;   |                     EDX                        |48
;    ------------------------------------------------
;   |                     ECX                        |44
;    ------------------------------------------------
;   |                     EAX                        |40
;    ------------------------------------------------
;   |                    EFLAGS                      |36
;    ------------------------------------------------
;   |                     EIP                        |32
;    ------------------------------------------------
;   |                   CR3(PDBR)                    |28
;    ------------------------------------------------
;   |        保留          |           SS2            |24
;    ------------------------------------------------
;   |                    ESP2                        |20
;    ------------------------------------------------
;   |        保留          |           SS1            |16
;    ------------------------------------------------
;   |                    ESP1                        |12
;    ------------------------------------------------
;   |        保留          |           SS0            |8
;    ------------------------------------------------
;   |                    ESP0                        |4
;    ------------------------------------------------
;   |        保留            |   前一个任务的指针TSS    |0
;    ------------------------------------------------
;
;TSS内偏移0处是前一个任务的TSS描述符选择子,和LDT一样,必须在全局描述符表GDT中创建每个TSS的描述符,当系统中有多个任务同时存在时,可以从一个任务切换到另一个任务执行
;此时称任务是嵌套的,被嵌套的任务用这个指针指向前一个任务,即嵌套它的那个任务,当控制返回前一个任务时,处理器需要这个指针来识别前一个任务,创建TSS时,可以为0
;SS0,SS1,SS2分别是0,1,2特权级的栈段选择子,ESP0,ESP1,ESP2,分别是0,1,2特权级栈的栈顶指针
;这些内容应当由任务的创建者填写,且属于填写后一般不变的的静态部分,当通过门进行特权级之间的控制转移时,处理器用这些信息来切换栈
;CR3和分页有关,有关分页的知识将在第16章讲述,此处一般由任务的创建者填写,如果没有使用分页,可以为0
;偏移为32~92的区域是处理器各个寄存器的快照部分,用于在进行任务切换时,保存处理器的状态以便将来恢复现场
;在一个多任务环境中,每次创建一个任务时,操作系统或者内核至少要填写EIP,EFLAGS,ESP,CS,SS,DS,ES,FS,GS,当该任务第一次获得执行时,处理器从这里加载初始执行环境
;并从CS:EIP处开始执行任务的第一条指令,在此之后的任务运行期间,该区域的内容由处理器固件进行更改
;在本章中,只有一个任务,而且自从进入保护模式时就开始运行了,只不过一开始是在0特权级的全局空间执行,所以这部分内容不需要填写
;LDT段选择子是当前任务的LDT描述符选择子,由内核或操作系统填写,以指向当前任务的LDT,该信息由处理器在任务切换时使用,在任务运行期间保持不变
;T位用于软件调试,在多任务的环境中,如果T=1,每次切换到该任务时,将引发一个调试异常中断,这是有益的,调试程序可以接管该中断以显示任务的状态,并执行一些调试操作,现在只需将这一位清零即可
;I/O映射基地址用于决定当前任务是否可以访问特定的硬件端口,对它的解释说来话长:
;是这样的,我们知道,特权指令是只有0特权级的程序才可以执行的指令,执行这些指令会影响整个机器的状态,现有的特权指令也许是处理器的设计者精心挑选的,因为即使较低特权级的程序不使用它们
;这些程序也能运行得很好,简直是非常好,不过另一些候选的指令就没有那么幸运了,尽管它们也适合作为特权指令,但其他特权级的程序同样需要它们
;一个典型的例子是硬件端口的输入输出指令in和out,它们应该对特权级别为1的程序开放,因为设备驱动程序就是工作在这个特权级别,不过这样做依然是不合理的,因为即使是特权级为3的程序
;在需要快速反应的场合,也需要直接访问某些硬件端口,所以如果需要,它们也可以向2,3特权级的程序开放
;处理器可以访问65536个硬件端口,如果只对应用程序开放那些它们需要的端口,而禁止它们访问另一些敏感的端口,操作系统肯定会对此持欢迎态度,因为这有利于设备的统一管理,同时也很安全
;每个任务都有EFLAGS寄存器的副本,其内容在任务创建的时候由内核或者操作系统初始化,在多任务系统中,每次当任务恢复运行时,就由处理器固件自动从TSS中恢复
;EFLAGS寄存器的IOPL位决定了当前任务的I/O特权级别,如果当前特权级CPL高于,或者和任务的I/O特权级IOPL相同时,即,在数值上  CPL <= IOPL 时,所有的I/O操作都是允许的,针对任何硬件端口的访问都可以通过
;相反,如果当前特权级CPL低于任务的I/O特权级IOPL,也并不意味着所有的硬件端口都对当前任务关闭了大门,事实上处理器的意思是总体上不允许,但个别端口除外,至于个别端口是哪些端口,要找到当前任务的TSS,并检索I/O许可位串
;如下图所示,I/O许可位串(I/O Permission Bit String)是一个比特序列,或者说是一个比特串,最多允许65536比特,即8KB,从第一个比特开始,各比特用它在串中的位置代表一个端口号
;因此第一个比特代表0号端口,第二个比特代表1号端口,第三个比特代表2号端口,...,第65536比特代表第65535号端口,每个比特的取值决定了相应的端口是否允许访问,1=禁止,0=允许
;处理器检查I/O许可位的方法是先计算它在I/O许可位映射区的字节编号,并读取该字节,然后进行测试,比如,当执行指令   out 0x09,al 时
;处理器通过计算就可以知道,该端口对应着I/O许可位映射区第2个字节的第2个比特(位1),于是它读取该字节,并测试那一位
;
;           端口1允许访问                 端口6允许访问                                                         端口65535禁止访问
;               |                             |                                                                     |
;   端口0禁止访问 |                             |                                                                     |
;         |     |                             |                                                                     |
;         |     |                             |                                                                     |
;       --------------------------------------------- ~~    ~~ ---------------------------------------------------------
;      |  1  |  0  |  1  |  1  |  1  |  1  |  0  |      ...       |  1  |  1  |  1  |  1  |  1  |  1  |  1  |  1  |  1  |
;       --------------------------------------------- ~~    ~~ ---------------------------------------------------------
;                                               最大长度的I/O许可串位示意图
;
;同其他和任务相关的信息一样,I/O许可位串位于任务TSS中,任务状态段TSS的最小长度是104字节,保存着最基本的任务信息,但这并不是它的最大长度
;事实上整个TSS还可包括一个I/O许可位串,它所占用的区域称为I/O许可位映射区,在TSS内偏移为102的那个字单元,保存着I/O许可位串(I/O许可位映射区)的起始位置,从TSS的起始处0开始算起
;因此如果该字单元的内容大于或等于TSS的段界限(在TSS描述符中),这表明没有I/O许可位串,在这种情况下,如果当前特权级CPL低于当前的I/O特权级,执行任何硬件I/O指令都会引发处理器异常中断
;说明一下,和LDT一样,必须在GDT中创建TSS的描述符,TSS描述符中包括了TSS的基地址和界限,该界限值包括I/O许可位映射区在内
;
;    -------------------------------------------------
;   |    FF     |                                     |
;   |                                                 |
;   |I/O许可位映射区最后一个字节的所有比特必须都是1          |
;   |  即,最后一字节是0xFF                              |
;   |                                                 |
;   |                                                 |
;   |                I/O许可位映射区                    |
;   |                                                 |
;   |                                                 |
;   |                                                 |
;    ------------------------------------------------- <--------+
;   |                                                 |         |
;   |                                                 |         |
;   |                                                 |         |
;   |                                                 |         |
;   |                软件可以使用的区域                  |         |
;   |                                                 |         |
;   |           ------------------------------------------------+
;   |          |                                      |
;   |          |                                      |
;   |          |                                      |
;    ----------|-------------------------------------
;   |     I/O映射基地址      |           保留     |  T  |100
;    ------------------------------------------------
;   |        保留           |       LDT段选择子        |96
;    ------------------------------------------------
;   |        保留           |           GS            |92
;    ------------------------------------------------
;   |        保留           |           FS            |88
;    ------------------------------------------------
;   |        保留           |           DS            |84
;    ------------------------------------------------
;   |        保留           |           SS            |80
;    ------------------------------------------------
;   |        保留           |           CS            |76
;    ------------------------------------------------
;   |        保留           |           ES            |72
;    ------------------------------------------------
;   |                     EDI                        |68
;    ------------------------------------------------
;   |                     ESI                        |64
;    ------------------------------------------------
;   |                     EBP                        |60
;    ------------------------------------------------
;   |                     ESP                        |56
;    ------------------------------------------------
;   |                     EBX                        |52
;    ------------------------------------------------
;   |                     EDX                        |48
;    ------------------------------------------------
;   |                     ECX                        |44
;    ------------------------------------------------
;   |                     EAX                        |40
;    ------------------------------------------------
;   |                    EFLAGS                      |36
;    ------------------------------------------------
;   |                     EIP                        |32
;    ------------------------------------------------
;   |                   CR3(PDBR)                    |28
;    ------------------------------------------------
;   |        保留          |           SS2            |24
;    ------------------------------------------------
;   |                    ESP2                        |20
;    ------------------------------------------------
;   |        保留          |           SS1            |16
;    ------------------------------------------------
;   |                    ESP1                        |12
;    ------------------------------------------------
;   |        保留          |           SS0            |8
;    ------------------------------------------------
;   |                    ESP0                        |4
;    ------------------------------------------------
;   |        保留            |   前一个任务的指针TSS    |0
;    ------------------------------------------------
;
;非常重要的一点是,I/O端口是按字节编址的,这句话的意思是,每个端口仅被设计用来读写一个字节的数据
;当以字或者双字访问时,实际上是访问连续的2个或4个端口,比如当从端口n读取一个字时,相当于同时从端口n和端口n+1个读取一个字节
;即,    in ax,0x3f8
;相当于同时执行:
;       in al,0x3f8
;       in ah,0x3f9     ;仅为示例,x86处理器不允许使用AH寄存器
;由于这个原因,当处理器执行一个字或双字I/O指令时,会检查许可位串中的2个或4个连续位,而且要求它们必须都是0,否则引发异常中断
;麻烦在于,这些连续的位可能是跨字节的,即,一些位于前一个字节,另一些位于后一个字节,为此处理器每次都要从I/O许可位映射区读两个连续的字节
;这种操作方式会直接导致另一个问题,即,如果要检查的比特在最后一字节中,那么这两字节的读操作将会越界,为防止这种情况,处理器要求I/O许可位映射区的最后必须附加一个额外的字节
;并要求它的所有比特都是1,即0xFF,当然,它必须位于TSS的界限之内
;处理器不要求为每一个I/O端口都提供位映射,对于那些没有在该区域映射的位,处理器假定它们对应的比特是1,假如,要是I/O许可位映射区的长度是11字节,那么除去最后一个所有比特都是1的字节
;前10个字节映射了80个端口,分别是0~79,访问更高的端口将引发异常中断
;显然,EFLAGS中的IOPL位对于控制任务的I/O特权来说是很重要的,通常,IOPL位由内核或者操作系统根据任务的实际需要进行初始化
;尽管不存在对EFLAGS寄存器整体写入或读出的指令,但存在将标志寄存器入栈和出栈的指令
;   pushf/pushfd    popf/popfd
;pushf并不是一条新指令,事实上早在8086时代就已经有了,用于将16位的FLAGS压栈,机器指令码为9C,在8086处理器上执行时,SP的内容减去2,然后将FLAGS的内容保存到栈段
;操作数的大小是一个字,同样的,popf指令将当前栈中的栈顶内容弹出到FLAGS寄存器
;在32位模式下,pushf压入的是整个32位的EFLAGS,即使有指令前缀,也不会只压入低16位,多总比少好,只压入低16位没有太大的意义,只会增加处理器的负担
;为了区分EFLAGS在16位模式下的两种压栈方式,编译器引入了符号pushfd,本质上它们对应着同一条指令,当你使用pushf时,编译器就知道,应当编译成无前缀的机器码9C,当使用pushfd时,编译器编译成66 9C,看下面例子
;
;       [bit 16]
;       pushf       ;9C,16位操作
;       pushfd      ;66 9C,32位操作
;
;       [bit 32]
;       pushf       ;9C,32位操作
;       pushfd      ;9C,32位操作
;可见在32位模式下,pushf和pushfd是相同的,上面的讨论同样适用于popf和popfd
;通过将EFLAGS压栈,局部修改后在出栈到EFLAGS中,可以间接地改变它的各种标志位,对多数标志位的修改不会威胁整个系统的安全
;但是如果修改了IOPL和IF位,就不同了,能够修改这两个标志的指令是   popf    iret    cli     sti
;注意,没有包括pushf指令,原因来自一个阴险的想法:你可以执行pushf指令,但我不允许你执行popf和iret指令,你就生气把,另外中断是由操作系统或者内核统一管理的
;cli和sti指令不能由低特权级别的程序随便执行,遗憾的是,这些指令并不是特权指令,原因很简单,其他特权级的程序也离不开它们
;最好的办法是用IOPL本身来控制它们,如果当前特权级CPL高于,或者和当前I/O特权级IOPL相同,即,在数值上 CPL <= IOPL
;则允许执行以上4条指令,也允许它们访问所有的硬件端口,否则,如果当前特权级CPL低于当前的I/O特权级IOPL,则执行popf和iret指令时,会引发处理器异常中断
;执行cli和sti时,不会引发异常中断,但不改变标志寄存器的IF位,同时,是否能访问特定的I/O端口,要参考TSS中的I/O许可位映射串
;
;
;                       创建任务状态段TSS
;回到代码清单c14_core.asm
;第684~688行,申请104字节的内存用于创建TSS,很显然我们是要创建一个标准大小的TSS,照例要把TSS的基地址和界限登记到任务控制块TCB中,将来创建TSS描述符时用得着
;TSS的界限值是16位,是它的大小(总字节数)减去1,这就是第686行的目的
;注意,界限值必须至少103,任何小于该值的TSS,在执行任务切换的时候,都会引发处理器异常中断
;第691行,将指向前一个任务的指针(任务链接域)填写为0,表明这是唯一任务
;第693~709行,等级0,1,2特权级栈的段选择子,以及它们的初始栈指针,所有的栈信息都在TCB中,先从TCB中取出,然后填写到TSS中的相应位置
;第711~712行,登记当前任务的LDT描述符选择子,在任务切换时,处理器需要用这里的信息找到当前任务的LDT,LDT对任务来说并不是必需的,如果高兴也可以把属于某个任务的段定义在GDT中,如果没有LDT,这里填写0
;第714~715行,填写I/O许可位映射区的地址,在这里,填写的是TSS段界限103,这意味着不存在该区域
;
;
;                       安装TSS描述符到GDT中
;和LDT一样,必须在GDT中安装TSS的描述符,这样做一方面为了对TSS进行段和特权级检查,另一方面也是执行任务切换的需要,当call far和jmp far指令的操作数是TSS描述符的选择子时,处理器执行任务切换操作
;下图是TSS描述符的示意图,除了TYPE位,和LDT描述符差不多
;
;
; 31            24 23  22  21   20  19         16 15 14 13 12 11     8 7            0 高32位
;|                |   |   |   |     |            |   |    |   |       |             |
;|  段基地址31~24  | G | D | L | AVL | 段界限19~16 | P | DPL| S |  TYPE | 段基地址23~16 |
;|                |   | 0 | 0 |     |            |   |    | 0 |  10B1 |             |
;
; 31                                           1615                                 0 低32位
;|                 段基地址15~0                   |             段界限15~0             |
;
;TSS描述符中的B位是忙位(Busy),在任务刚刚创建的时候,应该为二进制的1001,即,B位=0,表明任务不忙,当任务开始执行时,或处于挂起状态(临时被中断执行)时,由处理器固件把B位置1
;任务是不可重入的,,就是说,在多任务环境中,如果一个任务是当前任务,它可以切换到其他任务,但不能从自己切换到自己
;在TSS描述符中设置B位,并由处理器固件进行管理,可以防止这种情况的发生
;第720~725行,先调用公共例程段内的过程make_seg_descriptor创建TSS描述符,需要传入三个参数,先从TCB中取出TSS的基地址,传送到EAX,然后EBX中的内容是TSS的界限,ECX的内容是描述符属性值
;0x00408900表明这是一个DPL=0的TSS描述符,字节粒度,接着调用公共例程段内的另一过程set_up_gdt_descriptor安装此描述符到GDT中,并将返回的描述符选择子登记在TCB中,TSS描述符选择子的RPL=0
;
;
;                       带参数的过程返回指令
;到此,任务创建完毕,可以从过程load_relocate_program返回了,返回之前,即,在执行ret之前,需要恢复现场,这是第727~730行的工作
;如下图所示,当执行ret指令时,栈恢复到刚进入过程时的状态,即,只有返回地址和调用者传递给过程的参数,因为当初是采用32位相对近调用进入过程load_relocate_program的,所以仅将EIP压栈,没有压入CS的内容
;
;                    |   高字               低字   |
;                    ~                           ~
;                    ~                           ~
;     堆栈推进方向     |                           |
;          |         |                           |
;          |          ---------------------------
;          |         |   50(用户程序起始逻辑扇区号)  |
;          |          ---------------------------
;          |         |        TCB线性地址         |<---- SS:ESP
;          |          ---------------------------
;         \|/        |           EIP             |
;                     ---------------------------
;                    |                           |
;                    ~                           ~
;                    ~                           ~
;                    |                           |
;
;再来看,一旦ret指令执行完毕,控制将返回调用者,且栈中只剩下两个参数,按道理这两个参数是由调用者压入的,应该再由调用者弹出即可:
;       push dword 50       ;用户程序位于逻辑50扇区
;       push ecx            ;压入任务控制块起始线性地址
;       call load_relocate_program  ;调用过程
;       add esp,8           ;过程返回后,调整栈指针使之越过参数
;
;不过最好的解决办法是在过程返回时,顺便弹出参数,这样做是可行的,过程的编写者最清楚栈中有几个参数
;如果希望过程返回时弹出参数,使ESP指向调用过程前的栈位置,使栈平衡,可以使用带操作数的过程返回指令:    ret imm16   retf imm16
;这两条指令都允许16位的立即数作为操作数,不同之处仅仅在于,前者是近返回,后者是远返回,立即数是16位的
;而且一般总是偶数,原因是栈操作数总是以字或者双字进行,它指示在将控制返回到调用者之前,应当从栈中弹出多少字节的数据
;因此第732行,当该指令执行时,除了将控制返回到过程的调用者之外,还要调整栈的指针,即,   ESP <---- ESP+8
;之所以指令的操作数是8,是因为要弹出两个双字
;这条指令该高级语言带来的好处是增加了它们的复杂性,比如这样的一个C语言函数:
;
;void func(int i,char *c) {
;   /* 这里是函数体 */
;}
;
;因为一般是通过栈传递参数,所以哪个参数先入栈,哪个后入栈,栈平衡的事情由调用者来做,还是由过程来做,就需要一个标准,即所谓的调用转换规则
;特别是在开发一些大软件时,需要用不同的高级语言来开发各个独立的,但能够协同工作的模块,尤其需要注意这个问题
;
;
;                               用户程序的执行
;                       通过调用门转移控制的完整过程
;第845~846行,在调用过程load_relocate_program创建任务之后,显示一条成功的消息
;接下来的工作是将控制转移到用户程序那,我们创建的是一个特权级3的任务,所以这是一个从0特权级到3特权级的控制转移,或者说是从任务自己的0特权级全局空间转移到3特权级的局部空间执行,通常这既不允许,也不太可能
;办法总还是有的,只不过稍微有点曲折,那就是假装从调用门返回,先来看看完整的调用门控制转移和返回过程是怎样的
;首先,通过调用门实施控制转移,可以使用jmp far和call far指令,指令执行时,描述符选择子必须指向调用门,32位偏移量被忽略,但无论采用哪种控制转移指令,都会使用下表的特权级检查规则,注意,表中的比较关系都是数值上的
;
;       指令                                                    特权检查规则
;   call far        CPL <= 调用门描述符的DPL    RPL <= 调用门描述符的DPL    对于依从和非依从的代码段:CPL >= 目标代码段描述符的DPL
;
;   jmp far         CPL <= 调用门描述符的DPL    RPL <= 调用门描述符的DPL    若目标代码段是依从的:   CPL >= 目标代码段描述符的DPL
;                                                                       若目标代码段是非依从的:   CPL = 目标代码段描述符的DPL
;从中可以看出,当使用jmp far指令通过调用门转移控制时,要求当前特权级和目标代码段的特权级相同,原因是用jmp far指令通过调用门转移控制时,不改变当前特权级CPL
;相反,使用call far指令可以通过调用门将控制转移到较高特权级别的代码段,之所以说"可以",是因为,如果目标代码段是依从的,则和jmp far指令一样,不改变当前特权级,否则,如果目标代码段是非依从的,则在目标代码段的特权级上执行
;其次,当使用call far指令通过调用门转移控制时,如果改变了当前的特权级别,则必须切换栈,即,从当前任务的固有栈切换到与目标代码段特权级相同的栈上,栈的切换是由处理器固件自动进行的
;当前栈是由SS和ESP的当前内容指示的,要切换到的新栈位于当前任务的TSS中,处理器知道如何找到它,在栈切换前,处理器要检查新栈是否有足够的空间完成本次控制转移,栈切换过程如下:
;   1.使用目标代码段的DPL(也就是新的CPL)到当前任务的TSS中选择一个栈,包括栈选择子和栈指针
;   2.从TSS中读取所选择的段选择子和栈指针,并用该选择子读取栈段描述符,在此期间,任何违反段界限检查的行为都会引发异常中断(无效TSS)
;   3.检查栈段描述符的特权级和类型,并可能引发处理器异常中断(无效TSS)
;   4.临时保存当前栈段寄存器SS和栈指针ESP的内容
;   5.把新的栈段选择子和栈指针代入SS和ESP,切换到新栈
;   6.将刚才临时保存的SS和ESP的内容压入当前栈,如下图
;   7.依据调用门描述符"参数个数"字段的指示,从旧栈中将所有的参数复制到新栈中,如果参数个数为0,不复制参数,如下图
;   8.将当前段寄存器CS和指令指针寄存器EIP的内容压入新栈,如下图,通过调用门实施的控制转移一定是远转移,所以要压入CS和EIP
;   9.从调用门描述符中依次将目标代码段选择子和段内偏移传送到CS和EIP,开始执行被调用过程
;
;                    |   高字               低字   |
;                    ~                           ~
;                    ~                           ~
;     堆栈推进方向     |    控制转移前的旧栈(32位)    |
;          |         |                           |
;          |          ---------------------------
;          |         |           参数1            |
;          |          ---------------------------
;          |         |           参数2            |
;          |          ---------------------------
;         \|/        |           参数3            |<---- SS:ESP
;                     ---------------------------
;                    |                           |
;                    ~                           ~
;                    ~                           ~
;                    |                           |
;
;
;
;                    |   高字               低字   |
;                    ~                           ~
;                    ~                           ~
;     堆栈推进方向     |    控制转移后的新栈(32位)    |
;          |         |                           |
;          |          ---------------------------
;          |         |      0      |     SS      |
;          |          ---------------------------
;          |         |            ESP            |
;          |          ---------------------------
;         \|/        |            参数1           |
;                     ---------------------------
;                    |            参数2           |
;                     ---------------------------
;                    |            参数3           |
;                     ---------------------------
;                    |      0      |     CS      |
;                     ---------------------------
;                    |            EIP            |<---- SS:ESP
;                     ---------------------------
;                    |                           |
;                    ~                           ~
;                    ~                           ~
;                    |                           |
;
;相反,如果没有改变特权级别,则不切换栈,继续使用调用者的当前栈,只在原来的基础上压入当前段寄存器CS和指令指针寄存器EIP,如下图所示
;再次,如果通过调用门的控制转移是使用jmp far指令发起的,结果就是肉包子打狗,有去无回,而且没有特权级变化,也不需要切换栈
;相反,如果通过调用门的控制转移是使用call far指令发起的,那么可以使用远返回指令retf把控制返回到调用者
;从同一特权级返回时,处理器将从栈中弹出调用者代码段选择子和指令指针,尽管它们通常是有效的,但是为了安全起见,处理器依然会进行特权级检查
;
;                    |   高字               低字   |
;                    ~                           ~
;                    ~                           ~
;     堆栈推进方向     |                           |
;          |         |                           |
;          |          ---------------------------
;          |         |           参数1            |
;          |          ---------------------------
;          |         |           参数2            |
;          |          ---------------------------
;         \|/        |           参数3            |<---- 调用前的ESP
;                     ---------------------------
;                    |      0      |     CS      |
;                     ---------------------------
;                    |            EIP            |<---- 调用后的ESP
;                     ---------------------------
;                    |                           |
;                    ~                           ~
;                    ~                           ~
;                    | 相同特权级控制转移前后的栈变化 |
;
;要求特权级变化的远返回,只能返回到较低的特权级别上,控制返回的全部过程如下:
;   1.检查栈中保存的CS的内容,根据其RPL字段决定返回时是否需要改变特权级别
;   2.从当前栈中读取CS和EIP,并针对代码段描述符和代码段选择子的RPL字段实施特权级检查
;   3.如果远返回指令是带参数的,则将参数和ESP寄存器的当前值相加,以跳过栈中的参数部分
;     最后的结果是ESP寄存器指向调用者SS和ESP的压栈值,注意,retf指令的字节计数值必须等于调用门中的参数个数乘以参数长度
;   4.如果返回时需要改变特权级,从栈中将SS和ESP的压栈值代入段寄存器SS和(书上写指令指针,可能是错误)栈指针寄存器ESP,切换到调用者的栈
;     在此期间,一旦检测到有任何界限违例情况都会引发处理器异常中断
;   5.如果远返回指令是带参数的,则将参数和ESP寄存器的当前值相加,以跳过调用者栈中的参数部分,最后的结果是调用者的栈恢复到平衡位置
;   6.如果返回时需要改变特权级,检查DS,ES,FS,GS中的内容,根据它们找到相应的段描述符
;     要是有任何一个段描述符的DPL高于调用者的特权级(返回后的新CPL),即,在数值上: 段描述符的DPL < 返回后的新CPL,那么处理器将把数值0传送到该段寄存器
;
;那么这是为什么呢?特权级检查不是在实际访问内存时进行的,而是在将段选择子代入段寄存器时进行的,下面则两条指令可以非常清楚地说明这一点:
;       mov ds,ax           ;进行特权级检查
;       mov edx,[0x2000]    ;不进行特权级检查
;
;要想访问内存中的数据,必须先指定一个段,即,将选择子代入某个段寄存器,正是因为如此,只在将选择子代入段寄存器时进行一次特权级检查,而在此之后的普通内存访问,不进行特权级检查
;处理器的意思是只要你能进入大门,就证明你是这里的主人,随后干什么处理器都不会干涉
;现在做一个假设,假设一个特权级3的应用程序通过调用门请求0特权级的操作系统服务,在进入操作系统例程后,当前特权级CPL变成0,在该例程内,操作系统可能会访问自己的0特权级数据段以进行某些内部操作
;当然它也必须先执行将选择子代入段寄存器的操作:  mov ds,ax   ;操作系统自己的选择子
;按道理,安全的做法是先将旧的DS值压栈,用完再出栈,像这样:
;       push ds
;       mov ds,ax
;       ...
;       pop ds
;       retf
;但是如果操作系统例程没有这么做,一定有它的道理,而处理器也无权干涉,唯一可以预料的是,当控制返回用户程序时,DS依然指向操作系统数据段
;因此应用程序就可以直接在3特权级下访问操作系统的数据段  mov edx,[0x000c]
;这是因为,特权级检查只在引用一个段的时候进行,即,只在将选择子传送到段寄存器的时候进行,只要通过了这一关,后面那些使用这个段寄存器的内存访问就都是合法的
;为了解决这个问题,在执行retf指令时,要检查数据段寄存器,根据它们找到相应的段描述符
;要是有任何一个段描述符的DPL高于调用者的特权级(返回后的新CPL),那么处理器将把数值0传送到该寄存器,使用这样的段寄存器访问内存时,会引发处理器异常中断
;特别需要注意的是,任务状态段TSS中的SS0,ESP0,SS1,ESP1,SS2,ESP2域是静态的,除非软件进行修改,否则处理器从来不会改变它们,举个例子,当处理器通过调用门进入0特权级的代码段时
;会切换到0特权级栈,返回时并不把0特权级栈指针的内容更新到TSS中的ESP0域,下次再通过调用门进入0特权级代码段时,使用的依然是ESP0的静态值,从来不会改变
;这就是说如果你希望通过0特权级栈返回数据,就必须自己来做,比如在返回到低特权级别的代码段之前,手工改写TSS中的ESP0域
;
;
;                       进入3特权级的用户程序的执行
;接着回到内核代码,任务寄存器TR总是指向当前任务的任务状态段TSS,而LDTR寄存器也总是指向当前任务的LDT,TSS是任务的主要标志,因此要使TR寄存器指向任务,而是用LDTR的原因是可以在任务执行期间加速段的访问
;在多任务环境中,随着任务的切换,每当一个任务开始运行时(成为前台活动任务),TR和LDT寄存器的内容都会更新,以指向新的当前任务
;现在的问题是我们只有一个任务,而且是个3特权级的任务,不能用任务切换的方法使它开始运行,这个问题可以表述为:如何从任务的0特权级全局空间转移到它自己的3特权级空间正常执行?
;答案是先确立身份,即,使TR和LDTR寄存器指向这个任务,然后假装从调用门返回
;和当前有关的任务信息都在它的任务控制块TCB中,因此第832~833行,先令DS指向4GB的内存段
;第851~852行,加载任务寄存器TR和局部描述符寄存器LDTR
;如下图所示,任务寄存器TR和局部描述符寄存器LDTR都包括16位的选择器部分
;以及描述符高速缓存器部分,选择器部分的内容是TR和LDT描述符的选择子,描述符高速缓存器部分的内容则指向当前任务的TSS和LDT,以加速这两个段(表)的访问
;
;                 选择器                     描述符高速缓存器
;             ------------      ---------------------------------------
;       LDTR |  LDT选择器  |    |   32位线性基地址   |  段界限  |   段属性  |
;             ------------      ---------------------------------------
;
;             ------------      ---------------------------------------
;         TR |  TSS选择器  |    |   32位线性基地址   |  段界限  |   段属性  |
;             ------------      ---------------------------------------
;
;加载任务寄存器TR需要使用ltr指令,格式为:    ltr r/m16
;这条指令的操作数可以是16位通用寄存器,也可以是指向一个16位单元的内存地址,但不管是寄存器还是内存单元,其内容都是16位的TSS选择子
;在将TSS选择子加载到TR寄存器之后,处理器用该选择子访问GDT中对应的TSS描述符,将段界限和基地址加载到任务寄存器TR的描述符高速缓存器部分
;同时处理器将该TSS描述符中的B位置1,也就是标志为忙,但并不执行切换
;在指令不影响标志寄存器,但属于只能在0特权级下执行的特权指令
;
;加载局部描述符表寄存器LDTR使用的是lldt指令,格式和ltr一样:  lldt r/m16
;但是指向的是16位LDT选择子,ltr和lldt指令执行时,处理器首先要检查描述符的有效性,包括审查它是不是TSS或LDT描述符
;在将LDT选择子加载到LDTR寄存器之后,处理器用该选择子访问GDT中对应的LDT描述符,将段界限和段基地址加载到LDTR的描述符高速缓存器部分
;CS,SS,DS,ES,FS,GS的当前内容不受该指令结果的影响,包括TSS中的LDT选择子字段
;如果执行这条指令时,代入LTR选择器的选择子,其高14位是全零,LDTR寄存器的内容被标记为无效,而该指令的执行也将不声不响地结束,即,不会引发异常中断
;当然,后续那些引用LDT的指令都将引发异常中断(对描述符进行校验的指令除外),例如将一个指向LDT的段选择子代入段寄存器
;最后如下图所示,这是一个任务的全景图,给出了与一个任务相关的各个组成部分
;
;                                                                                -------------              |           |
;                                                                               |             |             ~           ~
;                                                       ------------            |             |             |           |
;                                                      |            |           |     TSS     |              -----------
;                                                      |            |           |             |             |           |
;       -------------------                             ------------            |             |             |   内存段   |
;      |        CPU        |                           |////////////|----------> -------------              |           |
;      |                   |                           |//TSS描述符//|                                 +----> -----------
;      |                   |                           |////////////|                                 |     |           |
;      |         TR----------------TSS选择子----------->|////////////|            -------------        |     |           |
;      |                   |                           |////////////|           |   段描述符   |-------+      -----------
;      |                   |                            ------------             -------------              |   内存段   |
;      |                   |                           |            |           |   段描述符   |------------> -----------
;      |                   |                           |            |            -------------              |           |
;      |                   |                            ------------            |             |             ~           ~
;      |                   |                           |////////////|           |     LDT     |             |           |
;      |                   |                           |//LDT描述符//|           |             |
;      |       LDTR---------------LDT选择子------------>|////////////|----------> -------------
;      |                   |                           |////////////|
;      |                   |                            ------------
;      |                   |                           |            |
;      |                   |                           |     GDT    |
;      |                   |                           |            |
;      |       GDTR-------------GDT的基地址和界限-------> -------------
;      |                   |
;       -------------------
;
;注意了,现在局部描述符表LDT已经生效了,可以通过它访问用户程序的私有内存段了
;第854~855行,访问任务的TCB,从用户程序头部内取出栈段选择子和栈指针,以及代码段选择子和入口点,并将它们顺序压入当前的0特权级栈中,这部分内容要结合第13章的用户程序头部来分析
;第864行,执行一个远返回指令retf,假装从调用门返回,于是控制转移到用户程序的3特权级代码开始执行,注意这里用的0特权级栈并非是来自于TSS,不过处理器不会在意这个
;下次,从3特权级的段再来到0特权级执行时,就会用到TSS中的0特权级栈了
;
;现在回到c13.asm,用户程序现在是工作在它的局部空间里,它可以通过调用门请求系统服务来显示字符串或者读取硬盘数据,这些指令可以再次加深我们对调用门的理解
;唯一的问题是,当它最后用jmp far指令将控制权返回到内核时,可能行不通了,这条指令是
;       jmp far [fs:TerminateProgram]   ;将控制权返回到系统
;这确实是一个调用门,而且通过jmp far指令使用调用门也没有任何问题,问题在于,当控制转移到内核时,当前特权级没有变化,还是3,因为使用jmp far指令通过调用门转移控制是不会改变当前特权级别的
;
;再回到c14_core.asm
;返回点是在第866行,因为当前特权级是3,以这样低的特权级来执行第867~868行的指令,一定会引发异常中断
;       mov eax,core_data_seg_sel
;       mov ds,eax
;在这里,当前特权级CPL=3,选择子core_data_seg_sel的请求特权级RPL=0,目标代码段的DPL=0,因为当前特权级CPL低于目标代码段的DPL,就算请求特权级RPL和目标代码段的DPL相同,也不可能通过特权级检查
;异常和异常中断的处理将在第17章讲解,我们现在还没有任何接管和处理异常中断的机制,所以这个异常可能不会明显被你观察到
;还需要特别提醒的是,进入3特权级的用户程序局部空间时,任务的I/O特权级IOPL=0,任务没有I/O操作的特权
;
;
;                       检查调用者的请求特权级RPL
;在本章的最后,回过头来聊一聊请求特权级RPL的有关问题,通过这个话题的深入,你会更进一步了解处理器引入RPL的原因和意义
;为了访问一个段,首先需要将段选择子代入段寄存器,这也是处理器进行特权级检查的大好机会:    mov fs,cx
;在绝大多数情况下,请求访问一个段的程序也是段选择子的提供者,就是说,当前特权级和请求特权级是相同的,即,CPL=RPL
;一般来说,用户程序的特权级别很低,而且不能执行I/O操作,假设操作系统提供了一个例程,可以从用户程序那里接受三个参数:逻辑扇区号,数据段选择子和段内偏移量,然后读硬盘,并把数据传送到用户程序缓冲区内
;为了使程序可以调用此例程,操作系统把它定义成调用门
;一般来说,用户程序会提供一个RPL=3的段选择子给操作系统例程,通过调用门实施控制转移后,当前特权级CPL变成0,实际的请求者是用户程序,选择子的请求特权级RPL=3,要访问的段属于用户程序,其描述符DPL=3
;在数值上符合   CPL <= DPL  ,并且   RPL <= DPL  可以正常执行
;
;想象一下,用户程序的编写者通过钻研,知道了内核数据段的选择子,而且希望用这个选择子访问内核数据段,当然他不可能在用户程序里访问内核数据段,因为那个数据段的DPL=0,而用户程序工作时的当前特权级CPL=3,处理器阻止
;但是他可以借助于刚才的那个调用门,特别地,他提供的是一个RPL=0的选择子,而且该选择子指向操作系统的段描述符,此时当前特权级CPL=0,请求特权级RPL=0
;目标数据段描述符的DPL=0,同样符合在数值上符合    CPL <= DPL ,并且   RPL <= DPL  的条件,并且允许向内核数据段写入扇区数据,就得逞了
;我知道有人会说,通过调用门进入内核例程时,用户程序的代码段选择子就作为返回地址压在栈中,代码段选择子的低2位就是用户程序的特权级,因此可以改造处理器固件,使它能够访问栈,用这个特权级来进行检查
;但是有这种认识的人别忘了,处理器的智商很低,它不可能真正知道谁是真正的请求者,你当然可以通过分析程序的行为来区分呢它们,但是处理器不能,因此,当指令:    mov ds,ax   或者    mov ds,cx
;执行时,AX或CX寄存器中的选择子可能是内核自己提供的,也可能来自恶意的用户程序,是不是合法的,这两种情况要区别对待,不能一棍子打死,所以这已经超出处理器的能力和职权范围了
;怎么办?
;记得在本章前面,在讨论RPL时,我说的是RPL只是在原来的基础上多增加的一种检查机制,并把如何能够通过这种检查的自由裁量权交给软件编写者
;引入请求特权级RPL的原因是处理器再遇到一条将选择子传送到段寄存器的指令时,无法区分真正的请求者是谁,但是引入RPL本身并不能完全解决这个问题,这只是处理器和操作系统之间的一种协议
;处理器负责检查请求特权级RPL,判断它是否有权访问,但前提是提供了正确的RPL,内核或者操作系统负责鉴别请求者的身份,并有义务保证RPL的值和它的请求者身份相符,因为这是处理器无能为力的
;因此在引入RPL这件事情上,处理器的潜台词是,仅仅依靠现有的CPL和DPL,无法解决由请求者不同而带来的安全隐患,那么再增加一道门,但前提是,操作系统只将通行证发放给正确的人
;为了帮助内核或操作系统检查请求者的身份,并提供正确的RPL值,处理器提供了arpl指令,arpl指令的作用是调整段选择子RPL的字段值(Adjust RPL Field of Segment Selector),其格式为
;   arpl r/m16,r16
;该指令比较两个段选择子的RPL字段,目的操作数可以是包含了16位段选择子的通用寄存器,或者一个指向16位单元的内存地址,该字单元存放的是段选择子,源操作数只能是包含了段选择子的16位通用寄存器
;该指令执行时,处理器检查目的操作数的RPL字段,如果它在数值上小于源操作数的RPL字段,则设置ZF标志,并增加目的操作数的RPL字段的值,使之和源操作数RPL字段的值相同,否则ZF标志清零,并且除此之外什么也不会发生
;arpl指令是典型的操作系统指令,它通常用于调整应用程序传递给操作系统的段选择子,使其RPL字段的值和应用程序的特权级相匹配,在这种情况下,传递给操作系统的段选择子是作为目的操作数出现的
;而应用程序的段选择子是作为源操作数出现的(可以从栈中取得),arpl也可以在应用程序中使用
;这样为了防止恶意的数据访问,操作系统应该从当前栈中取得用户程序的代码段选择子(调用者代码段基础器CS的内容)作为源操作数,并把作为参数传递进来的数据段选择子作为目的操作数,来执行arpl指令
;把数据段选择子的请求特权级RPL调整(恢复)到调用者的特权级别上
;一旦调整了请求特权级,那么当前特权级CPL=0,请求特权级RPL=3,数据段描述符特权级DPL=0,数值上并不符合    CPL <= DPL ,并且   RPL <= DPL  的条件,禁止访问,并引发处理器异常中断
;引入RPL检查机制和arpl指令,主要是防止对段的不安全访问,不管是恶意的,还是因为疏漏引起的,不管怎么说,一旦引入了RPL检查机制,就会处处起作用,同时也就成了编写程序时不得不考虑和妥善处理的问题
;结合下面的例子说明,就是县长知道农民就是农民,就算农民伪造了一张专家的身份证给县长,县长会将假的专家身份证替换成真的农民的身份证,这样到科技站时,出示身份也就是农民的代理人,RPL=3
;
;引用自网络:
;1.当前特权CPL(Current Privilege Level)
;  CPL是当前进程的权限级别(Current Privilege Level),是当前正在执行的代码所在的段的特权级,存在于cs寄存器的低两位。
;
;2.描述符特权级DPL(Descriptor Privilege Level)
;  DPL存储在段描述符中,规定访问该段的权限级别(Descriptor Privilege Level),每个段的DPL固定
;
;3.请求特权级RPL(Request Privilege Level)
;  RPL保存在选择子的最低两位,RPL说明的是进程对段访问的请求权限,意思是当前进程想要的请求权限
;  RPL的值由程序员自己来自由的设置,并不一定RPL>=CPL,但是当RPL<CPL时,实际起作用的就是CPL了
;  因为访问时的特权检查是判断：EPL=max(RPL,CPL)<=DPL是否成立,所以RPL可以看成是每次访问时的附加限制
;  RPL=0时附加限制最小,RPL=3时附加限制最大,所以你不要想通过来随便设置一个RPL来访问一个比CPL更内层的段
;
;对于为什么在CPL之外在增加一个RPL的原因：
;Intel手册上的解释为：The RPL can be used to insure that privileged code does not access a segment on behalf of an application program unless the program itself has access privileges for that segment.
;                 （RPL能够用来确保具有特权级的代码不会代表另一个应用程序去访问一个段,除非那个应用程序具有访问那个段的权限.）
;比方说：A进程的DPL为0,C进程的DPL为1,现在有一个B进程他的DPL为2,这B进程想委托A进程去访问C的数据,如果没有RPL的话这样的委托访问是可以成功的,但这样是非常不安全的
;有了RPL以后A进程在访问C的时候还要受到RPL的约束,此时可以将访问C的选择子的RPL设为B的DPL,这样A的访问权限就相当为EPL=max（RPL,DPL）=2,这样他就无法代表B去越权访问C了
;在网上还有一个形象的例子：
;
;一个农民(低特权级)请县长(高特权级)打听一种超级种子,如果找到的话帮忙拿一点回来,听闻这种超级种子可让收成倍增,县长说：好！我认识很多当官的,我可以帮你打听一下哪里有
;但是有些地方如果需要表示身分的话我只能说我是农民的代理人,县长利用自己的身份很容易找到了种子在哪里---找的时候没有人问起他代表谁
;县长问种子管理员可不可以给他一点,管理员说种子不能给农民因为种子还在试验阶段,我们可以给县长让他们带回当地的专家来帮忙一起做试验,但是一定要县长来申请,那你是谁？
;县长说我是农民的代理人,因为县长保证他会这样回答的(他也不知道那农民是不是专家),管理员当然不给,县长没办法只能告诉农民拿不到种子
;这件事里面县长是以县长的身份帮农民找到种子,但需要表示身份的时候他说只是农民的代理人,这样做县长可以帮人但也不会给别人利用(农民可能把种子拿回来卖钱也说不定,没人知道)
;在这里RPL就是县长的另一个身份---农民的代理人也就是农民---他会带在身上,人家没有问他的时候他不会告诉别人,所以别人也就以县长的身分来看待他,当查身份的时候他才告诉你---我是农民的代理人
;
;自己的理解:农民的身份证DPL=3,RPL=3,然后县长的DPL=0,CPL=0,县长拿着农民的身份证去种子站,然后管理员要求出示请求者的身份证以查验RPL,农民的RPL=3,不够资格,请求失败