         ;代码清单15-1
         ;文件名：c15_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2011-11-19 21:40

         ;以下常量定义部分。内核的大部分内容都应当固定 
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
      
         mov edx,eax
         and edx,0xffff0000                 ;得到偏移地址高16位 
         or dx,cx                           ;组装属性部分到EDX
       
         and eax,0x0000ffff                 ;得到偏移地址低16位 
         shl ebx,16                          
         or eax,ebx                         ;组装段选择子部分
      
         pop ecx
         pop ebx
      
         retf                                   
                             
;-------------------------------------------------------------------------------
terminate_current_task:                     ;终止当前任务
                                            ;注意，执行此例程时，当前任务仍在
                                            ;运行中。此例程其实也是当前任务的
                                            ;一部分 
         pushfd                             ;第358~360行,先将EFLAGS的当前内容压栈,然后用ESP作为地址操作数访问栈,取得EFLAGS的压栈值,并传送到EDX寄存器,接着将ESP的内容加上4,使栈平衡,保持压入EFLAGS寄存器前的状态
         mov edx,[esp]                      ;获得EFLAGS寄存器内容
         add esp,4                          ;恢复堆栈指针

         mov eax,core_data_seg_sel          ;令DS指向内核数据段,方便后续操作
         mov ds,eax

         test dx,0100_0000_0000_0000B       ;测试NT位
         jnz .b1                            ;当前任务是嵌套的，到.b1执行iretd 
         mov ebx,core_msg1                  ;当前任务不是嵌套的，直接切换到 
         call sys_routine_seg_sel:put_string
         jmp far [prgman_tss]               ;程序管理器任务 
       
  .b1: 
         mov ebx,core_msg0
         call sys_routine_seg_sel:put_string
         iretd                              ;第374行,通过iret指令转换到前一个任务,即程序管理器任务,执行任务切换时,当前用户任务的TSS描述符的B位被清零,EFLAGS的NT位也被清零,并被保存到它的TSS中
      
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
                          dd  put_string
                          dw  sys_routine_seg_sel

         salt_2           db  '@ReadDiskData'
                     times 256-($-salt_2) db 0
                          dd  read_hard_disk_0
                          dw  sys_routine_seg_sel

         salt_3           db  '@PrintDwordAsHexString'
                     times 256-($-salt_3) db 0
                          dd  put_hex_dword
                          dw  sys_routine_seg_sel

         salt_4           db  '@TerminateProgram'
                     times 256-($-salt_4) db 0
                          dd  terminate_current_task
                          dw  sys_routine_seg_sel

         salt_item_len   equ $-salt_4
         salt_items      equ ($-salt)/salt_item_len

         message_1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message_2        db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0
         
         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 2048 db 0         ;内核用的缓冲区

         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 52 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0

         ;任务控制块链
         tcb_chain        dd  0
;第431行声明并初始化了6字节的空间,前32位用于保存TSS的基地址,后16位则是它的选择子
         ;程序管理器的任务信息 
         prgman_tss       dd  0             ;程序管理器的TSS基地址
                          dw  0             ;程序管理器的TSS描述符选择子 

         prgman_msg1      db  0x0d,0x0a
                          db  '[PROGRAM MANAGER]: Hello! I am Program Manager,'
                          db  'run at CPL=0.Now,create user task and switch '
                          db  'to it by the CALL instruction...',0x0d,0x0a,0
                 
         prgman_msg2      db  0x0d,0x0a
                          db  '[PROGRAM MANAGER]: I am glad to regain control.'
                          db  'Now,create another user task and switch to '
                          db  'it by the JMP instruction...',0x0d,0x0a,0
                 
         prgman_msg3      db  0x0d,0x0a
                          db  '[PROGRAM MANAGER]: I am gain control again,'
                          db  'HALT...',0

         core_msg0        db  0x0d,0x0a
                          db  '[SYSTEM CORE]: Uh...This task initiated with '
                          db  'CALL instruction or an exeception/ interrupt,'
                          db  'should use IRETD instruction to switch back...'
                          db  0x0d,0x0a,0

         core_msg1        db  0x0d,0x0a
                          db  '[SYSTEM CORE]: Uh...This task initiated with '
                          db  'JMP instruction,  should switch to Program '
                          db  'Manager directly by the JMP instruction...'
                          db  0x0d,0x0a,0

core_data_end:
               
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

         mov ecx,mem_0_4_gb_seg_sel
         mov ds,ecx

         mov edi,[ebx+0x0c]                 ;获得LDT基地址
         
         xor ecx,ecx
         mov cx,[ebx+0x0a]                  ;获得LDT界限
         inc cx                             ;LDT的总字节数，即新描述符偏移地址
         
         mov [edi+ecx+0x00],eax
         mov [edi+ecx+0x04],edx             ;安装描述符

         add cx,8                           
         dec cx                             ;得到新的LDT界限值 

         mov [ebx+0x0a],cx                  ;更新LDT界限值到TCB

         mov ax,cx
         xor dx,dx
         mov cx,8
         div cx
         
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
         pushad
      
         push ds
         push es
      
         mov ebp,esp                        ;为访问通过堆栈传递的参数做准备
      
         mov ecx,mem_0_4_gb_seg_sel
         mov es,ecx
      
         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址

         ;以下申请创建LDT所需要的内存
         mov ecx,160                        ;允许安装20个LDT描述符
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x0c],ecx              ;登记LDT基地址到TCB中
         mov word [es:esi+0x0a],0xffff      ;登记LDT初始的界限到TCB中 

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
      
         mov ecx,eax                        ;实际需要申请的内存数量
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x06],ecx              ;登记程序加载基地址到TCB中
      
         mov ebx,ecx                        ;ebx -> 申请到的内存首地址
         xor edx,edx
         mov ecx,512
         div ecx
         mov ecx,eax                        ;总扇区数 
      
         mov eax,mem_0_4_gb_seg_sel         ;切换DS到0-4GB的段
         mov ds,eax

         mov eax,[ebp+12*4]                 ;起始扇区号 
  .b1:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b1                           ;循环读，直到读完整个用户程序

         mov edi,[es:esi+0x06]              ;获得程序加载基地址

         ;建立程序头部段描述符
         mov eax,edi                        ;程序头部起始线性地址
         mov ebx,[edi+0x04]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040f200                 ;字节粒度的数据段描述符，特权级3 
         call sys_routine_seg_sel:make_seg_descriptor
      
         ;安装头部段描述符到LDT中 
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt

         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         mov [es:esi+0x44],cx               ;登记程序头部段选择子到TCB 
         mov [edi+0x04],cx                  ;和头部内 
      
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

         ;重定位SALT 
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

         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址

         ;创建0特权级堆栈
         mov ecx,4096
         mov eax,ecx                        ;为生成堆栈高端地址做准备 
         mov [es:esi+0x1a],ecx
         shr dword [es:esi+0x1a],12         ;登记0特权级堆栈尺寸到TCB 
         call sys_routine_seg_sel:allocate_memory
         add eax,ecx                        ;堆栈必须使用高端地址为基地址
         mov [es:esi+0x1e],eax              ;登记0特权级堆栈基地址到TCB 
         mov ebx,0xffffe                    ;段长度（界限）
         mov ecx,0x00c09600                 ;4KB粒度，读写，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         ;or cx,0000_0000_0000_0000          ;设置选择子的特权级为0
         mov [es:esi+0x22],cx               ;登记0特权级堆栈选择子到TCB
         mov dword [es:esi+0x24],0          ;登记0特权级堆栈初始ESP到TCB
      
         ;创建1特权级堆栈
         mov ecx,4096
         mov eax,ecx                        ;为生成堆栈高端地址做准备
         mov [es:esi+0x28],ecx
         shr dword [es:esi+0x28],12         ;登记1特权级堆栈尺寸到TCB
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
         shr dword [es:esi+0x36],12         ;登记2特权级堆栈尺寸到TCB
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
      
         ;在GDT中登记LDT描述符
         mov eax,[es:esi+0x0c]              ;LDT的起始线性地址
         movzx ebx,word [es:esi+0x0a]       ;LDT段界限
         mov ecx,0x00408200                 ;LDT描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x10],cx               ;登记LDT选择子到TCB中
       
         ;创建用户程序的TSS
         mov ecx,104                        ;tss的基本尺寸
         mov [es:esi+0x12],cx              
         dec word [es:esi+0x12]             ;登记TSS界限值到TCB 
         call sys_routine_seg_sel:allocate_memory
         mov [es:esi+0x14],ecx              ;登记TSS基地址到TCB
      
         ;登记基本的TSS表格内容
         mov word [es:ecx+0],0              ;反向链=0
      
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

         mov dx,[es:esi+0x10]               ;登记任务的LDT选择子
         mov [es:ecx+96],dx                 ;到TSS中
      
         mov dx,[es:esi+0x12]               ;登记任务的I/O位图偏移
         mov [es:ecx+102],dx                ;到TSS中 
      
         mov word [es:ecx+100],0            ;T=0
      
         mov dword [es:ecx+28],0            ;登记CR3(PDBR)
;第766行开始到790行结束的,首先从栈中取出TCB的基地址,然后通过4GB的内存段访问TCB,取出用户程序加载的起始地址,这也是用户程序头部的起始地址
         ;访问用户程序头部，获取数据填充TSS      ;接着依次登记指令指针寄存器EIP和各个段寄存器的内容,因为这是用户程序的第一次执行,所以TSS中的EIP域应该等级用户程序的入口点,CS域应该登记用户程序入口点所在的代码段选择子
         mov ebx,[ebp+11*4]                 ;从堆栈中取得TCB的基地址
         mov edi,[es:ebx+0x06]              ;用户程序加载的基地址 

         mov edx,[es:edi+0x10]              ;登记程序入口点（EIP） 
         mov [es:ecx+32],edx                ;到TSS

         mov dx,[es:edi+0x14]               ;登记程序代码段（CS）选择子
         mov [es:ecx+76],dx                 ;到TSS中

         mov dx,[es:edi+0x08]               ;登记程序堆栈段（SS）选择子
         mov [es:ecx+80],dx                 ;到TSS中

         mov dx,[es:edi+0x04]               ;登记程序数据段（DS）选择子
         mov word [es:ecx+84],dx            ;到TSS中。注意，它指向程序头部段
      
         mov word [es:ecx+72],0             ;TSS中的ES=0

         mov word [es:ecx+88],0             ;TSS中的FS=0

         mov word [es:ecx+92],0             ;TSS中的GS=0
;第787~790行,先将EFLAGS寄存器的内容压入栈,再将其弹出到EDX寄存器,因为不存在将标志寄存器的内容完整传送到通用寄存器的指令
         pushfd                             ;接着把EDX的内容写入TSS中EFLAGS域,注意,这是当前任务(程序管理器)EFLAGS寄存器的副本,新任务将使用这个副本作为初始化的EFLAGS寄存器
         pop edx                            ;一般来说,此时的EFLAGS寄存器的IOPL字段为00,将来新任务开始执行时,会用这个副本作为处理器EFLAGS寄存器的当前值,并因此而没有足够的I/O特权
         
         mov dword [es:ecx+36],edx          ;EFLAGS

         ;在GDT中登记TSS描述符
         mov eax,[es:esi+0x14]              ;TSS的起始线性地址
         movzx ebx,word [es:esi+0x12]       ;段长度（界限）
         mov ecx,0x00408900                 ;TSS描述符，特权级0
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
         
         mov eax,core_data_seg_sel          ;令DS指向内核数据段 
         mov ds,eax
         mov eax,mem_0_4_gb_seg_sel         ;令ES指向0..4GB段
         mov es,eax
         
         mov dword [es: ecx+0x00],0         ;当前TCB指针域清零，以指示这是最
                                            ;后一个TCB
                                             
         mov eax,[tcb_chain]                ;TCB表头指针
         or eax,eax                         ;链表为空？
         jz .notcb 
         
  .searc:
         mov edx,eax
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
start:                                      ;内核入口点
         mov ecx,core_data_seg_sel          ;令DS指向核心数据段 
         mov ds,ecx

         mov ecx,mem_0_4_gb_seg_sel         ;令ES指向4GB数据段 
         mov es,ecx

         mov ebx,message_1                    
         call sys_routine_seg_sel:put_string
                                         
         ;显示处理器品牌信息 
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

         mov ebx,cpu_brnd0                  ;显示处理器品牌信息 
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brand
         call sys_routine_seg_sel:put_string
         mov ebx,cpu_brnd1
         call sys_routine_seg_sel:put_string

         ;以下开始安装为整个系统服务的调用门。特权级之间的控制转移必须使用门
         mov edi,salt                       ;C-SALT表的起始位置 
         mov ecx,salt_items                 ;C-SALT表的条目数量 
  .b3:
         push ecx   
         mov eax,[edi+256]                  ;该条目入口点的32位偏移地址 
         mov bx,[edi+260]                   ;该条目入口点的段选择子 
         mov cx,1_11_0_1100_000_00000B      ;特权级3的调用门(3以上的特权级才
                                            ;允许访问)，0个参数(因为用寄存器
                                            ;传递参数，而没有用栈) 
         call sys_routine_seg_sel:make_gate_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [edi+260],cx                   ;将返回的门描述符选择子回填
         add edi,salt_item_len              ;指向下一个C-SALT条目 
         pop ecx
         loop .b3

         ;对门进行测试 
         mov ebx,message_2
         call far [salt_1+256]              ;通过门显示信息(偏移量将被忽略) 
;第909~911行用于申请创建TSS所需的内存,为了追踪程序管理器的TSS,需要保存它的基地址和选择子,保存的位置是内核数据段
         ;为程序管理器的TSS分配内存空间 
         mov ecx,104                        ;为该任务的TSS分配内存
         call sys_routine_seg_sel:allocate_memory
         mov [prgman_tss+0x00],ecx          ;保存程序管理器的TSS基地址 
;第914~918行对TSS进行最基本的设置,程序管理器任务没有自己的LDT,任务可以没有自己的LDT,这是允许的,程序管理器可以将自己所使用的段描述符安装在GDT中,另外,程序管理器任务是运行在0特权级上的
         ;在程序管理器的TSS中设置必要的项目 
         mov word [es:ecx+96],0             ;没有LDT。处理器允许没有LDT的任务。
         mov word [es:ecx+102],103          ;没有I/O位图。0特权级事实上不需要。
         mov word [es:ecx+0],0              ;反向链=0
         mov dword [es:ecx+28],0            ;登记CR3(PDBR)
         mov word [es:ecx+100],0            ;T=0
                                            ;不需要0、1、2特权级堆栈。0特级不
                                            ;会向低特权级转移控制。
;第923~928行,在GDT中创建TSS的描述符,必须创建TSS的描述符,而且只能安装在GDT中
         ;创建TSS描述符，并安装到GDT中 
         mov eax,ecx                        ;TSS的起始线性地址
         mov ebx,103                        ;段长度（界限）
         mov ecx,0x00408900                 ;TSS描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [prgman_tss+0x04],cx           ;保存程序管理器的TSS描述符选择子 

         ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
         ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
         ltr cx                              
;第935~936行,任务管理器显示一条信息,信息文本位于内核数据段中,第434行声明了标号prgman_msg1,并初始化了以上字符串
         ;现在可认为“程序管理器”任务正执行中
         mov ebx,prgman_msg1
         call sys_routine_seg_sel:put_string
;第938~945行是用来加载用户程序的,先分配一个任务控制块TCB,然后将它挂到TCB链上,接着压入用户程序的逻辑扇区及其TCB基地址,作为参数调用过程load_relocate_program
         mov ecx,0x46
         call sys_routine_seg_sel:allocate_memory
         call append_to_tcb_link            ;将此TCB添加到TCB链中 
      
         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址 
       
         call load_relocate_program         
;第947行是一条32位间接远调用指令call,操作数是一个内存地址,指向TCB内的0x14单元,这样的指令我们很熟悉,一般来说,转移到的目标位置可以由16位的代码段选择子和32位段内偏移量组成,也可以由16位的调用门选择子和32位偏移量组成
         call far [es:ecx+0x14]             ;执行任务切换。和上一章不同，任务切
                                            ;换时要恢复TSS内容，所以在创建任务
                                            ;时TSS要填写完整 
;第952~953行,程序管理器先显示一条消息,标号prgman_msg2的位置是在第439行,位于内核数据段,在那里初始化了字符串                            
         ;重新加载并切换任务 
         mov ebx,prgman_msg2
         call sys_routine_seg_sel:put_string
;第955~964行,创建新的用户任务并发起任务切换,与上次相比,这次的任务切换有几个值得注意的特点,首先可以看出该任务也是从硬盘的50号逻辑扇区开始加载的,也就是说它和上一个用户任务一样,来自同一个程序
         mov ecx,0x46
         call sys_routine_seg_sel:allocate_memory
         call append_to_tcb_link            ;将此TCB添加到TCB链中

         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址

         call load_relocate_program

         jmp far [es:ecx+0x14]              ;执行任务切换

         mov ebx,prgman_msg3
         call sys_routine_seg_sel:put_string

         hlt
            
core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
;以下是注释说明:
;从80286开始的处理器是面向多任务系统而设计的,在一个多任务的环境中,可以同时存在多个任务,每个任务都有各自的局部描述符表LDT和任务状态段TSS
;在LDT中存放着专属于任务局部空间的段描述符,可以在多个任务之间切换,使它们轮流执行,从一个任务切换到另一个任务时,具体的切换过程是由处理器固件负责进行的
;所谓多任务系统,是指能够同时执行两个以上任务的系统,即使前一个任务还没执行完,下一个任务也可以开始执行,但是什么时候切换到另一个任务,以及切换到哪一个任务执行,主要是操作系统的责任
;处理器只负责具体的切换过程,包括保护前一个任务的现场
;有两种基本的任务切换方式,一种是协同式的,从一个任务切换到另一个任务,需要当前任务主动地请求暂时放弃执行权,或者在通过调用门请求操作系统服务时,由操作系统"趁机"将控制转移到另一个任务
;这种方式依赖于每个任务的"自律性",当一个任务失控时,其他任务得不到执行的机会
;另一种是抢占式多任务,在这种方式下,可以安装一个定时器中断,并在中断服务程序中实施任务切换,硬件中断信号总会定时出现,不管处理器当时在做什么,中断都会适时地发生,而任务切换也就可以顺利进行
;在这种情况下,每个任务都能得到平等的执行机会,而且即使一个任务失控,也不会导致其他任务没有机会执行
;抢占式多任务将在第17章讲解,本章先介绍多任务任务切换的一般工作原理,掌握任务切换的几种方法以及各自的特点
;                               任务切换前的设置
;在上一章里介绍了许多关于特权级之间的控制转移,容易使读者混淆它和任务切换之间的区别
;在一个任务内,全局空间和局部空间具有不同的特权级别,使用们,可以在任务内将控制从3特权级的局部空间转移到0特权级的全局空间,以使用内核或者操作系统提供的服务
;任务切换是以任务为单位的,是指离开一个任务转到另一个任务中去执行,任务转移相对来说要复杂的多,当一个任务正在执行时,处理器的各个部件都和该任务息息相关:
;段寄存器指向该任务所使用的内存段,通用寄存器保存着该任务的中间结果,等等
;离开当前任务,转到另一个任务开始执行时,要保存旧的任务的各种状态并恢复新任务的运行环境
;这就是说,要执行任务切换,系统中必须至少有两个任务,而且已经有一个正在执行中,在上一章中,我们已经创建过一个任务,那个任务的特权级是3,一开始处理器是在任务的全局空间执行,当前特权级是0
;然后我们通过一个虚假的调用门返回,使处理器回到任务的局部空间执行,当前特权级降为3
;事实上这是没有必要的,这样做很别扭,首先处理器在刚进入保护模式时,是以0特权级运行的,而且执行的一般都是操作系统代码,也必须是0特权级,这样才能方便地控制整个计算机
;其次,任务并不一定非得是3特权级,也可以是0特权级的,特别是操作系统除了为每一个任务提供服务外,也会有一个作为任务而独立存在的部分,而且是0特权级的任务,以完成一些管理和控制功能,比如提供一个界面和用户交互
;既然是这样,我们在这一章里就要首先创建0特权级的操作系统(内核)任务,本章内核部分有所改动,增加了有关任务切换的代码
;
;内核的入口点在第848行,第906行之前的工作都和上一章相同,主要是显示处理器品牌信息,以及安装共每个任务使用的调用门
;接下来的工作是创建0特权级的内核任务,并将正在执行的内核代码段划归该任务,当前代码的作用是创建其他任务,管理他们,所以称作任务管理器,或者叫程序管理器
;任务状态段TSS是任务存在的标志,没有它就无法执行任务切换,因为任务切换时需要保存旧任务的各种状态数据
;第909~911行用于申请创建TSS所需的内存,为了追踪程序管理器的TSS,需要保存它的基地址和选择子,保存的位置是内核数据段
;第431行声明并初始化了6字节的空间,前32位用于保存TSS的基地址,后16位则是它的选择子
;接着第914~918行对TSS进行最基本的设置,程序管理器任务没有自己的LDT,任务可以没有自己的LDT,这是允许的,程序管理器可以将自己所使用的段描述符安装在GDT中,另外,程序管理器任务是运行在0特权级上的
;不需要创建额外的栈,因为除了从门返回外,不能将控制从高特权级的代码段转移到低特权级的代码段
;第923~928行,在GDT中创建TSS的描述符,必须创建TSS的描述符,而且只能安装在GDT中
;为了表明当前正在任务中执行,所要做的最后一个工作就是将当前任务的TSS选择子传送到任务寄存器TR中,第932行正是用来完成这个工作的,执行这条指令后,处理器用该选择子访问GDT,找到相应的TSS描述符
;将其B位置1,表示该任务正在执行中(或者处于挂起状态),同时还要将该描述符传送到TR寄存器的描述符高速缓存器中
;第935~936行,任务管理器显示一条信息,信息文本位于内核数据段中,第434行声明了标号prgman_msg1,并初始化了以上字符串
;
;                               任务切换的方法
;对多任务的支持是现代处理器的标志之一,为此Intel处理器提供了多种方法以灵活地在任务之间实施切换
;尽管如此,处理器并没有提供额外的指令用于任务切换,事实上用的都是熟悉的老指令和老手段,但是扩展了它们的功能,使之除了能够继续执行原有的功能外,也能用于实施任务的切换操作
;第一种实施任务切换的方法是借助于中断,这也是现代抢占式多任务的基础,原因很简单,只要中断没有被屏蔽,它就能随时发生,特别是定时器中断,能够以准确的时间间隔发生,可以用来实施强制任务切换
;毕竟没有哪个任务愿意交出处理器的控制权,也没有哪个任务能精确地把握交出控制权的时机
;我们知道在实模式下,内存最低端的1KB是中断向量表,保存着256个中断处理过程的段地址和偏移地址,当中断发生时,处理器把中断号乘以4,作为表内索引访问中断向量表,从对应的位置取出中断处理过程的段地址和偏移地址,并转移到那里执行
;在保护模式下,中断向量表不再使用,取而代之的是中断描述符表,和GDT,LDT,一样用于保存描述符,唯一不同的地方是它保存的是门描述符,包括中断门,陷阱们,任务门
;当中断发生时,处理器用中断号乘以8,因为每个描述符8字节,作为索引访问中断描述符表,从中取出门描述符,门描述符中有中断处理过程的代码段选择子和段内偏移,和调用门一样,接着转移到相应的位置执行
;一般的中断处理可以使用中断门和陷阱门,回忆一下调用门的工作原理,它只是从任务的局部空间转移到更高特权级的全局空间中执行,本质上是一种任务内的控制转移行为
;与此相同中断门和陷阱门允许在任务内实施中断处理,转到全局空间中去执行一些系统级的管理工作,本质上也是任务内的控制转移行为
;但是当中断发生时,如果该中断号对应的门是任务门,那么性质就截然不同了,必须进行任务切换,即,要中断当前任务的执行,保护当前任务的现场,并转换到另一个任务去执行
;下图是任务门描述符的格式,相对于其他各种描述符,任务门描述符中的多数区域没有使用,显得特别简单
;
; 31                                          16 15 14 13 12 11         8  7            0 高32位
;|                      不使用                   | P | DPL| S |  0 0 1 0 1 |     不使用    |
;
; 31                                          16 15                                     0 低32位
;|                  TSS选择子                    |                 不使用                  |
;
;任务门描述符的主要成分是任务的TSS选择子,任务门用于在中断发生时执行任务切换,而执行任务切换时必须找到新任务的TSS,所以任务门应当指向任务的TSS
;为了指向任务的TSS,只需要在任务门描述符中给出任务的TSS即可
;任务门描述符中的P位用于指示该门是否有效,当P位=0时,不允许通过此门实施任务切换
;DPL是任务门描述符的特权级,但是对因中断而发起的任务切换不起作用,处理器不按特权级施加任何保护,但是这并不意味着DPL字段没有用,当以非中断的方式通过任务门
;实施任务切换时,就有用了
;这样当中断发生时,处理器用中断号乘以8作为索引访问中断描述符表,当它发现这是一个任务门时,就知道应该发起任务切换,于是它取出任务门描述符
;再从任务门描述符中取出新任务的TSS选择子,接着再用TSS选择子访问GDT,取出新任务的TSS描述符
;在转到新任务执行前,处理器要先把当前任务的状态保存起来,当前任务的TSS是由任务寄存器TR所指向的,所以处理器把每个寄存器的"快照"保存到由TR指向的TSS中
;然后处理器访问新任务的TSS,从中恢复各个寄存器的内容,包括通用寄存器,标志寄存器,段寄存器,指令指针寄存器,栈指针寄存器,以及局部描述符表寄存器LDTR等
;最终,TR指向新任务的TSS,而处理器旋即开始执行新的任务,一旦新任务开始执行,处理器固件会自动将其TSS描述符的B位置1,表示该任务的状态为忙
;当中断发生时,可以执行常规的中断处理过程,也可以进行任务切换,尽管性质不同,但它们都要使用iret指令返回,前者是返回到同一任务的不同代码段,后者是返回到被中断的那个任务
;问题是处理器如何区分这两种截然不同的返回类型呢?
;
;如下图所示,32位处理器的EFLAGS有NT位(14),意思是嵌套任务标志(Nested Task Flag),每个任务的TSS中都有一个任务链接域(指向前一个任务的指针)
;可以填写为前一个任务的TSS描述符选择子,如果当前任务EFLAGS的NT位是1,则表示当前正在执行的任务嵌套于其他任务内,并且能够通过TSS任务链接域的指针返回到前一个任务
;
;   31                    21                            15  14  13  12  11   10   09   08   07   06   05  04   03  02   01   00
;   |     保留,设置为0    | ID |    |    |    |    |    | 0 | NT | IOPL | OF | DF | IF | TF | SF | ZF | 0 | AF | 0 | PF | 1 | CF |
;
;因中断而引发任务切换时,取决于当前任务(旧任务)是否嵌套于其他任务内,其EFLAGS寄存器的NT位可能是0,也可能是1,不过这无关紧要,因为处理器不会改变它,而是和其他寄存器一道,写入TSS中保护起来
;另外,当前任务(旧任务)肯定处于忙状态,其TSS描述符的B=1,在任务切换后同样保持不变
;对新任务的处理是,要把老任务的TSS选择子填写到新任务TSS中的任务链接域,同时将新任务EFLAGS寄存器的NT位置1,以允许返回(转换)到前一个任务(老任务)继续执行,同时还要把新任务TSS描述符的B位置1(忙)
;可以使用iret指令从当前任务返回(转换)到前一个任务,前提是当前任务EFLAGS的NT位必须是1,无论任何时候处理器碰到iret指令,它都要检查NT位,如果此位是0,表明是一般的中断过程,按一般的中断处理
;即,中断返回是任务内的(中断处理过程虽然属于操作系统,但属于任务的全局空间),如果此位是1,则表明当前任务之所以能够正在执行,是因为中断了别的任务,因此应当返回原先被中断的任务继续执行
;此时由处理器固件把当前任务的EFLAGS的NT位改成0,并把TSS描述符的B位改成0(非忙),在保存了当前任务的状态之后,接着用新任务(被中断的任务)的TSS恢复现场
;
;除了因中断引发的任务切换之外,还可以用远过程调用指令call,或者远跳转指令jmp直接发起任务切换,在这两种情况下,call和jmp指令的操作数是任务的TSS描述符选择子或任务门
;以下是两个例子:    call 0x0010:0x0000 0000
;                 jmp 0x0010:0x0000 0000
;当处理器执行这两条指令时,先用指令中给出的描述符选择子访问GDT,分析它的描述符类型,如果是一般的代码段描述符,就按普通的段间转移规则进行
;如果是调用门,按调用门的规则执行,如果是TSS描述符,或者任务门,则执行任务切换
;此时指令中给出的32位偏移量被忽略,原因是执行任务切换时,所有处理器的状态都可以从TSS中获得
;注意,任务门描述符可以安装在中断描述符表中,也可以安装在GDT或LDT中
;
;如果是用于发起任务切换,call指令和jmp指令也有不同之处,使用call指令发起的任务切换类似于因中断发起的任务切换,这就是说,由call指令发起的任务切换是嵌套的
;当前任务(旧任务)TSS描述符的B位保持原来的1不变,EFLAGS寄存器的NT位也不发生变化,新任务TSS描述符的B位置1,EFLAGS寄存器的NT位也置1,表示此任务嵌套于其他任务中
;同时,TSS任务链接域的内容改为旧任务的TSS描述符选择子
;如下图所示,假设任务1是整个系统中的第一个任务,当任务1开始执行时,其TSS描述符的B位是1,EFLAGS寄存器的NT位是0,不嵌套于其他任务
;当任务1转换到任务2时,任务1依旧为忙,EFLAGS寄存器的NT位不变(在其TSS中),任务2也变为忙,EFLAGS寄存器的NT位变为1,表示嵌套于任务1中,同时,任务1的TSS描述符选择子也被复制到任务2的TSS中(任务链接域)
;
;           任务1的TSS                         任务2的TSS                       任务3的TSS
;          TSS描述符B=1                       TSS描述符B=1                     TSS描述符B=1
;
;       ----------------------          ----------------------          ----------------------
;      |                      |        |                      |        |                      |
;       ----------------------          ----------------------          ----------------------
;      |     EFLAGS(NT=0)     |        |     EFLAGS(NT=1)     |        |     EFLAGS(NT=1)---------+
;       ----------------------          ----------------------          ----------------------    |     --------处理器---------
;      |                      |        |                      |        |                      |   |    |                      |
;      |                      |        |                      |        |                      |   +----------EFLAGS(NT=1)     |
;      |                      |        |                      |        |                      |        |                      |
;      |                      |        |                      |        |                      |        |                      |
;      |                      |        |                      |        |                      |        |                      |
;      |                      |        |                      |        |                      |        |   TR  |              |
;      |              |   0   |        |              |       |        |               |      |        |    |                 |
;       ----------------------          ----------------------          ------------------|---          ----|-----------------
;                        /|\                              |  /|\                          |  /|\            |
;                         +-------------------------------+   +---------------------------+   +-------------+指向当前任务
;
;最后是任务2转换到任务3执行,和从前一样,任务2保持忙的状态,EFALGS寄存器的NT位保持不变(在其TSS中),任务3称为当前任务,其TSS描述符的B位也变成1(忙),EFLAGS寄存器的NT位也变为1,同时,其TSS的任务链接域指向任务2
;用call指令发起的任务切换,可以通过iret指令返回到前一个任务,此时,旧任务的TSS描述符的B位,以及EFLAGS寄存器的NT位都恢复到0
;和call指令不同,使用jmp指令发起的任务切换,不会形成任务之间的嵌套关系,执行任务切换时,当前任务(旧任务)TSS描述符的B位清零,变为非忙状态
;EFLAGS寄存器的NT位不变,新任务TSS描述符的B位置1,进入忙状态,EFLAGS寄存器的NT位保持从TSS中加载时的状态不变
;
;任务是不可重入的
;任务不可重入的本质是,执行任务切换时,新任务的状态不能为忙,这里有两个典型的情形:
;   1.执行任务切换时,新任务不能是当前任务自己
;   2.如上图所示,不允许使用call指令从任务3切换到任务2和任务1上,如果不禁止这种情况的话,任务之间的嵌套关系将会因为TSS任务链接域的破坏而错乱
;处理器是通过TSS描述符的B位来检测重入的,因中断,iret,call,jmp指令发起的任务切换时,处理器固件会检测新任务TSS描述符的B位,如果为1,不允许这样的切换
;
;
;                               用call/jmp/iret指令发起任务切换的实例
;保护模式下的中断和异常中断处理要在第17章讲解,和中断有关的任务切换也将在第17章介绍,在本章重点关注的是用iret,call,jmp指令发起的任务切换
;回到代码,第938~945行是用来加载用户程序的,先分配一个任务控制块TCB,然后将它挂到TCB链上,接着压入用户程序的逻辑扇区及其TCB基地址,作为参数调用过程load_relocate_program
;过程load_relocate_program的工作和上一章相比没有太大变化,仅仅是对TSS的填写比较完整,注意,这是任务切换的要求,从一个任务切换到另一个任务时,处理器要从新任务的TSS中恢复(加载)各个寄存器的内容
;尽管这是任务的第一次执行,但是处理器并不知道,这是它的例行工作,你得把任务执行时,各个寄存器的内容放到TSS中供处理器加载
;新增加的指令是从第766行开始到790行结束的,首先从栈中取出TCB的基地址,然后通过4GB的内存段访问TCB,取出用户程序加载的起始地址,这也是用户程序头部的起始地址
;接着依次登记指令指针寄存器EIP和各个段寄存器的内容,因为这是用户程序的第一次执行,所以TSS中的EIP域应该等级用户程序的入口点,CS域应该登记用户程序入口点所在的代码段选择子
;第787~790行,先将EFLAGS寄存器的内容压入栈,再将其弹出到EDX寄存器,因为不存在将标志寄存器的内容完整传送到通用寄存器的指令
;接着把EDX的内容写入TSS中EFLAGS域,注意,这是当前任务(程序管理器)EFLAGS寄存器的副本,新任务将使用这个副本作为初始化的EFLAGS寄存器
;一般来说,此时的EFLAGS寄存器的IOPL字段为00,将来新任务开始执行时,会用这个副本作为处理器EFLAGS寄存器的当前值,并因此而没有足够的I/O特权
;回到第947行
;这是一条32位间接远调用指令call,操作数是一个内存地址,指向TCB内的0x14单元,这样的指令我们很熟悉,一般来说,转移到的目标位置可以由16位的代码段选择子和32位段内偏移量组成,也可以由16位的调用门选择子和32位偏移量组成
;所以,从TCB内偏移量为0x14的地方,应当先是一个32位的段内偏移量,接着是一个16位的代码段或者调用门选择子
;但是回到前一章的图,如下图
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
;TCB内偏移量为0x14的地方,是任务的TSS基地址,再往后是TSS选择子,很奇怪却是合法的,当处理器发现得到的是一个TSS选择子,就执行任务切换
;和通过调用门的控制转移一样,32位偏移部分丢弃不用,这就是为什么我们可以把TSS基地址作为32位偏移量使用的原因
;当执行任务切换时,处理器用得到的选择子访问GDT,一旦它发现那是一个TSS描述符,就知道应该执行任务切换的操作,首先,因为当前正在执行的任务是由任务寄存器TR指示的,所以它要把每个寄存器的"快照"保存到由TR指向的TSS中
;然后处理器用指令中给出的TSS选择子访问GDT,取得新任务的TSS描述符,并从该TSS中恢复各个寄存器的内容,包括通用寄存器,标志寄存器,段寄存器,指令指针寄存器,栈指针寄存器,以及局部描述符表寄存器LDTR等
;最终,TR指向新任务的TSS,而处理器旋即开始执行新的任务
;幸亏我们已经在load_relocate_program过程内完整地设置了新任务的TSS,尤其是它的LDT域,EIP域,CS域,DS域,LDT域指向用户程序的局部描述符表,EIP域指向用户程序的入口点,CS域指向用户程序的代码段,DS域指向用户程序的头部段
;程序管理器是计算机启动以来的第一个任务,在任务切换前,其TSS描述符的B位=1,EFLAGS的NT位是0,因为本次任务切换是用call指令发起的,因此任务切换后,其TSS描述符的B位还是1,EFLAGS的NT位不变
;当任务切换完成,用户任务成为当前任务,其TSS描述符的B位置1,表示该任务的状态为忙,EFLAGS的NT位置1,表示它嵌套于程序管理器任务,TSS的任务链接域被修改为前一个任务(程序管理器任务)的TSS描述符选择子
;现在用户程序作为任务开始执行了,所以,转到代码清单c15.asm
;总体上用户程序结构和上一章相比没有什么变化,而且功能非常简单,大部分工作都是通过调用门来完成的,程序的入口点在第55行
;当用户任务开始执行时,段寄存器DS指向头部段,第57~58行令FS指向头部段,其目的是保存指向头部段的指针以备后用,同时,腾出DS来完成后续操作
;第60~61行,令DS指向当前任务自己的数据段
;接下来的额工作是显示问候语,并报告自己的当前特权级,因为当前特权级是计算出来的,所以字符串要分成两部分显示,第63~64行先显示一部分
;   [USER TASK]: Hi! nice to meet you,I am run at CPL=
;这一句话没有说完,因为这个CPL需要计算才能知道
;第66~69行,计算当前特权级,转换成ASCII码后填写到数据段,作为第二个字符串的第一个字符,当前特权级别是由CS当前内容的低2位指示的,因此先将CS中的内容传送到AX
;接着清除AX的高6位,保留低2位,最后将这个数值加上0x30转换成可显示和打印的ASCII码,并填写到数据段中由标号message_2所指示的字节单元中
;第71~72行,显示包括特权级数值在内的第二个字符串,据我们所知,当前任务的特权级是3,因此显示的完整内容是:
;   [USER TASK]: Hi! nice to meet you,I am run at CPL=3.Now,I must exit...
;
;通过在中断描述符表中安装任务门,可以在中断信号的驱使下周期性地发起任务切换,否则.每个任务都应该在适当时候主动转换到其他任务,以免计算机的操作者发现别的任务都僵在那里
;如果每个任务都能自觉做到这点,那么这种任务切换机制被称为是协同式的
;一般来说可以在任务内的任何地方设置一条任务切换指令,以发起任务切换,当然,如果你是为某个流行的操作系统写程序,必须听从操作系统设计者的建议
;当前任务的做法稍有特殊,在显示了消息之后,第74行,通过调用门转到全局空间执行,从该调用门的符号名"TerminateProgram"上看,意图是终止当前任务的执行,而不是临时转换其他任务
;不管怎么样,回到内核代码中,看看任务在进入全局空间之后都做了什么
;用户程序通过调用门进入任务的全局空间后,实际的入口点在第354行,即,名字为terminate_current_task的过程,该过程用来结束当前任务的执行,并转换到其他任务
;
;不要忘了现在还处在用户任务中,要结束当前的用户任务,可以先切换到程序管理器任务,然后回收用户程序所占用的内存空间,并保证不再转换到该任务
;为了切换到程序管理器任务,需要根据当前任务EFALGS寄存器的NT位决定是采用iret指令还是jmp指令
;第358~360行,先将EFLAGS的当前内容压栈,然后用ESP作为地址操作数访问栈,取得EFLAGS的压栈值,并传送到EDX寄存器,接着将ESP的内容加上4,使栈平衡,保持压入EFLAGS寄存器前的状态
;可能会奇怪为什么不直接使用下面两条指令来完成以上功能:
;       pushfd
;       pop edx
;的确这两种做法的效果是一样的,之所以采用3条指令,是因为想演示如何通过ESP直接访问栈,在16位模式下,不能使用SP作为基址,所以下面的指令是错误的
;       mov ax,[sp]     ;错误
;注意,使用ESP作为指令的地址操作数时,默认使用的段寄存器是SS,即,访问栈段
;第362~363行,令DS指向内核数据段,方便后续操作
;DX寄存器包含了EFLAGS的低16位,其中位14是NT位,第365~366行,测试DX的位14,看NT位是0还是1,以决定采用哪种方式(iret或call)回到程序管理器任务
;因为当前任务是嵌套在程序管理器任务内的,所以NT位必然是1,应当转到标号.b1处继续执行
;第372~373行,也就是标号.b1处,先显示字符串
;   [SYSTEM CORE]: Uh...This task initiated with CALL instruction or an exeception/ interrupt,should use IRETD instruction to switch back...
;该字符串位于第448行,在内核数据段内,用标号core_msg0声明并初始化的,该字符串的内容显示,消息来源同样是系统内核
;第374行,通过iret指令转换到前一个任务,即程序管理器任务,执行任务切换时,当前用户任务的TSS描述符的B位被清零,EFLAGS的NT位也被清零,并被保存到它的TSS中
;注意,在此处,我们用的是iretd而不是iret,实际上,这是同一条指令,机器码都是CF,在16位模式下,iret指令的操作数默认是16位的,要按32位操作数执行,必须加指令前缀0x66,即,66 CF
;为了方便,编译器创造了iretd,当在16位模式下使用iretd时,编译器就知道应当加上指令前缀0x66,在32位模式下,iret和iretd是相同的,下面是例子
;       [bits 16]
;       iret        ;编译后的机器码为CF
;       iretd       ;编译后的机器码为66 CF
;
;       [bits 32]
;       iret        ;编译后的机器码为CF
;       iretd       ;编译后的机器码为CF
;
;当程序管理器任务恢复执行时,它的所有原始状态都从TSS中加载到处理器,包括指令指针寄存器EIP,它指向第952行的那条指令,紧接当初发起任务切换的那条指令
;对于刚刚被挂起的那个旧任务,如果它没有被终止执行,则可以不予理会,并在下一个适当的实际再次切换到它那里执行,不过现在的情况是它希望自己被终止,所以理论上接下来的工作是回收它所占用的内存空间
;并从任务控制块TCB链上去掉,以确保不会再切换到该任务执行(当然,现在TCB链还没有体现出自己的用处),遗憾的是,我们并没有提供这样的代码
;所以这个任务将一直存在,一直有效,不会消失,在整个系统的运行期间可以随时切换过去
;接下来我们再创建一个新任务并转移到该任务执行
;第952~953行,程序管理器先显示一条消息,标号prgman_msg2的位置是在第439行,位于内核数据段,在那里初始化了字符串
;   [PROGRAM MANAGER]: I am glad to regain control.Now,create another user task and switch to it by the JMP instruction...
;这是程序管理器在说话,方括号中的文字显示了消息的来源,意思是"我很高兴又获得了控制,现在,创建其他用户任务,并使用jmp指令切换到它那里"
;第955~964行,创建新的用户任务并发起任务切换,与上次相比,这次的任务切换有几个值得注意的特点,首先可以看出该任务也是从硬盘的50号逻辑扇区开始加载的,也就是说它和上一个用户任务一样,来自同一个程序
;这就很清楚地说明了,一个程序可以对应着多个运行中的副本,或者说多个任务,尽管如此,它们彼此却没有任何关系,在内存中的位置不同,运行状态也不一样
;其次,这是用jmp指令发起的任务切换,新任务不会嵌套于旧任务中,任务切换之后,程序管理器任务TSS描述符的B位被清零,EFLAGS的NT位不变,保持它从TSS加载时的状态,任务链接域的内容不变
;由于两个任务来自同一个程序,故完成相同的工作,最终都会通过调用门进入任务的全局空间执行,而且在执行到第365~366行时,EFLAGS的NT位测试结果必定是0,NT=0,当前任务并未嵌套于其他任务中
;于是执行第367~369行,首先显示字符串
;   [SYSTEM CORE]: Uh...This task initiated with JMP instruction,  should switch to Program Manager directly by the JMP instruction...
;方括号内显示了消息的来源,即系统内核,该消息的意思是,"唔......该任务使用jmp指令发起的,应当直接用jmp指令转换到程序管理器......"
;然后使用32位间接远转移指令jmp转换到程序管理器任务,指令中的标号prgman_tss位于内核数据段第431行,在那里初始化了6字节,即16位的TSS描述符选择子和32位的TSS基地址
;按道理这里不应该是TSS基地址,而应当是一个32位偏移量,不过这是无所谓的,当处理器看到选择子部分是一个TSS描述符选择子时,它将偏移量丢弃不用
;从第二个任务返回程序管理器任务时,执行点在第966行,从这一行开始一直到第969行,用于显示一条消息,然后停机,消息的内容是:
;   [PROGRAM MANAGER]: I am gain control again,HALT...
;消息的来源是程序管理器任务,它说,我又获得了控制,停机......
;最后处理器执行halt指令,停机
;
;
;                               处理器在实施任务切换时的操作
;处理器用以下四种方法将控制转换到其他任务
;   1.当前程序,任务或过程执行一个将控制转移到GDT内某个TSS描述符的jmp或者call指令
;   2.当前程序,任务或过程执行一个将控制转移到GDT或者当前LDT内某个任务门描述符的jmp或者call指令
;   3.一个异常或者中断发生,中断号指向中断描述符表内的任务门
;   4.在EFLAGS寄存器的NT位置位的情况下,当前任务执行了一个iret指令
;
;jmp,call,iret指令或者异常和中断,是程序重定向的基址,它们所引用的TSS描述符或者任务门,以及EFLAGS寄存器NT标志的状态,决定了任务切换是否,以及如何发生
;在任务切换时,处理器执行以下操作:
;   1.从jmp或call指令的操作数,任务门或者当前任务的TSS任务链接域取得新任务的TSS描述符选择子,最后一种方法适用于以iret指令发起的任务切换
;   2.检查是否允许从当前任务(旧任务)切换到新任务,数据访问的特权级检查规则适用于jmp和call指令,当前(旧)任务的CPL和新任务段选择子的RPL必须在数值上小于或者等于目标TSS或者任务门的DPL
;异常,中断(除了以int n指令引发的中断)和iret指令引起的任务切换忽略目标任务门或者TSS描述符的DPL,对于int n指令产生的中断,要检查DPL
;   3.检查新任务的TSS描述符是否已经标记为有效(P=1),并且界限也有效(大于或等于0x67,十进制103)
;   4.检查新任务是否可用,不忙(B=0,对于以call,jmp,异常或者中断发起的任务切换)或者忙(B=1,对于以iret发起的任务切换)
;   5.检查当前任务(旧任务)和新任务的TSS,以及所有在任务切换时用到的段描述符已经安排到系统内存中
;   6.如果任务切换是由jmp或者iret发起的,处理器清除当前(旧)任务的忙(B)标志,如果是由call指令,异常或者中断发起的,忙(B)标志保持原来的置位状态
;   7.如果任务切换是由iret指令发起的,处理器建立EFLAGS的一个临时副本并清除其NT标志,如果是由call指令,jmp指令,异常或者中断发起的,副本中的NT标志不变
;   8.保存当前(旧)任务的状态到它的TSS中,处理器从任务寄存器中找到当前TSS的基地址,然后将以下寄存器的状态复制到当前TSS中,所有通用寄存器,段寄存器中的段选择子,刚才哪个EFLAGS的副本,以及指令指针寄存器EIP
;   9.如果任务切换是由call指令,异常或者中断发起的,处理器把从新任务加载的EFLAGS寄存器的NT标志置位,如果是由iret或者jmp指令发起的,NT标志位的状态对应着从新任务加载的EFLAGS寄存器的NT位
;   10.如果任务切换是由call指令,jmp指令,异常或者中断发起的,处理器将新任务TSS描述符中的B位置位,如果是由iret指令发起的,B位保持原先的置位状态不变
;   11.用新任务的TSS选择子和TSS描述符加载任务寄存器TR
;   12.新任务的TSS状态数据被加载到处理器,这包括LDTR寄存器,PDBR(控制寄存器CR3),EFLAGS寄存器,EIP寄存器,通用寄存器,以及段选择子
;      载入状态期间只要发生一个故障,架构状态就会被破坏(因为有些寄存器的内容已经被改变,而且无法撤销和回退)
;      所谓架构是指处理器对外公开的那一部分规格和构造,所谓架构状态是指处理器内部的各种构件,在不同的条件下所建立起来的确定状态,当处理器处于某种状态时,再施加另一种确定的条件,可以进入另一种确定的状态
;      这应当是严格的,众所周知的,可预见的,否则就意味着架构状态遭到了破坏
;   13.与段选择子相对应的描述符在经过验证后也被加载,与加载和验证新任务环境有关的任何错误都将破坏架构状态,注意,如果所有的检查和保护工作都已经成功实施,处理器提交任务切换
;      如果在从第一步到第11步的过程中发生了不可恢复性的错误,处理器不能完成任务切换,并确保处理器返回到执行发起任务切换的那条指令前的状态,如果在第12步发生了不可恢复性的错误,架构状态将被破坏
;      如果在提交点第13步之后发生了不可恢复性的错误,处理器完成任务切换并在开始执行新任务之前产生一个相应的异常
;   13.开始执行新任务
;
;在任务切换时,当前任务的状态总要被保存起来,在恢复执行时,处理器从EIP寄存器的保存值所指向的那条指令开始执行,这个寄存器的值是在当初任务被挂起时保存的
;任务切换时,新任务的特权级别并不是从那个被挂起的任务继承来的,新任务的特权级是由其段寄存器CS的低2位决定的,而该寄存器的内容取自新任务的TSS,因为每个任务都有自己的独立的地址空间和任务状态段TSS
;所以任务之间是彼此隔离的,只需要用特权级规则控制对TSS的访问就行,软件不需要在任务切换时进行显式的特权级检查
;任务状态段TSS的任务链接域和EFLAGS寄存器的NT位用于返回前一个任务执行,当前EFLAGS寄存器的NT位是1,表明当前任务嵌套于其他任务中
;无论如何,新任务的TSS描述符的B位都会被置位,旧任务的B位取决于任务切换的方法,下表给出了不同条件下,B位,NT位和任务链接域的变化情况
;
;
;
;     标志或TSS任务链接域                   jmp指令的影响                   call指令或中断的影响                iret指令的影响
;
;       新任务的B位                       置位,原先必须为0                    置位,原先必须为0                不变,原先必须被置位
;
;       旧任务的B位                            清零                         不变,原先必须是置位的                   清零
;
;       新任务的NT标志                  设置为新任务TSS中的对应值                   置位                    设置为新任务TSS中的对应值
;
;       旧任务的NT标志                         不变                              不变                             清零
;
;       新任务的任务链接域                      不变                      用旧任务的TSS描述符选择子加载                不变
;
;       旧任务的任务链接域                      不变                              不变                             不变
;
;
;主引导扇区使用13章的,内核写入逻辑1扇区,用户程序写入逻辑50扇区