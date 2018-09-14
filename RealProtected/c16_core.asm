         ;代码清单16-1
         ;文件名：c16_core.asm
         ;文件说明：保护模式微型核心程序 
         ;创建日期：2012-06-20 00:05

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
         mov eax,video_ram_seg_sel          ;0x800b8000段的选择子
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
allocate_a_4k_page:                         ;分配一个4KB的页
                                            ;输入：无
                                            ;输出：EAX=页的物理地址
         push ebx
         push ecx
         push edx
         push ds
         
         mov eax,core_data_seg_sel
         mov ds,eax
         
         xor eax,eax
  .b1:
         bts [page_bit_map],eax
         jnc .b2
         inc eax
         cmp eax,page_map_len*8
         jl .b1
         
         mov ebx,message_3
         call sys_routine_seg_sel:put_string
         hlt                                ;没有可以分配的页，停机 
         
  .b2:
         shl eax,12                         ;乘以4096（0x1000） 
         
         pop ds
         pop edx
         pop ecx
         pop ebx
         
         ret
         
;-------------------------------------------------------------------------------
alloc_inst_a_page:                          ;分配一个页，并安装在当前活动的
                                            ;层级分页结构中
                                            ;输入：EBX=页的线性地址
         push eax
         push ebx
         push esi
         push ds
         
         mov eax,mem_0_4_gb_seg_sel
         mov ds,eax
         
         ;检查该线性地址所对应的页表是否存在
         mov esi,ebx
         and esi,0xffc00000
         shr esi,20                         ;得到页目录索引，并乘以4 
         or esi,0xfffff000                  ;页目录自身的线性地址+表内偏移 

         test dword [esi],0x00000001        ;P位是否为“1”。检查该线性地址是 
         jnz .b1                            ;否已经有对应的页表
          
         ;创建该线性地址所对应的页表 
         call allocate_a_4k_page            ;分配一个页做为页表 
         or eax,0x00000007
         mov [esi],eax                      ;在页目录中登记该页表
          
  .b1:
         ;开始访问该线性地址所对应的页表 
         mov esi,ebx
         shr esi,10
         and esi,0x003ff000                 ;或者0xfffff000，因高10位是零 
         or esi,0xffc00000                  ;得到该页表的线性地址
         
         ;得到该线性地址在页表内的对应条目（页表项） 
         and ebx,0x003ff000
         shr ebx,10                         ;相当于右移12位，再乘以4
         or esi,ebx                         ;页表项的线性地址 
         call allocate_a_4k_page            ;分配一个页，这才是要安装的页
         or eax,0x00000007
         mov [esi],eax 
          
         pop ds
         pop esi
         pop ebx
         pop eax
         
         retf  

;-------------------------------------------------------------------------------
create_copy_cur_pdir:                       ;创建新页目录，并复制当前页目录内容
                                            ;输入：无
                                            ;输出：EAX=新页目录的物理地址 
         push ds
         push es
         push esi
         push edi
         push ebx
         push ecx
         
         mov ebx,mem_0_4_gb_seg_sel
         mov ds,ebx
         mov es,ebx
         
         call allocate_a_4k_page            
         mov ebx,eax
         or ebx,0x00000007
         mov [0xfffffff8],ebx
         
         mov esi,0xfffff000                 ;ESI->当前页目录的线性地址
         mov edi,0xffffe000                 ;EDI->新页目录的线性地址
         mov ecx,1024                       ;ECX=要复制的目录项数
         cld
         repe movsd 
         
         pop ecx
         pop ebx
         pop edi
         pop esi
         pop es
         pop ds
         
         retf
         
;-------------------------------------------------------------------------------
terminate_current_task:                     ;终止当前任务
                                            ;注意，执行此例程时，当前任务仍在
                                            ;运行中。此例程其实也是当前任务的
                                            ;一部分 
         mov eax,core_data_seg_sel
         mov ds,eax

         pushfd
         pop edx
 
         test dx,0100_0000_0000_0000B       ;测试NT位
         jnz .b1                            ;当前任务是嵌套的，到.b1执行iretd 
         jmp far [program_man_tss]          ;程序管理器任务 
  .b1: 
         iretd

sys_routine_end:

;===============================================================================
SECTION core_data vstart=0                  ;系统核心的数据段 
;------------------------------------------------------------------------------- 
         pgdt             dw  0             ;用于设置和修改GDT 
                          dd  0

         page_bit_map     db  0xff,0xff,0xff,0xff,0xff,0x55,0x55,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                          db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                          db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
         page_map_len     equ $-page_bit_map
                          
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

         message_0        db  '  Working in system core,protect mode.'
                          db  0x0d,0x0a,0

         message_1        db  '  Paging is enabled.System core is mapped to'
                          db  ' address 0x80000000.',0x0d,0x0a,0
         
         message_2        db  0x0d,0x0a
                          db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0
         
         message_3        db  '********No more pages********',0
         
         message_4        db  0x0d,0x0a,'  Task switching...@_@',0x0d,0x0a,0
         
         message_5        db  0x0d,0x0a,'  Processor HALT.',0
         
        
         bin_hex          db '0123456789ABCDEF'
                                            ;put_hex_dword子过程用的查找表 

         core_buf   times 512 db 0          ;内核用的缓冲区

         cpu_brnd0        db 0x0d,0x0a,'  ',0
         cpu_brand  times 52 db 0
         cpu_brnd1        db 0x0d,0x0a,0x0d,0x0a,0

         ;任务控制块链
         tcb_chain        dd  0

         ;内核信息
         core_next_laddr  dd  0x80100000    ;内核空间中下一个可分配的线性地址        
         program_man_tss  dd  0             ;程序管理器的TSS描述符选择子 
                          dw  0

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
      
         ;清空当前页目录的前半部分（对应低2GB的局部地址空间） 
         mov ebx,0xfffff000
         xor esi,esi
  .b1:
         mov dword [es:ebx+esi*4],0x00000000
         inc esi
         cmp esi,512
         jl .b1
         
         ;以下开始分配内存并加载用户程序
         mov eax,core_data_seg_sel
         mov ds,eax                         ;切换DS到内核数据段

         mov eax,[ebp+12*4]                 ;从堆栈中取出用户程序起始扇区号
         mov ebx,core_buf                   ;读取程序头部数据
         call sys_routine_seg_sel:read_hard_disk_0

         ;以下判断整个程序有多大
         mov eax,[core_buf]                 ;程序尺寸
         mov ebx,eax
         and ebx,0xfffff000                 ;使之4KB对齐 
         add ebx,0x1000                        
         test eax,0x00000fff                ;程序的大小正好是4KB的倍数吗? 
         cmovnz eax,ebx                     ;不是。使用凑整的结果

         mov ecx,eax
         shr ecx,12                         ;程序占用的总4KB页数 
         
         mov eax,mem_0_4_gb_seg_sel         ;切换DS到0-4GB的段
         mov ds,eax

         mov eax,[ebp+12*4]                 ;起始扇区号
         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址
  .b2:
         mov ebx,[es:esi+0x06]              ;取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page

         push ecx
         mov ecx,8
  .b3:
         call sys_routine_seg_sel:read_hard_disk_0
         inc eax
         loop .b3

         pop ecx
         loop .b2

         ;在内核地址空间内创建用户任务的TSS
         mov eax,core_data_seg_sel          ;切换DS到内核数据段
         mov ds,eax

         mov ebx,[core_next_laddr]          ;用户任务的TSS必须在全局空间上分配 
         call sys_routine_seg_sel:alloc_inst_a_page
         add dword [core_next_laddr],4096
         
         mov [es:esi+0x14],ebx              ;在TCB中填写TSS的线性地址 
         mov word [es:esi+0x12],103         ;在TCB中填写TSS的界限值 
          
         ;在用户任务的局部地址空间内创建LDT 
         mov ebx,[es:esi+0x06]              ;从TCB中取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page
         mov [es:esi+0x0c],ebx              ;填写LDT线性地址到TCB中 

         ;建立程序代码段描述符
         mov eax,0x00000000
         mov ebx,0x000fffff                 
         mov ecx,0x00c0f800                 ;4KB粒度的代码段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         
         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+76],cx                 ;填写TSS的CS域 

         ;建立程序数据段描述符
         mov eax,0x00000000
         mov ebx,0x000fffff                 
         mov ecx,0x00c0f200                 ;4KB粒度的数据段描述符，特权级3
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0011B         ;设置选择子的特权级为3
         
         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+84],cx                 ;填写TSS的DS域 
         mov [es:ebx+72],cx                 ;填写TSS的ES域
         mov [es:ebx+88],cx                 ;填写TSS的FS域
         mov [es:ebx+92],cx                 ;填写TSS的GS域
         
         ;将数据段作为用户任务的3特权级固有堆栈 
         mov ebx,[es:esi+0x06]              ;从TCB中取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page
         
         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+80],cx                 ;填写TSS的SS域
         mov edx,[es:esi+0x06]              ;堆栈的高端线性地址 
         mov [es:ebx+56],edx                ;填写TSS的ESP域 

         ;在用户任务的局部地址空间内创建0特权级堆栈
         mov ebx,[es:esi+0x06]              ;从TCB中取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page

         mov eax,0x00000000
         mov ebx,0x000fffff
         mov ecx,0x00c09200                 ;4KB粒度的堆栈段描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0000B         ;设置选择子的特权级为0

         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+8],cx                  ;填写TSS的SS0域
         mov edx,[es:esi+0x06]              ;堆栈的高端线性地址
         mov [es:ebx+4],edx                 ;填写TSS的ESP0域 

         ;在用户任务的局部地址空间内创建1特权级堆栈
         mov ebx,[es:esi+0x06]              ;从TCB中取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page

         mov eax,0x00000000
         mov ebx,0x000fffff
         mov ecx,0x00c0b200                 ;4KB粒度的堆栈段描述符，特权级1
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0001B         ;设置选择子的特权级为1

         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+16],cx                 ;填写TSS的SS1域
         mov edx,[es:esi+0x06]              ;堆栈的高端线性地址
         mov [es:ebx+12],edx                ;填写TSS的ESP1域 

         ;在用户任务的局部地址空间内创建2特权级堆栈
         mov ebx,[es:esi+0x06]              ;从TCB中取得可用的线性地址
         add dword [es:esi+0x06],0x1000
         call sys_routine_seg_sel:alloc_inst_a_page

         mov eax,0x00000000
         mov ebx,0x000fffff
         mov ecx,0x00c0d200                 ;4KB粒度的堆栈段描述符，特权级2
         call sys_routine_seg_sel:make_seg_descriptor
         mov ebx,esi                        ;TCB的基地址
         call fill_descriptor_in_ldt
         or cx,0000_0000_0000_0010B         ;设置选择子的特权级为2

         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+24],cx                 ;填写TSS的SS2域
         mov edx,[es:esi+0x06]              ;堆栈的高端线性地址
         mov [es:ebx+20],edx                ;填写TSS的ESP2域 


         ;重定位SALT 
         mov eax,mem_0_4_gb_seg_sel         ;访问任务的4GB虚拟地址空间时用 
         mov es,eax                         
                                                    
         mov eax,core_data_seg_sel
         mov ds,eax
      
         cld

         mov ecx,[es:0x0c]                  ;U-SALT条目数 
         mov edi,[es:0x08]                  ;U-SALT在4GB空间内的偏移 
  .b4:
         push ecx
         push edi
      
         mov ecx,salt_items
         mov esi,salt
  .b5:
         push edi
         push esi
         push ecx

         mov ecx,64                         ;检索表中，每条目的比较次数 
         repe cmpsd                         ;每次比较4字节 
         jnz .b6
         mov eax,[esi]                      ;若匹配，则esi恰好指向其后的地址
         mov [es:edi-256],eax               ;将字符串改写成偏移地址 
         mov ax,[esi+4]
         or ax,0000000000000011B            ;以用户程序自己的特权级使用调用门
                                            ;故RPL=3 
         mov [es:edi-252],ax                ;回填调用门选择子 
  .b6:
      
         pop ecx
         pop esi
         add esi,salt_item_len
         pop edi                            ;从头比较 
         loop .b5
      
         pop edi
         add edi,256
         pop ecx
         loop .b4

         ;在GDT中登记LDT描述符
         mov esi,[ebp+11*4]                 ;从堆栈中取得TCB的基地址
         mov eax,[es:esi+0x0c]              ;LDT的起始线性地址
         movzx ebx,word [es:esi+0x0a]       ;LDT段界限
         mov ecx,0x00408200                 ;LDT描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x10],cx               ;登记LDT选择子到TCB中

         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov [es:ebx+96],cx                 ;填写TSS的LDT域 

         mov word [es:ebx+0],0              ;反向链=0
      
         mov dx,[es:esi+0x12]               ;段长度（界限）
         mov [es:ebx+102],dx                ;填写TSS的I/O位图偏移域 
      
         mov word [es:ebx+100],0            ;T=0
      
         mov eax,[es:0x04]                  ;从任务的4GB地址空间获取入口点 
         mov [es:ebx+32],eax                ;填写TSS的EIP域 

         pushfd
         pop edx
         mov [es:ebx+36],edx                ;填写TSS的EFLAGS域 

         ;在GDT中登记TSS描述符
         mov eax,[es:esi+0x14]              ;从TCB中获取TSS的起始线性地址
         movzx ebx,word [es:esi+0x12]       ;段长度（界限）
         mov ecx,0x00408900                 ;TSS描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [es:esi+0x18],cx               ;登记TSS选择子到TCB

         ;创建用户任务的页目录
         ;注意！页的分配和使用是由页位图决定的，可以不占用线性地址空间 
         call sys_routine_seg_sel:create_copy_cur_pdir
         mov ebx,[es:esi+0x14]              ;从TCB中获取TSS的线性地址
         mov dword [es:ebx+28],eax          ;填写TSS的CR3(PDBR)域
                   
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
start:
         mov ecx,core_data_seg_sel          ;令DS指向核心数据段 
         mov ds,ecx

         mov ecx,mem_0_4_gb_seg_sel         ;令ES指向4GB数据段 
         mov es,ecx

         mov ebx,message_0                    
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

         ;准备打开分页机制
         
         ;创建系统内核的页目录表PDT
         ;页目录表清零 
         mov ecx,1024                       ;1024个目录项
         mov ebx,0x00020000                 ;页目录的物理地址
         xor esi,esi
  .b1:
         mov dword [es:ebx+esi],0x00000000  ;页目录表项清零 
         add esi,4
         loop .b1
         
         ;在页目录内创建指向页目录自己的目录项
         mov dword [es:ebx+4092],0x00020003 

         ;在页目录内创建与线性地址0x00000000对应的目录项
         mov dword [es:ebx+0],0x00021003    ;写入目录项（页表的物理地址和属性）      

         ;创建与上面那个目录项相对应的页表，初始化页表项 
         mov ebx,0x00021000                 ;页表的物理地址
         xor eax,eax                        ;起始页的物理地址 
         xor esi,esi
  .b2:       
         mov edx,eax
         or edx,0x00000003                                                      
         mov [es:ebx+esi*4],edx             ;登记页的物理地址
         add eax,0x1000                     ;下一个相邻页的物理地址 
         inc esi
         cmp esi,256                        ;仅低端1MB内存对应的页才是有效的 
         jl .b2
         
  .b3:                                      ;其余的页表项置为无效
         mov dword [es:ebx+esi*4],0x00000000  
         inc esi
         cmp esi,1024
         jl .b3 

         ;令CR3寄存器指向页目录，并正式开启页功能 
         mov eax,0x00020000                 ;PCD=PWT=0
         mov cr3,eax

         mov eax,cr0
         or eax,0x80000000
         mov cr0,eax                        ;开启分页机制

         ;在页目录内创建与线性地址0x80000000对应的目录项
         mov ebx,0xfffff000                 ;页目录自己的线性地址 
         mov esi,0x80000000                 ;映射的起始地址
         shr esi,22                         ;线性地址的高10位是目录索引
         shl esi,2
         mov dword [es:ebx+esi],0x00021003  ;写入目录项（页表的物理地址和属性）
                                            ;目标单元的线性地址为0xFFFFF200
                                             
         ;将GDT中的段描述符映射到线性地址0x80000000
         sgdt [pgdt]
         
         mov ebx,[pgdt+2]
         
         or dword [es:ebx+0x10+4],0x80000000
         or dword [es:ebx+0x18+4],0x80000000
         or dword [es:ebx+0x20+4],0x80000000
         or dword [es:ebx+0x28+4],0x80000000
         or dword [es:ebx+0x30+4],0x80000000
         or dword [es:ebx+0x38+4],0x80000000
         
         add dword [pgdt+2],0x80000000      ;GDTR也用的是线性地址 
         
         lgdt [pgdt]
        
         jmp core_code_seg_sel:flush        ;刷新段寄存器CS，启用高端线性地址 
                                             
   flush:
         mov eax,core_stack_seg_sel
         mov ss,eax
         
         mov eax,core_data_seg_sel
         mov ds,eax
          
         mov ebx,message_1
         call sys_routine_seg_sel:put_string

         ;以下开始安装为整个系统服务的调用门。特权级之间的控制转移必须使用门
         mov edi,salt                       ;C-SALT表的起始位置 
         mov ecx,salt_items                 ;C-SALT表的条目数量 
  .b4:
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
         loop .b4

         ;对门进行测试 
         mov ebx,message_2
         call far [salt_1+256]              ;通过门显示信息(偏移量将被忽略) 
      
         ;为程序管理器的TSS分配内存空间
         mov ebx,[core_next_laddr]
         call sys_routine_seg_sel:alloc_inst_a_page
         add dword [core_next_laddr],4096

         ;在程序管理器的TSS中设置必要的项目 
         mov word [es:ebx+0],0              ;反向链=0

         mov eax,cr3
         mov dword [es:ebx+28],eax          ;登记CR3(PDBR)

         mov word [es:ebx+96],0             ;没有LDT。处理器允许没有LDT的任务。
         mov word [es:ebx+100],0            ;T=0
         mov word [es:ebx+102],103          ;没有I/O位图。0特权级事实上不需要。
         
         ;创建程序管理器的TSS描述符，并安装到GDT中 
         mov eax,ebx                        ;TSS的起始线性地址
         mov ebx,103                        ;段长度（界限）
         mov ecx,0x00408900                 ;TSS描述符，特权级0
         call sys_routine_seg_sel:make_seg_descriptor
         call sys_routine_seg_sel:set_up_gdt_descriptor
         mov [program_man_tss+4],cx         ;保存程序管理器的TSS描述符选择子 

         ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
         ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
         ltr cx

         ;现在可认为“程序管理器”任务正执行中

         ;创建用户任务的任务控制块 
         mov ebx,[core_next_laddr]
         call sys_routine_seg_sel:alloc_inst_a_page
         add dword [core_next_laddr],4096
         
         mov dword [es:ebx+0x06],0          ;用户任务局部空间的分配从0开始。
         mov word [es:ebx+0x0a],0xffff      ;登记LDT初始的界限到TCB中
         mov ecx,ebx
         call append_to_tcb_link            ;将此TCB添加到TCB链中 
      
         push dword 50                      ;用户程序位于逻辑50扇区
         push ecx                           ;压入任务控制块起始线性地址 
       
         call load_relocate_program         
      
         mov ebx,message_4
         call sys_routine_seg_sel:put_string
         
         call far [es:ecx+0x14]             ;执行任务切换。
         
         mov ebx,message_5
         call sys_routine_seg_sel:put_string

         hlt
            
core_code_end:

;-------------------------------------------------------------------------------
SECTION core_trail
;-------------------------------------------------------------------------------
core_end:
;以下是注释说明:
;Intel处理器访问内存的基本策略是分段,在16位实模式下,段的起始位置必须对齐在16字节边界上,而且段的长度最大为64KB
;进入32位保护模式之后,进一步强化了分段功能,并提供了保护机制,此时段可以起始于任何位置,段的长度可以扩展到处理器的最大寻址范围边界
;典型地,早期的32位处理器拥有32根地址线,段的长度可以扩展到4GB
;在32位保护模式下,对段的访问本着先登记,后访问的原则进行,登记就是在GDT或LDT中登记段的描述符,规定了段的地址和边界,以及访问权限,访问时则需要拿着一个段描述符的选择子才行
;处理器用段界限和特权级来审查对段的访问,任何非法的造访行为都会被处理器阻止,并立即引发异常中断
;一般来说人们使用计算机要先安装一个操作系统,在这种情况下,段是由操作系统负责管理的,操作系统加载应用程序,根据程序的要求,为它创建一个或多个段,然后把控制权交给它
;当同时运行的程序和任务很多事,内存可能就不够用了,这时候操作系统的价值就体现出来了,每个段描述符有A位,每当访问一个段时,处理器会将其置位,A位的清零了由操作系统定时进行
;它可以借此机会统计段的访问频度,当内存不够用时,它可以将那些较少访问的段换出到磁盘上,以腾出空间给马上要运行的段使用,一旦某个段被挪到磁盘上上,操作系统应当将其描述符的P位清零
;过了一段时间后,当这个段又被访问时,引起描述符的P位是0,处理器引发段不存在的异常,中断号11,这类中断通常是由操作系统负责处理的,它会用同样的方法腾出空间,将这个段的内容从磁盘调入内存
;这类中断返回时,处理器会再次执行引发异常的那条指令(而不是下一条),于是程序又能继续执行了
;但是因为段的长度不定,在分配内存时可能会发生内存中的空闲区域小于要加载的段,或者空闲区域远远大于要加载的段,在前一种情况下,需要另外寻找合适的空闲区域
;在后一种情况下,分配会成功,但太过于浪费,为了解决这个问题,从80386开始,引入了分页机制
;分页功能从总体上说,是用长度固定的的页来代替长度不一定的段,借此解决因段长度不同而带来的内存空间管理问题,尽管操作系统也可以用软件来实施固定长度的内存分配,但太过于复杂,由处理器固件来做这件事,可以使速度和效率最大化
;本章的学习目标是:
;     1.了解页录,页表的结构和作用,清楚为什么当我们访问一个段中的某单元时,处理器能准确地知道它在哪个页,以及页内位置的基本原理
;     2.了解开启分页机制的方法和需要的准备工作
;     3.了解任务的全局空间和局部空间是如何与它的页目录建立映射关系的
;     4.学习按需分配页面(动态分配页面)的一般方法
;     5.因为在分页机制下无法使用物理地址工作,因此需要掌握用线性地址访问页目录表和页表,并修改目录项及页表项的手段
;     6.了解什么是平坦内存模型,学习如何在平坦模型下创建程序的段描述符,知道向上扩展的数据段也能作为栈段
;     7.学习用Bochs调试分页机制下的程序
;     8.新的X86指令,包括bts,btr,btc和bt
;
;                                               分页机制概述
;                                   简单的分页模型
;分段的内存管理模式是我们一贯的工作方式,如下图,在处理器中有负责分段管理的段部件,每个程序或任务都有自己的段,这些段都用段描述符定义,随着程序的执行,当要访问内存时
;就用段地址加上偏移量,段部件就会输出一个线性地址,在单纯的分段模式下,线性地址就是物理地址
;                                                                                                            |                   |
;                                                                                                            |                   |
;                                                                                                  0020 2008  -------------------
;                                                                                                            |                   |
;                                                              --------------------                          |                   |
;           描述符中的段基地址为0x0020 0000  ------段地址--------> |                   |                         |      内存段        |
;           界限为0x2007,字节粒度                                |                   |                         |    8200字节长度    |
;                                                              |  处理器的段管理部件  |--------线性地址--------->|                   |
;                                                              |                   |                         | 0x2007=十进制8199  |
;           mov edx,[0x1008]              -----偏移量-------->  |                   |                         |                   |
;                                                               -------------------                 0020 0000 -------------------
;                                                                                                            |                   |
;                                                                                                            |                   |
;一旦决定采用页式内存管理,就应当把4GB内存分成大小相同的页,但是页在物理内存中的位置是有讲究的,并不是在内存中随便找个位置定义页从这里开始,事实上并不是这样定义的
;页的最小单位是4KB,也就是4096字节,用十六进制表示就是0x1000,因此第一个页的物理地址是0x0000 0000,第二个页的物理地址是0x0000 1000,第三个页的物理地址是0x0000 2000
;最后一个页的物理地址是0xFFFF F000,这样可以将4GB内存划分为1048576(0x1000 0000)个页,很显然,页的物理地址低12位始终是全0
;段管理机制对于Intel处理器来说是最基本的,任何时候都无法关闭,也就是说,即使启用页管理功能,分段机制依然是起作用的,段部件也正常工作
;分页机制也没有增加程序员的负担,程序依然是按段来组织,问题在于如何将较大的段,映射到大小相同的页面上?
;
;
;                 差个图??????
;
;
;
;在分页模式下,操作系统可以创建一个为所有任务共用的4GB虚拟内存空间,也可以为每一个任务创建独立的4GB虚拟内存空间,这都是可行的
;当一个程序加载时,操作系统既要在左边的虚拟内存中分配段空间,又要在右边的物理内存中分配相应的页面,因此第一个步骤是寻找空闲的段空间
;该段空间既没有被其他程序使用,也没有被同一程序内的其他段使用,如上图所示,假设已经成功找到并分配了一个段空间,其基地址为0x0020 0000,长度为8200字节
;页的最小尺寸是4KB,也就是4096字节,因此8200字节的段,需要占用3个页面,其中最后一个页只用了8字节,其余的都浪费着,但这无关紧要,如果允许页共享,多个段或多个程序可以用同一个页来存放各自的数据
;在分段之后,操作系统的任务是把段拆开,并分别映射到物理页,注意,段必须是连续的,但不要求所分配的页都是连续的,挨在一起的,事实上在开机之后,会运行不同的程序,这都要分配页
;然后有些程序关闭了,页面要回收,几个回合下来,空闲的页零零散散地分布在物理内存中,一般不会是连续的,在分配页面时,操作系统会搜索那些空闲的页,并分配给程序使用,所分配的页面的总长度要大于等于段长度
;作为一个具体的例子,操作系统为程序分配了一个段,段是在虚拟内存中分配的,起始地址为0x0020 0000,该段有8200字节,需要分配3个页面,为此操作系统在物理内存中搜索可用的空闲页,还真找到了
;这三个页面的物理地址分别是0x0000 2000,0x0000 4000和0x0000 7000
;接下来要建立线性地址和页之间的对应关系,在图中,0x0020 0000~0x0020 0FFF对应着物理地址为0x0000 2000的页
;0x0020 1000~0x0020 1FFF对应着物理地址为0x0000 4000的页
;0x0020 2000~0x0020 2007对应着物理地址为0x0000 7000的页
;这只是示例,线性地址区间和页的对应关系可以随意
;4GB虚拟内存空间不可能用来保存任何数据,因为它是虚幻的,它只是用来指示内存的使用情况,当操作系统加载一个程序并创建为任务时,操作系统在虚拟内存空间寻找空闲的段,并映射到空闲的页
;然后到真正开始加载程序时,再把原来属于段的数据按页的尺寸拆开,分开写入对应的页中
;从段部件输出的是线性地址,或者叫虚拟地址,为了根据线性地址找到页的物理地址是,操作系统必须维护一张表,把线性地址转换成物理地址,这是一个反过程
;如下图所示,因为有1048576个页,所以转换表也有1048576项,这是个一维表格,每个表项占4字节,内容为页的物理地址,这个表格的用法是这样的:
;     因为页的尺寸是4KB,故线性地址的低12位可用于访问页内偏移,高20位可用于指定一个物理页,因此,把线性地址的高20位当成索引,乘以4作为表内偏移量,从表中取出一个双字,那就是该线性地址所对应的页的物理地址
;如下图所示,如果执行指令:     mov edx,[0x2002]
;那么段部件用段地址0x0020 0000加上指令中给出的偏移量0x2002,得到线性地址0x0020 2002
;线性地址的高20位是表格索引,即0x0 0202,将索引乘以4,得到0x0 0808,这就是表内偏移,如图,从该单元可以取出一个双字0x0000 7000,这就是页物理地址
;线性地址的第12位是页内偏移量,用页物理地址加上页内偏移量,就是最终的物理内存地址,0x0000 7000加上0x002,得到0x0000 7002,这就是实际要访问的物理内存地址
;问题在于,为什么在表内偏移量为0x0 0808的地方,会恰好是页地址0x0000 7000,而不是其他页地址呢?当程序加载时,操作系统会首先在虚拟内存中分配段,然后根据段需要分成多少页,来搜索空闲页面
;当段较大时,要按页的尺寸分成几个地址区段,操作系统用每
;
;
;
;
;
;
;
;
;
;
;
;                                   页目录,页表和页
;
;
;
;
;
;
;
;
;
;
;                                   地址变换的具体过程
;
;
;
;
;
;
;                                               使内核在分页机制下工作
;                                   创建内核的页目录表和页表
;
;
;
;
;
;
;
;
;
;
;
;                                   任务全局空间和局部空间的页面映射
;
;
;
;
;
;
;
;
;
;                                               创建内核任务
;                                   内核的虚拟内存分配
;
;
;
;
;
;
;
;
;
;
;
;
;                                   页面位映射串和空闲页的查找
;
;
;
;
;
;
;
;
;
;
;                                   创建页表并登记分配的页
;
;
;
;
;
;
;
;
;
;                                   创建内核任务的TSS
;
;
;
;
;
;
;
;
;                                               用户任务的创建和切换
;                                   多段模型和段页式内存管理
;
;
;
;
;
;
;                                   平坦模型和用户程序的结构
;
;
;
;
;
;
;
;
;                                   用户任务的虚拟地址空间分配
;
;
;
;
;
;
;
;
;
;                                   用户程序的加载
;
;
;
;
;
;
;
;
;
;                                   段描述符的创建(平坦模型)
;
;
;
;
;
;
;
;
;
;
;
;
;                                   重定位U-SALT并复制页目录表
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;                                   切换到用户任务执行
;
;
;
;
;
;
;
;
;
;
;                                               程序的编译,执行和调试
;                                   本章程序的编译和运行方法
;
;
;
;                                   查看CR3寄存器的内容
;
;
;
;
;
;                                   查看线性地址对应的物理页信息
;
;
;
;
;
;
;
;                                   查看当前任务的页表信息
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;
;                                   使用线性(虚拟)地址调试程序
;
;
;
;
;
;
;
;
;