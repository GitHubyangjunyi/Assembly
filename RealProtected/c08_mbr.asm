         ;代码清单8-1
         ;文件名：c08_mbr.asm
         ;文件说明：硬盘主引导扇区代码（加载程序） ,c08_mbr.bin写入虚拟硬盘主引导扇区,c08.bin写入逻辑100扇区
         ;创建日期：2011-5-5 18:17
         
         app_lba_start equ 100           ;使用伪指令equ声明常数app_lba_start=100D（用户程序起始逻辑扇区号）
                                         ;常数的声明不会占用汇编地址
                                    
SECTION mbr align=16 vstart=0x7c00       ;将主引导扇区定义成一个段,即使不定义这个段,编译器也会自动将其看成一个段,因为定义中的vstart=0x7c00子句使其发挥作用
                                         ;段内所有元素的汇编地址都将从0x7c00开始计算,因为主引导程序的实际加载地址是0x0000:0x7c00,引用标号时可不用加上0x7c00,方便许多
         ;设置堆栈段和栈指针,栈的段地址是0x0000,栈指针将在段内0x0000和0xFFFF之间变化,段的长度是64KB
         mov ax,0      
         mov ss,ax
         mov sp,ax                       ;ds和es有其他用途所以使用cs作为段超越前缀,现在cs=0x0000,MBR位于0x0000:0x7c00处,所以理论上指令中的偏移地址应当是0x7c00+phy_base
;不过在定义段时使用了vstart=0x7c00,所以段内所有的汇编地址都是在0x7c00基础上增加的,不用加上0x7c00
         mov ax,[cs:phy_base]            ;计算用于加载用户程序的逻辑段地址,用户程序将从此处加载,该地址实际上保存在标号phy_base处的一个双字单元里,8086是16位处理器,只能用两个寄存器存放
         mov dx,[cs:phy_base+0x02]       ;这里硬盘使用LBA28模式,要提供28位逻辑地址        ;高16位处于phy_base+0x02,放在dx中,低16位处在phy_base处,放在ax中
         mov bx,16                       ;将该物理地址变成16位的段地址并传送到ds和es寄存器,因为物理地址是16位对齐的,直接除以16即可,dx:ax除以16,商在ax中,ax=0x1000
         div bx
         mov ds,ax                       ;令DS和ES指向该段以进行操作
         mov es,ax                       ;ds=0x1000,es=0x1000
         ;至此确定完用户程序的加载位置
         ;以下读取程序的起始部分 
         xor di,di                       ;25-25行用于指定用户程序在硬盘上的起始逻辑扇区号,定义的read_hard_disk_0过程要求用di:si来提供这个扇区号
         mov si,app_lba_start            ;程序在硬盘上的起始逻辑扇区号,app_lba_start=100D,这时,di:si=0x0000:0x0064
         xor bx,bx                       ;指定调用过程read_hard_disk_0存放数据的内存地址,加载到DS:0x0000处
         call read_hard_disk_0
      
         ;以下判断整个程序有多大,一次要读多少个扇区,因为程序不一定只有512字节,判断是否要读下一个扇区
         mov dx,[2]                      ;曾经把dx写成了ds，花了二十分钟排错
         mov ax,[0]                      ;将程序总长度传到dx:ax=program_end(双字)
         mov bx,512                      ;512字节每扇区,硬盘每次读写操作都是按一个扇区为单位
         div bx                          ;凑巧的情况下,用户程序正好是512字节的整数倍,做完除法后,ax是用户程序实际占用的扇区数
         cmp dx,0                        ;但是大多数情况下,有余数,有余数意味着最后一个扇区没有填满而落下了,没有纳入总扇区数
         jnz @1                          ;未除尽，因此结果比实际扇区数少1
         dec ax                          ;除尽,已经读了一个扇区，扇区总数减1
   @1:                                   ;为什么除不尽不管,除尽了还要减去1,这是因为刚才已经预读了一个扇区了
         cmp ax,0                        ;考虑实际长度小于等于512个字节的情况
         jz direct
         
         ;读取剩余的扇区
         push ds                         ;以下要用到并改变DS寄存器,段地址的改变是临时的,只是为了读硬盘,所以第42行将当前数据段寄存器DS的内容压栈保存

         mov cx,ax                       ;循环次数（剩余扇区数）
   @2:
         mov ax,ds                       ;用户程序被加载的位置是由ds和es指向的逻辑段,最大才64KB,如果程序太大会容纳不下,解决办法是每次往内存加载一个扇区前,都重新在前面的数据段的尾部构造新的逻辑段
         add ax,0x20                     ;得到下一个以512字节为边界的段地址,因为每个扇区都是512字节
         mov ds,ax  
                              
         xor bx,bx                       ;每次读时，偏移地址始终为0x0000
         inc si                          ;下一个逻辑扇区
         call read_hard_disk_0
         loop @2                         ;循环读，直到读完整个功能程序,循环量为cx,之前已经传进去了dx:ax除以bx的商ax

         pop ds                          ;恢复数据段基址到用户程序头部段
      
         ;计算入口点代码段基址 
   direct:
         mov dx,[0x08]                   ;此时DS指向的是用户程序被加载的逻辑段地址,也是用户程序的头部
         mov ax,[0x06]
         call calc_segment_base
         mov [0x06],ax                   ;回填修正后的入口点代码段基址,就是将刚才计算出来的逻辑段地址写回原处,仅覆盖低16位,高16位不用理会,原处是用户程序的头部的code_entry
      
         ;开始处理段重定位表
         mov cx,[0x0a]                   ;需要重定位的项目数量,供后面的循环指令使用
         mov bx,0x0c                     ;重定位表首地址,协议约定好的
          
 realloc:;循环体每次循环开始后,bx总是指向需要重定位的段的汇编地址,而且都是双字,需要传送到dx和ax,然后调用过程calc_segment_base计算相应的逻辑段地址并覆盖原来的位置
         mov dx,[bx+0x02]                ;32位地址的高16位  ;最后将基址寄存器加上4,指向下一表项,当CX寄存器为0时,所有表项均处理完
         mov ax,[bx]
         call calc_segment_base
         mov [bx],ax                     ;回填段的基址
         add bx,4                        ;下一个重定位项（每项占4个字节） 
         loop realloc                    ;当寄存器为0时,所有表项均处理完

         jmp far [0x04]                  ;使用间接绝对远转移转移到用户程序,访问ds指向的数据段,从偏移地址0x04取出两个字,分别传送到cs和ip
;Bochs调试断点b 0x7c70
;-------------------------------------------------------------------------------
read_hard_disk_0:                        ;从硬盘读取一个逻辑扇区
                                         ;输入：DI:SI=起始逻辑扇区号
                                         ;      DS:BX=目标缓冲区地址
         push ax                         ;压栈以保存调用时的寄存器状态,保护现场
         push bx
         push cx
         push dx
      
         mov dx,0x1f2                    ;设置读取的扇区数量,这个数值要写入0x1f2端口,将端口变量存放在dx中
         mov al,1                        ;1个扇区,如果写入值为0表示要读取256个扇区,每读取一个扇区,数值减去一,因此在读写过程中发生错误,该端口包含尚未读取的扇区数
         out dx,al                       ;dx=0x1f2,out表示写端口,这条指令表示向0x1f2端口写入要读取的扇区数1
;91~101行向硬盘接口写入起始逻辑扇区的低24位,低16位在si中,高12位在di中,需要不停倒换到al中,以方便端口写入
         inc dx                          ;0x1f3
         mov ax,si                       ;si=100D=0x0064
         out dx,al                       ;LBA地址7~0

         inc dx                          ;0x1f4
         mov al,ah
         out dx,al                       ;LBA地址15~8

         inc dx                          ;0x1f5
         mov ax,di                       ;di=0x0000
         out dx,al                       ;LBA地址23~16
;在现行的体系下,每个PATA/SATA接口允许连接两块硬盘,主盘Master和从盘Slave
         inc dx                          ;0x1f6端口的低4位(0~3位)用于存放逻辑扇区号的24~27位,第4位用于指示硬盘号,0表示主盘,1表示从盘,第6位指示硬盘模式,0表示CHS,1表示LBA,高3位111表示LBA模式
         mov al,0xe0                     ;LBA28模式，主盘,现在0x1f6端口的状态是1110 0000=0xe0
         or al,ah                        ;LBA地址27~24
         out dx,al

         inc dx                          ;0x1f7,向0x1f7写入0x20请求硬盘读,0x1f7端口既是命令端口又是状态端口,通过这个端口发送读写命令后,硬盘就开始忙活
         mov al,0x20                     ;读命令
         out dx,al                       ;硬盘操作期间,它将0x1f7端口的第7位置1,表示自己很忙,一旦硬盘准备就绪,该位置0,同时将第3位置1表示准备就绪,端口0x1f7的0位为错误位,为1时表示上一个命令执行错误

  .waits:
         in al,dx
         and al,0x88                     ;0x88=1000 1000D,这条指令用于保留寄存器的7位和3位,清零其他位
         cmp al,0x08                     ;0x08=0000 1000D
         jnz .waits                      ;不忙，且硬盘已准备好数据传输,jnz结果不为0则转移,如果硬盘不忙结果为0,不跳转,继续往下执行

         mov cx,256                      ;总共要读取的字数,这里是字数,因为寄存器是16位的
         mov dx,0x1f0                    ;0x1f0端口是16位数据端口,一旦硬盘控制器空闲且准备就绪,就可以连续向这个端口读写数据
  .readw:
         in ax,dx                        ;读取的数据存放到由段寄存器ds指定的数据段,偏移地址由bx指定
         mov [bx],ax                     ;bx存放的是偏移地址,第一次bx=0x0000
         add bx,2                        ;每传送一个字,bx+2以指向下一个偏移地址
         loop .readw                     ;0x1f1是错误端口,包含硬盘驱动器最后一次执行命令后的状态(错误原因)

         pop dx                          ;出栈以恢复调用时的寄存器的状态,恢复现场
         pop cx
         pop bx
         pop ax
      
         ret                             ;恢复完现场过程返回ret

;-------------------------------------------------------------------------------
calc_segment_base:                       ;计算16位段地址
                                         ;输入：DX:AX=32位物理地址
                                         ;返回：AX=16位段基地址 
         push dx                         ;计算过程中要破坏DX,压栈保存

         add ax,[cs:phy_base]            ;就不用加上0x7c00了,因为vstart=0x7c00      ;phy_base dd 0x10000             ;用户程序被加载的物理起始地址
         adc dx,[cs:phy_base+0x02]       ;abc是带进位的加法,将目的操作数和源操作数相加后再加上标志寄存器CF位的值,完成32位加法,这样得到的dx:ax是入口代码的起始物理地址,需要将这个数右移4位得到逻辑段地址
         shr ax,4                        ;分别移动然后拼接,shr逻辑右移指令,比如11111111逻辑右移4位等于00001111,cf位存放最后一个进来的比特,cf=1
         ror dx,4                        ;ror循环右移指令,12345678循环右移4位等于56781234,cf位存放最后一个进来的比特,cf=5
         and dx,0xf000                   ;清零dx的低12位
         or ax,dx                        ;合并ax与dx得到段地址
;尽管DX:AX中是32位的用户程序起始物理地址,理论上只有20位有效的,低16位在AX,高4位在DX的低4位,AX逻辑右移后高4位空出,只需将DX的最低4位挪过来即可,但是由于DX循环移位后,高12位跑到低12位,所以要清零低12位这些无关位
         pop dx
         
         ret

;-------------------------------------------------------------------------------
         phy_base dd 0x10000             ;用户程序被加载的物理起始地址,要求该地址的最低4位必须是0,加载的起始地址必须是16字节对齐的,才能形成一个有效的段地址
                                         ;0x10000=0000 0000 0000 0001 0000 0000 0000 0000B
 times 510-($-$$) db 0
                  db 0x55,0xaa
;===============================================================================
;加载器要加载一个程序并使其执行需要决定两件事
;     1.查看空闲内存,确定从哪个物理地址开始加载用户程序,该加载器在151行声明这个物理地址,但是可以是别的物理地址,要求是该地址的最后4位必须是0(16字节对齐)
;     2.用户程序位于硬盘上的什么位置,起始逻辑扇区号是多少
;这个加载器的思路是先读取一个扇区,并判断程序的大小以决定是否接着读取下一个扇区,并不一定只读一个扇区,因为有的程序大于一个扇区
;内存分配情况
;       ________________
;FFFFF |    ROM-BIOS    |
;      |      64KB      |
;F0000 |                |
;       ----------------
;      |                |
;      |                |
;      |   外围设备映射   |
;      |                |
;      |                |
;      |                |
;BFFFF  ----------------
;      |   映射显卡显存   |   将要显示的字符的ASCII码写入显存,其余工作由字符发生器完成
;      |     32KB       |
;B8000  ----------------
;      |                |
;      |   外围设备映射   |
;      |                |
;A0000  ----------------
;9FFFF |                |
;      |                |
;      |                |
;      |                |
;      ~   可用空闲空间   ~
;      ~                ~
;      |                |
;      |                |
;      |                |
;0FFFF  ----------------
;      |       MBR      |
;      |      加载器     |
;      |      及其栈     |
;       ----------------
;      |                |
;      |                |
;00000  ----------------
;加载器的下一步工作是从硬盘读取用户程序,从23行开始,也就是访问硬盘,硬盘是一种外部设备
;每种设备都有自己的工作方式,不同设备发送不同的信号,要和处理器通信就需要一些信号转换和变速齿轮,就是I/O接口
;但是有两个问题:
;     1.不可能让所有的接口都直接和处理器相连,设备那么多,还有的没有发明出来,以后将扩展的
;     2.冲突处理
;1的解决方式使用总线,所有的接口都可以连到总线,但是每个接口都必须有一个电子开关,用来断开和连接总线
;2的解决方式是使用输入输出设备集中控制器ICH,协调各个接口对处理器的访问,称为南桥
;当处理器想跟某个设备说话时,ICH会收到通知,关闭无关设备
;
;                        ----输入输出控制设备集中器(ICH芯片)---
;                       |                                 |
;                       |                                 |               |--------鼠标
;                       |                ||<====>USB接口<==|==USB总线==>HUB{---------键盘
;                       |                ||               |               |--------U盘
;                       |                ||               |
;                       |                ||               |                |--------硬盘1
;                       |                ||<==>IDE/SATA接口<====IDE/SATA总线{
;                       |                ||               |                |--------硬盘2
; ----------            |                ||               |
;|          |           |                ||               |
;|  处理器   |<==局部总线===>处理器接口<====>||<====>时钟/DMA  |
;|          |           |                ||      中断/LPC  |      LPC为老式总线接口
; ----------            |                ||      定时器    |
;                       |                ||      网络      |
;                       |                ||      电源管理   |
;                       |                ||      等等接口    |
;                       |                ||<=>PCI/PCIE接口<=|===PCIE总线==独立声卡/显卡
;                       |                ||               |
;                        ---------------------------------
;
;在细节上,处理器使用端口与外围设备通信,本质上说端口就是寄存器,不同之处在于端口位于I/O接口电路中,每个接口都可能有好几个端口
;比如连接硬盘的PATA/SATA接口就有好几个端口,命令端口(当向该端口写入0x20时表示从硬盘读数据,写入0x30时表示写数据),状态端口,数据端口,参数端口
;端口有两种实现方式,一种是直接映射到地址空间,一种是独立编址,不和内存有关系
;处理器地址线既连接着内存也连接着I/O接口,此外处理器还有一个特殊的引脚M/IO#,#表示低电平有效
;当处理器访问内存时,它会让M/IO#引脚高电平,这时候和内存相关的电路就会打开
;如果处理器访问端口,M/IO#引脚低电平,内存电路被禁止,此时访问的就是I/O端口,与此同时,处理器发出的地址和M/IO#信号一起用于打开某个I/O接口
;所有端口都是统一编址的,比如0x0001,0x0002...,每个I/O接口都分配了若干个端口,比如I/O接口A有3个端口,分别是0x0021~0x0023
;每个PATA和SATA接口分配了8个端口,但是ICH芯片通常集成了两个PATA/SATA接口,分别是主硬盘和副硬盘
;主硬盘分配的端口号是0x1f0~0x1f7,副硬盘分配的端口号是0x170~0x177
;在Intel系统中只允许65536个端口存在,0x0000~0xffff
;
;                            内存 <---        I/O接口 <-    I/O接口 <--
;                            /||\   |          /||\   |     /||\    |
; ----------------            ||    |           ||    |      ||     |
;|                |           ||    |           ||    |      ||     |
;|                |===========||====|===========||====|======||=====|=========地址线
;|     处理器      |                 |                 |             |
;|                |                 |                 |             |
;|                |                 |                 |             |
;|                |-----------------+-----------------+-------------+---------M/IO#
; ----------------
;
;因为是独立编址,所以端口的访问不能使用mov指令,取而代之的是in和out指令
;     in al,dx
;     in ax,dx
;in指令的目的操作数必须是寄存器al或ax,al访问8位端口,ax访问16位端口,源操作数应当是dx
;in al,dx的机器指令码是0xEC,in ax,dx的机器指令码是0xED,都是一字节,之所以这么简短是因为in指令不允许使用别的寄存器,也不允许使用内存单元作为操作数
;in指令还有两字节的形式,前一字节分别是0xE4或0xE5,分别用于指明8位或16位端口,后一字节是立即数,表示端口号
;因此机器指令E4 F0就相当于in al,0xf0,机器指令E5 03就相当于in ax,0x03,但是这种指令形式的操作数只允许一个字节,只能访问0~255端口
;
;out指令用来向外围设备发送数据,和in指令相反,目的操作数可以是寄存器dx或8位立即数,源操作数必须是寄存器al或ax
;     out 0x37,al       写0x37号端口
;     out 0xf5,ax       写0xf5号端口
;     out dx,al         8位端口号在dx中
;     out dx,ax        16位端口号在dx中
;
;in和out指令不影响标志位
;硬盘读写的基本单位是扇区,最经典的方式(CHS模式)是向硬盘控制器发送磁头号,柱面号和扇区号
;实际上很多时候我们并不关心扇区的物理位置,而是希望所有的扇区都统一编址,逻辑扇区
;最早的逻辑扇区编址方法是LBA28,使用28位比特来表示逻辑扇区,2的28次方个扇区乘以每个扇区512字节等于128GB
;主硬盘控制器被分配了8位端口,端口号是0x1f0~0x1f7
;第一步设置读取的扇区数,这个数要写入0x1f2端口,8位端口,每次最大能读255个扇区
;     mov dx,0x1f2
;     mov al,0x01       如果写入值为0表示要读取256个扇区,每读取一个扇区,数值减去一,因此在读写过程中发生错误,该端口包含尚未读取的扇区数
;     out dx,al
;第二步设置LBA扇区号,扇区的读写是连续的,所以只要给出第一个扇区,28位扇区需要将其分成4段,分别写入0x1f3,0x1f4,0x1f5,0x1f6号端口
;     0x1f6       最后4位
;     0x1f5       16~32位
;     0x1f4       8~15位
;     0x1f3       0~7位
;假设要读取0x02扇区
;     mov dx,0x1f3
;     mov al,0x02
;     out dx,al
;     inc dx
;     mov al,0x00
;     out dx,al
;     inc dx
;     out dx,al
;     inc dx
;     mov al,0xe0       0x1f6端口的低4位(0~3位)用于存放逻辑扇区号的24~27位,第4位用于指示硬盘号,0表示主盘,1表示从盘,第6位指示硬盘模式,0表示CHS,1表示LBA,高3位111表示LBA模式
;     out dx,al         LBA28模式，主盘,现在0x1f6端口的状态是1110 0000=0xe0
;
;端口0x1f6各位的含义
;     7     6     5     4     3     2     1     0
;     1    /|\    1    /|\   {  逻辑扇区号27~24位  }
;           |           |
;           0:CHS       0:主硬盘
;           1:LBA       1:副硬盘
;    1110 0000=0xe0 
;
;第三步向端口0x1f7写入0x20读命令请求硬盘读
;     mov dx,0x1f7
;     mov al,0x20
;     out dx,al
;第四步等待读写操作完成,端口0x1f7既是命令端口又是状态端口,通过这个端口发送读写命令后硬盘就开始读写了(应该是设定为读操作,要配合后面的第五步连续取出数据)
;端口0x1f7各位的含义
;     7     6     5     4     3     2     1     0
;    /|\                     /|\               /|\  
;     |                       |                 |
;  1表示很忙                1表示准备就绪       1表示前一个命令错误,错误原因在0x1f1
;
;
;在内部操作时,硬盘将0x1f7端口第7位置1,表示正忙,一旦硬盘准备就绪再将此位置0,同时将第3位置1,辨明准备就绪
;     mov dx,0x1f7
;.waits:
;         in al,dx
;         and al,0x88                     ;0x88=1000 1000D,这条指令用于保留寄存器的7位和3位,清零其他位
;         cmp al,0x08                     ;0x08=0000 1000D
;         jnz .waits
;第五步连续取出数据,0x1f0是硬盘的16位数据端口,一旦硬盘控制器空闲且准备就绪,就可以连续向这个端口读写数据,读取的数据存放到由段寄存器ds指定的数据段,偏移地址由bx指定
;         mov cx,256                      ;总共要读取的字数,这里是字数,因为寄存器是16位的
;         mov dx,0x1f0                    ;0x1f0端口是16位数据端口,一旦硬盘控制器空闲且准备就绪,就可以连续向这个端口读写数据
;.readw:
;         in ax,dx                        ;读取的数据存放到由段寄存器ds指定的数据段,偏移地址由bx指定
;         mov [bx],ax                     ;bx存放的是偏移地址,第一次bx=0x0000
;         add bx,2
;         loop .readw
;过程调用
;           主程序执行流程
;                 |
;                 |
;                 |
;                 |
;                 |                 子程序执行流程
;                 |          _>---------------------->|
;                \|/        /                        \|/
;           调用过程call--->/                    push指令保护现场
;                                                     |
;                 |<---------------\                  |
;                 |                 \                 |
;                 |                  \               \|/
;                 |                   \        pop指令恢复现场
;                 |                    \       ret过程返回
;                 |                     \_____________/
;                \|/
;在加载器这个程序中,我们使用app_lba_start equ 100           ;使用伪指令equ声明常数app_lba_start=100D（用户程序起始逻辑扇区号）
;24~27行用于从硬盘读取这个扇区的内容,因为不知道程序有多大,占用多少个扇区,所以先读取第一个扇区,该扇区包含了用户程序头部,头部又包含了程序的大小
;通过分析头部判断还要读取多少个扇区才能完全加载用户程序,因为要多次读取硬盘,所以设计过程来反复调用,过程的第一条指令一般需要用标号引用
;每次要读取的扇区号和要保存的位置都不一样,就涉及到参数的传递,使用寄存器传递参数最为方便
;
;调用过程指令是call,intel处理器支持4种调用方式
;     1.16位相对近调用,调用过程位于当前代码段内,只需得到偏移地址
;           16位相对近调用是3字节指令,操作码为0xE8,后跟16位的操作数,该操作数是当前call指令相对于目标过程的偏移量,计算过程如下
;                 用目标过程的汇编地址减去当前call指令的汇编地址再减去当前指令长度3,保留16位结果
;           call near proc_1  近调用的特征是使用near关键字,proc_1是标号,near是非必需的,如果call没有提供任何关键字,则编译器认为该指令是近调用
;           在编译阶段,编译器用标号处的汇编地址减去当前指令的汇编地址再减去3作为当前指令的操作数
;           因为16位相对近调用的操作数是两个汇编地址相减的相对量,如果被调用过程在当前指令的前方,那么该相对量是正数,反之负数,所以该操作数是16位有符号数
;           被调用过程必须位于距离当前指令-32768~32767字节的地方
;           在指令执行阶段,处理器看到操作码0xE8就知道这是一个调用,用指令指针ip加上指令中的操作数,在加上3的到一个新的偏移地址,接着将原有ip压人栈中,最后用新的ip取代原有的ip,导致处理器跳转
;           call 0x0500的操作数是0x0500-当前指令汇编地址-3
;           call相对近调用中的操作数并不是直接被CPU用了,CPU又将其恢复成绝对地址     当前ip + 机器码长度 + 操作数 = 目标函数绝对地址
;           所以不能称之为"直接"相对近调用
;     2.16位间接绝对近调用,也是近调用,只能调用当前代码段内的过程,指令中的操作数不是偏移量,而是被调用过程的真实偏移地址,称为绝对地址,这个绝对地址由16位通用寄存器或内存单元间接给出
;           call cx           ;目标地址在cx中,near省略,下同
;           call [0x3000]     ;先访问内存才能取得目标偏移地址
;           call [bx]         ;先访问内存才能取得目标偏移地址
;           call [bx+si+0x02] ;先访问内存才能取得目标偏移地址
;           第一条指令的机器码为FF D1,被调用过程的偏移地址位于cx内,直接取代ip
;           第二条指令的机器码为FF 16 00 30,指令执行时,处理器使用ds访问数据段,从偏移地址0x3000处取得一个字,作为真实偏移地址取代ip
;           剩下的指令只是寻址方式不同
;     3.16位直接绝对远调用,这种调用属于段间调用,需要被调用过程的段地址和偏移地址,其中16位针对偏移地址
;           call 0x2000:0x0300      ;段地址和偏移地址直接在call中给出
;           这条指令编译后的机器码是9A 30 00 00 20,按规定偏移地址在前,段地址在后
;           处理器在执行时,先将cs压栈,接着把ip压栈,用指令中的段地址和偏移地址替换原有的cs:ip,导致处理器跳转
;           这个指令也可以用在当前代码段,就是从当前代码段"转移"到当前代码段
;     4.16位间接绝对远调用,也属于段间调用,被调用过程的段地址和偏移地址是间接给出的,其中16位针对偏移地址
;           关键字far必须使用
;           call far [0x2000]
;           call far [proc_1]
;           call far [bx]
;           call far [bx+si]
;           前两条指令是等效的,不同之处仅仅在于第一条给出的是数值,第二条给出的是标号,但本质上是一样的,在编译阶段标号会被转化成数值
;           假设在数据段内声明了标号并初始化2个字,32位
;           proc_1 dw 0x0102,0x2000
;           这两个字分别是某个过程的段地址和偏移地址,处理器要求偏移在前段在后,调用该过程使用call far [proc_1]
;           当这条指令执行时,处理器访问ds指向的数据段,从指令中指定的偏移地址(由标号proc_1提供)处取得2个字,然后先将cs压栈,接着把ip压栈,用取得的段地址和偏移地址替换原有的cs:ip,导致处理器跳转
;
;过程返回ret/retf,是call/call far配对指令,但并不依赖于call指令
;     ret是近返回指令,执行时处理器从栈中弹出1个字到ip
;     retf是远返回指令,执行时处理器分别从栈中弹出2个字到ip和cs
;ret/retf/call不影响任何标志位
;
;8.3.7加载用户程序
;第一次读硬盘将得到用户程序最开始的512字节,这512字节包括头部以及一些实际的指令或数据
;用户程序最开始的双字是整个程序的大小,为此第30~31行分别将该数值的高16位和低16位传送到DX和AX,第32行将512传送到BX,用作除数
;在凑巧情况下,用户程序的大小刚好是512的整数倍,做完除法后,AX是用户程序实际占用的扇区数,但是绝大多数情况下,会有余数,有余数意味着最后一个扇区没有填满而落下了,没有纳入总扇区数
;关于这个问题的解释,硬盘的读写是以扇区为单位,所以如果没有除尽则转移到后面的代码,去读剩余的扇区,如果除尽了则总扇区数减去1
;为什么除不尽不管,除尽了还要减去1,这是因为刚才已经预读了一个扇区了
;需要注意的是,用户程序的长度有可能小于512字节,或者恰好等于512字节,在这两种情况下,当程序执行到第38行时,AX必然为0,所以第38行cmp指令比较,第39行条件转移,当AX=0就意味着用户程序已经读完,就不用继续读硬盘了
;因为刚才已经读过了
;
;用户程序被加载的位置是由DS和ES指向的逻辑段,一个逻辑段最大是64KB,当用户程序特别大时根本容纳不下,最大偏移是0xffff,再大就会绕回到0x0000,覆盖原先的内容
;解决方案是每次往内存中加载一个扇区前,都重新在前面的数据尾部构造一个新的逻辑段,并把要读取的数据加载到这个新段内,如此一来,因为每个段的大小是512字节,即0x200,右移4位后是0x20
;这就是各个段地址之间的差值,每次构造新段时只需要在前面段地址的基础上加上0x20即可得到新段的段地址(第47行)
;
;段地址的改变是临时的,只是为了读硬盘,所以第42行将当前数据段寄存器DS的内容压栈保存
;第44行,将用户程序剩余扇区数传送到CX
;第46~48行,将DS增加0x20构造出下一个逻辑段,为从硬盘上读取下一个512字节数据作准备
;第50行,清零BX,BX作为段内偏移,每次传输都在一个新段内进行,每次传输前都应当清零
;第51行,每次读取硬盘前,将SI加1,以指向下一个逻辑扇区,SI刚开始传入的是用户程序的LBA地址
;第52~53行,调用读硬盘的过程read_hard_disk_0,并开始下一轮循环,直到CX=0(loop执行时先将CX减去1再判断是不是0,是0不循环)
;
;8.3.8用户程序重定位
;用户程序在编写时是分段的,因此加载器的下一步工作是计算和确定每个段的段地址
;第55行,从栈中恢复DS,使其指向用户程序被加载的起始位置,也就是用户程序头部
;第58~62行,用于重定位用户程序入口点的代码段,用户程序头部内,偏移为0x06处的双字,存放的是入口点代码段的汇编地址,加载器首先将高字和低字分别传送到DX和AX,然后调用calc_segment_base计算该代码段在内存中的段地址
;calc_segment_base接受一个32位的汇编地址DX:AX,计算完成后向主程序返回一个16位的逻辑段地址(AX),详情见代码段实现
;
;因为在16位处理器上,每次只能进行16位数的运算,第139行,先将用户程序在内存中物理起始地址phy_base dd 0x10000的低16位加到寄存器AX中
;然后第140行,再将该起始地址的高16位加到寄存器DX中,abc是带进位的加法,将目的操作数和源操作数相加后再加上标志寄存器CF位的值,完成32位加法,这样得到的dx:ax是入口代码的起始物理地址,需要将这个数右移4位得到逻辑段地址
;至此在DX:AX中得到32位的入口点代码段的起始物理地址,只需要将这32位数右移4位即可得到逻辑段地址,麻烦在于这个数位于两个寄存器中
;尽管在DX:AX中得到32位的用户程序起始物理内存地址,理论上只有20位是有效的,低16位在AX中,高4位在DX的低4位中
;解决办法是先将AX逻辑右移4位,这样将高4位空出,用来填入DX的低4位
;然后DX使用循环右移,将低4位挪到高4位,再与AX(AX高4位空出)合并,相当于将DX:AX右移4位,这样就得到16位的逻辑段地址,在AX中
;
;注意,过程最后,第146~148行,恢复DX并返回
;然后回到第62行,将刚刚计算出来的逻辑段地址写回到原处,仅覆盖低16位,高16位不用理会,原处是用户程序的头部的code_entry
;至此处理完入口点代码段的重定位,下面开始处理用户程序的所有段,它们位于用户程序头部的段重定位表中
;
;重定位表的表项数存放在用户程序头部偏移0x0a处,第65行用于将它从该内存地址处传送到CX,供loop使用
;段重定位表的首地址存放在用户程序头部偏移0x0c处,第66行将0x0c传送到基地址寄存器BX,以后每次BX加上4就指向下一个重定位表项
;第68~74行是具体的循环体,循环体执行完后用户程序就在内存中准备就绪,剩下的工作就是把处理器的控制权交给它第76行使用16位的间接绝对远转移转移到用户程序
;
;
;总结8086处理器的无条件转移指令
;     1.相对短转移
;           操作码为0xEB,操作数是相对于目标位置的偏移量,仅1字节,是个有符号数,所以该指令属于段内跳转指令,而且只允许转移到距离当前指令-128~127字节的地方,相对短转移必须使用关键字short
;                 jmp short infinite
;           在源程序编译阶段,编译器会检查标号infinite所代表的值,如果数值超过一字节所能允许的数值范围,则无法通过编译
;           否则编译器用目标位置的汇编地址减去当前指令的汇编地址再减去指令长度2,保留一字节的结果作为操作数
;           相对短转移指令的汇编语言操作数只能是标号和数值,下面是直接使用数值的情况
;                jmp short 0x2000
;           但数值和标号是等价的,在编译阶段,都被用来计算一个8位的偏移量
;           在指令执行时处理器把指令中的操作数加上2,再加到IP上,导致处理器的执行流程转向目标地址处
;
;     2.16位相对近转移
;           和相对短转移不同,16位相对近转移指令的转移范围稍大一些,操作码为0xE9,而且指令长度3字节,包括2字节的操作数
;           因为是近转移,所以是段内转移,相对的意思同样是指它的操作数是一个相对量,是相对于目标位置处的偏移量,在源程序编译阶段,编译器用目标位置的汇编地址减去当前指令的汇编地址再减去指令长度3
;           保留2字节的结果作为操作数,16位的有符号数,故可以转移到距离当前指令-32768~32767字节的地方
;           16位相对近转移应当使用关键字near,比如
;                 jmp near infinite
;                 jmp near 0x2000
;           在早先的NASM版本中,关键字near是可以省略的,如果没有指定short或者near关键字,那么默认near,但是最近的版本改变了这一规则,没有指定关键字
;           那么,如果目标位置距离当前指令-128~127字节,自动采用short,否则采用near
;
;     3.16位间接绝对近转移
;           这种转移方式也是近转移,只在段内转移,但是转移到的目标偏移地址不是在指令中直接给出的,而是用一个16位的通用寄存器或者内存地址来间接给出
;                 jmp near bx
;                 jmp near cx
;           指令中的关键字near可以省略,间接绝对近转移原本就是near的,以上两条指令执行时,处理器将用BX/CX的内容来取代ip的当前内容
;           以上是目标偏移地址位于通用寄存器的情况,该偏移地址也可以位于内存中,而且这是最常见的情况,假设某程序的数据段声明了标号jmp_dest并初始化了一个字
;                 jmp_dest dw 0xc000
;           而且假定我们已经知道它是转移目标的起始偏移地址,那么在该程序的代码段内,就可以使用以下的16位间接绝对近转移指令
;                 jmp [jmp_dest]    ;关键字near省略
;           当这条指令执行时,处理器访问由DS指向的数据段,从指令中指定的偏移地址处取出一个字(这里是0xc000),取代ip
;           既然是间接地寻找目标位置的偏移地址,其他寻址方式也是可以的,比如
;                 jmp [bx]    jmp [bx+si]
;
;     4.16位直接绝对远转移
;                 jmp 0x0000:0x7c00
;           这里的0x0000:0x7c00分别是段地址和偏移地址,编译之后,机器指令为EA 00 7C 00 00,偏移地址在前,段地址在后
;           执行这条指令后,处理器用指令中给出的段地址和偏移地址代替原先的CS:IP,执行段间转移,16位用来限定偏移地址
;
;     5.16位间接绝对远转移(jmp far)
;           远转移的目标地址可以通过访问内存来间接得到,但是要使用关键字far,假设在程序的数据段内声明了标号jmp_far,并在其后初始化了两个字
;                 jmp_far dw 0x33c0,0xf000
;           这不是两个普通的数值,分别是某个程序片段的偏移地址和段地址,为了转移到该程序片段上执行,可以使用下面的指令
;                 jmp far [jmp_far]
;           关键字far告诉编译器该指令应当编译成一个远转移,处理器执行这条指令后,访问DS指向的数据段,从指令中给出的偏移地址取出两个字,分别替换CS:IP
;           16位间接绝对远转移(jmp far)的操作数可以是任何一种内存寻址方式,如
;                 jmp far [bx]    jmp far [bx+si]
;           16位的意思是要转移到的目标位置的偏移地址是16位的