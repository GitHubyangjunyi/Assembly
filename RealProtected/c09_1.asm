         ;代码清单9-1
         ;文件名：c09_1.asm
         ;文件说明：用户程序 
         ;创建日期：2011-4-16 22:03
         
;===============================================================================
SECTION header vstart=0                     ;定义用户程序头部段 
    program_length  dd program_end          ;程序总长度[0x00]
    
    ;用户程序入口点
    code_entry      dw start                ;偏移地址[0x04]
                    dd section.code.start   ;段地址[0x06] 
    
    realloc_tbl_len dw (header_end-realloc_begin)/4
                                            ;段重定位表项个数[0x0a]
    
    realloc_begin:
    ;段重定位表           
    code_segment    dd section.code.start   ;[0x0c]
    data_segment    dd section.data.start   ;[0x14]
    stack_segment   dd section.stack.start  ;[0x1c]
    
header_end:                
    
;===============================================================================
SECTION code align=16 vstart=0           ;定义代码段（16字节对齐） 
new_int_0x70:;中断处理过程,该中断处理过程的起始地址,这里是绝对地址,在0x10020处,bochs下使用命令u/100 0x10020即可反汇编出本中断处理过程代码,0x10020是这样得到的,首先中断号是0x70,然后要乘以4,得到0x1c0(这是中断入口点在中断向量表中的偏移地址),然后xp/1 0x01c0得到该内存地址存放的数据(0x10020000),高四位是段地址,低四位是偏移地址,左移段地址4位后加上0000得到0x10020,中断向量表中的其他大多数都是0xf000ff53,绝对地址是0xfff53,然后该地址处存放的指令是iret
      push ax                            ;寄存器压栈保护现场,这一点特别重要,中断处理过程必须无痕执行
      push bx
      push cx
      push dx
      push es
      
  .w0:;34~40行用于读RTC寄存器A,根据UIP位的状态来决定是等待更新周期结束,还是继续往下执行,UIP位0表示现在访问CMOS RAM中的日期和时间是安全的                                   
      mov al,0x0a                        ;阻断NMI。当然，通常是不必要的,当NMI发生时,整个计算机都应当停止工作,也不在乎中断处理过程能不能正常执行了
      or al,0x80                         ;用or指令将最高位置1再写端口0x70,在访问RTC期间最好阻断NMI,因为0x70端口的最高位是控制NMI中断的开关,当它为0时,允许NMI中断到达处理器,为1时,阻断所有中断
      out 0x70,al                        ;将al中的内容写入0x70端口,以指定访问0x0a端口(CMOS RAM中的寄存器A)
      in al,0x71                         ;读寄存器A,CMOS RAM的访问需要通过两个端口进行,0x70或0x74端口是索引端口,用来指定CMOS RAM内的单元,0x71或0x75是数据端口,用来读写相应单元里的内容
      test al,0x80                       ;测试第7位UIP,test指令顾名思义是用来测试的,可以测试某个寄存器或内存单元里的内容是否带有某个特征,test指令在功能上与and指令是一样的,都是将这两个操作数按位逻辑与,并根据结果设置相应标志位,但是test指令执行后运算结果被丢弃,并不影响两个操作数的内容
      jnz .w0                            ;以上代码对于更新周期结束中断来说(接上面一句,比如要测试al寄存器的第3位是0还是1,可以编写test al,0x80,只要al的第3位是0,执行后结果一定是0000 0000,标志位ZF=1)
                                         ;是不必要的,因为正常情况下访问CMOS RAM中的日期和时间,必须等待RTC更新周期结束,所以上面的判断过程是必须的,也适用于正常的访问过程,但是当前中断处理过程是针对更新周期结束中断的,而当此中断发生时,本身就说明对CMOS RAM的访问是安全的,有999毫秒的时间留给我们,能执行千万跳指令了,所以判断过程是没有必要的,但是加上也无所谓
      xor al,al
      or al,0x80
      out 0x70,al
      in al,0x71                         ;读RTC当前时间(秒)
      push ax

      mov al,2
      or al,0x80
      out 0x70,al
      in al,0x71                         ;读RTC当前时间(分)
      push ax

      mov al,4
      or al,0x80
      out 0x70,al
      in al,0x71                         ;读RTC当前时间(时)
      push ax

      mov al,0x0c                        ;寄存器C的索引。且开放NMI 
      out 0x70,al
      in al,0x71                         ;读一下RTC的寄存器C，使得所有中断标志复位,等于告诉RTC中断已经处理,可以继续下一次中断,否则RTC看到中断未被处理,旧不再产生中断信号,只发生一次中断
                                         ;RTC产生中断的原因有很多,可以通多读寄存器C来判断原因,但这里不需要,因为除了更新周期结束中断外,没有其他中断发发生,因为其他中断都被关闭了,此处不考虑闹钟和周期性中断的情况 
      mov ax,0xb800
      mov es,ax

      pop ax
      call bcd_to_ascii
      mov bx,12*160 + 36*2               ;从屏幕上的12行36列开始显示

      mov [es:bx],ah
      mov [es:bx+2],al                   ;显示两位小时数字

      mov al,':'
      mov [es:bx+4],al                   ;显示分隔符':'
      not byte [es:bx+5]                 ;反转显示属性 

      pop ax
      call bcd_to_ascii
      mov [es:bx+6],ah
      mov [es:bx+8],al                   ;显示两位分钟数字

      mov al,':'
      mov [es:bx+10],al                  ;显示分隔符':'
      not byte [es:bx+11]                ;反转显示属性

      pop ax
      call bcd_to_ascii
      mov [es:bx+12],ah
      mov [es:bx+14],al                  ;显示两位小时数字
;在8259芯片内部有一个8位中断服务寄存器ISR,每一位都对应着一个中断输入引脚,当中段处理过程开始时,8259芯片会将相应的位置1,表明正在服务从该引脚来的中断      
      mov al,0x20                        ;中断结束命令EOI的代码是0x20,如果外部中断是主片处理的,那么中断结束命令只需要发送给主片,端口号0x20,如果是从片,则既要发给从片也要发给主片
      out 0xa0,al                        ;向从片发送 
      out 0x20,al                        ;向主片发送 
;一旦响应了中断,8259中断控制器无法知道该中断什么时候处理结束,同时,如果不清除相应的位,下次从同一引脚出现的中断将得不到处理,这种情况下需要在中断处理过程结尾,显式地对8259芯片编程清除该标志,方法是向8259芯片发送中断结束命令EOI
      pop es                             ;从栈中恢复被中断程序的现场,并用中断返回指令iret返回中断之前的地方
      pop dx
      pop cx
      pop bx
      pop ax

      iret
;该程序运行时,时间每秒更新一次,冒号的显示属性每秒钟翻转一次,不同的是,@字符以很快的速度闪烁,这意味着,把处理器从停机状态唤醒的不单单是实时时钟的更新周期结束中断,还有其他硬件中断
;-------------------------------------------------------------------------------
bcd_to_ascii:                            ;BCD码转ASCII
                                         ;输入：AL=bcd码
                                         ;输出：AX=ascii
      mov ah,al                          ;分拆成两个数字 
      and al,0x0f                        ;仅保留低4位 
      add al,0x30                        ;转换成ASCII 

      shr ah,4                           ;逻辑右移4位 
      and ah,0x0f                        
      add ah,0x30

      ret

;-------------------------------------------------------------------------------
start:
      mov ax,[stack_segment]
      mov ss,ax
      mov sp,ss_pointer
      mov ax,[data_segment]
      mov ds,ax
      
      mov bx,init_msg                    ;显示初始信息 
      call put_string

      mov bx,inst_msg                    ;显示安装信息 
      call put_string
      
      mov al,0x70                        ;计算机启动后,RTC芯片的中断号默认是0x70,尽管可以通过对8259编程改变,但是没有必要
      mov bl,4
      mul bl                             ;计算0x70号中断在IVT中的偏移
      mov bx,ax                          

      cli                                ;防止改动期间发生新的0x70号中断

      push es
      mov ax,0x0000
      mov es,ax
      mov word [es:bx],new_int_0x70      ;偏移地址。
                                          
      mov word [es:bx+2],cs              ;段地址
      pop es

      mov al,0x0b                        ;RTC寄存器B,CMOS RAM的访问需要通过两个端口进行,0x70或0x74端口是索引端口,用来指定CMOS RAM内的单元,0x71或0x75是数据端口,用来读写相应单元里的内容
      or al,0x80                         ;阻断NMI,这里或0x80运算是没错的,用or指令将最高位置1再写端口0x70,在访问RTC期间最好阻断NMI,因为0x70端口的最高位是控制NMI中断的开关,当它为0时,允许NMI中断到达处理器,为1时,阻断所有中断
      out 0x70,al                        ;其他7个比特实际上用于指定CMOS RAM单元的索引号,所以or al,0x80没有错,我刚开始以为作者错了,差点给他发邮件,自己修改成or al,0x00后编译在写入虚拟机运行并没有出错,可能是因为运行中没有发送0x70号中断(第158页有说明为什么要屏蔽中断)
      mov al,0x12                        ;设置寄存器B，禁止周期性中断，开放更 
      out 0x71,al                        ;新结束后中断，BCD码，24小时制 

      mov al,0x0c
      out 0x70,al                        ;没有or al,0x80,所以打开了NMI,毕竟这是最后一次在主程序中访问RTC
      in al,0x71                         ;读RTC寄存器C检查中断原因，复位未决的中断状态
;每当更新周期结束中断发生时,RTC就将它的第4位置1,该寄存器C还有一个特点,就是每次读取它后,所有内容自动清零,如果不读取它的话,相应的位没有清零,同样的中断将不再产生
      in al,0xa1                         ;通过端口0xa1读取8259从片的IMR寄存器
      and al,0xfe                        ;用and al,0xfe(1111 1110)指令清除bit 0(此位连接RTC)
      out 0xa1,al                        ;写回此寄存器 
;RTCX芯片设置完后,再来打通它到8259的最后一道屏障,正常情况下8259芯片是不允许RCT中断的,所以需要修改它内部的中断屏蔽器IMR,IMR是一个8位寄存器,位0对应着中断输入引脚IR0,位7对应着引脚IR7,对应位是0时允许中断
      sti                                ;重新开放中断 

      mov bx,done_msg                    ;显示安装完成信息 
      call put_string

      mov bx,tips_msg                    ;显示提示信息
      call put_string
      
      mov cx,0xb800
      mov ds,cx
      mov byte [12*160 + 33*2],'@'       ;屏幕第12行，35列显示一个字符@
;以下的174~176行构成一个循环,先是停机,接着某个外部中断唤醒处理器恢复执行,一旦处理器的执行点来到hlt指令之后,则立即使它继续处于停机状态      
 .idle:
      hlt                                ;使CPU进入低功耗状态，直到用中断唤醒,相对于jmp $指令,使用hlt指令会大大降低处理器的占用率
      not byte [12*160 + 33*2+1]         ;反转显示属性,not指令执行时会将操作数的每一位反转,0变1,1变0,从显示效果上看,循环将显示属性反转将会取得动画效果,not指令不影响任何标志位
      jmp .idle
;主程序就是这样了,停机-执行-接着停机,与此同时,中断也在不停发生着,处理器还要抽空来执行中断处理过程,RTC的更新周期结束中断处理,该处理过程是从第27行开始
;-------------------------------------------------------------------------------
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
         mov ax,bx                       
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

;===============================================================================
SECTION data align=16 vstart=0

    init_msg       db 'Starting...',0x0d,0x0a,0
                   
    inst_msg       db 'Installing a new interrupt 70H...',0
    
    done_msg       db 'Done.',0x0d,0x0a,0

    tips_msg       db 'Clock is now working.',0
                   
;===============================================================================
SECTION stack align=16 vstart=0
           
                 resb 256
ss_pointer:
 
;===============================================================================
SECTION program_trail
program_end:
;说明注释:
;CMOS RAM中的时间信息
;偏移地址     内容
;
;0x00         秒   
;0x01        闹钟秒
;0x02         分
;0x03        闹钟分
;0x04         时
;0x05        闹钟时
;0x06        星期
;0x07         日
;0x08         月
;0x09         年
;0x0A       寄存器A
;0x0B       寄存器B
;0x0C       寄存器C
;0x0D       寄存器D
;对CMOS RAM的访问,需要通过两个端口来进行,0x70或0x74是索引端口,用来指定CMOS RAM中的单元
;0x71或0x75是数据端口,用来读写相应的单元,以下代码用于读取星期几:
;     mov al,0x06
;     out 0x70,al
;     in al,0x71
;从很早开始,0x70端口的最高位是控制NMI中断的开关,当它为0时,允许NMI中断到达处理器,为1时,阻断所有中断,其他7个比特实际上用于指定CMOS RAM单元的索引号
;通常来说,再往0x70端口写入索引时,应当先读取其中的内容,但是该端口是只写的,不能用于读出