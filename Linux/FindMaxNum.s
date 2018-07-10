#目的:本程序寻找一组数据中的最大值
#寄存器作用:
#%edi   保存正在检测的数据项索引
#%ebx   当前已经找到的最大数据项
#%eax   当前数据项
#使用以下内存位置:
#data_items:    包含数据项,0表示结束

.section .data

data_items:
.long 3,67,34,255,45,75,54,34,44,33,22,11,66,0

.section .text

.globl _start
_start:
    movl $0,%edi                    #将0移入索引寄存器
    movl data_items(,%edi,4), %eax  #加载数据的第一个字节,汇编中使用的索引寻址movl 起始地址(,%索引寄存器,字长)4其实是比例因子,从data_items开始递增%edi*4个位置
    movl %eax,%ebx                  #由于第一项,%eax就是最大值,实际上是复制值

start_loop:                         #开始循环start_loop标记循环位置
    cmpl $0,%eax                    #检测是否到达末尾,这条指令也会影响%eflags状态寄存器,该比较结果放在状态寄存器中
    je loop_exit                    #je中的e表示equal,若值相等则跳转
    incl %edi                       #自增1加载下一个值
    movl data_items(,%edi,4), %eax  #加载新值
    cmpl %ebx,%eax                  #比较值
    jle start_loop                  #若新数据项小于等于原最大值则跳到循环开始处否则执行下一条指令
    movl %eax,%ebx                  #将新值移入最大值寄存器%ebx
    jmp start_loop                  #跳到循环起始处

loop_exit:
    movl $1,%eax                    #1是exit系统调用
    int $0x80
#主要数据类型
#.byte 一个字节0~255
#.int 每个整形数字(这种类型与int指令不同)占用两个存储位置0~65535
#.long 长整形4个字节,与程序中所使用的寄存器空间相同0~4294967295
#.ascii 该指令用于将字符输入内存
#如果给出.ascii "Hello there\0"汇编程序将保留12个存储位置(12字节)
#注意:在该程序中并未声明data_items为globl,因为只在本程序中引用这些位置,也可以使用globl只是没必要
#_start与此相反,linux需要知道它在哪才知道程序从哪里开始执行
#255是允许的最大退出码
#movl   移动长整形
#cmpl   比较
#je     若值相等则跳转,e表示equal,
#jg     若第二个值大于第一个值则跳转
#jge    若第二个值大于等于第一个值则跳转
#jl     若第二个值小于第一个值则跳转
#jle    若第二个值小于等于第一个值则跳转
#jmp    无条件跳转
