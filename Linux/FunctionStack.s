#目的:展示函数如何工作
#本程序将计算
#       2的3次方+5的2次方
#
#主程序中的所有内容都存储在寄存器中
#因此数据段不包含任何内容
.code32

.section .data

.section .text

.globl _start
_start:
    push $3    #压人第二个参数
    push $2    #压人第一个参数
    call power
    add $8,%esp
    push %eax
    push $2
    push $5
    call power
    add $8,%esp
    pop %ebx
    add %eax,%ebx  #相加并存储在%ebx中
    mov $1,%eax
    int $0x80

#目的:本函数用于计算一个数的幂
#输入:第一个参数--底数  第二个参数--底数的指数(必须大于等于1)
#输出:以返回值的形式给出结果
#变量:
#   %eax用于暂时存储    %ebx保存底数    %ecx--保存指数  -4(%ebp)--保存当前结果

.type power,@function
power:
    push %ebp
    mov %esp,%ebp
    sub $4,%esp
    movl 8(%ebp),%ebx   #这里引起段错误
    movl 12(%ebp),%ecx
    mov %ebx,-4(%ebp)

power_loop_start:
    cmp $1,%ecx
    je end_power
    mov -4(%ebp),%eax
    imul %ebx,%eax
    mov %eax,-4(%ebp)
    dec %ecx
    jmp power_loop_start

end_power:
    mov -4(%ebp),%eax
    mov %ebp,%esp
    pop %ebp
    ret
