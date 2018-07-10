.section .data
.section .text
.globl _start
_start:
movl $18,%eax
mov $78,%ah
add $8,%eax
movl %eax,%ebx
add %eax,%ebx
movl $1,%eax    #用于退出程序的linux内核命令号(系统调用)
int $0x80
