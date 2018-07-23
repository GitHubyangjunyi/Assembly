# Assembly

AssemblyExample 网上整理的汇编程序</br>
AssemblyLanguage 王爽汇编语言</br>
Bochs 虚拟机调试文件夹</br>
Linux 深入理解程序设计使用linux汇编,编译器使用as</br>
RealProtected x86汇编语言:从实模式到保护模式</br>
SystemExploration X86X64体系探索及编程</br>

第一步汇编程序

as x.s -g -o y.out</br>
-g在最前面表示生成调试信息,才能在gdb中调试</br>
-o告诉编译器将输出放在文件y.out中,y.out称为目标文件</br>

第二步链接文件

ld y.out -o y</br>
ld命令运行链接器</br>

第三步运行汇编程序

./y</br>
./用来告诉计算机,该程序并非位于常用程序目录下,而在当前目录下</br>
命令echo $?获得程序运行后返回系统的退出状态码,一切正常返回0</br>

调试

编译选项加-g在最前面表示生成调试信息,这样编译完的程序才能使用gdb调试</br>
注意:</br>
在启动调试前可以在监视器上添加监视,如表达式$rax监视寄存器rax</br>
可以直接在侧边栏断点添加断点,输入行号或者函数名即可添加断点</br>
然后单步调试查看寄存器的值</br>
也可以打完断点使用调试控制台命令-exec info r监视所有寄存器</br>
单步执行-exec next但感觉不如上面的方法好</br>
在深度linux下需要安装Gnome Terminal</br>
objdump 可执行文件 -S 反汇编可执行文件