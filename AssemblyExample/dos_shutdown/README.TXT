


                              ÄúShutDown v1.2úÄ
                                documentation



ÄúWhat does it do?
It has 2 functions: ú shutdown your computer
                    ú reboot your computer

The shutdown option only works on ATX systems with Advanced Power Management
version 1.2 or higher. Just try the program, if it works, your system meets
the requirements.
The reboot option works on any modern personal computer.


ÄúHow to use it?
Just type SHUTDOWN and you will see the following text:

  ShutDown v1.2 þ www.blacklight.wxs.org
  S(hutdown), R(estart), or C(ancel)? _

Now, enter one of the following keys:
  S          to shutdown
  R          to restart
  C or ESC   to cancel and return to DOS

You can also operate ShutDown from the command-line.

To shutdown, type:    SHUTDOWN S   and press Enter
To reboot, type:      SHUTDOWN R   and press Enter
To show help, type:   SHUTDOWN ?   and press Enter

Well, isn't that simple? Put it in your PATH and whenever you're too lazy to
move your hands to the power/reset-button, use this program.
It's also very useful in batch-files, after long file operations, for example
SCANDISK, VIRSCAN and DEFRAG.


ÄúHow does it work?
First it checks if you are running Microsoft Windows. If it is detected, the
program will abort, because Windows does check for APM instructions, so it
allows the program to shutdown the computer. This can cause errors on your
harddisk because it's possible that some other program is writing to the
disk when ShutDown turns off the power.

When everything is okay, the program checks to see if there are any command
line parameters, if not it asks you what to do.
If something has been entered on the command line, it checks if there was a
'S' or a 'R' character, if not, it shows you the syntax. Otherwise it will
first send a signal to SmartDrive, to write any buffers to disk, then it does
what is has to do (shutdown or restart).

The complete program was written in 8086 assembly language, and assembled it
takes only 451 bytes diskspace.


ÄúCredits
This program was written by BlackLight 2001. Assembled with the MAGIC
ASSEMBLER by Magic Software Rotterdam. Download it here (freeware):
  http://www.math.leidenuniv.nl/~bgreeven
  http://www.caiw.nl/~bert
If you want to see my other DOS tools and utilities, visit my own website at:
  http://www.blacklight.wxs.org

ShutDown v1.0 has been released as PublicDomain software. I take no
responsibility whatsoever. The source-code is included in the ZIPfile. Do
with it what you want.
