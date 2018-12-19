ca65 squirt3.s -l squirt3.lst
ld65 squirt3.o -o SOS.INTERP#0c0000 -C apple3.cfg
java -jar ac.jar -d F:\_emu\Mess\a3\squirt3.dsk SOS.INTERP
java -jar ac.jar -p F:\_emu\Mess\a3\squirt3.dsk SOS.INTERP SOS $0000 < SOS.INTERP#0c0000
F:\_emu\Mess\mess.exe apple3 -rompath F:\_emu\Mess\roms -skip_gameinfo -resolution 640x480 -window -nothrottle -flop1 F:\_emu\Mess\a3\squirt3.dsk -flop2 F:\_emu\Mess\a3\Apple3BusBasic.dsk -flop3 F:\_emu\Mess\a3\wap\APPLE-3-WAP-inf-09b.dsk -flop4 F:\_emu\Mess\a3\DOM1S2.po -debug
