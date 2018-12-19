# Squirt3
Port of the Apple II program selector/viewer to Apple /// SOS

This is the start of porting over the Apple II Prodos file selector Squirt to Apple /// SOS. I have the program loading volumes, navigating directories and viewing text files. This is using SOS calls and .CONSOLE for input/output.

The source code may be useful for someone working with SOS programs. I have attached a disk image with this loaded as the SOS.INTERP.  

Very much a work in progress, as there is still alot to be done.

Included is a bootable SOS disk with the Squirt3 interpreter on it.

Also, my batch file that I used for development, this assembles using ca65/ld65, and then adds the file using AppleCommander to the disk image, and then finally runs this in MESS/MAME. (the lots of disk images in the command line is to give me many volumes to test.

Special Thanks to the Squirt author.
