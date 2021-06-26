# c64interface
 Commdore 64 Userport Interface
 
 I have been working on interface for the Commodore 64's userport. This interface allows user to use the C64 like you would any microcontroller. At this time it only turns on and off 8 pins, pins PB0-7.

 

There are many things that can be done with the C64's userport, which I will be looking into adding in the future. For now take a look at acarmony1/c64interface: Commodore 64 Userport Interface (github.com) for more information, schematics, and a demo program.

https://carmony.xyz/10-commodore/commodore-64-projects/9-c64-userport-interface.html

https://carmony.xyz/


Use the interface.prg as a demo to use this interface. Take note if the interface is powered up, and the C64 is off all ports are turned ON. This was the design I used from the Projects for the C64 book. In future versions, I may impletment a 'fail safe', where if no power from the C64 the unit shuts down.
