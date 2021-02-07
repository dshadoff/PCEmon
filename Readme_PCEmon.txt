+------------------------------------------------------------------+
| PCEmon RS232/TTL MONITOR BY CHRIS COVELL <chris_covell@yahoo.ca> |
+------------------------------------------------------------------+
 Version 1.27.4  (10/25/2015)


This is a monitor/debugger program for the PC-Engine / Turbografx.
It uses a custom (but simple!) cable to communicate with a PC
terminal over a (+5V) RS-232 or TTL serial line.

Please check the online instructions (or run the ROM!!) to read
how to connect the 3 wires for a serial connection.  Obviously,
this program makes sense only on a real PC-Engine / Turbografx-16.

---

There are several versions of PCEmon included that are hardcoded to
start up at different baud rates.  Of course, you can change the
baud rate from within PCEmon as well.  PCEmon can also run in a
more constrained space (16K and 8K only) so shorter versions of the
program are also included -- but they contain terse messages and/or
no graphical text font.


---

The RS-232 cable and a PCE Pad cannot be connected at the same time,
so how do we load and run the program?

Well, if you're using an Everdrive PCE Flashcard, then put only 
a single file in the root directory of your SD Card: the version of
PCEmon that you want to run.  This PCE file will show up at the top
of the menu when you turn on the PCE with the Everdrive in.  If you
already have the serial cable connected, then start up your terminal
software, and hold down any key (Enter, for example).  Once this key
auto-repeats, it'll strobe the RUN line on the PCE via the RS-232
connection, simulating a "RUN" button press.

Alternately, you can load PCEmon from a CDR burned with the included ISO.


(Try uploading the included graphics to VRAM/VDC with a "U V 0" command!)


That's basically all the info for now!  Please read the included
command descriptions, and look on my homepage for updates!
http://www.chrismcovell.com/PCEmon/
