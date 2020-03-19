# EmbeddedHC12_Calculator-and-Digital-Clock
The calculator and digital clock rules are:

Input positive decimal numbers only\
Input maximum three digit numbers only\
Valid operators are: +, -, *, and /\
Input number with leading zero is OK\
Input only two numbers and one operator in between, no spaces\
Show 'Tcalc> 'prompt and echo print user keystrokes unltil Return key\
Repeat print user input and print answer after the '=' sign\
In case of an invalid input format, repeat print the user input until the error character\
In case of an invalid input format, print error message on the next line: 'Invalid input format'\
Keep 16bit internal binary number format, detect and flag overflow error\
Use integer division and truncate any fraction\
12 hour clock\
"s" for 'set time' command\
Update the time display every second\
Fixed time display area on the terminal screen (mid center)\
Fixed calculator display area on the terminal screen (top or bottom three lines)\
Use Real Time Interrupt feature to keep the time\
Set the SCI port Tx and Rx baud rate to 115200 baud for the terminal I/O

# Display Example
Tcalc> 123+4\
       123+4=127\
Tcalc> 96*15\
       96*15=1440\
Tcalc> 456@5\
       456@\
       Invalid input format\
Tcalc> 7h4*12\
       7h\
       Invalid input format\
Tcalc> 3*1234\
       3*1234\
       Invalid input format	;due to 4th digit\
Tcalc> 003-678\
       003-678=-675\
Tcalc> 100+999*2\
       100+999*\
       Invalid input format\
Tcalc> 555/3\
       555/3=185\
Tcalc> 7*(45+123)\
       7*(\
       Invalid input format\
Tcalc> 78*999\
       78*999\
       Overflow error\
Tcalc> -2*123\
       -\
       Invalid input format\
Tcalc> 73/15\
       73/15=4\
Tcalc>\
Tcalc> s 10:39:59\
Tcalc> \
Tcalc> s 05:552:5\
       Invalid time format. Correct example => hh:mm:ss\
Tcalc> s 05:55:75\
       Invalid time format. Correct example => hh:mm:ss\
Tcalc> s 12\
       Invalid time format. Correct example => hh:mm:ss \
Tcalc>

# Trouble Shoot
You MUST set the HyperTerminal to VT100 emulation mode and refer to HW8 information. You may

Clear screen\
Enable the scrolling of only top 4 lines (eg. HW8 Sample Program 3)\
Calculator input and output on top 4 lines\
Digital clock on fixed screen position (eg. below 4th line, center)\
Store and recall current cursor position\
Back-space (if you like)\
There are many other features following the escape sequence\
