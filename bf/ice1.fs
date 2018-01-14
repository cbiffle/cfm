\ Icestick support code

: ledtog  1 + #bit OUTTOG io! ;

:noname
  uart-rx-init
  347 UARTRD io! \ Set baud rate to 115200

  ['] tx 'emit !
  ['] rx! 'key !
  ei ;
oncold !

$1C00 ramtop !

( install cold as the reset vector )
' cold  u2/  0 !

.( Compilation complete. HERE is... )
here host.
