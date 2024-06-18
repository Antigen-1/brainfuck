#lang brag
program: unit+
unit: OTHER | ptrop | slotop | loopstart unit* loopend
loopstart: LOOPSTART
loopend: LOOPEND
ptrop: PTROP
slotop: SLOTOP
