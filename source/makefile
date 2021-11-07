#
# Графический Turbo Vision by Berezin, Oleinick, Shimon and Karasik.
#
# Version 1.00 (based on TV version 2), Revision 013
#
# Usage : MAKE [-D{RUSSIAN | UKRANIAN | ENGLISH}] [-DBITMAPS] [-DDEBUG]
#              [-DNEWCOLORS] [-DSCREENINEMS]
#

!if $d(DEBUG)
ASM = tasm -t -w- -z
BPC = bpc -E -Q -V -L -$D+,L+,F+,X+,I-,S-,Q-,R-,G+
!else
ASM = tasm -t -w- -z
BPC = bpc -E -Q -L -$D-,L-,F+,X+,I-,S-,Q-,R-,G+
!endif

LANG=RUSSIAN
!if $d(ENGLISH)
LANG=ENGLISH
!endif
!if $d(UKRAINIAN)
LANG=UKRAINIAN
!endif

!if $d(BITMAPS)
BMPS=USESTANDARDBITMAPS
!endif

!if $d(NEWCOLORS)
CLRS=SHIMONCOLORS
!endif

!if $d(SCREENINEMS)
SEM=SCREENINEMS
!endif

.asm.obj:
 $(ASM) -e -i$(INC) $*,$*.obj

.asm.obp:
 $(ASM) -e -i$(INC) -d_DPMI_ $*,$*.obp

.pas.exe:
  $(BPC) -M -d$(LANG);$(BMPS);$(CLRS);$(SEM) $*

.pas.tpu:
  $(BPC) -M -d$(LANG);$(BMPS);$(CLRS);$(SEM) -cd $*

.pas.tpp:
  $(BPC) -M -d$(LANG);$(BMPS);$(CLRS);$(SEM);DPMI -cp $*

OBJDPMI=sysint.obp char.obp
OBJREAL=sysint.obj char.obj
OBJS=format.obj snddelay.obj\
    stdcur.obj mfont.obj _scrolls.obj checks.obj stdcrm.obj\
    _buttons.obj _signs.obj\
    leather.obj concrete.obj doomwall.obj\
    egcase.obj egcomp.obj egsearch.obj egtab.obj egbm.obj\
    hole.obj winicons.obj folders.obj gdi.obj
UNITDPMI=buildtv.tpp
UNITREAL=buildtv.tpu

all: clean real dpmi

real: $(OBJREAL) $(OBJS) $(UNITREAL)

dpmi: $(OBJDPMI) $(OBJS) $(UNITDPMI)

clean:
    del *.tpu
    del *.tpp

demos:

wallpape.tpp: leather.obj concrete.obj doomwall.obj
wallpape.tpu: leather.obj concrete.obj doomwall.obj
egstring.tpp: egcase.obj egcomp.obj egsearch.obj egtab.obj egbm.obj
egstring.tpu: egcase.obj egcomp.obj egsearch.obj egtab.obj egbm.obj
egfont.pas: wrstrn.inc
fntlib.tpp: courier.obj small.obj
fntlib.tpu: courier.obj small.obj
gdi.tpu: gdi.obj stdcrm.obj
stddlg.tpu: folders.obj

