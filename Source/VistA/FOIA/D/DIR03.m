DIR03 ;SFISC/MKO-MULTILINE FIELD EDITOR ;12:36 PM  15 Feb 1995
 ;;22.0;VA FileMan;;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 F  D E X IOXY Q:DIR0DN!$G(DIR0QT)
 Q
 ;
E I $G(DIR0("REP"))&DIR0C>1!(DIR0C>$L(DIR0A)),$S(DIR0LN<DIR0NL:DIR0F,1:DIR0FL)>DX,'$D(DIR0KD) D
 . D PREAD^DIR01($S(DIR0LN<DIR0NL:DIR0F,1:DIR0FL)-DX,.DIR0ST,.DIR0CH)
 . Q:'$L(DIR0ST)
 . I '$G(DIR0("REP")) S DIR0A=DIR0A_DIR0ST
 . E  S $E(DIR0A,DIR0C,DIR0C+$L(DIR0ST)-1)=DIR0ST
 . S DX=DX+$L(DIR0ST),DIR0C=DIR0C+$L(DIR0ST)
 E  D READ^DIR01(.DIR0CH)
 Q:DIR0CH=""
 ;
 I "?^"[DIR0CH,DIR0C=1,'DIR0QU D  Q
 . D DEOF X IOXY
 . S DIR0A="",DIR0QU=1 D REP
 D @$S($L(DIR0CH)>1:DIR0CH,$G(DIR0("REP")):"REP",1:"INS")
 I DIR0QU,"?^"'[$E(DIR0A)!'$L(DIR0A) S DIR0QU=0,DIR0A="" D CLR
 Q
 ;
REP I DIR0C>DIR0M W $C(7) Q
 S DIR0CHG=1
 S DIR0A=$E(DIR0A,1,DIR0C-1)_DIR0CH_$E(DIR0A,DIR0C+1,999)
 S DIR0C=DIR0C+1
 W DIR0CH
 I DX<DIR0F S DX=DX+1 Q
 S DIR0LN=DIR0LN+1,DY=DY+1,DX=DIR0S Q
 Q
 ;
INS I $L(DIR0A)'<DIR0M W $C(7) Q
 S DIR0CHG=1
 S DIR0A=$E(DIR0A,1,DIR0C-1)_DIR0CH_$E(DIR0A,DIR0C,999)
 W $E(DIR0A,DIR0C,DIR0C+DIR0F-DX)
 D
 . N DIR0LN,DY,DX
 . S DX=DIR0S
 . F DIR0LN=DIR0C-1\DIR0L+2:1:$L(DIR0A)\DIR0L+1 D
 .. S DY=DIR0R+DIR0LN-1 X IOXY
 .. W $E(DIR0A,DIR0LN-1*DIR0L+1,DIR0LN*DIR0L)
 S DIR0C=DIR0C+1
 I DX<DIR0F S DX=DX+1 Q
 S DIR0LN=DIR0LN+1,DY=DY+1,DX=DIR0S
 Q
 ;
RIGHT Q:DIR0C>$L(DIR0A)
 S DIR0C=DIR0C+1
 I DX<DIR0F!(DIR0LN=DIR0NL) S DX=DX+1 Q
 S DIR0LN=DIR0LN+1,DY=DY+1,DX=DIR0S
 Q
 ;
LEFT Q:DIR0C'>1
 S DIR0C=DIR0C-1
 I DX>DIR0S S DX=DX-1 Q
 S DIR0LN=DIR0LN-1,DY=DY-1,DX=DIR0F
 Q
 ;
JRT Q:DIR0C>$L(DIR0A)
 Q:DX=DIR0F
 S DIR0C=DIR0LN*DIR0L S:DIR0C>$L(DIR0A) DIR0C=$L(DIR0A)+1
 S DX=DIR0C#DIR0L-1+DIR0S S:DX<DIR0S DX=DIR0F
 Q
 ;
JLT Q:DIR0C'>1
 Q:DX=DIR0S
 S DIR0C=DIR0C-DX+DIR0S,DX=DIR0S
 Q
 ;
UP Q:DIR0LN=1
 S DIR0C=DIR0C-DIR0L,DIR0LN=DIR0LN-1,DY=DY-1
 Q
 ;
DOWN Q:DIR0LN=DIR0NL
 Q:$L(DIR0A)\DIR0L<DIR0LN
 S DIR0C=DIR0C+DIR0L,DIR0LN=DIR0LN+1,DY=DY+1
 S:DIR0C>($L(DIR0A)+1) DIR0C=$L(DIR0A)+1,DX=DIR0C#DIR0L+DIR0S-1
 Q
 ;
FDE ;
NP Q:DIR0C>$L(DIR0A)
 S DIR0C=$L(DIR0A)+1,DIR0LN=DIR0C-1\DIR0L+1,DX=DIR0C-1#DIR0L+DIR0S
 S:DIR0LN>DIR0NL DIR0LN=DIR0NL,DX=DIR0S+DIR0NC
 S DY=DIR0R+DIR0LN-1
 Q
 ;
FDB ;
PP Q:DIR0C'>1
 S DIR0LN=1,DY=DIR0R,DX=DIR0S,DIR0C=1
 Q
 ;
BS Q:DIR0C'>1
 S DIR0CHG=1
 S DX=DX-1,DIR0C=DIR0C-1
 S DIR0A=$E(DIR0A,1,DIR0C-1)_$E(DIR0A,DIR0C+1,999)_" "
 I DX<DIR0S S DIR0LN=DIR0LN-1,DY=DY-1,DX=DIR0F
 X IOXY W $E(DIR0A,DIR0C,DIR0C+DIR0F-DX)
 D
 . N DIR0LN,DY,DX
 . S DX=DIR0S
 . F DIR0LN=DIR0C-1\DIR0L+2:1:$L(DIR0A)\DIR0L+1 D
 .. S DY=DIR0R+DIR0LN-1 X IOXY
 .. W $E(DIR0A,DIR0LN-1*DIR0L+1,DIR0LN*DIR0L)
 S DIR0A=$E(DIR0A,1,$L(DIR0A)-1)
 Q
 ;
DEL Q:DIR0C>$L(DIR0A)
 S DIR0CHG=1
 S DIR0A=$E(DIR0A,1,DIR0C-1)_$E(DIR0A,DIR0C+1,999)_" "
 W $E(DIR0A,DIR0C,DIR0C+DIR0F-DX)
 D
 . N DIR0LN,DY,DX
 . S DX=DIR0S
 . F DIR0LN=DIR0C-1\DIR0L+2:1:$L(DIR0A)\DIR0L+1 D
 .. S DY=DIR0R+DIR0LN-1 X IOXY
 .. W $E(DIR0A,DIR0LN-1*DIR0L+1,DIR0LN*DIR0L)
 S DIR0A=$E(DIR0A,1,$L(DIR0A)-1)
 Q
 ;
CLR N %X
 S DIR0CHG=1
 S %X=DIR0A
 I DIR0A]"",DIR0A'=DIR0D S DIR0SV=DIR0A
 S DIR0A=$S(DIR0A=DIR0D:DIR0SV,DIR0A="":DIR0D,1:"")
 S %X=DIR0A_$J("",$L(%X)-$L(DIR0A))
 S DX=DIR0S
 F DIR0LN=1:1:$L(%X)\DIR0L+1 D
 . S DY=DIR0R+DIR0LN-1 X IOXY
 . W $E(%X,DIR0LN-1*DIR0L+1,DIR0LN*DIR0L)
 S (DIR0C,DIR0LN)=1,DY=DIR0R
 Q
 ;
DEOF N %X
 Q:DIR0C>$L(DIR0A)
 S DIR0CHG=1
 S %X=DIR0A,DIR0A=$E(DIR0A,1,DIR0C-1),%X=DIR0A_$J("",$L(%X)-$L(DIR0A))
 W $E(%X,DIR0C,DIR0C+DIR0F-DX)
 D
 . N DIR0LN,DY,DX
 . S DX=DIR0S
 . F DIR0LN=DIR0C-1\DIR0L+2:1:$L(%X)\DIR0L+1 D
 .. S DY=DIR0R+DIR0LN-1 X IOXY
 .. W $E(%X,DIR0LN-1*DIR0L+1,DIR0LN*DIR0L)
 Q
 ;
RPM N DX,DY
 I $D(DDS) S DX=IOM-8,DY=IOSL-1 X IOXY
 I $G(DIR0("REP")) W "Insert " K DIR0("REP")
 E  W "Replace" S DIR0("REP")=1
 Q
 ;
KPM I $G(DDGLKPNM) K DDGLKPNM W $P(DDGLED,DDGLDEL,9)
 E  S DDGLKPNM=1 W $P(DDGLED,DDGLDEL,10)
 Q
 ;
WRT G WRT2^DIR0W
WLT ;
FDL G WLT2^DIR0W
DLW G DLW2^DIR0W
 ;
HLP ;
NB ;
SEL ;
SV ;
RF ;
NOP W $C(7)
 Q
TO I $D(DIR0TO)#2 D @DIR0TO Q
 S DTOUT=1
ZM ;
QT ;
EX ;
CL ;
TAB ;
CR S DIR0DN=1
 Q
