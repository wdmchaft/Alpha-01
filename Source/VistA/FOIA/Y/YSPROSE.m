YSPROSE ;SLC/RWF,SLC/DKG,SLC/TGA-PROSE TEXT GENERATOR ; 7/5/89  11:47 ;
 ;;5.01;MENTAL HEALTH;;Dec 30, 1994
L1 ;
 S YSPTF=YSX_YSPGI_",0)",YSPX=@YSPTF G SKC:YSPTRUE,CMD:$A(YSPX)=124 ;"|"
 D JU
L2 ;
 S YSPGI=YSPGI+1 G CL:YSPGI>YSPGL,L1
 ;
CMD ;
 S YSPCMD=YSPX,YSPX=$P(YSPX,"|") G CMDQ:YSPX=""
 I $E(YSPX)=" " S YSPX=$E(YSPX,2,999) D JU G CMDQ
 S YSPY=+$E(YSPX,1,2) I YSPY S YSPX=$E(YSPX,$E(YSPX,3)?1P+3,255) G CMD2
 S YSPY=$F("TAB,BLA,TOP,MAR,FIL,   ,   ,NEW,PAR",$E(YSPX,1,3))\4+9 S:YSPY<10 YSPY=0 S:YSPY YSPX=$P(YSPX," ",2,256)
CMD2 G CMDQ:YSPY=0,IR:YSPY=1,DIR:YSPY=2,SKIP:YSPY=5,DO:YSPY=7,XQT:YSPY=8,CL:YSPY=19
 S YSPY=$P("VT,TAB,BL,TOP,MAR,FILL,PAGE,SPACE,OUT,PAR,,PGN",",",YSPY-8)
 D:YSPY]"" @YSPY G CMDQ
 D JU
CMDQ ;
 Q:'$D(YSPCMD)  S YSPX=$P(YSPCMD,"|",2,99) G CMD:YSPX]"" K YSPCMD G L2
JU ;
 G JU1:YSPJU=1,JU2:YSPJU>1
 S YSPOL=YSPOL_YSPX G OUT
RL ;
 S YSPI=1
F ;
 S J=YSPTRM-YSPLM-$L(YSPOL),L=J I J<$L(YSPX) F J=J:-1:YSPI Q:$E(YSPX,J)=" "
 I J'>YSPI S J=YSPTRM-YSPLM+YSPI W J ;DEBUG
 D OUT:$L(YSPOL)+YSPLM+J-YSPI>YSPTRM S YSPOL=YSPOL_$E(YSPX,YSPI,J)_$E(" ",J=L),YSPI=J+1
 G F:$L(YSPX)>YSPI Q
JU1 ;
 S YSPI=1
J S J=$F(YSPX," ",YSPI),L=$E(YSPX,YSPI,$S(J:J-1,1:$L(YSPX))),YSPI=J I 'J S L=L_" "
 I $E(YSPX,YSPI)'=" ",".?!"[$E(L,$L(L)-1),$L(L)>4 S L=L_" "
 I YSPLM+$L(L)+$L(YSPOL)>YSPTRM D OUT
 S YSPOL=YSPOL_L G J:J K I,J,YSPX Q
JU2 ;
 Q:YSPX=""  S L=$P(YSPX," "),YSPX=$P(YSPX," ",2,255)
 G JU2:L?." "
 I YSPLM+$L(L)+$L(YSPOL)>YSPTRM D OUT S YSPWC=0
 I $L(L)>3,".?!"[$E(L,$L(L)) S L=L_" "
 S YSPOL=YSPOL_L_" " G JU2 ;%WC=YSPWC+1 G JU2
OUT ;
 I $Y>YSPPL D H
 I YSPOL]"" W @($E("!!!!",1,YSPSKC)),?YSPLM,YSPOL S YSPOL=""
 I YSPPS S YSPLM=YSPPS,YSPPS=0
 Q
IR ;
 S @("YSPX="_YSPX) D JU G CMDQ
 G CMDQ
DIR ;
 S YSPY=$P(YSPX,U),YSPZ=$P(YSPX,U,2) S:YSPY="" YSPY="^"_YSPZ,YSPZ=$P(YSPX,U,3) I $D(@YSPY) S YSPX=$P(@YSPY,U,+YSPZ) D JU
 G CMDQ
SKIP ;
 S YSPSV=$P(YSPX,":",2),YSPX=$P(YSPX,":") I @YSPX G SK2
 G CMDQ
SK2 ;
 S YSPTRUE=$T,YSPST=YSPSV?1A.AN G L2
SKC ;
 I YSPST,$E(YSPX,1,2)="|0",$E(YSPX,5,99)=YSPSV S YSPTRUE=0 G L2
 I 'YSPST,$E(YSPX,1,2)="|0" S YSPSV=YSPSV-1 I YSPSV=0 S YSPTRUE=0 G L2
 G L2
DO ;
 D @YSPX G CMDQ
XQT ;
 X YSPX G CMDQ
VT ;
 D OUT S YSPX=$S(YSPX>YSPPL:YSPPL,1:YSPX) F YSPI=$Y:1:+YSPX W !
 Q
BL ;
 D OUT,H:$Y+YSPX>YSPPL Q:YSLFT  F YSPI=1:1:+YSPX W !
 K YSPI Q
TOP ;
 Q:$D(YSNOFORM)  D ENHD^YSFORM Q
MAR ;
 D OUT S YSPLM=$P(YSPX,U),YSPTRM=$P(YSPX,U,2) Q
FILL ;
 D OUT S YSPJU=+YSPX S:YSPJU>3 YSPJU=2 Q
PAGE ;
 S YSPPL=+YSPX I (YSPPL<3)!(YSPPL>66) S YSPPL=50
 Q
SPACE ;
 D OUT S YSPSKC=+YSPX Q
TAB ;
 S YSPY=+YSPX,YSPZ=$P(YSPX,U,2),YSPX=$L(YSPOL)+YSPLM S:'YSPY YSPY=80-$L(YSPZ)\2 F YSPI=YSPX:1:YSPY-1 S YSPOL=YSPOL_" "
 S YSPX=YSPZ K YSPI,YSPZ G JU
PAR ;
 D OUT S YSPOL=" " D OUT S YSPPS=YSPLM I YSPX?1P,"+-"[YSPX S @("YSPLM=YSPLM"_YSPX_"5") Q
 Q
PGN ;
 S:YSPX YSPPGN=YSPX K:'YSPX YSPPGN Q
 E  S:YSPX?.N YSPLM=+YSPX Q
CL ;
 D OUT Q
H ;
 I $D(YSNOFORM) D:'YST WAIT W @IOF Q
 S:YST YSCON=1 D ENFT^YSFORM:YST,WAIT:'YST Q:YSLFT  D:YST ENHD^YSFORM Q
WAIT ;
 F I0=1:1:IOSL-$Y-2 W !
 W:$Y+1<IOSL !
 N DTOUT,DUOUT,DIRUT
 S DIR(0)="E" D ^DIR K DIR S YSTOUT=$D(DTOUT),YSUOUT=$D(DUOUT),YSLFT=$D(DIRUT)
 W @IOF Q:'YSLFT  S YSPGI=YSPGL,(YSPCMD,YSPOL,YSPX,L)=""
 Q
EN1 ; Called by routine YSPHYR, YSPP7
 S (YSCON,YSLFT)=0,YSPTF=YSX_"YSPGI)",YSPGI=0,YSPGL=$P(@YSPTF,U,4),YSPGI=1,YSPTRM=78,YSPOL="",YSPJU=1,YSPLM=0,YSPSKC=1,YSPPS=0,YSPWC=0,YSPTRUE=0,YST=$S(IOST?1"P".E:1,1:0),YSPPL=$S(YST:IOSL-8,1:IOSL-3) U IO D L1
 K YSPCMD,YSPGI,YSPGL,YSPJU,YSPLM,YSPOL,YSPPS,YSPSKC,YSPTF,YSPTRM,YSPTRUE,YSPWC,YSPX Q
