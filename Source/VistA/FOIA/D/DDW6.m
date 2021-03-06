DDW6 ;SFISC/MKO-JOIN ;10:41 AM  16 Jun 2000
 ;;22.0;VA FileMan;**18**;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
REFMT ;Reformat
 N DDWRFMT
 I $D(DDWMARK),DDWRW+DDWA'>$P(DDWMARK,U,3) D UNMARK^DDW7
 D POS(DDWRW,DDWLMAR,"R")
 S DDWRFMT=0 F  D JOIN Q:DDWRFMT
 Q
 ;
JOIN ;Join
 N DDWI,DDWSCR,DDWNSV,DDWLL,DDWTXT,DDWTXT0
 I $D(DDWMARK),DDWRW+DDWA'>$P(DDWMARK,U,3) D UNMARK^DDW7
 ;
 ;Get current line
 S (DDWTXT(1),DDWNSV)=DDWN
 ;
 ;Get next line
 I DDWRW=DDWMR S:DDWSTB DDWTXT(2)=^TMP("DDW1",$J,DDWSTB)
 E  S:DDWA+DDWRW<DDWCNT DDWTXT(2)=DDWL(DDWRW+1)
 ;
 I $G(DDWTXT(2))?." " D  Q:$G(DDWRFMT)
 . I $L(DDWN)>DDWRMAR S:$D(DDWTXT(2))#2 DDWLL=DDWTXT(2)
 . E  I $D(DDWRFMT) S DDWRFMT=1
 ;
 ;Adjust
 S DDWTXT0=$O(DDWTXT(""),-1)
 D ADJMAR(.DDWTXT,"","I")
 S:$D(DDWLL) DDWTXT=DDWTXT+1,DDWTXT(DDWTXT)=DDWLL
 S (DDWN,DDWL(DDWRW))=DDWTXT(1)
 ;
 ;Delete next line
 I DDWTXT0>1,DDWTXT=1 D
 . I DDWRW=DDWMR S DDWSTB=DDWSTB-1,DDWCNT=DDWCNT-1,$E(DDWBF,1,3)=111
 . E  D POS(DDWRW+1,DDWC,"RN"),XLINE^DDW5(1),POS(DDWRW-1,DDWC,"RN")
 ;
 ;DDWSCR: curr scr = final scr
 I DDWTXT=1,'$D(DDWRFMT) S DDWSCR=$L(DDWTXT(1))+1-DDWOFS
 E  S DDWSCR=DDWLMAR-DDWOFS
 S DDWSCR=DDWSCR'<1&(DDWSCR'>IOM)
 ;
 I DDWSCR,DDWNSV'=DDWN D
 . I DDWNSV]"",$P(DDWNSV,DDWN)="" D
 .. D CUP(DDWRW,$$MAX($L(DDWN)+1-DDWOFS,1))
 .. W $P(DDGLCLR,DDGLDEL)
 . E  I DDWN]"",$P(DDWN,DDWNSV)="" D
 .. D CUP(DDWRW,$$MAX($L(DDWNSV)+1-DDWOFS,1))
 .. W $E(DDWN,$$MAX($L(DDWNSV),DDWOFS)+1,IOM+DDWOFS)
 . E  D
 .. D CUP(DDWRW,DDWOFS+1)
 .. W $P(DDGLCLR,DDGLDEL)_$E(DDWN,DDWOFS+1,IOM+DDWOFS)
 ;
 I DDWTXT=1 D
 . I '$D(DDWRFMT) D
 .. D POS(DDWRW,"E","RN")
 . E  D POS(DDWRW,DDWLMAR,"RN")
 E  D JOIN2
 Q
 ;
JOIN2 ;Join produced >1 lines
 D POS(DDWRW,DDWLMAR,"R")
 ;
 I DDWTXT0=2 D
 . I DDWRW<DDWMR D
 .. S DDWL(DDWRW+1)=DDWTXT(2)
 .. S DDWRW=DDWRW+1
 .. I DDWSCR D
 ... D CUP(DDWRW,1)
 ... W $P(DDGLCLR,DDGLDEL)_$E(DDWL(DDWRW),1+DDWOFS,IOM+DDWOFS)
 . E  D
 .. S ^TMP("DDW1",$J,DDWSTB)=DDWTXT(2)
 .. D MVFWD^DDW3(1)
 ;
 F DDWI=DDWTXT0+1:1:DDWTXT D
 . D ILINE^DDW5
 . S (DDWN,DDWL(DDWRW))=DDWTXT(DDWI)
 . D CUP(DDWRW,1)
 . W $P(DDGLCLR,DDGLDEL)_$E(DDWN,1+DDWOFS,IOM+DDWOFS)
 ;
 D POS(DDWRW-($D(DDWLL)#2),DDWLMAR,"RN")
 Q
 ;
ADJMAR(DDWT,DDWW,DDWFLG) ;Adjust length of text in DDWT array
 ;  DDWT = Text array
 ;  DDWW = Width
 ;DDWFLG = I:First line $L=DDWRMAR, subsequent $L=DDWRMAR-DDWLMAR+1
 ;
 N DDWJ
 S DDWJ=1
 I $G(DDWFLG)["I" S DDWW=DDWRMAR
 E  I '$D(DDWW) S DDWW=DDWRMAR-DDWLMAR+1
 ;
 F  Q:'$D(DDWT(DDWJ))  D AMLOOP
 S DDWT=$O(DDWT(""),-1)
 I DDWLMAR>1 F DDWJ=$G(DDWFLG)["I"+1:1:DDWT D
 . S DDWT(DDWJ)=$J("",DDWLMAR-1)_DDWT(DDWJ)
 Q
 ;
AMLOOP ;Process DDWT(DDWJ)
 I $E(DDWT(DDWJ),1,DDWW)=$J("",DDWW) S DDWT(DDWJ)=$$LD(DDWT(DDWJ))
 ;
 E  I $L(DDWT(DDWJ))>DDWW F  D  Q:$L(DDWT(DDWJ))'>DDWW
 . N DDWK,DDWFST,DDWLST
 . F DDWK=$O(DDWT(""),-1)+1:-1:DDWJ+2 S DDWT(DDWK)=DDWT(DDWK-1)
 . D SLICE(DDWT(DDWJ),DDWW,.DDWFST,.DDWLST)
 . S DDWT(DDWJ)=DDWFST,DDWT(DDWJ+1)=DDWLST
 . D AMINCJ
 ;
 E  I $L(DDWT(DDWJ))=DDWW!'$D(DDWT(DDWJ+1)) D
 . I DDWRAP,$D(DDWT(DDWJ+1)) S DDWT(DDWJ+1)=$$LD(DDWT(DDWJ+1))
 . D AMINCJ
 ;
 E  I 'DDWRAP D
 . N DDWK S DDWK=DDWW-$L(DDWT(DDWJ))
 . S DDWT(DDWJ)=DDWT(DDWJ)_$E(DDWT(DDWJ+1),1,DDWK)
 . S DDWT(DDWJ+1)=$E(DDWT(DDWJ+1),DDWK+1,999)
 . D:DDWT(DDWJ+1)="" AMSHIFT(.DDWT,DDWJ+1)
 ;
 E  D
 . N DDWD,DDWI,DDWNXT,DDWSP,DDWX1,DDWX2
 . S DDWD=0 F  D  Q:DDWD
 .. S DDWX1=DDWT(DDWJ),(DDWX2,DDWT(DDWJ+1))=$$LD(DDWT(DDWJ+1))
 .. I DDWX2="" S DDWD=1 Q
 .. S DDWNXT=$P(DDWX2," "),DDWI=$L(DDWNXT)
 .. I $E(DDWX2,DDWI+2)=" ",$E(DDWX2,DDWI+3,999)'?." " D
 ... F DDWI=DDWI+2:1 Q:$E(DDWX2,DDWI+1)'=" "
 .. S DDWSP=DDWX1'?.E1" "
 .. I $L(DDWX1)+DDWSP+$L($E(DDWX2,1,DDWI))>DDWW S DDWD=1 Q
 .. S DDWT(DDWJ)=DDWX1_$E(" ",DDWSP)_$E(DDWX2,1,DDWI)
 .. S DDWT(DDWJ+1)=$$LD($E(DDWX2,DDWI+1,999))
 . ;
 . I DDWT(DDWJ+1)="" D
 .. D AMSHIFT(.DDWT,DDWJ+1)
 . E  D AMINCJ
 Q
 ;
AMSHIFT(DDWT,DDWJ) ;Delete DDWT(DDWJ) and shift up
 N DDWI
 F DDWI=DDWJ:1:$O(DDWT(""),-1)-1 S DDWT(DDWI)=DDWT(DDWI+1)
 K DDWT($O(DDWT(""),-1))
 Q
 ;
AMINCJ ;Incr DDWJ
 I DDWJ=1,$G(DDWFLG)["I" S DDWW=DDWRMAR-DDWLMAR+1
 S DDWJ=DDWJ+1
 Q
 ;
SLICE(DDWN,DDWW,DDWFST,DDWRST) ;
 ;Out: DDWFST=first part of text, $L<=DDWRMAR
 ;     DDWRST=remaining part (lead blanks removed)
 N DDWI,DDWP,DDWX
 S:'$G(DDWW) DDWW=DDWRMAR
 I 'DDWRAP S DDWFST=$E(DDWN,1,DDWW),DDWLST=$E(DDWN,DDWW+1,999) Q
 ;
 ;Set DDWI to column # at which to break
 S DDWX=$E(DDWN,1,DDWW),DDWI=DDWW
 I DDWX'[" "
 E  I DDWX?." "
 E  I $E(DDWX,DDWW)=" ",$E(DDWN,DDWW+1)'=" "
 E  D
 . F DDWP=$L(DDWX," "):-1:0 Q:$P(DDWX," ",DDWP)]""
 . Q:DDWP=1
 . S DDWI=$L($P(DDWX," ",1,DDWP-1))+1
 . S:DDWI'>$S(DDWW=DDWRMAR:DDWLMAR,1:1) DDWI=DDWW
 ;
 S DDWFST=$E(DDWN,1,DDWI),DDWRST=$$LD($E(DDWN,DDWI+1,999))
 Q
 ;
TR(X) Q:$G(X)="" X
 N I
 F I=$L(X):-1:0 Q:$E(X,I)'=" "
 Q $E(X,1,I)
 ;
LD(X) Q:$G(X)="" X
 N I
 F I=1:1:$L(X)+1 Q:$E(X,I)'=" "
 Q $E(X,I,999)
 ;
CUP(Y,X) ;
 S DY=IOTM+Y-2,DX=X-1 X IOXY
 Q
 ;
POS(R,C,F) ;Pos cursor
 N DDWX
 S:$G(C)="E" C=$L($G(DDWL(R)))+1
 S:$G(F)["N" DDWN=$G(DDWL(R))
 S:$G(F)["R" DDWRW=R,DDWC=C
 ;
 S DDWX=C-DDWOFS
 I DDWX>IOM!(DDWX<1) D SHIFT^DDW3(C,.DDWOFS)
 S DY=IOTM+R-2,DX=C-DDWOFS-1 X IOXY
 Q
 ;
SCR(C) ;Screen number
 Q C-$P(DDWOFS,U,2)-1\$P(DDWOFS,U,3)+1
 ;
MIN(X,Y) ;
 Q $S(X<Y:X,1:Y)
MAX(X,Y) ;
 Q $S(X>Y:X,1:Y)
