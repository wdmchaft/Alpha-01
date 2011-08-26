PSOFSIG ;BIR/RTR-Parse out and create Pharmacy Sig ;7/21/96
 ;;7.0;OUTPATIENT PHARMACY;**46**;DEC 1997
 ;External reference to File #50.7 supported by DBIA 2223
 ;External reference to File #51 supported by DBIA 2224
 ;External reference to File #51.1 supported by DBIA 2225
 ;External reference to File #51.2 supported by DBIA 2226
 ;External reference to File #50.606 supported by DBIA 2174
EN(PSOFX,PSOPTSIG) ;
 N LIM,VAR,VAR1
 N SDF,SZZ,ZZS,ZZSB,SSZZ,SCHHOLD,GGGZ,SGLFLAG,SGLOOP,ZSCHED,SPFG,PSNOUN,MEDEXP,PSDUR,NOUN,SCHED,INTERVAL,SIG0,SIG2,SIG3,SDL,WW,TODOSE,PDAYS,WWFL,PSOCJ
 N VERBX,SSS,TT,DCOUNT,PREP,VERB,FFF,GGG,SIGDS,SIGRT,PSOROUTE,PSOSG1,PSOSG2,RTC,RTCA,RTCF,RTCNT,PSODCT,PSOBDCT
 K SIG
 S TODOSE=0 F WW=0:0 S WW=$O(PSOFX("DOSE",WW)) Q:'WW  S TODOSE=WW
 Q:'TODOSE
 S SIGDS=+$P($G(^PS(50.7,+$G(PSODRUG("OI")),0)),"^",2),PREP=$P($G(^PS(50.606,SIGDS,"MISC")),"^",3)
 S RTCNT=0 K RTC,RTCA,RTCF F SSS=1:1:TODOSE D
 .S SIG0(SSS)=$S($G(PSOFX("DOSE ORDERED",SSS))'="":$G(PSOFX("DOSE ORDERED",SSS)),1:$G(PSOFX("DOSE",SSS)))
 .S VERBX(SSS)=$S($G(PSOFX("VERB",SSS))'="":$G(PSOFX("VERB",SSS)),1:"")
 .S PSNOUN(SSS)=$G(PSOFX("NOUN",SSS))
 .S RTC=+$G(PSOFX("ROUTE",SSS)) I RTC S:'RTCNT RTCA=RTC S RTCNT=RTCNT+1
 .I RTCNT>1,$G(RTC),$G(RTC)'=$G(RTCA) S RTCF=1
 .S PSOROUTE(SSS)=$S($P($G(^PS(51.2,+$G(PSOFX("ROUTE",SSS)),0)),"^",2)'="":$P(^(0),"^",2),$P($G(^(0)),"^",3)'="":$P(^(0),"^",3),1:$P($G(^(0)),"^")) S MEDEXP(SSS)=$S($P($G(^PS(51.2,+$G(PSOFX("ROUTE",SSS)),0)),"^",2)="":0,1:1)
 .S PDAYS(SSS)=$G(PSOFX("DURATION",SSS))
 .I $G(PSOFX("DURATION",SSS))'="",($E(PSOFX("DURATION",SSS),$L(PSOFX("DURATION",SSS)))'?1A) S PDAYS(SSS)=PDAYS(SSS)_"D"
 .S PSDUR(SSS)=$S($G(PDAYS(SSS))="":"NULL",1:"FOR "_$E($G(PDAYS(SSS)),1,($L($G(PDAYS(SSS)))-1))) D  I PSDUR(SSS)'="NULL" S PSDUR(SSS)=PSDUR(SSS)_" "_INTERVAL
 ..I PSDUR(SSS)'="NULL" S INTERVAL=$E(PDAYS(SSS),$L(PDAYS(SSS))),INTERVAL=$S(INTERVAL="D":"DAYS",INTERVAL="W":"WEEKS",INTERVAL="H":"HOURS",INTERVAL="L":"MONTHS",INTERVAL="M":"MINUTES",INTERVAL="S":"SECONDS",1:"") D
 ...I $G(INTERVAL)'="",$G(PSOFX("DURATION",SSS)),$G(PSOFX("DURATION",SSS))'>1 S INTERVAL=$E(INTERVAL,1,($L(INTERVAL)-1))
 F GGG=1:1:TODOSE S ZSCHED(GGG)=$G(PSOFX("SCHEDULE",GGG)) D
 .I $G(ZSCHED(GGG))="" S SCHED(GGG)="" Q
 .S SGLFLAG=0 F WW=0:0 S WW=$O(^PS(51.1,"B",ZSCHED(GGG),WW)) Q:'WW!($G(SGLFLAG))  I $P($G(^PS(51.1,WW,0)),"^",8)'="" S SCHED(GGG)=$P($G(^(0)),"^",8),SGLFLAG=1
 .Q:$G(SGLFLAG)
 .I $G(^PS(51,"A",ZSCHED(GGG)))'="" S SCHED(GGG)=$P(^(ZSCHED(GGG)),"^") Q
 .S ZZSB=0 F ZZS=1:1:$L(ZSCHED(GGG)) S SZZ=$E(ZSCHED(GGG),ZZS) I SZZ=" " S ZZSB=ZZSB+1
 .S ZZSB=ZZSB+1
 .K SCHHOLD F GGGZ=1:1:ZZSB S (SDL,SCHHOLD(GGGZ))=$P(ZSCHED(GGG)," ",GGGZ) D
 ..Q:$G(SDL)=""
 ..S SGLFLAG=0 F WW=0:0 S WW=$O(^PS(51.1,"B",SDL,WW)) Q:'WW!($G(SGLFLAG))  I $P($G(^PS(51.1,WW,0)),"^",8)'="" S SCHHOLD(GGGZ)=$P($G(^(0)),"^",8),SGLFLAG=1
 ..Q:$G(SGLFLAG)
 ..I $G(^PS(51,"A",SDL))'="" S SCHHOLD(GGGZ)=$P(^(SDL),"^")
 .S SCHED(GGG)="",SGLFLAG=0 F WW=1:1:ZZSB S SCHED(GGG)=SCHED(GGG)_$S($G(SGLFLAG):" ",1:"")_$G(SCHHOLD(WW)),SGLFLAG=1
 S (RTC,RTCA,PSOBDCT)=0 F FFF=0:0 S FFF=$O(SIG0(FFF)) Q:'FFF  D
 .K PSOSG1,PSOSG2 S VERB=$G(VERBX(FFF)) D VERB D:$G(PSNOUN(FFF))'=""&('$G(PSOSG1)) SSS
 .D FRAC
 .S SIG2(FFF)=$S($G(VERB)'=""&('$G(PSOSG1)):$G(VERB)_" ",1:"")_$S($G(PSOFX("DOSE ORDERED",FFF))'="":$S($G(PSOFRAC)'="":$G(PSOFRAC),1:$G(PSOFX("DOSE ORDERED",FFF)))_" ",1:$G(PSOFX("DOSE",FFF))_" ")
 .S PSOBDCT=PSOBDCT+1
 .K PSOFRAC,PSOFRACX
 .I RTC>0,$G(PSOROUTE(FFF))'="",'$G(RTCF) S RTCA=1
 .I $G(PSOROUTE(FFF))'="" S RTC=RTC+1
 .S SIG2(FFF)=SIG2(FFF)_$S($G(PSNOUN(FFF))'=""&('$G(PSOSG2)):$G(PSNOUN(FFF))_" ",1:"")_$S(PREP'=""&($G(MEDEXP(FFF)))&('RTCA):PREP_" ",1:"")
 .S SIG2(FFF)=SIG2(FFF)_$S(PSOROUTE(FFF)'=""&('RTCA):PSOROUTE(FFF)_" ",1:"")
 .;S SIG2(FFF)=SIG2(FFF)_$S(SCHED(FFF)'="":SCHED(FFF)_" ",1:"")_$S(PSDUR(FFF)'="NULL":PSDUR(FFF)_" ",1:"")_$S($G(PSOFX("CONJUNCTION",FFF))="A":"AND",$G(PSOFX("CONJUNCTION",FFF))="T":"THEN",$G(PSOFX("CONJUNCTION",FFF))="S":"THEN",1:"")
 .S SIG2(FFF)=SIG2(FFF)_$S(SCHED(FFF)'="":SCHED(FFF)_$S($G(PSDUR(FFF))="NULL"&($G(PSOFX("CONJUNCTION",FFF))="")&('$O(SIG0(FFF))):"",1:" "),1:"")
 .S PSOCJ=$E($G(PSOFX("CONJUNCTION",FFF)))
 .S SIG2(FFF)=SIG2(FFF)_$S(PSDUR(FFF)'="NULL":PSDUR(FFF)_$S($G(PSOFX("CONJUNCTION",FFF))=""&('$O(SIG0(FFF))):"",1:", "),1:"")_$S($G(PSOCJ)="A":"AND",$G(PSOCJ)="T":"THEN",$G(PSOCJ)="S":"THEN",$G(PSOCJ)="X":"EXCEPT",1:"")
 .K PSOSG1,PSOSG2
 .K PSOUCS S SIG2(FFF)=$$UPPER(SIG2(FFF)) K PSOUCS
 ;I $G(PSOFX("SIG"))'="" S TODOSE=TODOSE+1,SIG2(TODOSE)=$G(PSOFX("SIG")) K PSOUCS S SIG2(TODOSE)=$$UPPER(SIG2(TODOSE)) K PSOUCS
 S PSODCT="" F  S PSODCT=$O(PSOFX("SIG",PSODCT)) Q:PSODCT=""  S PSOBDCT=PSOBDCT+1 S SIG2(PSOBDCT)=$G(PSOFX("SIG",PSODCT)) K PSOUCS S SIG2(PSOBDCT)=$$UPPER(SIG2(PSOBDCT)) K PSOUCS
STUFF ;
 S DCOUNT=0
 I '$D(SIG2(1)) G QUIT
 I '$O(SIG2(1)),$L(SIG2(1))<71 S SIG(1)=SIG2(1) G PTSIG
 S (VAR,VAR1)="",II=1
 F FF=0:0 S FF=$O(SIG2(FF)) Q:'FF  S CT=0 F NN=1:1:$L(SIG2(FF)) I $E(SIG2(FF),NN)=" "!($L(SIG2(FF))=NN) S CT=CT+1 D  I $L(VAR)>70 S SIG(II)=LIM_" ",II=II+1,VAR=VAR1
 .S VAR1=$P(SIG2(FF)," ",(CT))
 .S LIM=VAR
 .S VAR=$S(VAR="":VAR1,1:VAR_" "_VAR1)
 I $G(VAR)'="" S SIG(II)=VAR
 ;F II=0:0 S II=$O(SIG3(II)) Q:'II  S DCOUNT=DCOUNT+1 S ^PS(52.41,PENDING,"SIG",DCOUNT,0)=SIG3(II)
 ;I DCOUNT S ^PS(52.41,PENDING,"SIG",0)="^52.4124A^"_DCOUNT_"^"_DCOUNT
PTSIG ;
 I '$G(PSOPTSIG) G QUIT
 I $O(SIG(0)) W ! S WWFL=0 F WW=0:0 S WW=$O(SIG(WW)) Q:'WW  D
 .W ! I 'WWFL W "("
 .W $G(SIG(WW)) S WWFL=1
 I $O(SIG(0)) W ")",!
QUIT K SSS,TT,DCOUNT,PREP,VERB,FFF,GGG,SIGDS,SIGRT,PSOROUTE,PSOSG1,PSOSG2 Q
SIG1 ;
 F FFF=0:0 S FFF=$O(SIG0(FFF)) Q:'FFF  S SIG2(FFF)=SIG0(FFF)
 Q
DAYS I +$E($P(SIG1(TT),"^",2))!($E($P(SIG1(TT),"^",2))=0) S $P(SIG1(TT),"^",2)="D"_$P(SIG1(TT),"^",2)
 Q
NON ;
 I $P($G(SIG0(SSS)),"&",2)'="" S PSNOUN(SSS)=$P($G(SIG0(SSS)),"&",2) Q
 Q
 F NOUN=0:0 S NOUN=$O(^PS(50.606,SIGDS,"NOUN",NOUN)) Q:'NOUN!($G(PSNOUN(SSS))'="")  I $P($G(^PS(50.606,SIGDS,"NOUN",NOUN,0)),"^")'="" S PSNOUN(SSS)=$P(^(0),"^")
 Q
VERB ;Check if verb and noun need to be added to SIG
 K PSOLCS,PSOUCS,PSOISL,PSOVL
 I $G(VERB)'="" S PSOVL=$L(VERB),PSOISL=$E($G(SIG0(FFF)),1,$G(PSOVL)) I $G(PSOISL)'="" D
 .S PSOUCS=VERB
 .S PSOUCS=$$UPPER(PSOUCS) I PSOUCS=PSOISL S PSOSG1=1 Q
 .S PSOUCS=$$LOWER(PSOUCS) I PSOUCS=PSOISL S PSOSG1=1 Q
 .S PSOUCS=$$UPPER($E(PSOUCS,1))_$$LOWER($E(PSOUCS,2,99)) I PSOUCS=PSOISL S PSOSG1=1 Q
 I $G(PSNOUN(FFF))="" G VERBEX
 S PSOISL=$G(SIG0(FFF)) I $G(PSOISL)="" G VERBEX
 S PSOVL=$F(PSNOUN(FFF),"(")
 I $G(PSOVL)>2 S PSOUCS=$E(PSNOUN(FFF),1,(PSOVL-2))
 I $G(PSOVL)'>2 S PSOUCS=PSNOUN(FFF)
 I $G(PSOISL)'="" D
 .S PSOUCS=$$UPPER(PSOUCS) I PSOISL[PSOUCS S PSOSG2=1 Q
 .S PSOUCS=$$LOWER(PSOUCS) I PSOISL[PSOUCS S PSOSG2=1 Q
 .S PSOUCS=$$UPPER($E(PSOUCS,1))_$$LOWER($E(PSOUCS,2,99)) I PSOISL[PSOUCS S PSOSG2=1
VERBEX K PSOLCS,PSOUCS,PSOISL,PSOVL Q
 ;
UPPER(PSOUCS) ;
 Q $TR(PSOUCS,"abcdefghijklmnopqrstuvwxyz","ABCDEFGHIJKLMNOPQRSTUVWXYZ")
 ;
LOWER(PSOLCS) ;
 Q $TR(PSOLCS,"ABCDEFGHIJKLMNOPQRSTUVWXYZ","abcdefghijklmnopqrstuvwxyz")
 Q
 ;
SSS ;
 K PSOFNL,PSOFNLF,PSOFNLX
 Q:$G(PSNOUN(FFF))=""
 Q:$L(PSNOUN(FFF))'>3
 Q:'$G(PSOFX("DOSE ORDERED",FFF))
 ;Q:$G(PSOFX("DOSE ORDERED",FFF))>1
 S PSOFNL=$E(PSNOUN(FFF),($L(PSNOUN(FFF))-2),$L(PSNOUN(FFF)))
 I $G(PSOFNL)="(S)"!($G(PSOFNL)="(s)") D
 .I $G(PSOFX("DOSE ORDERED",FFF))'>1 S PSNOUN(FFF)=$E(PSNOUN(FFF),1,($L(PSNOUN(FFF))-3))
 .I $G(PSOFX("DOSE ORDERED",FFF))>1 S PSNOUN(FFF)=$E(PSNOUN(FFF),1,($L(PSNOUN(FFF))-3))_$E(PSOFNL,2)
 Q
FRAC ;
 K PSOFRAC,PSOFRACX,PSOFRAC1,PSOFRAC2
 I $G(PSOFX("DOSE ORDERED",FFF))="" Q
 I $G(PSOFX("DOSE ORDERED",FFF))'["." S (PSOFRAC1,PSOFRAC)=$G(PSOFX("DOSE ORDERED",FFF)) D NUM D  G FRACQ
 .I $G(PSOFRAC1)=$G(PSOFRAC) K PSOFRAC,PSOFRAC1 Q
 .S PSOFRAC=$G(PSOFRAC1)
 S PSOFRAC1=$P(PSOFX("DOSE ORDERED",FFF),"."),PSOFRAC2=$P(PSOFX("DOSE ORDERED",FFF),".",2)
 S PSOFRACX="."_$G(PSOFRAC2)
 S PSOFRAC=$S(PSOFRACX=".5":"ONE-HALF",PSOFRACX=".25":"ONE-FOURTH",PSOFRACX=".33":"ONE-THIRD",PSOFRACX=".34":"ONE-THIRD",PSOFRACX=".50":"ONE-HALF",PSOFRACX=".66":"TWO-THIRDS",PSOFRACX=".67":"TWO-THIRDS",PSOFRACX=".75":"THREE-FOURTHS",1:"")
 I $G(PSOFRAC)="" K PSOFRAC G FRACQ
 I $G(PSOFRAC1)'="",+$G(PSOFRAC1) D NUM S PSOFRAC=$G(PSOFRAC1)_" AND "_$G(PSOFRAC)
FRACQ K PSOFRAC1,PSOFRAC2
 Q
NUM ;
 Q:$G(PSOFRAC1)=""
 S PSOFRAC1=$S(PSOFRAC1="1":"ONE",PSOFRAC1="2":"TWO",PSOFRAC1="3":"THREE",PSOFRAC1="4":"FOUR",PSOFRAC1="5":"FIVE",PSOFRAC1="6":"SIX",PSOFRAC1="7":"SEVEN",PSOFRAC1="8":"EIGHT",PSOFRAC1="9":"NINE",PSOFRAC1="10":"TEN",1:PSOFRAC1)
 Q
SET ;Set duration to proper format for storage
 Q
KILL ;kills duration data field
 Q
DUR ;Input Transform for duration
 K:X'?.N&(X'?.N1".".N)&(X'?.N1"D")&(X'?.N1".".N1"D")&(X'?.N1"M")&(X'?.N1".".N1"M")&(X'?.N1"H")&(X'?.N1".".N1"H")&(X'?.N1"W")&(X'?.N1".".N1"W")&(X'?.N1"L")&(X'?.N1".".N1"L") X
 K:'$G(X) X
 Q
