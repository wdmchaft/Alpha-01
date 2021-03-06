PSOHLSG3 ;BHAM ISC/SAB/LC - BUILD PROFILE ; 03/20/96 19:38
 ;;7.0;OUTPATIENT PHARMACY;;DEC 1997
START ;build profile for the NTE4 segment
 Q:'$D(DFN)
 N II K ^TMP($J,"PRF")
 S PSODFN=DFN I '$D(PSODTCUT) D CUTDATE^PSOFUNC
 S:'$D(Z) Z=1 S:'$D(NEW1) (NEW1,NEW11)="^" S %DT="",X="T" D ^%DT S DT=Y S X1=DT,X2=-365 D C^%DTC S EXPS=X S X1=DT,X2=-182 D C^%DTC S EXP=X
 F RXX=0:0 S RXX=$O(^PS(55,DFN,"P",RXX)) Q:'RXX  S RXNN=+^(RXX,0) I $D(^PSRX(RXNN,0)),$P($G(^("STA")),"^")'=13 S RXPX=^PSRX(RXNN,0),$P(RXPX,"^",15)=$P($G(^("STA")),"^"),RXPX2=^(2) D CHK
 I '$D(^TMP($J,"PRF")) G PPP
 ;
SD S CNT=1 F SD="A","C","S" I $D(^TMP($J,"PRF",SD)) S DRNME="" D DRNME
PPP D PEND
 K ^TMP($J,"PRF")
 K A,B,DRNME,DRP,EXP,EXPS,I,II,ISSD,J,LINE,LN,MESS,MJK,NEW1,NEW11,PHYS,POP,QTY,TTTT,RFL,RFS,RXF,RXNN,RXPX,RXPX2,RXPNO,RXX
 K SD,SIG,STA,X,X1,X2,Y,Z,CNT,PEND,PSODTCUT,PSOPRPAS,PZZODRUG,RFDATE,RS
 Q
DRNME S DRNME=$O(^TMP($J,"PRF",SD,DRNME)) Q:DRNME=""  D ISSD G DRNME
 ;
ISSD F ISSD=0:0 S ISSD=$O(^TMP($J,"PRF",SD,DRNME,ISSD)) Q:'ISSD  S RXPNO="" D RXPNO
 Q
RXPNO S RXPNO=$O(^TMP($J,"PRF",SD,DRNME,ISSD,RXPNO)) Q:RXPNO=""  S RXNN=^(RXPNO) I $D(^PSRX(RXNN,0)) S RXPX=^(0),RXPX2=^(2) D PRT G RXPNO
 ;
CHK Q:PSODTCUT>$P(RXPX2,"^",6)
 I $P(^PSRX(RXNN,"STA"),"^")=12 S II=RXNN D LAST^PSORFL Q:PSODTCUT>RFDATE
 I $P(RXPX,"^",3)=7!($P(RXPX,"^",3)=8)&('PSOPRPAS) Q
 S J="^"_RXNN_"^" Q:(NEW1[J)!(NEW11[J)  Q:$P(RXPX,"^",13)<EXPS  S RXPNO=$P(RXPX,"^"),ISSD=$P(RXPX,"^",13)
 Q:'$D(^PSDRUG($P(RXPX,"^",6),0))  S DRP=^(0),SD=$S($P(DRP,"^",3)["S":"S",$P(RXPX,"^",15)=12:"C",1:"A"),DRNME=$P(DRP,"^"),^TMP($J,"PRF",SD,DRNME,ISSD,RXPNO)=RXNN
 Q
PRT S RFS=$P(RXPX,"^",9),QTY=$P(RXPX,"^",7)
 S PHYS=$S($D(^VA(200,$P(RXPX,"^",4),0)):$P(^(0),"^"),1:"UNKNOWN"),II=RXNN D LAST^PSORFL S RXF=0 F MJK=0:0 S MJK=$O(^PSRX(RXNN,1,MJK)) Q:'MJK  S RXF=RXF+1
 S STA=$S($P(^PSRX(RXNN,"STA"),"^")=14:"DC",$P(^PSRX(RXNN,"STA"),"^")=15:"DE",$P(^PSRX(RXNN,"STA"),"^")=16:"PH",1:$E("ANRHPS     ECD",(1+$P(^PSRX(RXNN,"STA"),"^")))),STA=$S(DT>$P(RXPX2,"^",6):"E",1:STA)
 D SIG F TTTT=0:0 S TTTT=$O(FSIG(TTTT)) Q:'TTTT  S FSIG=FSIG(TTTT)
 S ^TMP("PSO",$J,PSI)="NTE"_FS_4_FS_FS,NTE4=1
 S ^TMP("PSO",$J,PSI,CNT)=SD_CS_RXPNO_CS_DRNME_CS_$E(ISSD,4,5)_"/"_$E(ISSD,6,7)
 S ^TMP("PSO",$J,PSI,CNT)=^TMP("PSO",$J,PSI,CNT)_CS_$E(RFL,1,5)_CS_RFS_CS_RXF_CS_QTY_CS_STA_CS_$E(PHYS,1,20)_CS_$S($G(FSIG)'="":FSIG,1:"""""")
 S CNT=CNT+1
 Q
SIG ;Format Sig
 S PSPROSIG=$P($G(^PSRX(RXNN,"SIG")),"^",2) K FSIG,BSIG D
 .I PSPROSIG D FSIG^PSOUTLA("R",RXNN,80) Q
 .D EN2^PSOUTLA1(RXNN,80) F GGGGG=0:0 S GGGGG=$O(BSIG(GGGGG)) Q:'GGGGG  S FSIG(GGGGG)=BSIG(GGGGG)
 K PSPROSIG,GGGGG,BSIG Q
PEND ;include pending orders in profile
 N PSPCOUNT,PSPPEND,ZXXX,PSPSTAT,FSIGZZ,PZZDRUG,PSSODRUG,PZXZERO,PPPPP,GGGGG
 S PSPCOUNT=1,PSPPEND="" F PPPPP=0:0 S PPPPP=$O(^PS(52.41,"P",DFN,PPPPP)) Q:'PPPPP  S PSPSTAT=$P($G(^PS(52.41,PPPPP,0)),"^",3) I PSPSTAT="NW"!(PSPSTAT="HD")!(PSPSTAT="RNW") S PSPPEND(PSPCOUNT)=PPPPP,PSPCOUNT=PSPCOUNT+1
 Q:'$O(PSPPEND(0))
 F ZXXX=0:0 S ZXXX=$O(PSPPEND(ZXXX)) Q:'ZXXX  S PZXZERO=$G(^PS(52.41,PSPPEND(ZXXX),0)) D:$P(PZXZERO,"^")
 .S PZZDRUG=$P(PZXZERO,"^",9),PZZODRUG=$P(PZXZERO,"^",8) Q:'PZZDRUG  Q:'PZZODRUG
 .S PEND="P"_CS_$S(PZZDRUG:$P($G(^PSDRUG(+PZZDRUG,0)),"^"),1:$P($G(^PS(50.7,+PZZODRUG,0)),"^")_" "_$P($G(^PS(50.606,+$P($G(^(0)),"^",2),0)),"^"))
 .S PEND=PEND_CS_$E($P(PZXZERO,"^",6),4,5)_"/"_$E($P(PZXZERO,"^",6),6,7)_"/"_$E($P(PZXZERO,"^",6),2,3)
 .S PEND=PEND_CS_$P(PZXZERO,"^",10)_CS_$P(PZXZERO,"^",11)_CS_$P($G(^VA(200,+$P(PZXZERO,"^",5),0)),"^")
 .D FSIG^PSOUTLA("P",PSPPEND(ZXXX),100) S PEND=PEND_CS_$G(FSIG(1)) F FSIGZZ=1:0 S FSIGZZ=$O(FSIG(FSIGZZ)) Q:'FSIGZZ  S PEND=PEND_CS_$G(FSIG(FSIGZZ))
 S:$D(PEND) ^TMP("PSO",$J,PSI,CNT)=PEND
 S CNT=CNT+1
 ;
START2 ;build NTE for drug interactions
 K PSOSERV
 S RX=IRXN,RXY=^PSRX(RX,0)
 S NTE5="NTE"_FS_5_FS_FS
 I $D(^PS(52.4,RX,0)) S SCRIPT=$P(^PS(52.4,RX,0),"^",10),SEV=$P(^PS(52.4,RX,0),"^",9) F X=1:1 S RXX(X)=$P(SCRIPT,",",X),SEV(X)=$P(SEV,",",X) Q:RXX(X)=""  D
 .S SER=$P(^PS(56,SEV(X),0),"^",4) S:$G(SER)=1 PSOSERV=1
 .S DIRX=$P($G(^PSRX(RXX(X),0)),"^"),TYP=$S(SER=1:"CRITICAL",SER=2:"SIGNIFICANT",1:"UNKNOWN")
 .S DRG=$P(^PSDRUG($P(^PSRX(RXX(X),0),"^",6),0),"^")
 .S:X=1 NTE5=NTE5_DIRX_CS_TYP_CS_DRG
 .S:X>1 NTE5=NTE5_RS_DIRX_CS_TYP_CS_DRG
 I '$D(^PS(52.4,RX,0)),$D(^PSRX(RX,"DRI")) S SCRIPT=$P(^PSRX(RX,"DRI"),"^",2),SEV=$P(^PSRX(RX,"DRI"),"^") F X=1:1 S RXX(X)=$P(SCRIPT,",",X),SEV(X)=$P(SEV,",",X) Q:RXX(X)=""  D
 .S SER=$P(^PS(56,SEV(X),0),"^",4)
 .S DIRX=$P($G(^PSRX(RXX(X),0)),"^"),TYP=$S(SER=1:"CRITICAL",SER=2:"SIGNIFICANT",1:"UNKNOWN")
 .S DRG=$P(^PSDRUG($P(^PSRX(RXX(X),0),"^",6),0),"^")
 .S:X=1 NTE5=NTE5_DIRX_CS_TYP_CS_DRG
 .S:X>1 NTE5=NTE5_RS_DIRX_CS_TYP_CS_DRG
 S NTE5=NTE5_CS_$S('$G(PSOSERV):"MAY REQUIRE",1:"REQUIRES")_$S('$G(PSOSERV):" REVIEWING BY A PHARMACIST",1:" INTERVENTION BY A PHARMACIST")
 K SER,SCRIPT,DIRX,TYP,DRG,SEV,RXX,RX,RXY
 Q
START3 ;build NTE for drug allergy warning label
 S NTE6="NTE"_FS_6_FS_FS
 I $G(DAW)&('$G(DIN)) S DARX=$P(^PSRX(IRXN,0),"^"),DRG=$P(^PSDRUG(IDGN,0),"^"),NTE6=NTE6_DARX_CS_DRG
 I $G(DAW)&($G(DIN)) S DARX=$P(^PSRX(IRXN,0),"^"),DRG=$P(^PSDRUG(IDGN,0),"^") D
 .S NTE6=NTE6_DARX_CS_DRG F XY=1:1 S INGRE=ING(XY) S:XY=1 NTE6=NTE6_CS_INGRE S:XY>1 NTE6=NTE6_RS_INGRE Q:'INGRE
 K DARX,DRG,XY,INGRE
 Q
