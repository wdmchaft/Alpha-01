DGPMGLG ;ALB/LM - G&L GENERATION, COLLECT DATA, CLEAN UP; 16 JAN 91
 ;;5.3;Registration;;Aug 13, 1993
 ;
 ;  Date of G&L and/or Bed Status Report
A S X1=RD,X2=-1 D C^%DTC S (FR,MV)=X_".999999",TO=$P(RD,".",1)_".999999"
 F I=0:0 S MV=$O(^DGPM("B",MV)) Q:MV'>0!(MV>TO)  F MN=0:0 S MN=$O(^DGPM("B",MV,MN)) Q:'MN  I $D(^DGPM(MN,0)) S MD=^(0) D ^DGPMGLG1,^DGPMGLG2,^DGPMGLG3 D ^DGPMBSG:RC
 I 'GL,'BS,'TSR Q
 K AD,BL,DFN,DPT,J,L,LN,MD,MDP,MV,NLS,X,X1,X2,Y,D,D1,ID,II,INS,INS1,JJ,JJ1,JJ2,NN,TO,FR,TSC,WDC
 S UL=$S(('$D(IOST)#2):"-",IOST["C-":"-",1:"_"),IOP=$S($D(ION)#2:ION,1:"HOME")_";"_$S($D(IOM)#2:IOM,1:"")_";"_$S($D(IOSL)#2:IOSL,1:"")
 S X=132 X ^%ZOSF("RM")
 D ^DGPMGLP:GL,^DGPMBSP:BS,^DGPMTSR:TSR W @IOF Q
 ;
DONE D CLEAN K RD,TSR,TSRI,DGPM,BS,GL,DGPM,REM,PD,RC,VN,TS,SS,SNM,SF,RM,TSD,CP,MT,OS,NOW,IOP,DGDIV,DGSRV,DIV,DR,PTNAME,UL,YD,NTOTAL,RCR,DGNOW D CLOSE^DGUTQ D ^%ZISC Q
 ;
CLEAN F I="TOD","CN","CN1","S","S1","SN","SN1","R","R1","NG","FR","6","VN","TV","T6","TF","AA","UA","PS","IP","TA","TP","TI","TU","OD","F","G","D","C","T","DN","RN","RN1","WBD","WNN","WON","WOR","WOS","WTOR" S X="DG"_I K ^UTILITY(X,$J)
 K ^UTILITY("DGWPL",$J),^UTILITY("DGWPLT",$J),^UTILITY("DGOD",$J),^UTILITY("DGTOD",$J),^UTILITY("DGAS",$J),^UTILITY("DGTAS",$J)
 ;
KVAR K %,A,AD,ADC,AT,ATS,BD,BDAY,BL,BO,C,C1,CB,CD,CN,CN1,CT,CUM,CW,D,D1,DA,DB,DC,DD,DFN,DGDOM,DGHEM,DGHX,DGNHCU,DGP,DGPMBO,DGPMDDF,DGPMY,DGSF,DGVT,DIC,DIE,DP,DPT,DV,E,FF,FM,FR,FY,I,I1,I2,I3,ID,II,INS,INS1,RCCK
 K J,J1,JJ,JJ1,JJ2,JUS,K,L,LA,LD,LDV,LEG,LN,LT,LTSDV,LW,M,MASD,MASDEV,MD,MDP,MIFN,MN,MV,MW,MW1,MW2,MW3,N,N1,NLS,NN,NN1,NOD,NOW,O,O1,OOS,ORDER
 K P,PAG,PARA,PL,POP,PP,PR,PR1,PRC,PRM,PRT,PT,PTS,PTSDV,PW,R,RA,T,T1,T2,T3,TAB,TB,TC,TL,TL1,TN,TO,TSC,TSLD,TSRIPD,TT
 K TX,TY,W,W1,W2,WD,WDC,X,X1,X2,X3,XX,XX1,XX2,XX3,Y,Z,Z1,ZMV,ZTDESC,ZTIO,ZTRTN,ZTSAVE
 D KVAR^VADPT30
 Q
 ;
VAR ;  RD=Report Date  ;  FR=From Date  ;  TO=To Date ;
 ;  MV=from start date (Movement date)  ;  MN=Movement Number  ;  MD=Movement Data ;
