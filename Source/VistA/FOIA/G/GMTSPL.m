GMTSPL ; SLC/JER,KER - Print/Queue HS for Patient Lists ; 02/27/2002 [1/27/05 8:27am]
 ;;2.7;Health Summary;**7,27,28,30,47,49,70**;Oct 20, 1995;Build 5
 ;
 ; External References
 ;    DBIA 10090  ^DIC(4
 ;    DBIA 10039  ^DIC(42
 ;    DBIA 10035  ^DPT(
 ;    DBIA 10035  ^DPT("CN"
 ;    DBIA 10040  ^SC(
 ;    DBIA    16  ^SRF(
 ;    DBIA   641  ^SRF("AOR"
 ;    DBIA   185  ^SRS("B"
 ;    DBIA 10091  ^XMB(1
 ;    DBIA 10000  C^%DTC
 ;    DBIA 10000  NOW^%DTC
 ;    DBIA 10026  ^DIR
 ;    DBIA   183  DFN^PSOSD1
 ;    DBIA 10104  $$UP^XLFSTR
 ;    DBIA  2056  $$GET1^DIQ (file #44)
 ;                          
MAIN ; Print/Queue for Patient Lists
 ;                          
 ; Call with:  
 ;                   
 ;   GMTSTYP  = Pointer to file 142
 ;   GMTSSC   = Pointer to file 44^Hosp Loc Name^
 ;              Hosp Loc Type^Begin Visit/Surg Date^
 ;              Opt end Visit/Surgery Date
 ;   GMTSSC() = GMTSSC - Array of multiple locations
 ;   [GMPSAP] = Optional flag set to 1 if OP Rx 
 ;              Action Profile is to print
 ;                        
 N MULTLOC,GMTSEXIT S GMTSEXIT=0
 I $D(GMTSSC("ALL")) D  Q
 . N IEN,BEG,END,COR,PRM,RAN,PAT
 . S PRM=$G(GMTSSC),BEG=$P(PRM,"^",4),END=$P(PRM,"^",5)
 . S RAN=BEG S:$L(END)&($L(RAN)) RAN=RAN_"^"_END S:$L(END)&('$L(RAN)) RAN=END
 . S IEN=0 F  S IEN=$O(^SC(IEN)) Q:+IEN=0  D  Q:$G(GMTSEXIT)["^^"
 . . N GMTSSC,NAM S NAM=$$GET1^DIQ(44,(+IEN_","),.01) Q:'$L(NAM)
 . . S COR=$$GET1^DIQ(44,(+IEN_","),2,"I") Q:COR=""  Q:"WCOR"'[COR
 . . S GMTSSC=IEN_"^"_NAM_"^"_COR
 . . S:"COR"[COR&($L($G(RAN))) GMTSSC=GMTSSC_"^"_RAN
 . . S PAT=$$PAT(GMTSSC) Q:+PAT=0
 . . D CTRL
 I +$O(GMTSSC(0))'>0 D CTRL
 I +$O(GMTSSC(0)) D
 . S MULTLOC=0 F  S MULTLOC=$O(GMTSSC(MULTLOC)) Q:+MULTLOC'>0!$D(DIROUT)  D
 . . S GMTSSC=GMTSSC(+MULTLOC) D CTRL
 Q
CTRL ; Controls Branching
 N DFN,GMTDFN,GMLTYPE,GMTSLTR,GMPNM,PSOPAR,PSONOPG,PSOINST,PSTYPE K ^TMP("GMTSPL",$J) U IO
 N GMTSBYE S GMTSBYE=0
 S GMLTYPE=$P(GMTSSC,U,3) S:GMLTYPE="C" GMTSBYE=$$CLINIC(GMTSSC) D:GMLTYPE="W" WARD(GMTSSC) D:GMLTYPE="OR" OR(GMTSSC)
 I GMTSBYE Q
 I $L($P(GMTSSC,U,2)),($E(IOST,1)'="C") S GMTSLTR=$E($P(GMTSSC,U,2),1,10) D ^GMTSLTR
 I $O(^TMP("GMTSPL",$J,0))="",$D(GMTSSC("ALL")) W !,"ALL" Q
 I $O(^TMP("GMTSPL",$J,0))="" D NOPAT($P(GMTSSC,U,2)) Q
 S GMPNM="" F  S GMPNM=$O(^TMP("GMTSPL",$J,GMPNM)) Q:(GMPNM="")!($D(DIROUT))  D
 . S GMTDFN=0 F  S GMTDFN=$O(^TMP("GMTSPL",$J,GMPNM,GMTDFN)) Q:(GMTDFN'>0)!($D(DIROUT))  D
 . . N GMDUOUT
 . . S DFN=GMTDFN D DRIVER Q:$D(DIROUT)!+$G(GMDUOUT)
 . . I +$G(GMPSAP) D
 . . . S (PSTYPE,PSONOPG)=1
 . . . S $P(PSOPAR,U)=$S($P($G(^GMT(142.99,1,0)),U,5)="Y":1,1:0)
 . . . S PSOINST=$S(+$G(PSOINST):PSOINST,1:+$P($G(^DIC(4,+$P($G(^XMB(1,1,"XUS")),U,17),99)),U))
 . . . D DFN^PSOSD1,PAGE
 K ^TMP("GMTSPL",$J)
 Q
PAGE ; Pause at BOP for interactive users
 N DIR,X,Y
 Q:$E(IOST)'="C"!(IOT="HFS")!((IOSL>998)&($G(GMPAT(+$O(GMPAT(""),-1)))'=$G(DFN)))
 I IOSL>($Y+5) F  W ! Q:IOSL<($Y+6)!($Y'<22)
 S DIR(0)="FO^1:1",DIR("A")="Press RETURN to continue or '^' to exit"
 S DIR("?")="Enter '^' to quit present report or '^^' to quit to menu"
 D ^DIR S:X["^^" DIROUT=1
 Q
NOPAT(LOC) ; Handles unpopulated Hospital location
 N %,%H,%I,%T,%Y,GMTS,GMTSDTM,GMTSTN,GMTSHDR,GMTSPG,GMTSTITL,GMTSDTM,GMTSLFG,X,Y
 D NOW^%DTC S X=% D REGDTM4^GMTSU S GMTSDTM=X,GMTSTN=$P($G(^GMT(142,+($G(GMTSTYP)),0)),"^",1)
 S DIC=142,DIC(0)="NXF",X=GMTSTN S Y=$$TYPE^GMTSULT K DIC
 S GMTSTITL=$S($D(^GMT(142,+Y,"T")):^("T"),1:$P(Y,U,2)),GMTSLFG=1
 W @IOF D HEADER^GMTSUP W !!,"No Patients found at ",LOC," location.",!
 Q
CLINIC(LOC) ; Gets list of next-day appointments for clinic
 N %,%H,%I,%T,%Y,GMI,X,X1,X2,VDT,Y,GMPNM,GMDT,GMBDT,GMEDT,GMTSRES,GMTSCDT,GMDFN,GMNAME,GMDATE,GMTSLAST
 S GMTSCDT=$P(LOC,U,4),GMI=0
 I 'GMTSCDT D NOW^%DTC S GMTSCDT=X
 S X=+GMTSCDT D REGDT4^GMTSU S GMBDT=X
 S X=+$P(LOC,U,5) D REGDT4^GMTSU S GMEDT=X
 S:+$P(LOC,U,5) X1=$P(LOC,U,5),X2=1
 S:+$P(LOC,U,5)'>0 X1=GMTSCDT,X2=1 D C^%DTC
 S GMTSLAST=X
 D GETPLIST^SDAMA202(+LOC,"1;4",,GMTSCDT,GMTSLAST,.GMTSRES)
 I GMTSRES<0 D  Q "-1"
 . N GMTSERR
 . S GMTSERR=$O(^TMP($J,"SDAMA202","GETPLIST","ERROR",0))
 . I 'GMTSERR Q
 . D MAIL^GMTSMAIL($G(^TMP($J,"SDAMA202","GETPLIST","ERROR",GMTSERR)),"Print/Queue HS for Patient Lists")
 . K ^TMP($J,"SDAMA202","GETPLIST")
 F  S GMI=$O(^TMP($J,"SDAMA202","GETPLIST",GMI)) Q:GMI=""  D
 . N X
 . S X=$G(^TMP($J,"SDAMA202","GETPLIST",GMI,1))
 . Q:X>GMTSLAST
 . D REGDT4^GMTSU S GMDATE=X
 . S GMDFN=+$G(^TMP($J,"SDAMA202","GETPLIST",GMI,4))
 . S GMNAME=$P($G(^TMP($J,"SDAMA202","GETPLIST",GMI,4)),U,2)
 . S ^TMP("GMTSPL",$J,GMNAME,+GMDFN)=$S($D(^TMP("GMTSPL",$J,GMNAME,+GMDFN)):GMBDT_" TO "_GMEDT,1:GMDATE)
 K ^TMP($J,"SDAMA202","GETPLIST")
 Q 0
WARD(LOC) ; Gets list of patients for a ward
 N DFN,GMLOC,X,Y,GMDT
 S GMLOC=$P($G(^DIC(42,+$G(^SC(+LOC,42)),0)),U)
 I $S('$L(GMLOC):1,'$O(^DPT("CN",GMLOC,0)):1,1:0) Q
 S DFN=0 F  S DFN=$O(^DPT("CN",GMLOC,DFN)) Q:+DFN'>0  D
 . N X
 . S X=+$G(DT) D REGDT4^GMTSU S GMDT=X
 . S ^TMP("GMTSPL",$J,$P($G(^DPT(+DFN,0)),U),+DFN)=GMDT
 Q
OR(LOC) ; Gets list of patients scheduled for surgery
 N GMBEG,GMEND,DFN,GMI,GMJ,GMPNM,GMDT,%,%H,%I,%T,%Y,X,X1,X2,Y
 S GMI=+$O(^SRS("B",+LOC,0)) I +GMI'>0 G ORX
 S GMBEG=$P(LOC,U,4)-.0001,GMEND=$S(+$P(LOC,U,5)>0:$P(LOC,U,5),1:$P(LOC,U,4))
 F  S GMBEG=$O(^SRF("AOR",+GMI,+GMBEG)) Q:+GMBEG'>0!(+GMBEG>+GMEND)  D
 . S GMJ=0 F  S GMJ=$O(^SRF("AOR",+GMI,+GMBEG,GMJ)) Q:+GMJ'>0  D
 . . S DFN=+$G(^SRF(+GMJ,0)) Q:DFN'>0
 . . S GMPNM=$P($G(^DPT(+DFN,0)),U)
 . . N X
 . . S X=+GMBEG D REGDT4^GMTSU S GMDT=X
 . . S ^TMP("GMTSPL",$J,GMPNM,+DFN)=$S($D(^TMP("GMTSPL",$J,GMPNM,+DFN)):^(+DFN)_", "_GMDT,1:GMDT)
ORX ; Exit Surgery
 Q
PAT(LOC) ; Checks for patients at selected location
 N %,%H,%T,LTYPE,X1,X2,X,Y,GMY,GMBEG,GMTSDATE,GMTSCDT,GMTSRES S LTYPE=$P(LOC,U,3),GMY=0
 I LTYPE="W" D
 . S LOC=$P($G(^DIC(42,+$G(^SC(+LOC,42)),0)),U),GMY=$S($G(LOC)']"":0,$O(^DPT("CN",LOC,0)):1,1:0)
 I $L(LOC,U)=4!($L(LOC,U)=5) D
 . S GMY=0 S:+$P(LOC,U,5) X1=$P(LOC,U,5),X2=1 S:+$P(LOC,U,5)'>0 X1=$P(LOC,U,4),X2=1 D C^%DTC
 . S GMTSCDT=$P(LOC,U,4)
 . D GETPLIST^SDAMA202(+LOC,"1",,GMTSCDT,X,.GMTSRES) Q:GMTSRES=0
 . I GMTSRES<0 D  Q
 . . N GMTSERR
 . . S GMTSERR=$O(^TMP($J,"SDAMA202","GETPLIST","ERROR",0))
 . . I 'GMTSERR Q
 . . D MAIL^GMTSMAIL($G(^TMP($J,"SDAMA202","GETPLIST","ERROR",GMTSERR)),"Print/Queue HS for Patient Lists")
 . . K ^TMP($J,"SDAMA202","GETPLIST")
 . N GMTSI S GMTSI=0,GMTSDATE=0
 . F  S GMTSI=$O(^TMP($J,"SDAMA202","GETPLIST",GMTSI)) Q:'GMTSI  D
 . . I $G(^TMP($J,"SDAMA202","GETPLIST",GMTSI,1))<X S GMTSDATE=$G(^TMP($J,"SDAMA202","GETPLIST",GMTSI,1))
 . K ^TMP($J,"SDAMA202","GETPLIST")
 . I LTYPE="C",(+GMTSDATE),(+GMTSDATE'>X) S GMY=1
 . I LTYPE="OR" D
 . . N OLOC S GMY=0,OLOC=+$O(^SRS("B",+LOC,0))
 . . I +OLOC,+$P(LOC,U,5)'>0,$O(^SRF("AOR",+OLOC,+$P(LOC,U,4),0)) S GMY=1
 . . I +OLOC,+$P(LOC,U,5) S GMBEG=$P(LOC,U,4) F  D  Q:GMBEG>$P(LOC,U,5)!(GMY>0)
 . . . S:$O(^SRF("AOR",+OLOC,+GMBEG,0)) GMY=1 Q:+GMY>0  S X1=GMBEG,X2=1 D C^%DTC S GMBEG=X
 Q $G(GMY)
DRIVER ; Sets variables for GMTS1 and calls ^%ZTLOAD
 N %T,C,D0,GMTS,GMTS0,GMTS1,GMTS2,GMTSDOB,GMTSDTM,GMTSLO,GMTSLOCK
 N GMTSLPG,GMTSEG,GMTSEGC,GMTSTN,GMTSEGI,GMTSPNM,GMTSRB
 N GMTSSN,GMTSTITL,GMTSWARD,GMTSX,GMTSPHDR,GMTSAGE,GMTSTOF,GMTSCDT
 N GMW,I,SEX,VA,VADM,VAIN,VAINDT,VAROOT,X,Y
 S GMTSCDT(0)=^TMP("GMTSPL",$J,GMPNM,+DFN),GMTSTN=$P($G(^GMT(142,+($G(GMTSTYP)),0)),"^",1)
 S DIC=142,DIC(0)="NXF",X=GMTSTN S Y=$$TYPE^GMTSULT K DIC
 S GMTSTITL=$$UP^XLFSTR($S($G(^GMT(142,+Y,"T"))]"":^("T"),1:$P(Y,U,2)))
 D:$D(GMTSEG)'>9 SELTYP1^GMTS D EN^GMTS1
 Q
