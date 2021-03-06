OCXDEBUG ;SLC/RJS,CLA - ORDER CHECK COMPILED CODE DEBUGGER (SINGLE/MULTI);10/29/98  12:37
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**32**;Dec 17,1997
 ;;  ;;ORDER CHECK EXPERT version 1.01 released OCT 29,1998
 ;
SINGLE ;
 ;
 N OCXOLOG,IOP
 ;
 I '$O(^OCXD(861,1)) W !!,"No entries in the raw data log..." Q
 ;
 K ^TMP("OCXDEBUG",$J)
 S ^TMP("OCXDEBUG",$J)=($P($H,",",2)+($H*86400)+(4*60*60))_" <- ^TMP ENTRY EXPIRATION DATE FOR ^OCXOPURG"
 S IOP=0 D ^%ZIS K IOP S X=132 U IO X ^%ZOSF("RM")
 ;
 F  W !! S OCXOLOG=+$$LOOKUP("Select Log entry number: ") Q:'OCXOLOG  D RUN(OCXOLOG,$$ASKFLAG,$$ASKPURG),REPORT
 Q
 ;
ASKFLAG() Q ('$$READ("Y","Do you want to send any alerts generated ","NO")*10)+1
 ;
ASKPURG() Q $$READ("Y","Do you want purge rule events ","NO")
 ;
SMOKE(OCXTRACE) ;
 N OCXOLOG,OCXP,OCX1,OCX2,OCX3
 ;
 I '$O(^OCXD(861,1)) W !!,"No entries in the raw data log..." Q
 S:'$L($G(OCXTRACE)) OCXTRACE=11
 ;
 W !
 W !,"First Entry: ",+$O(^OCXD(861,1))
 W !,"Last Entry: ",+$O(^OCXD(861," "),-1)
 K ^TMP("OCXDEBUG",$J)
 S ^TMP("OCXDEBUG",$J)=($P($H,",",2)+($H*86400)+(4*60*60))_" <- ^TMP ENTRY EXPIRATION DATE FOR ^OCXOPURG"
 S IOP=0 D ^%ZIS K IOP S X=132 U IO X ^%ZOSF("RM")
 ;
 S OCXP=$$ASKPURG
 S OCX1=$$LOOKUP("Start at Log Number: ")
 S OCX2=$$LOOKUP(" Stop at Log Number: ")
 S:(OCX1>OCX2) OCX3=OCX2,OCX2=OCX1,OCX1=OCX3
 S OCXOLOG=OCX1 D  F  S OCXOLOG=$O(^OCXD(861,OCXOLOG)) Q:'OCXOLOG  Q:(OCXOLOG>OCX2)  D
 .W @IOF D RUN(OCXOLOG,OCXTRACE,OCXP)
 D ^%ZIS
 U IO D REPORT D ^%ZISC
 Q
 ;
REPORT ;
 N OCXD0,OCXD1
 I '$O(^TMP("OCXDEBUG",$J,0)) W !!,"No rules triggered.",!! Q
 W !!,"************* Rules triggered ****************",!
 S OCXD0=0 F  S OCXD0=$O(^TMP("OCXDEBUG",$J,OCXD0)) Q:'OCXD0  D
 .S OCXD1=0 F  S OCXD1=$O(^TMP("OCXDEBUG",$J,OCXD0,OCXD1)) Q:'OCXD1  D
 ..W !,"Rule: (",(+OCXD0),")  ",$P($G(^OCXS(860.2,+OCXD0,0)),U,1)
 ..W !,"Relation: (",(+OCXD1),")  ",$G(^OCXS(860.2,+OCXD0,"R",+OCXD1,"E"))
 ..W !,"       Number triggered: ",+$G(^TMP("OCXDEBUG",$J,OCXD0,OCXD1))
 ..W !
 Q
 ;
RUN(OCXDEBUG,OCXTRACE,OCXPURGE) ;
 ;
 Q:'$D(^OCXD(861,OCXDEBUG))  Q:'$D(^OCXD(861,OCXDEBUG,"SOURCE"))
 ;
 N OCXA,OCXAR,OCXD0
 ;
 M OCXA=^OCXD(861,OCXDEBUG)
 ;
 W !,"RUN: ",OCXDEBUG,"   ",OCXA("SOURCE")
 ;
 I OCXPURGE D
 .N DFN S DFN=+$P($G(OCXA("PATIENT")),"[",2) Q:'DFN
 .K ^OCXD(860.7,DFN,1)
 .K ^OCXD(860.7,"AT",+$H,DFN)
 ;
 I (OCXA("SOURCE")="HL7") D  Q
 .S OCXAR=OCXA("ARRAY")
 .I '(OCXAR[U) N @OCXAR
 .N OCXDBMSG
 .S OCXD0=0 F  S OCXD0=$O(OCXA("DATA",OCXD0)) Q:'OCXD0  D
 ..N LAST,TEXT
 ..S LAST=$O(@OCXAR@(" "),-1)
 ..S TEXT=OCXA("DATA",OCXD0,0)
 ..I LAST,($E(TEXT,1,3)=">>>") S TEXT=$E(TEXT,4,$L(TEXT))
 ..E  S LAST=LAST+1
 ..S @OCXAR@(LAST)=$G(@OCXAR@(LAST))_TEXT
 .S OCXDBMSG=""
 .W !!
 .I '(OCXAR[U) X "D SILENT^OCXOHL7(."_OCXAR_",.OCXDBMSG)" I 1
 .E  D SILENT^OCXOHL7(.OCXAR,.OCXDBMSG)
 .I ($D(OCXDBMSG)>1) D ZW("OCXDBMSG")
 .I (OCXAR[U) K @OCXAR
 ;
 I (OCXA("SOURCE")="DGPM") D  Q
 .N DGPM0,DGPMA,DGPMDA,DFN,OCXDBMSG
 .S OCXD0=0 F  S OCXD0=$O(OCXA("DATA",OCXD0)) Q:'OCXD0  D
 ..N TEXT S TEXT=OCXA("DATA",OCXD0,0)
 ..S @$P(TEXT,"=",1)=$P(TEXT,"=",2,999)
 .S DFN=+$P($G(OCXA("PATIENT")),"[",2)
 .S OCXDBMSG=""
 .W !! D SILENT^OCXODGPM(.OCXDBMSG)
 .I ($D(OCXDBMSG)>1) D ZW("OCXDBMSG")
 ;
 I (OCXA("SOURCE")="OEPS") D  Q
 .N OCXPSD,OCXPSM,DFN,OCXDBMSG
 .S OCXD0=0 F  S OCXD0=$O(OCXA("DATA",OCXD0)) Q:'OCXD0  D
 ..N TEXT S TEXT=OCXA("DATA",OCXD0,0)
 ..S @$P(TEXT,"=",1)=$P(TEXT,"=",2,999)
 .S DFN=+$P($G(OCXA("PATIENT")),"[",2)
 .S OCXDBMSG=""
 .W ! D EN^OCXOEPS(.OCXDBMSG,DFN,OCXPSD,OCXPSM) ;
 .I ($D(OCXDBMSG)>1) D ZW("OCXDBMSG")
 ;
 I (OCXA("SOURCE")="OERR") D  Q
 .N OCXORD,DFN,OCXDBMSG
 .S OCXD0=0 F  S OCXD0=$O(OCXA("DATA",OCXD0)) Q:'OCXD0  D
 ..N TEXT S TEXT=OCXA("DATA",OCXD0,0)
 ..S @$P(TEXT,"=",1)=$P(TEXT,"=",2,999)
 .S DFN=+$P($G(OCXA("PATIENT")),"[",2)
 .S OCXDBMSG=""
 .W ! D SILENT^OCXOERR(OCXORD,.OCXDBMSG) ;
 .I ($D(OCXDBMSG)>1) D ZW("OCXDBMSG")
 Q
 ;
LOOKUP(OCXDLG) ;
 ;
 N DIC,X,Y
 S:$L($G(OCXDLG)) DIC("A")=OCXDLG
 S DIC="^OCXD(861,",DIC(0)="AMNEQ"
 S DIC("S")="I (Y>1)"
 S DIC("W")="W ""  "",$G(^(""SOURCE"")),""  "",$G(^(""PATIENT""))"
 S Y=0 D ^DIC
 Q:(Y<0) 0 Q +Y
 ;
ZW(ARRAY) ;
 W !,ARRAY
 N NODE
 I ($D(@ARRAY)#10) W !,ARRAY," = ",@ARRAY
 S:($E(ARRAY,$L(ARRAY))=")") ARRAY=$E(ARRAY,1,$L(ARRAY)-1)_","
 S NODE=ARRAY F  S NODE=$Q(@NODE) Q:'$L(NODE)  Q:'($E(NODE,1,$L(ARRAY))=ARRAY)  W !,NODE," = ",@NODE
 Q
 ;
LOGAL(OCXR0,OCXR1,OCXN,OCXDFN,OCXNUM,OCXADUZ,OCXPMSG,OCXPDATA) ;
 ;
 ;    OCXR0: Rule IEN
 ;    OCXR1: Relation IEN
 ;     OCXN: notification identifier (required)
 ;   OCXDFN: patient identifier   (required)
 ;   OCXNUM: order number - used to determine ordering provider
 ;  OCXADUZ: array of package-identified recipients
 ;  OCXPMSG: package-defined message
 ; OCXPDATA: package-defined data for follow-up action
 ;
 I $G(OCXR0),$G(OCXR1) S ^TMP("OCXDEBUG",$J,+OCXR0,+OCXR1)=$G(^TMP("OCXDEBUG",$J,+OCXR0,+OCXR1))+1
 ;
 Q
 ;
READ(OCXZ0,OCXZA,OCXZB,OCXZL) ;
 N OCXLINE,DIR,DTOUT,DUOUT,DIRUT,DIROUT
 Q:'$L($G(OCXZ0)) U
 S DIR(0)=OCXZ0
 S:$L($G(OCXZA)) DIR("A")=OCXZA
 S:$L($G(OCXZB)) DIR("B")=OCXZB
 F OCXLINE=1:1:($G(OCXZL)-1) W !
 D ^DIR
 I $D(DTOUT)!$D(DUOUT)!$D(DIRUT)!$D(DIROUT) Q U
 Q Y
 ;
TIME ;
 ;
 N OCXD0,OCXT1,OCXT2,OCXT3,OCXTIME,OCXMAX
 ;
 S (OCXMAX,OCXD0)=0 F  S OCXD0=$O(^OCXD(861,OCXD0)) Q:'OCXD0  D
 .Q:'$D(^OCXD(861,OCXD0,"STATUS"))
 .S OCXT1=+$G(^OCXD(861,OCXD0,0))
 .S OCXT1=$P(OCXT1,".",2)
 .S OCXT1=$E(OCXT1_"000000",1,6)
 .S OCXT1=($E(OCXT1,1,2)*3600)+($E(OCXT1,3,4)*60)+($E(OCXT1,5,6))
 .S OCXT2=$G(^OCXD(861,OCXD0,"STATUS"))
 .S OCXT2=+$P(OCXT2," AT ",2)
 .S OCXT2=$P(OCXT2,".",2)
 .S OCXT2=$E(OCXT2_"000000",1,6)
 .S OCXT2=($E(OCXT2,1,2)*3600)+($E(OCXT2,3,4)*60)+($E(OCXT2,5,6))
 .S OCXT3=(OCXT2-OCXT1) Q:(OCXT3<0)
 .I (OCXT3>OCXMAX) S OCXMAX=OCXT3_U_OCXD0
 .S OCXTIME(OCXT3)=$G(OCXTIME(OCXT3))+1
 ;
 W !!,"Number of seconds",?20,"Number of occurences",!
 S OCXT1="" F  S OCXT1=$O(OCXTIME(OCXT1)) Q:'$L(OCXT1)  D
 .I OCXT1 W !,$J(OCXT1,4)
 .E  W !,"Less than 1 second"
 .W ?20,$J(+$G(OCXTIME(OCXT1)),6)
 I $P(OCXMAX,U,2) W !!,"Maximum wait: ",+OCXMAX," second(s).  Log entry: ",$P(OCXMAX,U,2)
 ;
 Q
 ;
