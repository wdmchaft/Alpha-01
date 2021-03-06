PRCOVTST ;WISC/DJM/BGJ-IFCAP VRQ TO-DO ROUTINE ; [10/19/98 11:20am]
V ;;5.1;IFCAP;**30**;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
NEW(VEN1,SITE,FLAG) ;VEN1 = VENDOR INTERNAL ENTRY NUMBER
 N %,B,DATE,GECSFMS,FLAGN,FY,I,J,PS,NAME,MO,PAY,PAY1,PRCOVA,PRCOVA3,PRCOVN,PRCOVN3,SEQ,SSNT,ST,TIME,TRANS,VEN,VEND,X,Y
 S FLAGN=$G(^PRC(440.3,VEN1,0))
 S PRCOVN=$G(^PRC(440,VEN1,0))
 S PRCOVN3=$G(^PRC(440,VEN1,3))
 S PAY=$G(^PRC(440,VEN1,7))
 I FLAGN]"" D
 .S PRCOVA=FLAGN
 .S PRCOVA3=$G(^PRC(440.3,VEN1,3))
 .S PAY1=$G(^PRC(440.3,VEN1,7))
 G:PRCOVN3="" EXIT ;THERE IS NO DATA IN NODE 3 FOR THIS VENDOR--THIS USUALLY WILL NOT HAPPEN.  CAN ONLY QUIT WITHOUT CREATING 'VRQ'
 ;
 G:$P(PRCOVN3,U,6)="N" EXIT ;NON-RECURRING VENDOR  "N"=ONE-TIME VENDOR--DON'T NEED TO 'ADD'
 ;
 I FLAG=1,$P(PRCOVN3,U,4)]"" G EXIT ;'ADD' VRQ & FMS VENDOR CODE???  VENDOR UPDATED--DON'T NEED TO 'ADD' AGAIN (SHOULD NOT SEE THIS)
 ;
 I FLAG=1,(($P(PRCOVN3,U,9)="")!($P(PRCOVN3,U,8)="")) G EXIT ;NO TAX ID/SSN OR SSN/TAX ID INDICATOR--DON'T HAVE ALL INFORMATION TO SEND 'VRQ'
 ;
 G:PAY="" EXIT ;DON'T HAVE ANY PAYMENT ADDRESS INFORMATION--DON'T SEND 'VRQ'
 ;
 I FLAGN="" G DOIT  ;THIS IS A NEW IFCAP VENDOR ENTRY--SEND IT
 I FLAG=1,$P(PRCOVN3,U,4)="",$P(PRCOVN3,U,12)="" G DOIT  ;THIS ENTRY NEEDS TO BE SENT BECAUSE IT WASEN'T EVER DONE BEFORE
 ;
 I $P(PRCOVN,U)'=$P(PRCOVA,U) G DOIT
 I $P(PRCOVN3,U,11)'=$P(PRCOVA3,U,11) G DOIT
 I $P(PRCOVN3,U,13)'=$P(PRCOVA3,U,13) G DOIT
 I $P(PRCOVN3,U,14)'=$P(PRCOVA3,U,14) G DOIT
 I $P(PAY,U,3)'=$P(PAY1,U,3) G DOIT
 I $P(PAY,U,4)'=$P(PAY1,U,4) G DOIT
 I $P(PAY,U,7)'=$P(PAY1,U,7) G DOIT
 I $P(PAY,U,8)'=$P(PAY1,U,8) G DOIT
 I $P(PAY,U,9)'=$P(PAY1,U,9) G DOIT
 G EXIT ;USER DIDN'T CHANGE ANYTHING USED TO CREAT A VENDOR REQUEST
 ;
DOIT ;COME HERE IF A VRQ SHOULD BE CREATED.
 S NOVRQ=0
 Q NOVRQ
 ;
EXIT ;USE THIS EXIT ONLY IF NO VRQ SHOULD BE CREATED.
 ;DON'T FORGET TO REMOVE UN-EDITED COPY OF VENDOR RECORD (IN 440.3).
 K ^PRC(440.3,VEN1)
 S NOVRQ=1
 Q NOVRQ
 ;
CHECK(DA,SITE,FLAG) ; CALL TO SEE IF VENDOR IS PROPERLY SET UP FROM AR
 ; VENDOR LOOKUP CALL -- VENSEL^PRCHUTL().
 ; COME HERE TO DECIDE WHAT NEEDS TO BE DONE WITH THE SELECTED
 ; VENDOR.
 ;
 ; RETURNED VALUE           MEANING
 ;     0                    NEED TO CREATE A VRQ - ALL DATA TO
 ;                          CREATE A VRQ IS HERE.
 ;     1                    NEED TO EDIT VENDOR RECORD BEFORE A
 ;                          VRQ CAN BE CREATED.
 ;     2                    THE VENDOR IS PROPERLY SET UP.  NO
 ;                          VRQ NEEDS TO BE CREATED.
 ;
 S PRCOVN3=$G(^PRC(440,DA,3))
 I FLAG=1,$P(PRCOVN3,U,4)]"" G EXIT2 ;ADD VRQ WITH FMS VENDOR CODE
 ; PRESENT???  VENDOR UPDATED--DON'T NEED TO 'ADD' AGAIN.
 ;
 S (I,J)=0
 F  S I=$O(^PRC(411,I)) Q:I'>0  S J=J+1
 I J>1 S PS=$O(^PRC(411,"AC","Y",0)) G:PS="" EXIT1
 ; 'PRIMARY STATION' NEEDS TO BE FILLED IN.
 ;
 S PAY=$G(^PRC(440,DA,7))
 G:PRCOVN3="" EXIT1 ; THIS RECORD NEEDS TO BE EDITED.
 ;
 G:$P(PRCOVN3,U,6)="N" EXIT1 ; NON-RECURRING VENDOR  THIS RECORD
 ; NEEDS TO BE EDITED.
 ;
 G:$P(PRCOVN3,U,14)="" EXIT1  ; VENDOR TYPE UNDEFINED.
 G:PAY="" EXIT1
 ; DON'T HAVE ANY PAYMENT ADDRESS INFORMATION--EDIT THIS RECORD.
 ;
 G:$P(PAY,U,3)=""!($P(PAY,U,7)="")!($P(PAY,U,8)="")!($P(PAY,U,9)="") EXIT1  ; PAYMENT FIELDS AREN'T FILLED IN.
 S ST=$P(PAY,U,8)
 S ST=$E($P($G(^DIC(5,ST,0)),U,2),1,2)
 G:ST="" EXIT1  ; FOR SOME REASON THIS STATE IS MISSING FROM THE
 ; STATE FILE.
 ;
 I FLAG=1,(($P(PRCOVN3,U,9)="")!($P(PRCOVN3,U,8)="")) G EXIT1
 ; NO TAX ID/SSN OR SSN/TAX ID INDICATOR--DON'T HAVE ALL INFORMATION
 ; TO SEND 'VRQ'.  EDIT THIS RECORD.
 ;
DOIT1 ; COME HERE IF A VRQ SHOULD BE CREATED.
 S NOVRQ=0
 Q NOVRQ
 ;
EXIT1 ; COME HERE IF THE VENDOR RECORD NEEDS TO BE EDITED.
 S NOVRQ=1
 Q NOVRQ
 ;
EXIT2 ; USE THIS EXIT ONLY IF NO VRQ SHOULD BE CREATED.
 ; IF THERE IS NO "AR" NODE PRESENT REMOVE UN-EDITED COPY OF VENDOR
 ; RECORD (IN 440.3).
 S NODE=$D(^PRC(440.3,DA,"AR"))
 I NODE]"" S NODE=1
 K:NODE=0 ^PRC(440.3,DA)
 S NOVRQ=2
 Q NOVRQ
 ;
VRQ(DA,SITE) ; COME HERE TO SEND A VRQ FOR THE VENDOR RECORD SELECTED
 ; BY THE AR USER.  THIS ENTRY POINT IS CALLED FROM VENSEL^PRCHUTL().
 S PRCXDA=DA
 K ^PRC(440.3,DA)
VRQ1 S PRCOVN3=$G(^PRC(440,DA,3))
 D NOW^%DTC
 S DATE=$P(%,".")
 S DATE=$E(DATE,2,7)
 S TIME=$P(%,".",2)_"000000"
 S TIME=$E(TIME,1,6)
 S FY=$E($P(%,"."),2,3)
 S MO=$E($P(%,U),4,5)
 S FY=$E(100+$S(+MO>9:FY+1,1:FY),2,3)
 K PRCFLN
 S X=SITE_"-"_FY_"-"_MO
 D COUNTER^PRCFACP
 S SEQ="000"_Y
 S SEQ=$E(SEQ,$L(SEQ)-3,99)
 S TRANS=SITE_FY_MO_SEQ
 S DA=PRCXDA
 S B="VRQ^"_DATE_"^"_TIME_"^"_SITE_"^"_DA_"^"_$P(PRCOVN3,U,8)_"^"
 S B=B_$S($P(PRCOVN3,U,5)]"":$P(PRCOVN3,U,5),1:"")
 S NAME=$P($G(^PRC(440,DA,0)),"^")
 S NAME=$E(NAME,1,30)
 S B=B_"^"_NAME_"^"
 S PAY=$G(^PRC(440,DA,7))
 S B=B_$E($P(PAY,U,3),1,30)_"^"
 S B=B_$S($P(PAY,U,4)]"":$E($P(PAY,U,4),1,30),1:"")_"^"
 S B=B_$E($P(PAY,U,7),1,19)_"^"
 S ST=$P(PAY,U,8)
 S ST=$E($P($G(^DIC(5,ST,0)),U,2),1,2)
 S B=B_ST_"^"_$TR($P(PAY,U,9),"-")_"^"
 S VEND=$S($P(PRCOVN3,U,11)]"":$P(PRCOVN3,U,11),1:"N")
 S SSNT=$S($P(PRCOVN3,U,9)]"":$P(PRCOVN3,U,9),1:"T")
 S:VEND="N" SSNT=""
 S B=B_SSNT_"^"_VEND_"^"_$P(PRCOVN3,U,14)_"^N^A^~"
 ;
 ; REQUEST GENERIC CODE SHEET PACKAGE SET UP AN ENTRY IN FILE 2100.1.
 ;
 D CONTROL^GECSUFMS("I",SITE,TRANS,"VR","","","","Vendor Request")
 ;
 ; ENTER THE 'VRQ' SEGMENT INTO FILE 2100.1 RECORD CREATED IN
 ; PREVIOUS CALL.
 ;
 D SETCS^GECSSTAA(GECSFMS("DA"),B)
 ;
 ; TELL GCS PACKAGE WHAT TO DO WITH THIS RECORD--'QUEUE' IT TO SEND
 ; THE NEXT TIME ANY FMS TRANSACTIONS ARE SENT TO AUSTIN.
 ;
 Q
 ;
VRQS(DA,SITE) ; COME HERE TO SEND A VRQ FROM THE 'SEND VRQ' PROTOCOL.
 ;
 S PRCXDA=DA
 ;
 ; NOW LETS GO OVER TO SEND THIS VRQ TO AUSTIN, WITHOUT KILLING THE 
 ; RECORD IN FILE 440.3.  THAT RECORD IS USED WITHIN THE AR EDIT
 ; LIST TEMPLATE UNTIL 'DELETE EDIT REQUEST' REMOVES THE RECORD.
 ;
 G VRQ1
