OCXOZ01 ;SLC/RJS,CLA - Order Check Scan ;MAR 8,2011 at 13:52
 ;;3.0;ORDER ENTRY/RESULTS REPORTING;**32,221,243**;Dec 17,1997;Build 242
 ;;  ;;ORDER CHECK EXPERT version 1.01 released OCT 29,1998
 ;
 ; ***************************************************************
 ; ** Warning: This routine is automatically generated by the   **
 ; ** Rule Compiler (^OCXOCMP) and ANY changes to this routine  **
 ; ** will be lost the next time the rule compiler executes.    **
 ; ***************************************************************
 ;
 ;    compiled code line length: 200
 ;        compiled routine size: 8000
 ; triggered rule ignore period: 300
 ;
 ;   Program Execution Trace Mode: OFF
 ;
 ;               Raw Data Logging: OFF
 ; Compiler mode:  ON
 ;   Compiled by: MOODY,SUSAN  (DUZ=17)
 Q
 ;
LOG() ; Returns the number of days to keep the Raw Data Log or 0 if logging is disabled.
 ;  External Call.
 ;
 Q 0
 ;
CDATA() ; Returns compiler flags, Execution TRACE ON/OFF, Time Logging ON/OFF, and Raw Data Logging ON/OFF
 ;  External Call.
 ;
 Q "0^0^0"
 ;
UPDATE(DFN,OCXSRC,OUTMSG) ; Main Entry point for evaluating Rules.
 ;  External Call.
 ;
 ;
 K ^TMP("OCXCHK",$J)
 S ^TMP("OCXCHK",$J)=($P($H,",",2)+($H*86400)+(2*60))_" <- ^TMP ENTRY EXPIRATION DATE FOR ^OCXOPURG"
 N OCXOERR,OCXOCMSG,OCXNDX,OCXDF,OCXX,OCXTSPI
 S OCXTSPI=300
 Q:'$G(DFN)
 I ($G(OCXOSRC)="GENERIC HL7 MESSAGE ARRAY") D GETDF,SWAPOUT("OCXODATA",.OCXODATA)
 I ($G(OCXOSRC)="GENERIC HL7 MESSAGE ARRAY") D CHK1^OCXOZ02
 I ($G(OCXOSRC)="DGPM PATIENT MOVEMENT PROTOCOL") D CHK23^OCXOZ03
 I ($G(OCXOSRC)="CPRS ORDER PRESCAN") D CHK58^OCXOZ05
 I ($G(OCXOSRC)="CPRS ORDER PROTOCOL") D CHK95^OCXOZ06
 ;
 D SCAN
 ;
 I $O(OCXOCMSG("")) D
 .N OCXNDX1,OCXNDX2
 .S OCXNDX1=0 F  S OCXNDX1=$O(OCXOCMSG(OCXNDX1)) Q:'OCXNDX1  D
 ..S OCXNDX2=0 F  S OCXNDX2=$O(OUTMSG(OCXNDX2)) Q:'OCXNDX2  Q:(OUTMSG(OCXNDX2)=OCXOCMSG(OCXNDX1))
 ..Q:OCXNDX2  S OUTMSG($O(OUTMSG(999999),-1)+1)=OCXOCMSG(OCXNDX1)
 K ^TMP("OCXCHK",$J)
 ;
 I ($G(OCXOSRC)="GENERIC HL7 MESSAGE ARRAY") K OCXDF D SWAPIN("OCXODATA",.OCXODATA)
 Q
 ;
GETDF ;This subroutine loads the OCXDF data field array from variables in the environment.
 ;  Called from UPDATE+9.
 ;
 Q:$G(OCXOERR)
 ;
 ;    Local GETDF Variables
 ; OCXDF(1) ----> Data Field: CONTROL CODE (FREE TEXT)
 ; OCXDF(2) ----> Data Field: FILLER (FREE TEXT)
 ; OCXDF(5) ----> Data Field: ORDER PRIORITY (OBR) (FREE TEXT)
 ; OCXDF(6) ----> Data Field: ABNORMAL FLAG (FREE TEXT)
 ; OCXDF(9) ----> Data Field: ORDER ST D/T (DATE/TIME)
 ; OCXDF(12) ---> Data Field: LAB RESULT (FREE TEXT)
 ; OCXDF(13) ---> Data Field: LAB COLLECTION D/T (DATE/TIME)
 ; OCXDF(15) ---> Data Field: RESULT STATUS (OBX) (FREE TEXT)
 ; OCXDF(21) ---> Data Field: ORDER PRIORITY (ORC) (FREE TEXT)
 ; OCXDF(23) ---> Data Field: REQUEST STATUS (OBR) (FREE TEXT)
 ; OCXDF(24) ---> Data Field: ORDERABLE ITEM LOCAL TEXT (FREE TEXT)
 ; OCXDF(34) ---> Data Field: ORDER NUMBER (NUMERIC)
 ; OCXDF(37) ---> Data Field: PATIENT IEN (NUMERIC)
 ; OCXDF(82) ---> Data Field: PHARMACY LOCAL ORDERABLE ITEM TEXT (FREE TEXT)
 ; OCXDF(113) --> Data Field: LAB TEST ID (NUMERIC)
 ; OCXDF(152) --> Data Field: LAB SPECIMEN ID (NUMERIC)
 ;
 ;      Local Extrinsic Functions
 ; DT2INT( ----------> CONVERT DATE FROM FILEMAN FORMAT TO OCX FORMAT
 ;
 S OCXDF(1)=$P($G(OCXODATA("ORC",1)),"^",1)
 S OCXDF(2)=$P($G(OCXODATA("ORC",3)),"^",2)
 S OCXDF(5)=$P($P($G(OCXODATA("OBR",27)),"^",6),";",1)
 S OCXDF(6)=$P($G(OCXODATA("OBX",8)),"^",1)
 S OCXDF(9)=$$DT2INT($P($G(OCXODATA("ORC",15)),"^",1))
 S OCXDF(12)=$P($G(OCXODATA("OBX",5)),"^",1)
 S OCXDF(13)=$$DT2INT($P($G(OCXODATA("OBR",7)),"^",1))
 S OCXDF(15)=$P($G(OCXODATA("OBX",11)),"^",1)
 S OCXDF(21)=$P($G(OCXODATA("ORC",7)),"^",6)
 S OCXDF(23)=$P($G(OCXODATA("OBR",25)),"^",1)
 S OCXDF(24)=$P($G(OCXODATA("OBR",4)),"^",5)
 S OCXDF(34)=$P($G(OCXODATA("ORC",2)),"^",1)
 S OCXDF(37)=$G(OCXODATA("PID",3))
 S OCXDF(82)=$P($G(OCXODATA("RXO",1)),"^",5)
 S OCXDF(113)=$P($G(OCXODATA("OBX",3)),"^",4)
 S OCXDF(152)=$P($P($G(OCXODATA("OBR",15)),"^",4),";",1)
 Q
 ;
SWAPOUT(NAME,ARRAY) ;
 ;  Called from UPDATE+9.
 ;
 Q:$G(OCXOERR)
 ;
 Q:'$L(NAME)
 K ^TMP("OCXSWAP",$J,NAME)
 S ^TMP("OCXSWAP",$J)=($P($H,",",2)+($H*86400)+(2*60))_" <- ^TMP ENTRY EXPIRATION DATE FOR ^OCXOPURG"
 M ^TMP("OCXSWAP",$J,NAME)=ARRAY
 K ARRAY
 Q
 ;
SWAPIN(NAME,ARRAY) ;
 ;  Called from UPDATE+24.
 ;
 Q:$G(OCXOERR)
 ;
 Q:'$L(NAME)
 K ARRAY
 M ARRAY=^TMP("OCXSWAP",$J,NAME)
 K ^TMP("OCXSWAP",$J,NAME)
 Q
 ;
SCAN ; Tests all Rules for Event/Elements that were found to be valid in the UPDATE subroutine.
 ;  Called from UPDATE+15.
 ;
 Q:$G(OCXOERR)
 ;
 ;
 N OCXD0,OCXRULE S OCXD0=0 F  S OCXD0=$O(^TMP("OCXCHK",$J,DFN,OCXD0)) Q:'OCXD0  D
 .Q:'($G(^TMP("OCXCHK",$J,DFN,OCXD0))=1)
 .N OCXPGM S OCXPGM=$O(^OCXS(860.3,"APGM",OCXD0,"")) Q:'$L(OCXPGM)  X "I $L($T("_OCXPGM_"))" E  Q
 .D @OCXPGM
 .S ^TMP("OCXCHK",$J,DFN,OCXD0)=$G(^TMP("OCXCHK",$J,DFN,OCXD0))+10
 K ^TMP("OCXCHK",$J)
 Q
 ;
TERM(OCXTERM,OCXLIST) ; Local Term Lookup
 ;  Internal Call.
 ;
 Q:$G(OCXOERR)
 ;
 Q:'$L(OCXTERM) 0
 ;
 N FILE,IEN,LINE,LTERM,NTERM,TEXT S FILE=0 K OCXLIST
 F LINE=1:1:999 S TEXT=$T(TERM+LINE) Q:$P(TEXT,";",2)  I ($E(TEXT,2,3)=";;") D
 .S TEXT=$P(TEXT,";;",2)
 .S NTERM=$P(TEXT,U,1) Q:'$L(NTERM)  Q:'(OCXTERM=NTERM)
 .S FILE=$P(TEXT,U,2),IEN=$P(TEXT,U,3),LTERM=$P(TEXT,U,4)
 .S OCXLIST(IEN)=LTERM,OCXLIST("B",LTERM,IEN)=""
 ;
 Q FILE
 ;
 ;TERM DATA;
 ;1;
 ;
 Q
 ;
DT2INT(OCXDT) ;      This Local Extrinsic Function converts a date into an integer
 ; By taking the Years, Months, Days, Hours and Minutes converting
 ; Them into Seconds and then adding them all together into one big integer
 ;
 Q:'$L($G(OCXDT)) ""
 N OCXDIFF,OCXVAL S (OCXDIFF,OCXVAL)=0
 ;
 I $L(OCXDT),'OCXDT,(OCXDT[" at ") D  ; EXTERNAL EXPERT SYSTEM FORMAT 1 TO EXTERNAL FORMAT
 .N OCXHR,OCXMIN,OCXTIME
 .S OCXTIME=$P($P(OCXDT," at ",2),".",1),OCXHR=$P(OCXTIME,":",1),OCXMIN=$P(OCXTIME,":",2)
 .S:(OCXDT["Midnight") OCXHR=00
 .S:(OCXDT["PM") OCXHR=OCXHR+12
 .S OCXDT=$P(OCXDT," at ")_"@"_$E(OCXHR+100,2,3)_$E(OCXMIN+100,2,3)
 ;
 I $L(OCXDT),(OCXDT?1.2N1"/"1.2N.1" ".2N.1":".2N) D  ; EXTERNAL EXPERT SYSTEM FORMAT 2 TO EXTERNAL FORMAT
 .N OCXMON
 .S OCXMON=$P("January^February^March^April^May^June^July^August^September^October^November^December",U,$P(OCXDT,"/",1))
 .I $L($P(OCXDT," ",2)) S OCXDT=OCXMON_" "_$P($P(OCXDT," ",1),"/",2)_"@"_$TR($P(OCXDT," ",2),":","")
 .E  S OCXDT=OCXMON_" "_$P($P(OCXDT," ",1),"/",2)
 ;
 I $L(OCXDT),(OCXDT?1.2N1"/"1.2N1"/"1.2N.1" ".2N.1":".2N) D  ; EXTERNAL EXPERT SYSTEM FORMAT 3 TO EXTERNAL FORMAT
 .N OCXMON
 .S OCXMON=$P("January^February^March^April^May^June^July^August^September^October^November^December",U,$P(OCXDT,"/",1))
 .I $L($P(OCXDT," ",2)) S OCXDT=OCXMON_" "_$P($P(OCXDT," ",1),"/",2)_","_$P($P(OCXDT," ",1),"/",3)_"@"_$TR($P(OCXDT," ",2),":","")
 .E  S OCXDT=OCXMON_" "_$P($P(OCXDT," ",1),"/",2)_", "_$P($P(OCXDT," ",1),"/",3)
 ;
 I $L(OCXDT),'OCXDT D  ; EXTERNAL FORMAT TO INTERNAL FILEMAN FORMAT
 .I (OCXDT["@0000") S OCXDT=$P(OCXDT,"@",1),OCXDIFF=1
 .N %DT,X,Y S X=OCXDT,%DT="" S:(OCXDT["@")!(OCXDT="N") %DT="T" D ^%DT S OCXDT=+Y
 ;
 I ($L(OCXDT\1)>7) S OCXDT=$$HL7TFM^XLFDT(OCXDT)  ; HL7 FORMAT TO INTERNAL FILEMAN FORMAT
 ;
 I ($L(OCXDT\1)=7) S OCXDT=$$FMTH^XLFDT(+OCXDT)   ; INTERNAL FILEMAN FORMAT TO $H FORMAT
 ;
 I (OCXDT?5N1","1.5N) S OCXVAL=(OCXDT*86400)+$P(OCXDT,",",2)     ;  $H FORMAT TO EXPERT SYSTEM INTERNAL FORMAT
 ;
 Q OCXVAL
 ;
