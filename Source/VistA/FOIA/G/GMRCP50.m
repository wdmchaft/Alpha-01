GMRCP50 ;ISP/TDP - PRE INSTALL FOR GMRC*3*50 ; 11/29/2005
 ;;3.0;CONSULT/REQUEST TRACKING;**50**;DEC 27, 1997;Build 8
 Q
EN ;Entry point for manual start from programmer's prompt
 N DIR,DIROUT,DIRUT,DTOUT,DUOUT,NMFLG,X,Y
 S DIR(0)="Y"
 S DIR("A")="Print Full Patient Names on pre-install report"
 S DIR("B")="YES"
 S DIR("?")="Answer 'NO' to print patients' initials and last 6 of SSN."
 S DIR("?",1)="Answer 'YES' to print patients' full name and last 4 of SSN."
 D ^DIR I (($G(DIROUT))!($G(DIRUT))!($G(DUOUT))!($G(DTOUT))) Q
 S NMFLG=Y
PRE ;Start of Pre-init of patch GMRC*3*50
 N GMRCTTL,GMRCITL
 K ^TMP("GMRCP50",$J),^TMP("GMRCP50 IFC",$J)
 I '$D(NMFLG) N NMFLG D
 . S NMFLG=$O(XPDQUES(""))
 . S NMFLG=$G(XPDQUES(NMFLG))
 D BMES^XPDUTL("Starting Pre-init...")
 D BMES^XPDUTL("   Searching for ampersand (""&"") in the SIGNIFICANT FINDINGS (#15) field")
 D MES^XPDUTL("   of the REQUEST/CONSULTATION (#123) file.")
 D MES^XPDUTL(" ")
 D SEARCH
 I GMRCTTL!(GMRCITL) D MSG^GMRCP50A
 D BMES^XPDUTL("Pre-init complete.")
 Q
SEARCH ;Search SIGNIFICANT FINDINGS (#15) field of the REQUEST/CONSULTATION
 ;(#123) file for ampersand ("&").
 N GMRC0,GMRC40,GMRCACT,GMRCACDT,GMRCADT,GMRCAIEN,GMRCCIEN,GMRCCOM
 N GMRCCPRS,GMRCDFN,GMRCDFN1,GMRCDONE,GMRCDT,GMRCIEN,GMRCIFC,GMRCSVC
 N GMRCSSN,GMRCSSN1,GMRCWHO
 S (GMRCDT,GMRCITL,GMRCTTL)=0
 F  S GMRCDT=$O(^GMR(123,"B",GMRCDT)) Q:GMRCDT=""  D
 . S GMRCIEN=""
 . F  S GMRCIEN=$O(^GMR(123,"B",GMRCDT,GMRCIEN)) Q:GMRCIEN=""  D
 .. S GMRCDONE=0
 .. S GMRC0=$G(^GMR(123,GMRCIEN,0)) I $P(GMRC0,U,19)'="&" Q
 .. S GMRCIFC="GMRCP50"
 .. I $P($G(^GMR(123,GMRCIEN,12)),U,5)="P" S GMRCIFC="GMRCP50 IFC"
 .. S GMRCDFN=+$P(GMRC0,U,2) S:GMRCDFN GMRCSSN=$P($G(^DPT(GMRCDFN,0)),U,9),GMRCDFN=$P($G(^DPT(GMRCDFN,0)),U,1)
 .. S GMRCSSN1="("_$E(GMRCDFN,1)_$E(GMRCSSN,6,9)_")"
 .. S GMRCDFN1=GMRCDFN
 .. I (GMRCDFN=0)!(GMRCDFN="") S GMRCDFN="PATIENT UNKNOWN"
 .. S GMRCDFN=GMRCDFN_" "_GMRCSSN1
 .. S GMRCSVC=+$P(GMRC0,U,5) S:GMRCSVC GMRCSVC=$P($G(^GMR(123.5,GMRCSVC,0)),U,1)
 .. I (GMRCSVC=0)!(GMRCSVC="") S GMRCSVC="SERVICE UNKNOWN"
 .. S GMRCCPRS=+$P(GMRC0,U,12) S:GMRCCPRS GMRCCPRS=$P($G(^ORD(100.01,GMRCCPRS,0)),U,1)
 .. I (GMRCCPRS=0)!(GMRCCPRS="") S GMRCCPRS="STATUS UNKNOWN"
 .. D ACTIVITY
 .. S ^TMP(GMRCIFC,$J,GMRCDFN,GMRCDT,GMRCIEN,0)=GMRCIEN_U_GMRCSVC_U_GMRCCPRS_U_GMRCACT_U_GMRCACDT_U_GMRCWHO
 .. I 'NMFLG D
 ... S ^TMP(GMRCIFC,$J,GMRCDFN,0)="("_$E($P(GMRCDFN1,",",2),1)_$E($P($P(GMRCDFN1,",",2)," ",2),1)_$E(GMRCDFN1,1)_$E(GMRCSSN,4,9)_")"
 .. W !,"   Consult entry "_GMRCIEN_" has an ampersand (""&"") as the Significant Finding."
 .. S GMRCTTL=GMRCTTL+1
 .. I GMRCIFC="GMRCP50 IFC" S GMRCITL=GMRCITL+1
 D MES^XPDUTL(" ")
 D BMES^XPDUTL(GMRCTTL_" total consults contain an ampersand as the Significant Finding.")
 Q
ACTIVITY ;Search thru all Request Processing Activities and return any
 ;Significant Findings or Administrative Completions.
 N GMRCSIG,GMRCFLG
 S GMRCSIG=$O(^GMR(123.1,"B","SIG FINDING UPDATE",""))
 S GMRCFLG=0
ACT1 S (GMRCACDT,GMRCACT,GMRCADT,GMRCWHO)=""
 F  S GMRCADT=$O(^GMR(123,GMRCIEN,40,"B",GMRCADT),-1) Q:GMRCADT=""  D  Q:GMRCDONE
 . S GMRCAIEN=""
 . F  S GMRCAIEN=$O(^GMR(123,GMRCIEN,40,"B",GMRCADT,GMRCAIEN)) Q:GMRCAIEN=""  D  Q:GMRCDONE
 .. S GMRC40=$G(^GMR(123,GMRCIEN,40,GMRCAIEN,0)) I $P(GMRC40,U,2)'=GMRCSIG Q
 .. S GMRCACT=+$P(GMRC40,U,2) S:GMRCACT GMRCACT=$P($G(^GMR(123.1,GMRCACT,0)),U,1)
 .. I (GMRCACT=0)!(GMRCACT="") S GMRCACT="ACTIVITY UNKNOWN"
 .. S GMRCACDT=+$P(GMRC40,U,3)
 .. S GMRCWHO=+$P(GMRC40,U,4) S:'GMRCWHO GMRCWHO=+$P(GMRC40,U,5)
 .. I 'GMRCWHO S GMRCWHO=$P($G(^GMR(123,GMRCIEN,40,GMRCAIEN,2)),U,2) S:'GMRCWHO GMRCWHO=$P($G(^GMR(123,GMRCIEN,40,GMRCAIEN,2)),U,1)
 .. S:+GMRCWHO GMRCWHO=$P($G(^VA(200,GMRCWHO,0)),U,1)
 .. I (GMRCWHO=0)!(GMRCWHO="") S GMRCWHO="RESP. PERSON UNKNOWN"
 .. D COMMENT
 .. S GMRCDONE=1
 I 'GMRCDONE,'GMRCFLG S GMRCSIG=$O(^GMR(123.1,"B","COMPLETE/UPDATE","")),GMRCFLG=1 D ACT1
 Q
COMMENT ;Gather comment for Activity
 I '$D(^GMR(123,GMRCIEN,40,GMRCAIEN,1,0)) Q
 S GMRCCIEN=0
 F  S GMRCCIEN=$O(^GMR(123,GMRCIEN,40,GMRCAIEN,1,GMRCCIEN)) Q:GMRCCIEN=""  D
 . S GMRCCOM=$G(^GMR(123,GMRCIEN,40,GMRCAIEN,1,GMRCCIEN,0))
 . I GMRCCOM="" S GMRCCOM="NO COMMENT AVAILABLE"
 . S ^TMP(GMRCIFC,$J,GMRCDFN,GMRCDT,GMRCIEN,GMRCCIEN)=GMRCCOM
 Q
