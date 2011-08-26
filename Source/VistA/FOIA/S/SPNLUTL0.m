SPNLUTL0 ;HISCWDE,DAD-SCD REGISTRY FILE UTILITIES ;8/16/96  14:06
 ;;2.0;Spinal Cord Dysfunction;**7,11,13,19**;01/02/1997
 ;
EN1(SPND0) ; ***COMPLETENESS OF INJURY
 ;  SPND0 = Internal entry number in SCD REGISTRY file (#154)
 N SPNDATA,SPNFEEL,SPNINJRY,SPNMOVE
 S SPNDATA(5)=$G(^SPNL(154,+SPND0,5))
 S SPNMOVE=$P(SPNDATA(5),U,11),SPNFEEL=$P(SPNDATA(5),U,12)
 S SPNINJRY=1
 I (SPNMOVE=1)&(SPNFEEL=1) S SPNINJRY=2
 I SPNMOVE=2 S SPNINJRY=3
 I SPNFEEL=2 S SPNINJRY=4
 I SPNMOVE=3 S SPNINJRY=5
 I SPNFEEL=3 S SPNINJRY=6
 I (SPNMOVE=2)&(SPNFEEL=2) S SPNINJRY=7
 I (SPNMOVE=2)&(SPNFEEL=3) S SPNINJRY=8
 I (SPNMOVE=3)&(SPNFEEL=2) S SPNINJRY=9
 I (SPNMOVE=3)&(SPNFEEL=3) S SPNINJRY=10
 Q $P($T(EN1DATA+SPNINJRY),";",3)
EN1DATA ;; Completeness of injury text
 ;;DON'T KNOW
 ;;NONE
 ;;INCOMPLETE MOTOR
 ;;INCOMPLETE SENSORY
 ;;COMPLETE MOTOR
 ;;COMPLETE SENSORY
 ;;INCOMPLETE SENSORY AND MOTOR
 ;;COMPLETE SENSORY AND INCOMPLETE MOTOR
 ;;INCOMPLETE SENSORY AND COMPLETE MOTOR
 ;;COMPLETE SENSORY AND MOTOR
 ;
EN2(SPND0) ; *** EXTENT OF PARALYSIS
 ;  SPND0 = Internal entry number in SCD REGISTRY file (#154)
 N SPNARM,SPNDATA,SPNLEG,SPNXTENT
 S SPNDATA(5)=$G(^SPNL(154,+SPND0,5))
 S SPNARM(1)=$P(SPNDATA(5),U,6),SPNARM(2)=$P(SPNDATA(5),U,8)
 S SPNLEG(1)=$P(SPNDATA(5),U,7),SPNLEG(2)=$P(SPNDATA(5),U,9)
 S SPNXTENT=1
 I SPNARM(1)!SPNLEG(1) S SPNXTENT=2
 I SPNARM(1)&SPNLEG(1) S SPNXTENT=3
 I SPNLEG(2) S SPNXTENT=4
 I SPNARM(2)&SPNLEG(2) S SPNXTENT=5
 Q $P($T(EN2DATA+SPNXTENT),";",3)
EN2DATA ;; Extent of paralysis text
 ;;DON'T KNOW
 ;;MONOPLEGIA
 ;;HEMIPLEGIA
 ;;PARAPLEGIA
 ;;TETRAPLEGIA
 ;
EN3(SPNLDA) ; *** ONE/BOTH ARM/LEG & OTHER BODY PART AFFECTED FIELD CHECK
 N SPNLDATA,SPNLERR,SPNLFLD,SPNLTEXT,DA
 K DDSERROR
 S DA=SPNLDA
 F SPNLFLD=2.4,2.5,5.06:.01:5.1 D
 . S SPNLDATA(SPNLFLD)=$$GET^DDSVAL(154,DA,SPNLFLD,.SPNLERR,"I")
 . Q
 S SPNLDATA(5.01)=$$GET^DDSVAL(154,DA,5.01,.SPNLERR,"E")
 S SPNLTEXT=0
 ;I SPNLDATA(5.01)'["OTHER",SPNLDATA(2.4)]"" D
 ;. S SPNLTEXT=SPNLTEXT+1
 ;. S SPNLTEXT(SPNLTEXT)="You have not answered OTHER for CAUSE OF INJURY,"
 ;. S SPNLTEXT=SPNLTEXT+1
 ;. S SPNLTEXT(SPNLTEXT)="the DESCRIBE OTHER field should be left blank."
 ;. Q
 I SPNLDATA(5.06),SPNLDATA(5.08) D
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="You have answered YES to ONE ARM AFFECTED and BOTH ARMS AFFECTED."
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="You may only answer YES to one of these two fields."
 . Q
 I SPNLDATA(5.07),SPNLDATA(5.09) D
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="You have answered YES to ONE LEG AFFECTED and BOTH LEGS AFFECTED."
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="You may only answer YES to one of these two fields."
 . Q
 I SPNLDATA(5.1)'>0,SPNLDATA(2.5)]"" D
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="You have answered NO to OTHER BODY PART AFFECTED, the"
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="DESCRIBE OTHER field should be left blank."
 . Q
 I SPNLTEXT D HLP^DDSUTL(.SPNLTEXT) S DDSERROR=1
 Q
 ;
EN4(SPND0) ; *** Cause of SCD
 ; Input:  SPND0 = Internal entry number in SCD REGISTRY file (#154)
 ; Output: Traumatic, Non-traumatic, Unknown, Indeterminate
 S (SPNCAUSE("TC"),SPNCAUSE("NTC"),SPNCAUSE("U"),SPND1)=0
 F  S SPND1=$O(^SPNL(154,SPND0,"E",SPND1)) Q:SPND1'>0  D
 . S SPNETIOL=$P($G(^SPNL(154,SPND0,"E",SPND1,0)),U,2)
 . S SPNCAUSE=$P($G(^SPNL(154.03,+SPNETIOL,0)),U,2)
 . I "^TC^NTC^U^"'[(U_SPNCAUSE_U) Q
 . S SPNCAUSE(SPNCAUSE)=SPNCAUSE(SPNCAUSE)+1
 . Q
 S X=""
 I SPNCAUSE("TC") S X="TRAUMATIC"
 I SPNCAUSE("NTC") S X=X_$S(X]"":", ",1:"")_"NON-TRAUMATIC"
 I SPNCAUSE("U") S X=X_$S(X]"":", ",1:"")_"UNKNOWN"
 I X="" S X="INDETERMINATE"
 Q X
EN5(SPNLDA) ; This routine is to determine if the patient has a
 ; Clinician on file for this FIM report.
 ;
 N SPNLTEXT
 S SPNLTEXT=0
 I $O(^SPNL(154.1,SPNLDA,1,0))<1 D
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="This patient has no Clinician entered for this FIM ."
 . Q
 I SPNLTEXT D HLP^DDSUTL(.SPNLTEXT) S DDSERROR=1
 Q
 ;
EN6(SPNLDA) ; This is to confirm that the patient does have a Etiology
 ; on file.
 ;
 N SPNLTEXT
 S SPNLTEXT=0
 I $O(^SPNL(154,SPNLDA,"E",0))<1 D
 . S SPNLTEXT=SPNLTEXT+1
 . S SPNLTEXT(SPNLTEXT)="There is no Cause of SCD (Etioloty) on file for this patient."
 . Q
 I SPNLTEXT D HLP^DDSUTL(.SPNLTEXT) S DDSERROR=1
 Q
EN7(SPNDT1,SPNDT2,SPNTYP) ; This function is to validate the two dates
 ; Input:
 ;    SPNDT1 THE FIRST DATE
 ;    SPNDT2 THE SECOND DATE
 ;    THE TYPE OF DATE:
 ;           1 = RECEIVE DATE
 ;           2 = NEXT DUE DATE
 ;
 N SPNLTEXT K DDSERROR,DDSBR
 I SPNDT2<SPNDT1 D
 .S SPNLTEXT(1)="The Date "_$S(SPNTYP=1:"Received",SPNTYP=2:"Next Due",1:"")_" must not be before the Date "_$S(SPNTYP=1:"Offered",SPNTYP=2:"Received",1:"")_"."
 .S DDSERROR=1
 .D HLP^DDSUTL(.SPNLTEXT)
 .S DDSBR=(SPNTYP+1)_U_3_U_3
 .Q
 Q
EN9(SPNDFN) ;See if pt has any etiology is on file.
 ;wde this is callable from all points just need the ifn of the pt from file 154
 ;   will return  1 if the pt has an etiology
 ;   will return 0 if the pt has zip entered in the etiology field
 N SPNLFLG,SPNTIFN
 S (SPNLFLG,SPNTIFN)=0
 F  S SPNTIFN=$O(^SPNL(154,SPNDFN,"E",SPNTIFN)) Q:(SPNTIFN="")!('+SPNTIFN)  D  Q:+SPNLFLG
 .I $P(^SPNL(154,SPNDFN,"E",SPNTIFN,0),U,1)'="" S SPNLFLG=1
 .Q
 Q SPNLFLG
EN10 ;data validation for a Reg Status of 'Expired'
 ;cm Called from Block SPNLPBLK1 of Form SPNLPFM1
 S SPNLFLG=0
 S:"EXxex"'[DDSX&($P($G(^DPT(D0,.35)),U,1)) SPNLFLG=1,DDSERROR=1 W:$D(DDSERROR) *7 D:SPNLFLG=1 HLP^DDSUTL("Patient is deceased.")
 Q
