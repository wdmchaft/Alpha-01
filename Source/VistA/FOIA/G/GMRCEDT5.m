GMRCEDT5 ;SLC/DCM - EDIT A CONSULT AND RE-SEND AS NEW ;6/17/98  10:45
 ;;3.0;CONSULT/REQUEST TRACKING;**5**;DEC 27, 1997
WP ;edit comment and request fields
 N CMDA,DIE,DIC,DWPK,DWLW,DIWESUB,GMRCNOW
 S DIE="^GMR(123,"
 S GMRCNOW=$$FMTE^XLFDT($$NOW^XLFDT)
 S DIC=DIE_+GMRCO_",40,",DWPK=1,DWLW=74
 I FLDA=40 S GMRCA=20 D  S DIWESUB="COMMENTS"
 .S CMDA=$$ADDCM^GMRCEDT4(+GMRCO),GMRCFLD(FLDA)="COMMENT ADDED: "_$$FMTE^XLFDT($$NOW^XLFDT)_"^"_CMDA D AUDIT0^GMRCEDT4(CMDA,+GMRCO) S DIC=DIC_CMDA_",1," Q
 .Q
 I $D(IOTM),$D(IOBM),$D(IOSTBM) D FULL^VALM1
 I $D(FLDA),FLDA=20 S DIC=DIE_(+GMRCO)_",20,",DWPK=1,DWLW=74 I '$D(GMRCFLD(FLDA)) S GMRCFLD(FLDA)="REASON FOR REQUEST" M GMRCFLD(FLDA)=^GMR(123,+GMRCO,20)
 D EN^DIWE
 I FLDA=40,$D(^GMR(123,+GMRCO,40,CMDA,0)),$P($G(^GMR(123.1,+$P(^GMR(123,+GMRCO,40,CMDA,0),U,2),0)),U)="ADDED COMMENT",'$O(^GMR(123,+GMRCO,40,CMDA,0)) D
 .I $D(CMDA),'$O(^GMR(123,+GMRCO,40,CMDA,0)) S DA=40,DA(1)=CMDA,DIK="^GMR(123,"_DA(1)_",40," D ^DIK K DIK,GMRCFLD(FLDA)
 .Q
 S VALMBCK="R"
 Q
SAVED ;These lines were removed from WP+8 to WP+20 to removed the
 ;ability to edit comments in the Edit Cancelled Consult
 ;Allert. They were placed here until it is determined that
 ;they should be permaneltly deleted.
 I GMRCEDCM>0 D  Q:GMRCQUT
 .F  W !,"Type The Comment Number To Edit Or Type 0 (Zero) To Add A NEW Comment",!!,"Comment: " R X:DTIME S GMRCQUT=$S('$T:1,X["^":1,X="":1,1:0) Q:GMRCQUT  D  I $D(CMDA),CMDA>0 Q
 ..I X["?" W !,"Choose from : " D  Q
 ...S GMRCLX=0 F  S GMRCLX=$O(GMRCED(GMRCLX)) Q:GMRCLX=""  S GMRCNDA=GMRCED(GMRCLX) W !,"COMMENT #",$J(GMRCLX,2)," "
 ...W "Entered: ",$$FMTE^XLFDT($P(^GMR(123,+GMRCO,40,GMRCNDA,0),"^",1)),"   By: ",$P(^VA(200,$P(^(0),"^",4),0),"^",1),!
 ...W !
 ...Q
 ..I X=0 S CMDA=$$ADDCM^GMRCEDT4(+GMRCO) D AUDIT0^GMRCEDT4(CMDA,+GMRCO) S DIC=DIC_CMDA_",1,",DTOUT=0,Y=$$FMTE^XLFDT(DT,"1D"),GMRCFLD(FLDA)="COMMENT ADDED: "_Y_"^"_CMDA Q
 ..I $D(GMRCED(X)) S CMDA=GMRCED(X),GMRCFLD(FLDA)="COMMENT EDITED: "_$$FMTE^XLFDT($P(^GMR(123,+GMRCO,40,CMDA,0),"^",1)),DIC=DIC_CMDA_",1," Q
 ..I '$D(GMRCED(X)) W $C(27)," ??"
 ..Q
 .Q
 Q
