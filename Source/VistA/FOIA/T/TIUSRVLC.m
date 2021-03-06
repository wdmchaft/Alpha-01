TIUSRVLC ; SLC/JER - Server functions for lists ;06/19/97 16:22
 ;;1.0;TEXT INTEGRATION UTILITIES;;Jun 20, 1997
NOTES(TIUY,DFN,EARLY,LATE,ROOTFLAG) ; Gets list of Notes
 I $S(+$G(DFN)'>0:1,'$D(^DPT(+$G(DFN),0)):1,1:0) Q
 D LIST(.TIUY,3,DFN,$G(EARLY),$G(LATE),$G(ROOTFLAG))
 Q
SUMMARY(TIUY,DFN,EARLY,LATE,ROOTFLAG) ; Gets list of Summaries
 I $S(+$G(DFN)'>0:1,'$D(^DPT(+$G(DFN),0)):1,1:0) Q
 D LIST(.TIUY,244,DFN,$G(EARLY),$G(LATE),$G(ROOTFLAG))
 Q
CONSULT(TIUY,DFN,EARLY,LATE,ROOTFLAG) ; Gets list of Consults
 I $S(+$G(DFN)'>0:1,'$D(^DPT(+$G(DFN),0)):1,1:0) Q
 D LIST(.TIUY,243,STATUS,QSTR,$G(EARLY),$G(LATE),$G(ROOTFLAG))
 Q
LIST(TIUY,CLASS,DFN,EARLY,LATE,ROOTFLAG) ; Build List
 N TIUCNT,TIUDT,TIUI,TIUJ,TIUK,TIUP,TIUQ,TIUREC,TIUPRM0,TIUPRM1
 N TIUPRM3,TIUT,TIUTP,XREF,TIUS,TIUCONT,TIUSTAT,TIUTYPE
 K ^TMP("TIULIST",$J),^TMP("TIUI",$J)
 I '$D(TIUPRM0) D SETPARM^TIULE
 S EARLY=9999999-+$G(EARLY)
 S (TIUI,LATE)=9999999-$S(+$G(LATE):+$G(LATE),1:3333333)
 F  S TIUI=$O(^TIU(8925,"APTCL",DFN,CLASS,TIUI)) Q:+TIUI'>0!(+TIUI>EARLY)  D GATHER(DFN,CLASS,TIUI,ROOTFLAG)
 I +$O(^TMP("TIULIST",$J,0)) S TIUY=$NA(^TMP("TIULIST",$J)),(TIUI,TIUK)=0
 Q
GATHER(DFN,CLASS,TIUI,ROOTFLAG) ; Find/sort records for the list
 N TIUDA
 I '$D(TIUDPRM0) D SETPARM^TIULE
 S TIUDA=0
 F  S TIUDA=$O(^TIU(8925,"APTCL",DFN,CLASS,TIUI,TIUDA)) Q:+TIUDA'>0  D
 . I ($P(TIUPRM0,U,6)="S"),(+$$CANDO^TIULP(TIUDA,"VIEW")'>0) Q
 . D ADDELMNT(TIUDA,.TIUCNT,ROOTFLAG)
 Q
ADDELMNT(DA,TIUCNT,ROOTFLAG) ; Add each element to the list
 N DOC,LOC,PT,AUT,EDT,TIUPT,TIULST4,TIUREC,TIUR0,TIUR12,TIUR13
 N STATUS,EDTCNT,LOCTYP,TIUADT,TIUDDT,TIUSUBJ
 S TIUR0=$G(^TIU(8925,+DA,0)),TIUR12=$G(^TIU(8925,+DA,12))
 S TIUR13=$G(^TIU(8925,+DA,13)),TIUPT=$G(^DPT(+$P(TIUR0,U,2),0))
 S TIUSUBJ=$G(^TIU(8925,+DA,17))
 I '+$P(TIUR0,U,7) D
 . S $P(TIUR0,U,7)=+$G(^AUPNVSIT(+$P(TIUR0,U,3),0))
 . I '+$P(TIUR0,U,7) S $P(TIUR0,U,7)=""
 S DOC=$$PNAME^TIULC1(+TIUR0)
 I DOC="Addendum" S DOC=DOC_" to "_$$PNAME^TIULC1(+$G(^TIU(8925,+$P(TIUR0,U,6),0)))
 I +$$HASADDEN^TIULC1(+DA) S DOC="+ "_DOC
 I +$$URGENCY^TIURM(+DA)=1 S DOC=$S(DOC["+":"*",1:"* ")_DOC
 S STATUS=$$LOWER^TIULS($P($G(^TIU(8925.6,+$P(TIUR0,U,5),0)),U))
 S LOC=$G(^SC(+$P(TIUR12,U,5),0)),LOCTYP=$P(LOC,U,3),LOC=$P(LOC,U)
 S TIUADT=$S(LOCTYP="W":"Adm: ",1:"Visit: ")_$$DATE^TIULS($P(TIUR0,U,7),"MM/DD/YY")
 S TIUDDT=$S(+$P(TIUR0,U,8):"Dis: ",1:"")_$$DATE^TIULS($P(TIUR0,U,8),"MM/DD/YY")
 S PT=$$NAME^TIULS($P(TIUPT,U),"LAST, FIRST MI")
 S TIULST4=$E($P(TIUPT,U,9),6,9)
 S TIULST4="("_$E(PT)_TIULST4_")"
 S AUT=$$SIGNAME^TIULS(+$P(TIUR12,U,2))
 S EDT=+TIUR13
 S TIUCNT=+$G(TIUCNT)+1
 S TIUREC=DA_U_DOC_U_EDT_U_PT_" "_TIULST4_U_AUT_U_LOC_U_STATUS_U_TIUADT_U_TIUDDT_U
 I ($L(TIUREC)+$L(TIUSUBJ))>255 S TIUSUBJ=$E(TIUSUBJ,1,(255-$L(TIUREC)))
 S TIUREC=TIUREC_TIUSUBJ
 S ^TMP("TIULIST",$J,TIUCNT)=TIUREC
 S:+$G(ROOTFLAG) $P(^TMP("TIULIST",$J),U)=TIUCNT
 S:+$G(ROOTFLAG)&(TIUCNT=1) $P(^TMP("TIULIST",$J),U,3)=EDT
 S:+$G(ROOTFLAG) $P(^TMP("TIULIST",$J),U,2)=EDT
 Q
