TIUBRWS ; SLC/JER - Single patient browse ; 11/12/00
 ;;1.0;TEXT INTEGRATION UTILITIES;**100**;Jun 20, 1997
MAIN(TIULTMP,TIUTYP) ; Control Branching
 N DFN,TIU,TIUOUT,TIUREL,TIUCHK,TIUA,TIUSEE,ACT,TIUY
 N TIUOUT,TIUSEE,TIUI,TIUQUIT
 N TIUGDATA
 I '$D(TIUPRM0) D SETPARM^TIULE
 S:$D(ORVP) DFN=+ORVP S TIUTYP=$G(TIUTYP,38)
 S TIUTYP(1)="1^"_TIUTYP_U_$P(^TIU(8925.1,+TIUTYP,0),U)
 F  D  Q:+$G(TIUOUT)
 . N TIUDAT,TIUGDATA
 . D SELPAT^TIULA2(.TIUDAT,TIUTYP,+$G(DFN))
 . I +$G(TIUDAT)'>0,($D(TIUDAT)'>9) D  S TIUOUT=1 Q
 . . W !!,"Nothing selected."
 . S TIUI=0
 . F  S TIUI=$O(TIUDAT(TIUI)) Q:+TIUI'>0  D  Q:$D(DUOUT)!$D(DIROUT)!+$G(TIUOUT)
 . . S TIUDA=+$G(TIUDAT(TIUI)) Q:TIUDA'>0
 . . D GETTIU^TIULD(.TIU,+TIUDA)
 . . I $D(TIU) D
 . . . S TIUSEE=$$CANDO^TIULP(TIUDA,"VIEW")
 . . . I 'TIUSEE D  Q
 . . . . W !!,$C(7),$P(TIUSEE,U,2),! K DFN
 . . . . I $D(ORVP) S TIUOUT=1
 . . . . S TIU=$$READ^TIUU("FOA","Press RETURN to continue...")
 . . . ; -- Get ID data needed for browse: --
 . . . S TIUGDATA=$$IDDATA^TIURECL1(TIUDA)
 . . . D EN^VALM(TIULTMP)
 . . . K ^TMP("TIUVIEW",$J),DFN
 . . . S:$D(TIUQUIT) TIUOUT=1
 Q
