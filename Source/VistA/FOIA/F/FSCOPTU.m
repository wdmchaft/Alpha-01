FSCOPTU ;SLC/STAFF-NOIS Option Uitility ;1/13/98  22:33
 ;;1.1;NOIS;;Sep 06, 1998
 ;
SETUP1(CALL) ; from FSCOPT
 ; sets up editing for one call
 K ^TMP("FSC LIST CALLS",$J)
 K ^TMP("FSC SELECT",$J)
 S ^TMP("FSC SELECT",$J,"EVALUES")=1,^("EVALUES",1)=""
 S ^TMP("FSC LIST CALLS",$J)="1^1",^($J,1)=1
 S ^TMP("FSC LIST CALLS",$J,1,0)=$$SHORT^FSCGETS(CALL,1)
 S ^TMP("FSC LIST CALLS",$J,"IDX",1,1)=""
 S ^TMP("FSC LIST CALLS",$J,"ICX",1,CALL)=""
 S ^TMP("FSC LIST CALLS",$J,"CX",CALL)=""
 S (FSC1,FSCCNT)=1
 Q
 ;
CLEANUP ; from FSCOPT, FSCRUDQ
 ; cleans up FSC variables
 K FSC1,FSCCALL,FSCCALLS,FSCCNT,FSCDEV,FSCEDIT,FSCEXIT,FSCINDX,FSCL0,FSCLNAME,FSCLNUM,FSCQUERY,FSCSTU,FSCSTYLE,FSCUD
 K D,DIC,DIR,X,Y,ZTSK
 K ^TMP("FSC LIST CALLS",$J)
 K ^TMP("FSC NEWLIST",$J)
 K ^TMP("FSC SELECT",$J)
 Q
 ;
TMP ; from FSCNAR, FSCOP, FSCOPT, FSCRPC
 ; clean TMP nodes
 N NODE S NODE="FSC" F  S NODE=$O(^TMP(NODE)) Q:NODE=""  Q:NODE]"FSCZ"  I NODE'="FSC SESSION" K ^TMP(NODE,$J)
 Q
