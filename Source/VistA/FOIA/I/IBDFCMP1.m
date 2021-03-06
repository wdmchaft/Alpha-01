IBDFCMP1 ;ALB/MAF - AICS list of components on a form (cont.); 29-JUL-96
 ;;3.0;AUTOMATED INFO COLLECTION SYS;;APR 24, 1997
 ;
 ;
EXP ;EXPAND
 ; -- expand action
 N IBI,IBAT,VALMY,IBDVALM
 S (IBDCNT,IBDCNT1,VALMCNT)=0
 N IBDVALM,IBDAT,VALMY
 S VALMBCK=""
 D FULL^VALM1 S VALMBCK="R"
 D EN^VALM2($G(XQORNOD(0))) G REP:'$O(VALMY(0))  S IBDVALM=0
 F IBDVALM=0:0 S IBDVALM=$O(VALMY(IBDVALM)) Q:'IBDVALM  S IBDOBJ=$G(IBBLOCK(IBDVALM)) S (IBDCNT,IBDCNT1,VALMCNT)=0 D EN^VALM("IBDF COMPONENT EXPAND")
 G REP
 ;
 ;
INIT  K ^TMP("FORMEXP",$J),^TMP("EXPIDX") D
 .S IBDFIFN=$P(IBDOBJ,"^")
 .F IBDNUM=0:0 S IBDNUM=$O(^TMP("FORM-OBJ",$J,IBDFIFN,IBDNUM)) Q:'IBDNUM   I $P(IBDOBJ,"^",11)=$P($G(^TMP("FORM-OBJ",$J,IBDFIFN,IBDNUM)),"^",9) D
 ..N IBDFSC,IBDFNUM,IBDFSEL,IBDFHP,IBDFMC,IBDFROW,IBDFCOL
 ..S (IBDFSC,IBDFNUM)=0
 ..S IBDOBJ1=$G(^TMP("FORM-OBJ",$J,IBDFIFN,IBDNUM))
 ..S IBDF("PI")=+$P(IBDOBJ1,"^",2),IBDF("TYPE")=$P(IBDOBJ1,"^",5)
 ..S IBDF("IEN")=+$P(IBDOBJ1,"^",6),IBDF("VITAL")=$P(IBDOBJ1,"^",7)
 ..I $P(IBDOBJ1,"^",5)="LIST" D  ;SELECTION LIST
 ...S IBDFSEL=$P(^IBE(357.2,$P(IBDOBJ1,"^",6),0),"^")
 ...S IBDFSNOD=$O(^IBE(357.2,$P(IBDOBJ1,"^",6),1,0)) S IBDFNODE=$G(^IBE(357.2,$P(IBDOBJ1,"^",6),1,+IBDFSNOD,0)) S IBDFSEL=IBDFSEL_"^"_$P(IBDFNODE,"^",4)_"^"_$P(IBDFNODE,"^",3) S IBDFROW=4,IBDFCOL=5 D SETUP(IBDFSEL,IBDFROW,IBDFCOL)
 ...F  S IBDFSC=$O(^IBE(357.2,IBDF("IEN"),2,"B",IBDFSC)) Q:'IBDFSC  F  S IBDFNUM=$O(^IBE(357.2,IBDF("IEN"),2,"B",IBDFSC,IBDFNUM)) Q:'IBDFNUM  S IBDFSEL=$G(^IBE(357.2,IBDF("IEN"),2,IBDFNUM,0)) D SETUP1(IBDFSEL)
 ..I $P(IBDOBJ1,"^",5)="MC" D  ;MULTIPLE CHOICE
 ...S IBDFMC=$G(^IBE(357.93,IBDF("IEN"),0)) S IBDFROW=4,IBDFCOL=3 D SETUP(IBDFMC,IBDFROW,IBDFCOL)
 ..I $P(IBDOBJ1,"^",5)="HP" D  ;HAND PRINT FIELD
 ...S IBDFHP=$G(^IBE(359.94,IBDF("IEN"),0)) S IBDFROW=4,IBDFCOL=3 D SETUP(IBDFHP,IBDFROW,IBDFCOL)
 ..I $P(IBDOBJ1,"^",5)="DF" D  ;DATA FIELDS
 ...S IBDFDF=$G(^IBE(357.5,IBDF("IEN"),0)) S IBDFROW=11,IBDFCOL=10 D SETUP(IBDFDF,IBDFROW,IBDFCOL)
 ..I $P(IBDOBJ1,"^",5)="FL" D  ;FORM LINE
 ...S IBDFFL=$G(^IBE(357.7,IBDF("IEN"),0)) S IBDFROW=3,IBDFCOL=2 D SETUP(IBDFFL,IBDFROW,IBDFCOL)
 ..I $P(IBDOBJ1,"^",5)="TA" D  ;TEXT AREA
 ...S IBDFTA=$G(^IBE(357.8,IBDF("IEN"),0)) S IBDFROW=4,IBDFCOL=3 D SETUP(IBDFTA,IBDFROW,IBDFCOL)
 Q
 ;
 ;
REP ;  -- Redisplay initial screen
 S IBDFIFN=$S('$D(IBDFIFN):+$P(IBDOBJ,"^"),1:IBDFIFN) D INIT1^IBDFCMP S VALMBCK="R" Q
 Q
 ;
 ;
SETUP(IBOBJECT,IBROW,IBCOL) ;  -- Setting up the data for list manager
 S IBDCNT1=IBDCNT1+1
 S X=""
 S X=$$SETSTR^VALM1(X,X,1,80) D TMP
 S X="",X=$P(IBOBJECT,"^")
 S X=$$SETSTR^VALM1(X,X,1,25)
 S IBDVAL=$S($P(IBDOBJ1,"^",5)]"":$P(IBDOBJ1,"^",5),1:"")
 S X=$$SETSTR^VALM1(IBDVAL,X,30,10)
 S IBDVAL=$S($P(IBOBJECT,"^",IBROW):$P(IBOBJECT,"^",IBROW)+1,$P(IBOBJECT,"^",IBROW)=0:1,1:"N/A")
 S X=$$SETSTR^VALM1($J(IBDVAL,3),X,48,6)
 S IBDVAL=$S($P(IBOBJECT,"^",IBCOL):$P(IBOBJECT,"^",IBCOL)+1,$P(IBOBJECT,"^",IBCOL)=0:1,1:"N/A")
 S X=$$SETSTR^VALM1($J(IBDVAL,4),X,58,5)
 ;D TMP,CNTRL^VALM10(VALMCNT,1,80,IOINHI,IOINORM,0)
 I X]"",$P(IBDOBJ1,"^",5)="LIST" D
 .S IBDVAL=$S($P(^IBE(357.2,IBDF("IEN"),0),"^",7):$P(^IBE(357.2,IBDF("IEN"),0),"^",7),1:"N/A")
 .S IBDVAL=$J($S(IBDVAL=1:"1 Space",IBDVAL=2:"2 Spaces",IBDVAL=3:"LINE",IBDVAL=4:"Sp/Ln/Sp",1:"N/A"),9)
 .S X=$$SETSTR^VALM1(IBDVAL,X,71,9)
 .D TMP,CNTRL^VALM10(VALMCNT,1,80,IOINHI,IOINORM,0)
 .S X="",X=$$SETSTR^VALM1(X,X,1,80) D TMP
 .S IBDVAL="   "_"Subcolumn"_"    "_"Type"_"          "_"Data"_"    "_"Width"_"     "_"Qualifier"_"       "_"Rule"_"       "_"Edit"
 .S X="",X=$$SETSTR^VALM1(IBDVAL,X,1,80) D TMP,CNTRL^VALM10(VALMCNT,1,80,IOINHI,IOINORM,0)
 I $P(IBDOBJ1,"^",5)'="LIST" D
 .D TMP,CNTRL^VALM10(VALMCNT,1,80,IOINHI,IOINORM,0)
 Q
SETUP1(IBOBJECT) ;  -- Setup of the subcolumn info for the Selection list
 S X=""
 S X=$$SETSTR^VALM1($P(IBOBJECT,"^"),X,8,2)
 S IBDVAL=$S($P(IBOBJECT,"^",4)]"":$P(IBOBJECT,"^",4),1:"")
 S IBDVAL=$J($$LOWER^VALM1($S(IBDVAL=1:"TEXT",IBDVAL=2:"MARKING",1:"")),10)
 S X=$$SETSTR^VALM1(IBDVAL,X,11,10)
 S IBDVAL=$S($P(IBOBJECT,"^",5):$P(IBOBJECT,"^",5),1:"")
 S IBDVAL=$J($$LOWER^VALM1($S(IBDVAL=1:"CODE",IBDVAL=2:"SHORT NAME",IBDVAL=3:"DESCRIP.",1:"N/A")),10)
 S X=$$SETSTR^VALM1(IBDVAL,X,25,10)
 S IBDVAL=$S($P(IBOBJECT,"^",3)]"":$P(IBOBJECT,"^",3),1:"")
 S X=$$SETSTR^VALM1($J(IBDVAL,3),X,40,3)
 S IBDVAL=$S($P(IBOBJECT,"^",3)]"":$P(IBOBJECT,"^",3),1:"")
 S IBDVAL=$S($P(IBOBJECT,"^",9):$P(IBOBJECT,"^",9),1:"")
 S IBDVAL=$P($G(^IBD(357.98,+IBDVAL,0)),"^",3)
 S IBDVAL=$J($$LOWER^VALM1($S(IBDVAL]"":IBDVAL,1:"N/A")),10)
 S X=$$SETSTR^VALM1(IBDVAL,X,47,10)
 S IBDVAL=$S($P(IBOBJECT,"^",10):$P(IBOBJECT,"^",10),1:"")
 S IBDVAL=$J($$LOWER^VALM1($S(IBDVAL=0:"ANY NUMBER",IBDVAL=1:"ONLY 1",IBDVAL=2:"AT MOST 1",IBDVAL=3:"AT LEAST 1",1:"N/A")),10)
 S X=$$SETSTR^VALM1(IBDVAL,X,59,10)
 ;I $P(IBDOBJ1,"^",5)="LIST" D
 ;S IBDVAL=$S($P(^IBE(357.2,IBDF("IEN"),0),"^",7):$P(^IBE(357.2,IBDF("IEN"),0),"^",7),1:"N/A")
 ;S IBDVAL=$S(IBDVAL=1:"1 S",IBDVAL=2:"2 S",IBDVAL=3:"LIN",IBDVAL=4:"SLS",1:"N/A")
 S IBDVAL=$S($P(IBOBJECT,"^",7)=1:"Yes",1:"No")
 S X=$$SETSTR^VALM1($J(IBDVAL,3),X,77,3)
 D TMP
 Q
 ;
 ;
TMP ; -- Set up Array
 S IBDCNT=IBDCNT+1,VALMCNT=VALMCNT+1
 S ^TMP("FORMEXP",$J,IBDCNT,0)=X,^TMP("FORMEXP",$J,"IDX",VALMCNT,IBDCNT1)=""
 S ^TMP("EXPIDX",$J,IBDCNT)=VALMCNT ;_"^"_IBDFIFN_"^"_IBDF("BLK")
 Q
 ;
 ;
HDR ; -- print patient header
 S X=""
 S X="      Form Name: "_$E($P($G(^IBE(357,IBDFIFN,0)),"^"),1,25)
 S VALMHDR(1)=X
 S X="     Block Name: "_$E($P($G(^IBE(357.1,+$P(IBDOBJ,"^",11),0)),"^"),1,25)
 S VALMHDR(2)=X
 Q
 ;
 ;
EXIT ;  -- Exit code
 K ^TMP("FORMEXP",$J),^TMP("EXPIDX")
 Q
HELP ; -- help code
 S X="?" D DISP^XQORM1 W !!
 Q
