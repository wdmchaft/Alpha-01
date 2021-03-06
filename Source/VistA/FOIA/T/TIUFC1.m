TIUFC1 ; SLC/MAM - LM Template C (Create DDEF) Action Create ;7/7/06  15:44
 ;;1.0;TEXT INTEGRATION UTILITIES;**211**;Jun 20, 1997;Build 26
 ;
CREATE ; TEMPLATE C Action Create.
 ; C in CNODE0, CSTATUS stands for Current position
 N DIC,DIE,DR,DLAYGO,X,Y,CREATE0,DA,DIK,PFILEDA,TENDA,MSG,PLINENO,PINFO,CREATE,FIELDS,NEWSTAT,NAME,TIUFXNOD,SHARED,NEWYES,SHARYES,CNODE0,LINENO
 N DIR,X,Y,DA,NAME,TIUFFULL,CSTATUS,TIUFTMSG,TIUFTLST,TIUFIMSG,SEQUENCE
 N DUOUT,DTOUT,DIRUT,DIROUT,XFLG
 S CREATE=0,TIUFXNOD=$G(XQORNOD(0)),VALMBCK="",CNODE0=^TIU(8925.1,TIUFCDA,0)
 I $P(CNODE0,U,13),TIUFWHO'="N",$P(CNODE0,U,4)="DOC"!($P(CNODE0,U,4)="CO") W !!," Parent is National, of Type TL or CO; Can't add or delete Items" D PAUSE^TIUFXHLX G CREAX
ANOTHER L -^TIU(8925.1,+$G(CREATEDA)) L -^TIU(8925.1,+$G(TIUFCDA),10,+$G(TENDA))
 I TIUFCTYP="TL" S CSTATUS=$$STATWORD^TIUFLF5($P(CNODE0,U,7)) I CSTATUS'="INACTIVE" W !!,"Cannot create a Component unless the parent Title is Inactive.",! D PAUSE^TIUFXHLX G CREAX
 N DIR S DIR(0)="FAO^3:60^S X=$$UPPER^TIULS(X) K:'(X'?1P.E) X",(DIR("?"),DIR("??"))="^D NAME^TIUFXHLX"
 S:'CREATE DIR("A")=$S(TIUFCTYP="TL":" Enter a new Component of ",1:" Enter the Name of a new ")_TIUFCNM_": "
 S:CREATE DIR("A")=" If you wish, you may enter another "_$S(TIUFCTYP="TL":"Component of ",1:"")_TIUFCNM_": "
 D ^DIR I Y=""!$D(DUOUT) G CREAX
 S NAME=Y,NAME=$$UPPER^TIULS(NAME),(SHARED,NEWYES,SHARYES)=""
 I TIUFCTYP="TL" K DIRUT D  Q:$D(DIRUT)  G ADD:SHARYES,ANOTHER:NEWYES=0,ANOTHER:SHARYES=0
 . S DIC=8925.1,DIC(0)="Z"
 . S DIC("S")="I ($P(^(0),U,4)=""CO"")&($P(^(0),U,10))"
 . D ^DIC
 . I Y=-1 S DIR("A",1)=" Are you adding '"_NAME_"' as",DIR("A")="a new TIU DOCUMENT DEFINITION?",DIR(0)="Y",DIR("B")="YES" D ^DIR S NEWYES=Y Q
 . I Y>0 W $E($P(Y,U,2),$L(NAME)+1,60) D  Q:'SHARYES
 . . S CREATEDA=+Y,CREATENM=$P(Y(0),U)
 . . S DIR("A",1)=" Are you adding shared component '"_$P(Y,U,2)_"' as",DIR("A")="a new Item",DIR("B")="YES",DIR(0)="Y" D ^DIR S SHARYES=Y
 . . I 'SHARYES K CREATEDA,CREATENM
 . I $$DUPITEM^TIUFLF7(CREATENM,TIUFCDA) W !!,TIUFIMSG,! D PAUSE^TIUFXHLX S DIRUT=1 Q
 . S CREATE0=^TIU(8925.1,CREATEDA,0)
 I $$DUPITEM^TIUFLF7(NAME,TIUFCDA) W !!,TIUFIMSG,! D PAUSE^TIUFXHLX G ANOTHER
 D TYPELIST^TIUFLF7(NAME,0,TIUFCDA,.TIUFTMSG,.TIUFTLST) G:$D(DTOUT) CREAX
 I TIUFTLST="" W !!," Please enter a different Name; File already has entries of every permitted Type",!,"with that Name",! D PAUSE^TIUFXHLX G ANOTHER
 N DIC S (DIC,DLAYGO)=8925.1,DIC(0)="L",X=""""_NAME_"""" D ^DIC
 I Y=-1 W !!,"Couldn't Create Entry; See IRM",! D PAUSE^TIUFXHLX G CREAX
 S CREATEDA=+Y,CREATENM=NAME
 L +^TIU(8925.1,CREATEDA):1 I '$T W !!," Another user is editing this entry; please try later.",! H 2 G CREAX
 D STUFFLDS^TIUFLF4(CREATEDA,TIUFCDA)
 S CREATE0=^TIU(8925.1,CREATEDA,0)
 S FIELDS=";.05;.06;" S:$P(CREATE0,U,4)="" FIELDS=";.04"_FIELDS S:TIUFWHO="N" FIELDS=FIELDS_".13;"
 D ASKFLDS^TIUFLF1(CREATEDA,FIELDS,TIUFCDA,.NEWSTAT,.XFLG) G:$D(DTOUT) CREAX
NOTYPE S CREATE0=^TIU(8925.1,CREATEDA,0)
 I $P(CREATE0,U,4)="" S DA=CREATEDA,DIK="^TIU(8925.1," D ^DIK W !!," "_$E(CREATENM,1,40)," Deleted: No Type.",! D PAUSE^TIUFXHLX G CREAX
 D OWNCHEC^TIUFLF8(CREATEDA)
ADD S PFILEDA=TIUFCDA
 L +^TIU(8925.1,TIUFCDA):3 I '$T D  G ANOTHER
 . W !!,"Another user is editing current branch; can't hang item under parent.  Entry",!,"deleted.  Please create entry again.",! D PAUSE^TIUFXHLX
 . L -^TIU(8925.1,CREATEDA) S CREATE=0,DA=CREATEDA,DIK="^TIU(8925.1," D ^DIK
 D ADDTEN^TIUFLF4(PFILEDA,CREATEDA,CREATE0,.TENDA)
 L -^TIU(8925.1,TIUFCDA)
 I 'TENDA S VALMQUIT=1 W !!," Can't hang item under parent; See IRM. Entry deleted.",! D PAUSE^TIUFXHLX L -^TIU(8925.1,CREATEDA) S DA=CREATEDA,DIK="^TIU(8925.1," D ^DIK G CREAX
 I '$G(XFLG),'$D(DTOUT),'SHARED S FIELDS=";.07;" S:$P(CREATE0,U,4)="DOC" FIELDS=";1501"_FIELDS D ASKFLDS^TIUFLF1(CREATEDA,FIELDS,TIUFCDA,.NEWSTAT,.XFLG) G:$D(DTOUT) CREAX
 I '$G(XFLG),'$D(DTOUT) L +^TIU(8925.1,TIUFCDA,10,TENDA):1 W:'$T !!," Another user is editing this item; please edit later using Detailed Display for",!,"Current Branch.",! H:'$T 2 G:'$T CREAX D  G:$D(DTOUT) CREAX
 . S DA(1)=TIUFCDA,DIE="^TIU(8925.1,DA(1),10,",DA=TENDA
 . S DR="3" D ^DIE I $D(Y)!$D(DTOUT) Q
 . I $P(CNODE0,U,4)="CL" S SEQUENCE=$P(^TIU(8925.1,TIUFCDA,10,TENDA,0),U,3),DR="2///^S X=SEQUENCE" I $L(SEQUENCE)<5,$L(SEQUENCE) D ^DIE ;Stuff mnem with seq value
 . S DR=$S($P(CNODE0,U,4)="CL":"2;4",1:4) D ^DIE
 . L -^TIU(8925.1,TIUFCDA,10,TENDA)
 S VALMBCK="R",MSG=" Entry Created"
 I SHARED S MSG=" Entry Added"
 W !!,MSG,! S CREATE=1 L -^TIU(8925.1,CREATEDA) G ANOTHER
CREAX L -^TIU(8925.1,+$G(CREATEDA))
 L -^TIU(8925.1,+$G(TIUFCDA),10,+$G(TENDA))
 I $D(DTOUT) S VALMBCK="Q" Q
 S PLINENO=$O(^TMP("TIUF1IDX",$J,"DAF",TIUFCDA,""))
 S PINFO=^TMP("TIUF1IDX",$J,PLINENO)
 D PARSE^TIUFLLM(.PINFO)
 N TIUFSAVE D SAVE(.PINFO) ;Speeds up BUFENTRY^TIUFLLM2
 S VALMCNT=VALMCNT-PINFO("XPDLCNT") D COLLAPSE^TIUFH1(.PINFO)
 D EXPAND1^TIUFH1(.PINFO) S VALMCNT=VALMCNT+PINFO("XPDLCNT")
 S TIUFCITM=$S($P(PINFO,U,3):1,1:0)
 I CREATE K TIUFCMSG D
 . S TIUFCMSG(1)=" Select "_$S(TIUFCTYP="DC":"TITLE",1:"CLASS/DOCUMENTCLASS")_" to create a new "_TIUFCNM
 . S TIUFCMSG(2)="or to Go Down a Level, Select NEXT LEVEL."
 . I VALMCNT>VALM("LINES") S TIUFCMSG(2)="or to Go Down a Level, Screen to (+/-) Desired ",TIUFCMSG(3)=TIUFCNM_" Item, and Select NEXT LEVEL."
 I $G(TIUFFULL) S VALMBCK="R" D RESET^TIUFXHLX
 S LINENO=+$O(^TMP("TIUF1IDX",$J,"DAF",+$G(CREATEDA),0))
 I LINENO,LINENO<VALMBG!(LINENO>(VALMBG+VALM("LINES")-1)) S VALMBG=LINENO
 Q
SAVE(EINFO) ;
 N LINENO,FILEDA
 F LINENO=+EINFO+1:1:+EINFO+EINFO("XPDLCNT")  D  Q:$D(DTOUT)
 . S FILEDA=$P(^TMP("TIUF1IDX",$J,LINENO),U,2)
 . S TIUFSAVE(FILEDA)=^TMP("TIUF1",$J,LINENO,0)
 Q
 ;
