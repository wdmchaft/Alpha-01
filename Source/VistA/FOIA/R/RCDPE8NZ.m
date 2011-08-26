RCDPE8NZ ;ALB/TMK-report of unlinked payments in 5287/8NZZ ;19 MAR 2003
 ;;4.5;Accounts Receivable;**173,212,208**;Mar 20, 1995
 ;;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
EN N RCDPPADT,%ZIS,ZTRTN,ZTSAVE,ZTDESC,POP,ZTSK,DIR,X,Y
 S DIR("A")="Start with Deposit Date: FIRST// ",DIR(0)="DOA^:"_DT_":AEP" W ! D ^DIR K DIR
 I $D(DUOUT)!$D(DTOUT) Q
 S RCDPPADT=+Y
 ; Ask device
 S %ZIS="QM" D ^%ZIS G:POP PRQ
 I $D(IO("Q")) D  G PRQ
 . S ZTRTN="PR^RCDPE8NZ",ZTDESC="AR - List of unlinked EFT deposit payments"
 . S ZTSAVE("RCDPPADT")=""
 . D ^%ZTLOAD
 . W !!,$S($D(ZTSK):"Your task number "_ZTSK_" has been queued.",1:"Unable to queue this job.")
 . K ZTSK,IO("Q") D HOME^%ZIS
 U IO
 ;
PR ; Entrypoint for queued job
 ;
 N RCCT,RCOK,RCPG,RCEFT,RCEFT1,RCDATA,RCDATA0,RCDA,RCREC,RCSTAT,RCDT,RCTOT,RCEFTD,RCSTOP,RCRDT,X,Y,Z
 ;
 ;  get list of unlinked EFT deposit data
 K ^TMP("RCDPE8NZZ_EFT",$J) ; subscripts: dep date,EFT ien,EFT det ien
 ;  Data is FMS doc indicator^FMS doc #^FMS Doc Status
 ;    FMS doc indicator = -1:no receipt  -2:no FMS doc  1:FMS doc exists
 ;
 S (RCCT,RCSTOP)=0
 S RCEFT1="" F  S RCEFT1=$O(^RCY(344.3,"ARDEP",RCEFT1)) Q:RCEFT1=""!RCSTOP  S RCDA=0 F  S RCDA=$O(^RCY(344.3,"ARDEP",RCEFT1,RCDA)) Q:'RCDA  D  Q:RCSTOP
 . S RCDATA=$G(^RCY(344.3,RCDA,0)),RCDT=$P(RCDATA,U,7),RCTOT=0
 . Q:RCDT<RCDPPADT  ; Before date selected
 . Q:'$P(RCDATA,"^",8)  ; no payment amt
 . S RCEFT=0 F  S RCEFT=$O(^RCY(344.31,"B",RCDA,RCEFT)) Q:'RCEFT!RCSTOP  S RCDATA0=$G(^RCY(344.31,RCEFT,0)) D  Q:RCSTOP  S:RCTOT ^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCDA)=RCTOT
 .. S RCCT=RCCT+1
 .. I '(RCCT#100),$D(ZTQUEUED),$$S^%ZTLOAD S (RCSTOP,ZTSTOP)=1 K ZTREQ Q
 .. S RCREC=+$P($G(^RCY(344.4,+$P(RCDATA0,U,10),0)),U,8)
 .. S RCOK=1
 .. I 'RCREC D  Q
 ... I $P(RCDATA0,U,16)'="" Q
 ... S ^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCDA,RCEFT)=-1,RCTOT=RCTOT+$P(RCDATA0,U,7) S RCOK=0  ; No receipt
 .. S RCSTAT=$$FMSSTAT^RCDPUREC(RCREC)
 .. Q:$P(RCSTAT,U,2)["ACCEPTED"!($P(RCSTAT,U,2)["FINAL")!($P(RCSTAT,U,2)["ON LINE ENTRY")
 .. S RCTOT=RCTOT+$P(RCDATA0,U,7)
 .. I RCSTAT=-1 S ^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCDA,RCEFT)="-2^^"_$P(RCSTAT,U) Q  ; No FMS doc
 .. S ^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCDA,RCEFT)="1^"_$P(RCSTAT,U,1,2)
 ;
 S (RCPG,RCDT)=0,RCRDT=$$FMTE^XLFDT($$NOW^XLFDT(),2)
 F  S RCDT=$O(^TMP("RCDPE8NZZ_EFT",$J,RCDT)) Q:'RCDT  D  Q:RCSTOP
 . I 'RCPG!(($Y+5)>IOSL) D  Q:RCSTOP
 .. D HDR(RCRDT,.RCPG,.RCSTOP)
 . E  W !
 . W ! S Z="DEPOSIT DATE: "_$$FMTE^XLFDT(RCDT,1) W ?(80-$L(Z)\2),Z,!
 . ;
 . S RCEFT1=0 F  S RCEFT1=$O(^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCEFT1)) Q:'RCEFT1  D
 .. S RCCT=RCCT+1
 .. I '(RCCT#100),$D(ZTQUEUED),$$S^%ZTLOAD S (RCSTOP,ZTSTOP)=1 W:$G(RCPG) !!,"TASK STOPPED BY USER!!" K ZTREQ Q
 .. S RCDATA0=$G(^RCY(344.3,RCEFT1,0))
 .. I ($Y+5)>IOSL D HDR(RCRDT,.RCPG,.RCSTOP) Q:RCSTOP
 .. W !,$J("",4),$E($P(RCDATA0,U,6)_$S('$$HACEFT^RCDPEU(RCEFT1):"",1:"-HAC")_$J("",10),1,10)_"  "_$E($$FMTE^XLFDT($P(RCDATA0,U,7),2)_$J("",16),1,16)_"  "_$E($J(+$P(RCDATA0,U,8),"",2)_$J("",20),1,20)
 .. W "  "_$J(+$G(^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCEFT1)),"",2)
 .. ;
 .. S RCEFT=0 F  S RCEFT=$O(^TMP("RCDPE8NZZ_EFT",$J,RCDT,RCEFT1,RCEFT)) Q:'RCEFT  S RCDATA=$G(^(RCEFT)),RCEFTD=$G(^RCY(344.31,RCEFT,0)) D
 ... I ($Y+5)>IOSL D HDR(RCRDT,.RCPG,.RCSTOP) Q:RCSTOP
 ... W !,$J("",6)_$E($E($P(RCEFTD,U,2),1,18)_"/"_$E($P(RCEFTD,U,3),1,10)_$J("",30),1,30)_$E($P(RCEFTD,U,4)_$J("",20),1,20)_" "
 ... W $E($J(+$P(RCEFTD,U,7),"",2)_$J("",12),1,12)_" "_$S($P(RCEFTD,U,9)'="":$P($G(^RCY(344,+$P(RCEFTD,U,9),0)),U),1:"NO RECEIPT")
 ... S Z=$P(RCEFTD,U,8)
 ... W !,$J("",8)_$E($S('Z:"UNMATCHED",Z=2:"PAPER EOB",1:"MATCHED TO ERA #: "_$P(RCEFTD,U,10)_$S(Z=-1:" (TOTALS MISMATCH)",1:""))_$J("",40),1,40)_"  "
 ... W $S($P(RCDATA,U)<0:"NO FMS DOCUMENT",1:$P(RCDATA,U,2)_" - "_$P(RCDATA,U,3)),!
 ;
 I $D(ZTQUEUED) S ZTREQ="@"
 I '$D(ZTQUEUED) D ^%ZISC
 G:RCSTOP PRQ
 I $E(IOST,1,2)="C-" D ASK(.RCSTOP)
 ;
PRQ K ^TMP("RCDPE8NZZ_EFT",$J)
 Q
 ;
HDR(RCRDT,RCPG,RCSTOP) ; Print header data
 I 'RCSTOP,RCPG D ASK(.RCSTOP) Q:RCSTOP
 I RCPG!($E(IOST,1,2)="C-") W @IOF,*13
 S RCPG=RCPG+1
 W !,$S(RCDPPADT'>0:"ALL ",1:"")_"UNAPPLIED EFT PAYMENT DEPOSITS"_$S(RCDPPADT>0:" AFTER "_$$FMTE^XLFDT(RCDPPADT,2),1:""),?50,RCRDT,?70,"PAGE: ",RCPG
 W !!,"    DEPOSIT #   DEPOSIT DATE      TOT AMT OF DEPOSIT    TOT AMT UNPOSTED"
 W !,$E("      PAYER/ID"_$J("",36),1,36)_"TRACE #              PAYMENT AMT  RECEIPT #",!,$J("",8)_$E("ERA MATCHED"_$J("",40),1,40)_"  FMS DOC #/STATUS"
 W !,$TR($J("",IOM)," ","=")
 Q
 ;
ASK(RCSTOP) ;
 I $E(IOST,1,2)'["C-" Q
 N DIR,DIROUT,DIRUT,DTOUT,DUOUT
 S DIR(0)="E" W ! D ^DIR
 I ($D(DIRUT))!($D(DUOUT)) S RCSTOP=1 Q
 Q
 ;
