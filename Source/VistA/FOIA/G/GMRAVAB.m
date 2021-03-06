GMRAVAB ;HIRMFO/RM-BULLETIN SENT TO VERIFY A/AR ; 12/18/90
 ;;4.0;Adverse Reaction Tracking;;Mar 29, 1996
EN1 ; SEND BULLETIN TO ALL VERIFIERS INDICATING A/AR NEEDS VERIFICATION
 D KILL^XM
 N GMRAGRUP,%
 S GMRANAM="",GMRALOC="",GMRASSN=""
 ; Build XMB array
 D VAD^GMRAUTL1($P(GMRAPA(0),U),"",.GMRALOC,.GMRANAM,"",.GMRASSN)
 I GMRALOC'="",+$G(^DIC(42,GMRALOC,44)) S GMRALOC=$P($G(^SC(+$G(^DIC(42,GMRALOC,44)),0)),U)
 I GMRALOC="" S GMRALOC="OUT PATIENT"
 S XMB="GMRA VERIFY ALLERGY"
 S XMB(1)=GMRANAM
 S XMB(2)=$P(GMRAPA(0),"^",2)
 S XMB(3)=$S(GMRALOC'="":GMRALOC,1:"Outpatient"),XMB(4)=GMRASSN
 S XMB(5)=$S($P(GMRAPA(0),U,6)="o":"Observed",$P(GMRAPA(0),U,6)="h":"Historical",1:"")
 ; Build XMT array
 F %=1:1:$L($P(GMRAPA(0),"^",20)) D
 .S GMRAGRUP=$E($P(GMRAPA(0),"^",20),%)
 .S XMY("G.GMRA VERIFY "_$S(GMRAGRUP="D":"DRUG",GMRAGRUP="F":"FOOD",1:"OTHER")_" ALLERGY")=""
 .Q
 K GMRAREC I $D(^GMR(120.8,GMRAPA,10,0)) D
 .S GMRAOTH=$O(^GMRD(120.83,"B","OTHER REACTION",0))
 .S GMRAREC=0 F  S GMRAREC=$O(^GMR(120.8,GMRAPA,10,GMRAREC)) Q:GMRAREC'>0  D
 ..S X=$G(^GMR(120.8,GMRAPA,10,GMRAREC,0))
 ..S GMRAREC(GMRAREC)=$S($P(X,U)'=GMRAOTH:$P($G(^GMRD(120.83,+$P(X,U),0)),"^"),1:$P(X,U,2))
 ..I +$P(X,U,4)>0 D
 ...N GMRASP,GMRAI S GMRASP=" "
 ...S GMRAREC(GMRAREC)=$E(GMRAREC(GMRAREC),1,40)
 ...F GMRAI=$L(GMRAREC(GMRAREC)):1:40 S GMRAREC(GMRAREC)=GMRAREC(GMRAREC)_GMRASP
 ...S GMRAREC(GMRAREC)=GMRAREC(GMRAREC)_" "_$$FMTE^XLFDT($P(X,U,4),1)
 ...Q
 ..Q
 .Q
 K GMRATXT
 I $D(GMRAREC)=11 S GMRACNT=3,GMRAREC=0 D
 .S GMRATXT(1)="Signs/Symptoms                            Date Observed"
 .S GMRATXT(2)=$$REPEAT^XLFSTR("-",60)
 .F  S GMRAREC=$O(GMRAREC(GMRAREC)) Q:GMRAREC<1  S GMRATXT(GMRACNT)=GMRAREC(GMRAREC),GMRACNT=GMRACNT+1
 .Q
 I $D(GMRATXT) S XMTEXT="GMRATXT("
 D ^XMB
 K XMB,XMY,GMRACNT,GMRAREC,GMRATXT,XMTEXT
 Q
