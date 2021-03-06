PSJ53P1 ;BIR/DB - API FOR INFORMATION FROM FILE 53.1; 5 Sep 03
 ;;5.0; INPATIENT MEDICATIONS ;**172**;16 DEC 97;Build 13
PSJ(PSJIEN,LIST) ;
 ;PSJIEN - INTERNAL ENTRY NUMBER [REQUIRED]
 ;LIST: Subscript name used in ^TMP global [REQUIRED]
 I $G(PSJIEN)="" Q
 I $G(LIST)="" Q
 K ^TMP($J,LIST),DA,^UTILITY("DIQ1",$J),DIQ
 I $G(PSJIEN)]"" S DA=PSJIEN I '$D(^PS(53.1,DA,0)) G RET0
 K ^UTILITY("DIQ1",$J),DIC S DIC=53.1,DR=".01;1;3;7;10;25;26;28;108;109",DIQ(0)="IE" D EN^DIQ1
 I '$D(^UTILITY("DIQ1",$J)) G RET0
 S ^TMP($J,LIST,0)=1,PSJDA=DA
 S PSJTXT=^UTILITY("DIQ1",$J,53.1,DA,.01,"E")
 F X=.01,1,3,7,10,26,28,25,108,109 S ^TMP($J,LIST,DA,X)=$G(^UTILITY("DIQ1",$J,53.1,DA,X,"I"))
 F X=1,3,7,10,28,25,108 S ^TMP($J,LIST,DA,X)=$S($G(^UTILITY("DIQ1",$J,53.1,DA,X,"E"))'="":^TMP($J,LIST,DA,X)_"^"_$G(^UTILITY("DIQ1",$J,53.1,DA,X,"E")),1:"")
 S ^TMP($J,LIST,"B",PSJTXT,DA)=""
 S PSJDRG=0 F  S PSJDRG=$O(^PS(53.1,PSJDA,1,PSJDRG)) Q:PSJDRG'>0  D
 .S DA=PSJDA,DIC=53.1,DR=2,DR(53.11)=".01;.02",DIQ(0)="IE",DA(53.11)=PSJDRG D EN^DIQ1 Q:'$D(^UTILITY("DIQ1",$J))
 .S ^TMP($J,LIST,PSJDA,"DDRUG",PSJDRG,.01)=$G(^UTILITY("DIQ1",$J,53.11,PSJDRG,.01,"I"))
 .S ^TMP($J,LIST,PSJDA,"DDRUG",PSJDRG,.02)=$G(^UTILITY("DIQ1",$J,53.11,PSJDRG,.02,"I"))
 .S ^TMP($J,LIST,PSJDA,"DDRUG",PSJDRG,.01)=$S($G(^UTILITY("DIQ1",$J,53.11,PSJDRG,.01,"E"))'="":^TMP($J,LIST,PSJDA,"DDRUG",PSJDRG,.01)_"^"_$G(^UTILITY("DIQ1",$J,53.11,PSJDRG,.01,"E")),1:"")
 .S ^TMP($J,LIST,PSJDA,"DDRUG",0)=$G(^TMP($J,LIST,PSJDA,"DDRUG",0))+1
 I '$D(^TMP($J,LIST,PSJDA,"DDRUG",0)) S ^TMP($J,LIST,PSJDA,"DDRUG",0)="-1^NO DATA FOUND"
 K PSJIEN,DA,X,PSJTXT,DR,DIC,^UTILITY("DIQ1",$J),DIQ,PSJDA,PSJDRG
 Q
RET0 ;return no data
 K PSJIEN,PSJTXT S ^TMP($J,LIST,0)="-1^NO DATA FOUND" Q
