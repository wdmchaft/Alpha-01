PRCSP1F ;SF-ISC/LJP-PRINT COMPLETED PO LIST FOR SCP $ RECONCILIATION ;3/19/91  16:53
V ;;5.1;IFCAP;;Oct 20, 2000
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 D EN^PRCSUT G W2^PRCSP1B:'$D(PRC("SITE")),EXIT^PRCSP1B:Y<0 S PRCSZZ=PRC("SITE")_"-"_PRC("FY")_"-"_PRC("QTR")_"-"_$P(PRC("CP")," ")
 S L=0,DIC="^PRCS(410,",FLDS="[PRCSCSCP]",BY="@.01,54",FR=PRCSZZ_"-0001,",TO=PRCSZZ_"-9999,",DHD="PO/SCP $ RECONCILIATION"_"  "_+PRC("CP")_"-"_PRC("FY")_"-"_PRC("QTR")
 ;S DIS(0)="I $D(D0),$D(^PRCS(410,D0,10)) S ZX=+$P(^(10),U,4) I ZX=$O(^PRCD(442.3,""C"",20,0))!(ZX=$O(^PRCD(442.3,""AC"",100,0)))!(ZX=$O(^PRCD(442.3,""C"",21,0)))"
 D EN1^DIP K DHD,DIS(0),BY,FR,TO,FLDS,PRCSZZ,ZX Q
OPN ;LIST OF OPEN (INCOMPLETE) 1358'S
 D EN^PRCSUT G W2^PRCSP1B:'$D(PRC("SITE")),EXIT^PRCSP1B:Y<0 S PRCSZZ=PRC("SITE")_"-"_PRC("FY")_"-"_PRC("QTR")_"-"_$P(PRC("CP")," ")
 S DHD="OPEN 1358 DAILY RECORDS",L=0,DIC="^PRC(424,"
 S FLDS=".01,.05,.1",BY="[PRCEC OPN1358]",(FR,TO)="",DIS(0)="I $P($G(^PRC(424,D0,0)),""-"")=PRC(""SITE""),$P($G(^PRC(424,D0,0)),U,2),+$P($G(^PRC(442,$P(^(0),U,2),0)),U,3)=+PRC(""CP"")"
 D EN1^DIP K PRCSZZ,DHD,L,DIC,FLDS,BY,FR,TO Q
MDL ;MULTIPLE DELIVERY SCHEDULE LIST
 D EN3^PRCSUT G W2^PRCSEB0:'$D(PRC("SITE")),EXIT^PRCSEB0:Y<0
 S DIC="^PRCS(410,",DIC(0)="AEMQZ",DIC("S")="I $D(^(3)),+^(3)=+PRC(""CP""),$P(^PRCS(410,+Y,0),U,5)=PRC(""SITE""),$O(^PRCS(410,+Y,""IT"",1,2,0))" D ^PRCSDIC K DIC("S") I Y<0 K DIC Q
 S L=0,FLDS="[PRCSMDS]",BY="@NUMBER",(FR,TO)=+Y D EN1^DIP K L,FLDS,BY,FR,TO Q
 ;
 Q
